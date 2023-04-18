#!/usr/bin/env Rscript

################################################################################
################################################################################
################################################################################
# NEEDS WORK
################################################################################
################################################################################
################################################################################

# Header -----------------------------------------------------------------------
# Proj: Bottle of Lies
# Author: Evan Flack (evanjflack@gmail.com) and
#         Tyler Mansfield (tyler.mansfield96@gmail.com)
# Desc: Identifies unique diagnoses for conditions of interest

# Start Script -----------------------------------------------------------------
package_list <- c("stringr", "fastDummies", "foreach", "doParallel",
                  "readstata13")
source("../00_pre_process/start_script.R")

option_list = list(
  make_option(c("-p", "--pct"), type='character', default = "20pct"),
  make_option(c("-f", "--first_year"), type='integer', default = 2010),
  make_option(c("-l", "--last_year"), type='integer', default = 2013)
)
unpack_opt(option_list)

# Prep ADR Xwalk ---------------------------------------------------------------
message("Prepping ADR xwalk...")

icd_xwalk <- readstata13::read.dta13(paste0(lib_base_data,
                                            "icd10cmtoicd9gem.dta"))

adr_codes <- fread(paste0(lib_base_data, "adr_icd10_codes.csv"))

icd_xwalk %<>%
  as.data.table() %>%
  .[, .(icd10cm, icd9cm)] %>%
  setnames(names(.), c("icd10", "icd9")) %>%
  .[, icd10_3 := substr(icd10, 1, 3)] %>%
  .[, icd10_4 := substr(icd10, 1, 4)]

adr_codes %<>%
  setnames(names(.), c("icd10", "meaning", "rating")) %>%
  .[, icd10 := gsub("\\.", "", icd10)] %>%
  .[, icd10_len := str_length(icd10)]

adr_codes4 <- adr_codes %>%
  .[icd10_len == 4, ] %>%
  merge(icd_xwalk, by.x = "icd10", by.y = "icd10_4", all.x = T) %>%
  .[, .(icd9, meaning, rating)]

adr_codes3 <- adr_codes %>%
  .[icd10_len == 3, ] %>%
  merge(icd_xwalk, by.x = "icd10", by.y = "icd10_3", all.x = T) %>%
  .[, .(icd9, meaning, rating)]

adr_codes <- rbind(adr_codes3, adr_codes4) %>%
  .[!is.na(icd9)] %>%
  .[order(rating)] %>%
  .[, .SD[1], by = .(icd9)] %>%
  .[!(rating %in% c("E", "V", "U"))]

rm(icd_xwalk, adr_codes3, adr_codes4)

# ID Diagnoses Outcomes --------------------------------------------------------
message("Identifying diagnoses...")

registerDoParallel(cores = length(years))
ret <- foreach(year = years) %dopar% {
  message(paste0("Begin ", year, "."))

  dgns_vars <- paste0("dgnscd", seq(1, 9))
  ip_vars <- c("bene_id", "clm_id", "from_dt", dgns_vars)

  # Read in IP claims, subset to claim line 1
  diags <- fread(paste0(lib_base_data, "sample_ip_", year, "_", pct,
                        ".csv")) %>%
    setnames(names(.), tolower(names(.))) %>%
    .[clm_ln == 1, ] %>%
    .[, ip_vars, with = FALSE]

  # Reshape 10 diagnoses codes to long, only keep unique diagnoses by bene/date
  diags <- suppressWarnings(melt(diags, id.var = c("bene_id", "clm_id",
                                                   "from_dt"),
                                 value.name = "dgnscd",
                                 variable.name = "code_num")) %>%
    .[!(is.na(dgnscd) | dgnscd == ""), ] %>%
    .[, code_num := as.numeric(gsub("dgnscd", "", code_num))] %>%
    .[, clm_id := NULL] %>%
    unique()

  # Make indicators for diagnoses of interest, remove obs without any diagnoses
  # of interest
  ind_vars <- c("adr", "ami", "stroke")
  diags %<>%
    .[, adr := ifelse(dgnscd %in% adr_codes$icd9, 1, 0)] %>%
    .[, ami := ifelse(substr(dgnscd, 1, 3) == "410", 1, 0)] %>%
    .[, stroke := ifelse(substr(dgnscd, 1, 3) %in%
                           as.character(seq(433, 435)), 1, 0)]

  # Subset to primary diagnoses
  p_diags <- diags %>%
    .[code_num == 1, ]

  # Export
  fwrite(diags, paste0(lib_base_data, "sample_ip_all_diags_long_", year, "_",
                       pct, ".csv"))

  fwrite(p_diags, paste0(lib_base_data, "sample_ip_primary_diags_long_", year,
                         "_", pct, ".csv"))

  message("Done ", year, ".")
}
stopImplicitCluster()