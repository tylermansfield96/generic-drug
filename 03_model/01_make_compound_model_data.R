#!/usr/bin/env Rscript

# Header -----------------------------------------------------------------------
# Proj: Bottle of Lies
# Author: Evan Flack (evanjflack@gmail.com) and
#         Tyler Mansfield (tyler.mansfield96@gmail.com)
# Desc: Makes class-specific model data of all generic fills made after any
#       generic exclusivity period

# Start Script -----------------------------------------------------------------
package_list <- c("stringr", "lubridate", "foreach", "doParallel")
source("../00_pre_process/start_script.R")

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type = 'character', default = "20pct"),
  make_option(c("-f", "--first_year"), type = 'integer', default = 2010),
  make_option(c("-l", "--last_year"), type = 'integer', default = 2013)
)
unpack_opt(option_list)

# Read in Data -----------------------------------------------------------------
message("Reading in data...")

# Classes of interest

classes <- fread(paste0(lib_base_data, "topdrugs_20pct.csv")) %>%
  .[1:50, .(name = key_word, g_rxcui)]

# Labeler code -> name xwalk
lab_names <- fread(paste0(lib_base_data, "ndc_lab_codes_2016.csv")) %>%
  setnames(names(.), c("lab", "lab_name")) %>%
  .[, lab := str_pad(lab, 5, pad = "0")]

year_level_controls <- read_and_combine(lib_base_data, "year_level_controls",
                                        years, pct) %>%
  .[, .(bene_id, rfrnc_yr, race, sex, zip5, median_zip_income, n_grxcui_year,
        total_spending_year, total_oop_year, total_scripts_year)]

month_level_sample <- read_and_combine(lib_base_data, "month_level_sample",
                                       years, pct) %>%
  .[, .(bene_id, rfrnc_yr, month, cntrct, pbp, dual_cstshr_ind, age_in_mo)]

# Iterate Over Classes ---------------------------------------------------------
message("Iterating over ", nrow(classes), " classes...")

registerDoParallel(cores = min(nrow(classes), detectCores() - 1))
ret <- foreach(g_rxcui_name1 = classes$name,
               g_rxcui = classes$g_rxcui
               ) %dopar%
  {
    message("Start ", g_rxcui_name1, ".")

    # Read in Data -------------------------------------------------------------

    # All scripts in class from PDP months
    scripts <- read_and_combine(lib_base_data,
                                paste0("compound_sample_", g_rxcui),
                                years, pct)

    # Build Sample -------------------------------------------------------------

    # Only scripts with an NPI prescriber ID (the overwhelming majority
    # 2010 onward)
    scripts %<>%
      .[prvdqlfr == 1, ]

    # Format date and labeler variables
    scripts %<>%
      .[, .(bene_id, rfrnc_yr, month, srvc_dt, rxcui, lab_prod, prvdqlfr,
            prvdr_id, strength, dayssply, totalcst, ptpayamt, branded, bnftphas)] %>%
      .[, srvc_dt := mdy(srvc_dt)] %>%
      .[, lab_prod := str_pad(lab_prod, 9, pad = "0")] %>%
      .[, lab := substr(lab_prod, 1, 5)]

    # Combine lab codes for those with same manufacturer name
    lab_names_edit <- scripts %>%
      .[, .(.N), by = .(lab)] %>%
      unique() %>%
      merge(lab_names, by = "lab", all.x = T) %>%
      .[, lab_name_short := str_split_fixed(lab_name, " ", 2)[, 1]] %>%
      .[order(lab_name_short, -N), ] %>%
      .[, lab_comb := lab[1], by = lab_name_short] %>%
      setnames("lab_name", "lab_name_long") %>%
      setnames("lab_name_short", "lab_name") %>%
      .[, .(lab, lab_name, lab_name_long, lab_comb)] %>%
      .[lab_name == "Dr.", lab_name := "Dr. Reddy's"]

    scripts %<>%
      merge(lab_names_edit, by = "lab")

    # Combines small manufacturers (those with less than a half of a
    # percent of all claims)
    scripts %<>%
      .[, lab_obs := .N, by = lab_name] %>%
      .[, perc := lab_obs/.N] %>%
      .[perc < .005, `:=`(lab_name = "Small",
                          lab_name_long = "Small manufacturers")]

    # Subset variables
    scripts %<>%
      .[, .(bene_id, rfrnc_yr, month, srvc_dt, lab, lab_name, lab_name_long,
            lab_comb, prvdr_id, totalcst, ptpayamt, strength, dayssply, branded,
            bnftphas)]

    # Merge in Month Level Sample Characteristics ------------------------------
    scripts %<>%
      merge(month_level_sample, by = c("bene_id", "rfrnc_yr", "month")) %>%
      merge(year_level_controls, by = c("bene_id", "rfrnc_yr"))

    # Defining time and contract FE variables
    scripts %<>%
      .[, week := week(srvc_dt)] %>%
      .[, year_week := paste0(rfrnc_yr, "_", week)] %>%
      .[, year_month := paste0(rfrnc_yr, "_", month)] %>%
      .[, cntrct_pbp_rfrnc_yr := paste(cntrct, pbp, rfrnc_yr, sep = "_")]

    # Order by beneficiary and fill date
    scripts %<>%
      .[order(bene_id, srvc_dt), ] %>%
      .[, bene_claim := seq(1, .N), by = bene_id]

    # Export -------------------------------------------------------------------

    fwrite(scripts, paste0(lib_base_data, "generic_sample_", g_rxcui_name1, "_",
                           pct, ".csv"))

    message("Done ", g_rxcui_name1, ".")
  }
stopImplicitCluster()

message("Done all.")