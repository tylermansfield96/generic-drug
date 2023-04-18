#!/usr/bin/env Rscript

# Header -----------------------------------------------------------------------
# Proj: Bottle of Lies
# Author: Evan Flack (evanjflack@gmail.com)
# Desc: Makes a subsetted pde data set of only claims for a given compound in a
#       month when the beneficiary is included included in the sample

# Start Script -----------------------------------------------------------------
package_list <- c("stringr", "foreach", "doParallel", "lubridate")
source("../00_pre_process/start_script.R")

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type = 'character', default = "20pct"),
  make_option(c("-f", "--first_year"), type = 'integer', default = 2010),
  make_option(c("-l", "--last_year"), type = 'integer', default = 2013)
)
unpack_opt(option_list)

# Data Read In -----------------------------------------------------------------
message("Reading in data...")

# Classes of interest
# classes <- readRDS(paste0(lib_base_data, "classes_of_interest.rds")) %>%
#   .[name == "lisinopril", ]

classes <- fread(paste0(lib_base_data, "topdrugs_20pct.csv")) %>%
  .[1:50, .(name = key_word, g_rxcui)]

# NDC9 to g_rxcui xwalk
ndc9_g_rxcui_xwalk <- fread(paste0(lib_base_data, "ndc9_g_rxcui_xwalk.csv")) %>%
  .[, lab_prod := str_pad(lab_prod, 9,pad = "0")] %>%
  setnames("mg", "strength") %>%
  .[, .(g_rxcui, rxcui, lab_prod, name, strength, branded)]

# Subset Samples ---------------------------------------------------------------

registerDoParallel(cores = length(years))
ret <- foreach(year = years) %dopar%
  {
    message("Start ", year, ".")
    # Month-level indicators for whether the bene was in a PDP
    sample <- fread(paste0(lib_base_data, "month_level_sample_", year, "_",
                           pct, ".csv")) %>%
      .[, .(bene_id, rfrnc_yr, month, pdp_ind)]

    # Read in pde claims, subset to classes of interest
    pde <- fread(paste0(lib_base_data, "sample_pde_", year, "_", pct,
                        ".csv")) %>%
      .[, .(bene_id, srvc_dt, rfrnc_yr, srvc_mo, lab_prod, dayssply, prvdr_id,
            prvdqlfr, totalcst, ptpayamt, othtroop, lics_amt, plro_amt, bnftphas)] %>%
      .[, lab_prod := str_pad(lab_prod, 9, pad = "0")] %>%
      merge(ndc9_g_rxcui_xwalk, by = "lab_prod") %>%
      .[, srvc_day := day(mdy(srvc_dt))] %>%
      # .[, name := NULL] %>%
      setnames("srvc_mo", "month")

    # Only keep observations from when benes were in a pdp
    pde %<>%
      merge(sample, by = c("bene_id", "rfrnc_yr", "month")) %>%
      .[pdp_ind == 1] %>%
      .[, pdp_ind := NULL]

    # Loop through classes, and save samples
    message("Looping through classes ", year, ".")
    for (i in classes$g_rxcui) {
      script_sample <- pde %>%
        .[g_rxcui == i]

      # Export
      fwrite(script_sample, paste0(lib_base_data, "compound_sample_", i,
                                   "_", year, "_", pct, ".csv"))
    }
    message("Done ", year, ".")
  }

# End --------------------------------------------------------------------------

stopImplicitCluster()