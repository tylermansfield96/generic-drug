#!/usr/bin/env Rscript

# Header -----------------------------------------------------------------------
# Proj: Bottle of Lies
# Author: Tyler Mansfield (tyler.mansfield96@gmail.com)
# Desc: Makes a full pde data set of claims for all compounds with grxcui when the 
#       beneficiary is included included in the sample

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

# NDC9 to g_rxcui xwalk
ndc9_g_rxcui_xwalk <- fread(paste0(lib_base_data, "ndc9_g_rxcui_xwalk.csv")) %>%
  .[, lab_prod := str_pad(lab_prod, 9,pad = "0")] %>%
  setnames("mg", "strength") %>%
  .[, .(g_rxcui, rxcui, lab_prod, strength, branded, concept_name)]

# Subset Samples ---------------------------------------------------------------

registerDoParallel(cores = length(years))
ret <- foreach(year = years) %dopar%
  {
    message("Begin ", year, ".")
    # Month-level indicators for whether the bene was in a PDP
    sample <- fread(paste0(lib_base_data, "month_level_sample_", year, "_",
                           pct, ".csv")) %>%
      .[, .(bene_id, rfrnc_yr, month, pdp_ind)]
    
    # Read in pde claims, subset to classes of interest
    pde <- fread(paste0(lib_base_data, "sample_pde_", year, "_", pct,
                        ".csv")) %>%
      .[, .(bene_id, srvc_dt, rfrnc_yr, srvc_mo, lab_prod, dayssply, prvdr_id,
            prvdqlfr, totalcst, ptpayamt, othtroop, lics_amt, plro_amt)] %>%
      .[, lab_prod := str_pad(lab_prod, 9, pad = "0")] %>%
      merge(ndc9_g_rxcui_xwalk, by = "lab_prod", all.x = TRUE) %>%
      .[, srvc_day := day(mdy(srvc_dt))] %>%
      setnames("srvc_mo", "month")
    
    # Only keep observations from when benes were in a pdp
    pde %<>%
      merge(sample, by = c("bene_id", "rfrnc_yr", "month")) %>%
      .[pdp_ind == 1] %>%
      .[, pdp_ind := NULL]
    
    fwrite(pde, paste0(lib_base_data, "sample_pde_with_grxcui_", year, "_", pct, ".csv"))
    message("Done ", year, ".")
  }
stopImplicitCluster()
