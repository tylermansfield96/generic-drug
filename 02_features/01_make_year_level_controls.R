#!/usr/bin/env Rscript

# Header -----------------------------------------------------------------------
# Proj: Bottle of Lies
# Author: Evan Flack (evanjflack@gmail.com) and
#         Tyler Mansfield (tyler.mansfield96@gmail.com)
# Desc: Makes year level control variables

# Start Script -----------------------------------------------------------------
package_list <- c("stringi", "stringr", "foreach", "doParallel", "tidycensus")
source("../00_pre_process/start_script.R")

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type = 'character', default = "20pct"),
  make_option(c("-f", "--first_year"), type = 'integer', default = 2010),
  make_option(c("-l", "--last_year"), type = 'integer', default = 2013)
)
unpack_opt(option_list)
census_api_key("fc43f75451d32b8acf64d44c285c3f6aebfc222f") #Tyler's API key

# Reading in Data --------------------------------------------------------------
message("Reading in data...")

registerDoParallel(cores = length(years))
ret <- foreach(year = years, .packages = c("stringi",
                                           "stringr",
                                           "tidycensus")) %dopar%
  {
    message("Starting ", year)
    
    constant_vars <- c("race", "sex", "state_cd", "age1", "birth_mo",
                       "bene_zip")
    # Read in bsf file
    controls <- fread(paste0(lib_base_data, "sample_", year, "_", pct,
                            ".csv"))

    # Subset variables, format zipcode
    controls %<>%
      .[, .SD[1], by = bene_id, .SDcols = constant_vars] %>%
      .[, rfrnc_yr := year] %>%
      .[, bene_zip := str_pad(bene_zip, 9, pad = "0")] %>%
      .[, zip5 := substr(bene_zip, 1, 5)] %>%
      .[, bene_zip := NULL]
    
    message("Finding data from sample PDE ", year)
    
    #Find number of unique drugs each person is on this year
    n_grxcui_bene <- fread(paste0(lib_base_data, "sample_pde_with_grxcui_",
                                     year, "_", pct, ".csv")) %>%
      .[, .(n_grxcui_year = length(unique(g_rxcui)),
            total_spending_year = sum(totalcst),
            total_oop_year = sum(ptpayamt),
            total_scripts_year = .N), by = bene_id]

    controls %<>% merge(n_grxcui_bene, by = "bene_id")
    
    message("Getting zip code info ", year)
    
    #Create zip code income variable
    zipcode_income <- get_acs(
      geography = "zcta",
      variables = "B19013_001",
      year = max(year,2011)) %>% #The 5-year census data only begins at 2011
      as.data.table() %>%
      .[, zip5 := stri_sub(NAME,-5)] %>%
      .[, .(zip5, median_zip_income = estimate)]
    
    us_median <- get_acs(
      geography = "us",
      variables = "B19013_001",
      year = max(year,2011))[["estimate"]]
    
    #Impute median for those zip codes we are missing
    controls %<>%
      merge(zipcode_income, by = "zip5", all.x = TRUE) %>%
      .[, median_zip_income := ifelse(is.na(median_zip_income),
                                    us_median,
                                    median_zip_income)] %>%
      .[order(bene_id), ]
      

    # Export
    fwrite(controls, paste0(lib_base_data, "year_level_controls_", year,
                            "_", pct, ".csv"))

    message("Done ", year)
  }
