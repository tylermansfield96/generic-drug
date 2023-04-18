#!/usr/bin/env Rscript

# Header -----------------------------------------------------------------------
# Proj: Bottle of Lies
# Author: Tyler Mansfield (tyler.mansfield96@gmail.com)
# Desc: Creates Table 1 for the paper


# Start Script -----------------------------------------------------------------
package_list <- c("lme4", "fixest", "stringr", "doParallel", "DescTools")
source("../00_pre_process/start_script.R")

option_list = list(
  make_option(c("-p", "--pct"), type = 'character', default = "20pct"),
  make_option(c("-o", "--outcome"), type = 'character', default = "death_within_30days"),
  make_option(c("-n", "--g_rxcui_name"), type = 'character',
              default = "atorvastatin"), # Set to either atorvastatin or pool
  make_option(c("-d", "--drop_small_manf"), type = 'logical', default = TRUE),
  # Because the pooled data is so large, this is the additional amount of subsetting to perform
  # by only keeping this percentage of bene_ids
  make_option(c("-s", "--subset_pct"), type = 'character', default = "100") 
  # If the sample is too large to work with, we can take a sample of it
)

unpack_opt(option_list)
message(subset_pct)

# Read in Data -----------------------------------------------------------------
  
message("Reading in ", g_rxcui_name," data...")

if (g_rxcui_name == "pool")
{
  DT_model <- data.table()
  
  cols_to_keep <- c("bene_id", "totalcst", "ptpayamt", "dayssply", "srvc_dt",
                    "strength", "dual_cstshr_ind", "age_in_mo", "race", "sex",
                    "n_grxcui_year", "total_spending_year", "total_oop_year",
                    "total_scripts_year", "dayssply_cat", "death_dt", "death_on_record",
                    "death_within_30days")
  
  classes <- fread(paste0(lib_base_data, "topdrugs_", pct, ".csv")) %>%
    .[1:50, .(name = key_word, g_rxcui)]
  
  file_name <- paste0(lib_base_data, "preprocessDT_pool50_", pct, "table1.csv")
  
  if (file.exists(file_name))
  {
    DT_model <- fread(file_name)
  } else
  {
    for (i in 1:nrow(classes))
    {
      drug_i <- classes$name[i]
      
      # Read in Data -----------------------------------------------------------------
      message("Reading in ", drug_i," data...")
      
      file_path <- paste0(lib_base_data, "preprocessDT_", drug_i, "_", pct, ".csv")
      
      if (file.exists(file_path))
      {
        drug_i_dt <- fread(file_path, select = cols_to_keep)[, drug := drug_i]
        
        DT_model %<>% rbind(drug_i_dt)
      } else {
        message("No data for ", drug_i)
      }
    }
    
    fwrite(DT_model, file_name)
  }
  
} else {
  preprocess_data_file_path <- paste0(lib_base_data, "preprocessDT_", 
                                      g_rxcui_name, "_", pct, ".csv")
  DT_model <- fread(preprocess_data_file_path)
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Subset -----------------------------------------------------------------------
message("Subsetting data to only ", subset_pct, "% of beneficiaries...")
unique_bene_ids <- unique(DT_model$bene_id)
message(length(unique_bene_ids))
sample_bene_id <- sample(unique_bene_ids, 
                         size = length(unique_bene_ids) * as.numeric(subset_pct) / 100)

DT_model %<>% .[bene_id %in% sample_bene_id]

unique_bene_ids <- unique(DT_model$bene_id)
message(length(unique_bene_ids))


# Create Table 1 ---------------------------------------------------------------

max_death_date <- as.IDate(max(DT_model$death_dt), format = "%m/%d/%Y")

# Patient level data
message("Aggregating by patient...")

DT_model_patient <- DT_model[, .(sex = as.factor(Mode(sex)), 
                                 race = as.factor(Mode(race)),
                                 age_in_yr = mean(age_in_mo) / 12,
                                 dual_cstshr_ind = median(as.numeric(dual_cstshr_ind)),
                                 nscripts = .N,
                                 first_script = min(srvc_dt),
                                 last_script = max(srvc_dt),
                                 last_possible_death_date = min(max(srvc_dt) + 30, max_death_date),
                                 death_on_record = max(death_on_record),
                                 median_n_grxcui_year = as.double(median(unique(n_grxcui_year))),
                                 oop_per_year = median(unique(total_oop_year)),
                                 spending_per_year = median(unique(total_spending_year))),
                             by = bene_id] %>%
  .[, n_days_in_cohort := last_possible_death_date - first_script]



DT_model_patient_race <- factor(DT_model_patient$race,
                               levels = 0:6,
                               labels = c("Unknown", "White", "Black", "Other",
                                          "Asian", "Hispanic", 
                                          "North American Native"))

DT_model_30_day_supply <- DT_model[dayssply == 30]

message("Compiling final results...")

results <- list(nscripts = nrow(DT_model),
                npatients = nrow(DT_model_patient),
                mean_age = mean(DT_model_patient$age_in_yr),
                sd_age = sd(DT_model_patient$age_in_yr),
                prop_female = mean(DT_model_patient$sex == 2),
                race_summary = summary(DT_model_patient_race),
                prop_dual = mean(as.numeric(DT_model_patient$dual_cstshr_ind)),
                n_unique_drugs = mean(DT_model_patient$median_n_grxcui_year),
                sd_unique_drugs = sd(DT_model_patient$median_n_grxcui_year),
                total_annual_drug_spending = mean(DT_model_patient$spending_per_year),
                sd_total_annual_drug_spending = sd(DT_model_patient$spending_per_year),
                oop_annual_drug_spending = mean(DT_model_patient$oop_per_year),
                sd_oop_annual_drug_spending = sd(DT_model_patient$oop_per_year),
                days_supply_dist = summary(factor(DT_model$dayssply_cat)),
                oop_price_30day_supply = mean(DT_model_30_day_supply$ptpayamt),
                oop_price_30day_supply_sd = sd(DT_model_30_day_supply$ptpayamt),
                list_price_30day_supply = mean(DT_model_30_day_supply$totalcst),
                list_price_30day_supply_sd = sd(DT_model_30_day_supply$totalcst))

message("Finished")


# Save Results -----------------------------------------------------------------

saveRDS(results, file=paste0(lib_base_data, 'table1_pool_', subset_pct, 'pct_subset.RData'))
b <- readRDS(paste0(lib_base_data, 'table1_pool_', subset_pct, 'pct_subset.RData'))
b$days_supply_dist / sum(b$days_supply_dist) * 100