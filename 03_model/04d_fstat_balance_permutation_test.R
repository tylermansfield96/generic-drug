#!/usr/bin/env Rscript

# Header -----------------------------------------------------------------------
# Proj: Bottle of Lies
# Author: Tyler Mansfield (tyler.mansfield96@gmail.com)
# Desc: Runs a F stat permutation test on the lab_vars coefficient generated
#       from 04a_estimate_covariate_balance.R

# Start Script -----------------------------------------------------------------
package_list <- c("lme4", "fixest", "doParallel")
source("../00_pre_process/start_script.R")

option_list = list(
  make_option(c("-p", "--pct"), type = 'character', default = "20pct"),
  make_option(c("-n", "--g_rxcui_name"), type = 'character',
              default = "atorvastatin"),
  make_option(c("-s", "--subset_data"), type = 'logical', default = FALSE),
  make_option(c("-d", "--drop_small_manf"), type = 'logical', default = FALSE),
  make_option(c("-o", "--model_name"), type = 'character',
              default = "pharmmonth_fe"),
  
  #The following two options are only relevant if `subset_data` = TRUE
  make_option(c("-a", "--min_arm_size"), type = 'integer', default = 1),
  make_option(c("-t", "--timeframe.length.months"), type = 'double', default = 1)
)

unpack_opt(option_list)

# Global Variables -------------------------------------------------------------
num_sims <- 200
keep_groups_together <- TRUE
permute_level <- "pharmacy_month"

outcomes <- c("female",
              "white",
              "dual_cstshr_ind",
              "age_in_mo",
              "price_per_pill",
              "days_since_last_fill_log",
              "n_grxcui_year_log",
              "median_zip_income_log",
              "random_num_beneid1",
              "random_num_beneid2")

fe_vars <- c("pharmacy_month",
             "strength",
             "dayssply_cat",
             "cntrct_pbp_rfrnc_yr",
             "region_month")

# Read in Data -----------------------------------------------------------------
message("Reading in data...")

if (subset_data == FALSE)
{
  preprocess_data_file_path <- paste0(lib_base_data, "preprocessDT_", 
                                      g_rxcui_name, "_", pct, ".csv")
} else {
  preprocess_data_file_path <- paste0(lib_base_data, "preprocessDT_", 
                                      g_rxcui_name, "_", pct, "_", min_arm_size, 
                                      "minarm_", timeframe.length.months, 
                                      "monthtimeframe.csv")
}


# Check if preprocess has already occurred
if (file.exists(preprocess_data_file_path)) {
  DT_model <- fread(preprocess_data_file_path)
} else {
  stop("Need to preprocess data")
}

# Permutation test prep --------------------------------------------------------
message("Prepping for Permutation Tests...")

#Define sampling function
sample_keep_groups_together <- function(x)
{
  x_new <- x
  groups <- unique(x)
  
  if (length(groups) == 1)
  {
    return(x)
  }
  
  groups_new <- sample(groups)
  
  for (i in 1:length(groups))
  {
    x_new[x == groups[i]] <- groups_new[i]
  }
  return(x_new)
}

if (keep_groups_together)
{
  sampling_function <- sample_keep_groups_together
} else {
  sampling_function <- sample
}

#Gather manufacturer information
lab_names_by_size <- DT_model[, .N, by = lab_name] %>% .[order(-N), lab_name]
lab_vars <- paste0("lab_name_", lab_names_by_size[2:length(lab_names_by_size)])
baseline_lab <- lab_names_by_size[1]

DT_model %<>%
  .[, lab_name := factor(lab_name, levels = lab_names_by_size)]

#Create fixed effects formulas
fe_form <- paste(fe_vars, collapse = " + ")
fstat_rhs <- paste0(lab_vars, collapse = " + ")
fstat_form <- as.formula(paste0("permuted_outcome ~ ", fstat_rhs))

DT_model_full <- DT_model

for (outcome in outcomes)
{
  #Give unique identifier for fstats from this scenario
  fstat_file_id <- paste("permutation_tests/nullfstat",
                         g_rxcui_name,
                         ifelse(subset_data, 
                                paste0("subset",min_arm_size,timeframe.length.months), 
                                "full"),
                         model_name,
                         outcome,
                         sep = '_')

  #Check to see if we've already run this permutation test before
  if(file.exists(paste0(lib_base_data, fstat_file_id, num_sims,".csv")))
  {
    message(num_sims, " tests already ran for ", outcome, "...")
    next
  }

  message("Prepping Permutation Test (", outcome, ")...")

  #Only use the part of the data that is not null for this outcome
  DT_model <- DT_model_full[!is.na(get(outcome))]

  #Demean the manufacturers since it will always be the same for each permutation
  lab_vars_dm <- demean(DT_model[, lab_vars, with = FALSE],
                        c(DT_model[, fe_vars, with = FALSE]))

  #Check which permutations need to be ran
  need_to_run <- c()
  for (i in 1:num_sims)
  {
    if (!file.exists(paste0(lib_base_data, fstat_file_id, i,".csv"))) 
    {
      need_to_run <- c(need_to_run, i)
    }
  }

  message("Running Permutation Test (parallel, n = ", length(need_to_run), ")...")

  parallelCluster <- makeCluster(min(floor(detectCores() * .8), 
                                     length(need_to_run)),
                                 type = "SOCK",
                                 methods = FALSE)

  setDefaultCluster(parallelCluster)
  registerDoParallel(parallelCluster)

  ret <- foreach(i = need_to_run, .packages = c("lme4",
                                                "fixest",
                                                "data.table",
                                                "tidyverse")) %dopar%
  {
    #Permute within group
    DT_model_permute <- DT_model %>%
      .[, permuted_outcome := sampling_function(get(outcome)), by = permute_level]

    #Demean
    DT_model_permute_dm <- demean(DT_model_permute[, c("permuted_outcome"), with = FALSE],
                                  c(DT_model_permute[, fe_vars, with = FALSE])) %>%
      cbind(lab_vars_dm)

    fit_fstat <- lm(fstat_form, data = DT_model_permute_dm)
    fstat1 <- summary(fit_fstat)$fstatistic["value"]

    write.csv(fstat1, file=paste0(lib_base_data, fstat_file_id, i,".csv"))
  }
  stopImplicitCluster()
}