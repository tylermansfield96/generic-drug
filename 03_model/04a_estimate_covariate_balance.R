#!/usr/bin/env Rscript

# Header -----------------------------------------------------------------------
# Proj: Bottle of Lies
# Author: Evan Flack (evanjflack@gmail.com) and
#         Tyler Mansfield (tyler.mansfield96@gmail.com)
# Desc: Estimates balance of demographic characteristics on individual labs 
#       net of FEs.
#       By changing the option 'model_name', you can apply different subsets
#       (via the function subset_DT_model) or different model specifications. 

# Start Script -----------------------------------------------------------------
package_list <- c("lme4", "fixest", "ggplot2", "doParallel", "stringr", "DescTools")
source("../00_pre_process/start_script.R")

option_list = list(
  make_option(c("-p", "--pct"), type = 'character', default = "20pct"),
  make_option(c("-n", "--g_rxcui_name"), type = 'character',
              default = "atorvastatin"),
  #Use "pool" for g_rxcui_name to combine data from all classes 
  #or "all" to iterate through all classes
  
  make_option(c("-o", "--model_name"), type = 'character',
              default = "full")
)

unpack_opt(option_list)

# For multiple drugs -----------------------------------------------------------
if (g_rxcui_name == "all")
{
  classes <- fread(paste0(lib_base_data, "topdrugs_", pct, ".csv")) %>%
    .[1:50, .(name = key_word, g_rxcui)]
} else {
  classes <- list(name = g_rxcui_name)
}

# Helper functions for subsetting -----------------------------------------------

#This function finds the mode of a vector (containing that value at least 
# min_num_mode times) and returns a logical vector with TRUE for every value 
# within tol of the mode
# Example: FindNearMode(c(2,2,2,4,2.5), min_num_mode = 2, tol = 1) 
#   would return c(TRUE,TRUE,TRUE,FALSE,TRUE)
FindNearMode <- function(x, min_num_mode = 2, tol = 1) {
  u <- unique(x)
  mode <- u[which.max(tabulate(match(x, u)))]
  keep <- (x <= mode + tol) & (x >= mode - tol)
  
  if (sum(keep) < min_num_mode & length(keep) >= min_num_mode)
  {
    #No real mode
    return(rep(FALSE, length(keep)))
  }
  return(keep)
}

#A function that subsets DT_model based on strings detected in the model_name
subset_DT_model <- function(DT_model, model_name)
{
  if (str_detect(model_name, "no_lt"))
  {
    orig_size <- nrow(DT_model)
    
    DT_model %<>% .[NPI_label != "Suppliers/Pharmacy Long-term Care Pharmacy"]
    
    new_size <- nrow(DT_model)
    
    message("LT Subset to ", 
            signif(new_size / orig_size * 100, 3), 
            "% original size, ",
            g_rxcui_name)
  }
  
  if (str_detect(model_name, "stable_phases"))
  {
    orig_size <- nrow(DT_model)
    
    DT_model %<>% .[bnftphas %in% c("PP", "CC")]
    
    new_size <- nrow(DT_model)
    
    message("Price Variation Subset to ", 
            signif(new_size / orig_size * 100, 3), 
            "% original size, ",
            g_rxcui_name)
  }
  
  if (str_detect(model_name, "same_list_price"))
  {
    orig_size <- nrow(DT_model)
    
    DT_model[, list_price := Winsorize(totalcst), 
             by = .(strength, dayssply)]
    
    fe_summary <- DT_model[, .(sd_price = sd(list_price),
                               n_samples = .N), 
                           by = fe_col] %>% 
      .[!is.na(sd_price)]
    
    #Keep those with no variation
    fes_to_keep <- fe_summary[sd_price < 0.05, fe_col]
    
    DT_model %<>% .[fe_col %in% fes_to_keep]
    
    new_size <- nrow(DT_model)
    
    message("Price Variation Subset to ", 
            signif(new_size / orig_size * 100, 3), 
            "% original size, ",
            g_rxcui_name)
  }
  
  if (str_detect(model_name, "same_OOP_price"))
  {
    orig_size <- nrow(DT_model)
    
    DT_model[, OOP_price := Winsorize(ptpayamt), 
             by = .(strength, dayssply)]
    
    fe_summary <- DT_model[, .(sd_price = sd(OOP_price),
                               n_samples = .N), 
                           by = fe_col] %>% 
      .[!is.na(sd_price)]
    
    #Keep those with no variation
    fes_to_keep <- fe_summary[sd_price < 0.01, fe_col]
    
    DT_model %<>% .[fe_col %in% fes_to_keep]
    
    new_size <- nrow(DT_model)
    
    message("Price Variation Subset to ", 
            signif(new_size / orig_size * 100, 3), 
            "% original size, ",
            g_rxcui_name)
  }
  
  if (str_detect(model_name, "mode_list_price"))
  {
    orig_size <- nrow(DT_model)
    
    DT_model[, list_price := Winsorize(totalcst), 
             by = .(strength, dayssply)]
    
    #Keep those within $1.00 of mode price
    DT_model[, keep_list_price := FindNearMode(list_price, tol = 1),
             by = fe_col]
    
    DT_model %<>% .[keep_list_price == TRUE]
    
    new_size <- nrow(DT_model)
    
    message("Mode Price Variation Subset to ", 
            signif(new_size / orig_size * 100, 3), 
            "% original size, ",
            g_rxcui_name)
  }
  
  if (str_detect(model_name, "mode_OOP_price"))
  {
    orig_size <- nrow(DT_model)
    
    DT_model[, oop_price := Winsorize(ptpayamt), 
             by = .(strength, dayssply)]
    
    #Keep those within $1.00 of mode OOP price
    DT_model[, keep_oop_price := FindNearMode(oop_price, tol = 1),
             by = fe_col]
    
    DT_model %<>% .[keep_oop_price == TRUE]
    
    new_size <- nrow(DT_model)
    
    message("Mode Price Variation Subset to ", 
            signif(new_size / orig_size * 100, 3), 
            "% original size, ",
            g_rxcui_name)
  }
  
  return(DT_model)
}

# Begin balance ---------------------------------------------------------------
for (g_rxcui_name in classes$name)
{
  message("Reading in data ", g_rxcui_name, "...")
  
  if (g_rxcui_name == "pool")
  {
    DT_model <- data.table()
    
    classes <- fread(paste0(lib_base_data, "topdrugs_", pct, ".csv")) %>%
      .[1:50, .(name = key_word, g_rxcui)]
    
    for (drug_i in classes$name)
    {
      # Read in Data -----------------------------------------------------------------
      message("Reading in ", drug_i," data...")
      
      file_path <- paste0(lib_base_data, "preprocessDT_", drug_i, "_", pct, ".csv")
      
      if (file.exists(file_path))
      {
        DT_model %<>% rbind(fread(file_path)[, drug := drug_i], fill = TRUE)
      }
    }
  } else {
    preprocess_data_file_path <- paste0(lib_base_data, "preprocessDT_",
                                        g_rxcui_name, "_", pct, ".csv")
    
    if (!file.exists(preprocess_data_file_path))
    {
      message("No data for ", g_rxcui_name)
      next
    }
    
    DT_model <- fread(preprocess_data_file_path)
  }
  
  # Prep Model --------------------------------------------------------------------
  message("Prepping model ", model_name, " for ", g_rxcui_name, "...")
  lab_names_by_size <- DT_model[, .N, by = lab_name] %>% .[order(-N), lab_name]
  
  lab_vars_all <- paste0("lab_name_", lab_names_by_size)
  baseline_lab <- lab_names_by_size[1]
  
  DT_model %<>%
    .[, lab_name := factor(lab_name, levels = lab_names_by_size)]
  
  DT_model[dayssply == 30, 
           listprice_30dayssply := Winsorize(totalcst),
           by = strength]
  
  DT_model[dayssply == 30, 
           oop_30dayssply := Winsorize(ptpayamt),
           by = strength]
  
  if ("SL_predicted_death_within_30days" %in% names(DT_model))
  {
    DT_model[, predicted_death_within_30days := 
               log(SL_predicted_death_within_30days)]
    #use log transform on Superlearner to make normal
  } else {
    DT_model[, predicted_death_within_30days := 
               OLS_predicted_death_within_30days]
  }
  
  outcomes <- c("predicted_death_within_30days",
                "age_in_mo",
                "female",
                "white",
                "dual_cstshr_ind",
                "n_grxcui_year_log",
                "total_spending_year_log",
                "total_oop_year_log",
                "days_since_last_fill_log")
  
  fe_vars <- c("pharmacy_month_str",
               "dayssply_cat",
               "cntrct_pbp_rfrnc_yr",
               "region_month",
               "bnftphas")
  
  se_cluster_level <- "pharmacy_month_str"
  
  if (g_rxcui_name == "pool") # If we pool, we need the fixed effects 
                              # to include the drug
  {
    DT_model[, pharmacy_month_str_drug := paste0(pharmacy_month_str, "_", drug)]
    
    fe_vars <- c("pharmacy_month_str_drug",
                 "dayssply_cat",
                 "cntrct_pbp_rfrnc_yr",
                 "region_month",
                 "bnftphas")
    
    se_cluster_level <- "pharmacy_month_str_drug"
    
    #Fill in zeros for the labs corresponding to other drugs
    DT_model <- setnafill(DT_model, fill = 0, cols = lab_vars_all)
  }
  
  fe_form <- paste(fe_vars, collapse = " + ")
  
  fstat_rhs <- paste0(lab_vars_all, collapse = " + ")
  
  ## Subset by pharmacy type ---------------------------------------------------------
  message("Subsetting model ", g_rxcui_name, "...")
  
  original_sample_size <- nrow(DT_model)
  
  DT_model <- subset_DT_model(DT_model, model_name)
  
  new_sample_size <- nrow(DT_model)
  
  sample_size_prop <- signif(new_sample_size / original_sample_size, 4)
  message(g_rxcui_name, " (", model_name, ") has subsetted by a factor of ", 
          sample_size_prop)
  
  # Estimate Balance -------------------------------------------------------------
  message("Estimating balance for ", g_rxcui_name)
  
  #Find outcomes with NA values
  outcome_sums <- DT_model[, c(outcomes), with = FALSE] %>% colSums()
  outcomes_na <- outcome_sums[is.na(outcome_sums)] %>% names()
  outcomes_no_na <- outcomes[!(outcomes %in% outcomes_na)]
  
  #For those outcomes with no NA values, we can do the demeaning step together
  if (length(outcomes_no_na) > 0)
  {
    DT_model_dm <- demean(DT_model[, c(outcomes_no_na, lab_vars_all), with = FALSE],
                          c(DT_model[, fe_vars, with = FALSE])) 
  }
  
  dt_fit <- data.table()
  
  for (outcome in outcomes)
  {
    message("Starting ", outcome, " for ", g_rxcui_name)
    
    # Sample mean
    sample_mean1 <- mean(unlist(DT_model[, outcome, with = FALSE]), na.rm = TRUE) %>%
      signif(digits = 3) %>%
      as.character()
  
    # F statistic (all)
    fstat_form <- as.formula(paste0(outcome, " ~ ", fstat_rhs))
  
    # Check to see if there are NA values
    message("Fitting F-Statistic ", outcome)
    if(outcome %in% outcomes_no_na) {
      fit_fstat <- lm(fstat_form, data = DT_model_dm)
    } else {
      DT_model2 <- DT_model[!is.na(get(outcome))]
      DT_model_dm2 <- demean(DT_model2[, c(outcome, lab_vars_all), with = FALSE],
                             c(DT_model2[, fe_vars, with = FALSE]))
      fit_fstat <- lm(fstat_form, data = DT_model_dm2)
    }
  
    fstat1 <- signif(summary(fit_fstat)$fstatistic["value"], 3)
    
    pval <- pf(summary(fit_fstat)$fstatistic["value"],
               summary(fit_fstat)$fstatistic["numdf"],
               summary(fit_fstat)$fstatistic["dendf"],
               lower.tail = FALSE) %>% signif(digits = 5)
  
    # Coefficients (all)
    message("Fitting feols model ", outcome)
    form_string <- paste0(outcome, " ~ lab_name | ", fe_form)
    message("Form : ", form_string)
    form_all <- as.formula(form_string)
    
    fit <- feols(form_all, DT_model, se = "cluster", cluster = se_cluster_level)
    
    #Save results
    if (TRUE)
    {
      sink(paste0(lib_base_data, "regression_", outcome, "_", g_rxcui_name,
                  "_", model_name, "_", pct, ".txt"))
      
      print(summary(fit))
      
      sink()
    }
    
    dt_fit1 <- fit_to_dt(fit, "lab_name") %>%
      .[, lab_name := gsub("lab_name", "", term)] %>%
      rbind(data.table(lab_name = baseline_lab, #Adding a row for the dropped lab
                       term = "", estimate = 0, std.error = 0,
                       statistic = 0, p.value = 0, lb = 0, ub = 0))
  
    dt_fit1 %<>%
      .[, `:=`(outcome = outcome, sample_mean = sample_mean1, fstat = fstat1, 
               anova_pval = pval, o_lab = baseline_lab, 
               sample_size_prop = sample_size_prop)]
    
    dt_fit %<>% rbind(dt_fit1)
  
    message("Finished ", outcome)
  }
  
  # Export -----------------------------------------------------------------------
  message("Exporting...")
  
  file_name <- paste0(lib_base_data, "dem_balance_estimates_", g_rxcui_name,
                      '_', pct, '_', model_name, ".csv")
  
  fwrite(dt_fit, file_name)
  
  message("Finished ", model_name, " for ", g_rxcui_name, ".")
}

stopImplicitCluster()