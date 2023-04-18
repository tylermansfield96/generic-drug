#!/usr/bin/env Rscript

# Header -----------------------------------------------------------------------
# Proj: Bottle of Lies
# Author: Tyler Mansfield (tyler.mansfield96@gmail.com)
# Desc: An initial OLS regression for mortality

# Start Script -----------------------------------------------------------------
package_list <- c("lme4", "fixest", "stringr", "doParallel")
source("../00_pre_process/start_script.R")

option_list = list(
  make_option(c("-p", "--pct"), type = 'character', default = "20pct"),
  make_option(c("-o", "--outcome"), type = 'character', default = "death_within_30days"),
  make_option(c("-n", "--g_rxcui_name"), type = 'character',
              default = "atorvastatin"),
  make_option(c("-b", "--model_name"), type = 'character',
              default = "full"),
  make_option(c("-a", "--add_to_balance_table"), type = 'logical', default = FALSE),
  make_option(c("-v", "--save_model_results"), type = 'logical', default = TRUE)
)

unpack_opt(option_list)

# For multiple drugs -----------------------------------------------------------
if (g_rxcui_name == "all")
{
  classes <- fread(paste0(lib_base_data, "topdrugs_", pct, ".csv")) %>%
    .[1:50, .(name = key_word, g_rxcui)] %>%
    merge(fread(paste0(lib_base_data, "orig_manf_xwalk.csv")),
          by = "g_rxcui", 
          all.x = TRUE)
} else {
  classes <- list(name = g_rxcui_name)
}

registerDoParallel(cores = min(nrow(classes)), detectCores() - 1)
ret <- foreach(g_rxcui_name = classes$name) %dopar%
{
  # Read in Data -----------------------------------------------------------------
  message("Reading in ", g_rxcui_name," data...")
  
  if (g_rxcui_name == "pool")
  {
    # Upload data if it already exists
    if (file.exists(paste0(lib_base_data, "preprocessDT_pool50_", pct, ".csv")))
    {
      DT_model <- fread(paste0(lib_base_data, "preprocessDT_pool50_", pct, ".csv"))
    } else
    {
      DT_model <- data.table()
      
      classes <- fread(paste0(lib_base_data, "topdrugs_", pct, ".csv")) %>%
        .[1:50, .(name = key_word, g_rxcui)] %>%
        merge(fread(paste0(lib_base_data, "orig_manf_xwalk.csv")),
              by = "g_rxcui", 
              all.x = TRUE)
      
      for (i in 1:nrow(classes))
      {
        drug_i <- classes$name[i]
        orig_manfs <- unlist(strsplit(classes$orig_manf[i], ","))
        
        # Read in Data -----------------------------------------------------------------
        message("Reading in ", drug_i," data...")
        
        file_path <- paste0(lib_base_data, "preprocessDT_", drug_i, "_", pct, ".csv")
        
        if (file.exists(file_path))
        {
          cols_to_keep <- c("bene_id", "prvdr_id", "lab_name", "totalcst", "ptpayamt", 
                            "strength", "dayssply", "bnftphas", "dual_cstshr_ind",
                            "age_in_mo", "race", "white", "female", "zip5", "median_zip_income_log",
                            "n_grxcui_year_log", "total_spending_year_log", "total_oop_year_log",
                            "total_scripts_year", "cntrct_pbp_rfrnc_yr", "pharmacy_month_str",
                            "dayssply_cat", "region_month", "NPI_label", "death_dt", 
                            "death_within_30days", "OLS_predicted_death_within_30days")
          
          drug_i_dt <- fread(file_path, select = cols_to_keep)[, drug := drug_i]
          drug_i_dt[, orig_manf := as.numeric(lab_name %in% orig_manfs)]
          
          DT_model %<>% rbind(drug_i_dt)
        } else {
          message("No data for ", drug_i)
        }
      }
      
      fwrite(DT_model, paste0(lib_base_data, "preprocessDT_pool50_", pct, ".csv"))
    }
    
  } else {
    preprocess_data_file_path <- paste0(lib_base_data, "preprocessDT_", 
                                        g_rxcui_name, "_", pct, ".csv")
    
    # Check if preprocess has already occurred
    
    #Make sure file exists
    if(!file.exists(preprocess_data_file_path))
    {
      message(preprocess_data_file_path, " does not exist.")
      return(0)
    }
    
    if (file.exists(preprocess_data_file_path)) {
      DT_model <- fread(preprocess_data_file_path)
    } else {
      stop("Need to preprocess data")
    } 
  }
  
  # Subset model function -------------------------------------------------------
  
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
    
    if (str_detect(model_name, "1421_only"))
    {
      orig_size <- nrow(DT_model)
      
      DT_model %<>% .[dayssply_cat == "14-21"]
      
      new_size <- nrow(DT_model)
      
      message("Days Supply Subset to ", 
              signif(new_size / orig_size * 100, 3), 
              "% original size, ",
              g_rxcui_name)
    }
    
    return(DT_model)
  }
  
  # Prep Model -----------------------------------------------------------------
  message("Prepping model ", g_rxcui_name, "...")
  
  #State outcomes and form of the fixed effect model
  covariates <- c("median_zip_income_log",
                  "age_in_mo",
                  "dayssply_cat",
                  "female",
                  "white",
                  "dual_cstshr_ind",
                  "n_grxcui_year_log",
                  "factor(bnftphas)")
  
  covariates_form <- paste(covariates, collapse = " + ")
  
  fe_vars <- c("pharmacy_month_str",
               "cntrct_pbp_rfrnc_yr",
               "region_month")
  
  se_cluster_level <- "pharmacy_month_str"
  
  if (g_rxcui_name == "pool")
  {
    DT_model[, pharmacy_month_str_drug := paste0(pharmacy_month_str, "_", drug)]
    
    fe_vars <- c("pharmacy_month_str_drug",
                 "cntrct_pbp_rfrnc_yr",
                 "region_month")
    
    se_cluster_level <- "pharmacy_month_str_drug"
  }
  
  fe_form <- paste(fe_vars, collapse = " + ")
  
  # Setting Baseline Factors -----------------------------------------------------
  #Reorder lab_names so that the largest lab is first
  lab_names_by_size <- DT_model[, .N, by = lab_name] %>%
    .[order(-N), lab_name]
  
  DT_model %<>% .[, lab_name := factor(lab_name, levels = lab_names_by_size)] %>%
    .[, dayssply_cat := factor(dayssply_cat, levels = c("22-40",
                                                        "14-21",
                                                        "41-65",
                                                        "66+"))]
  
  ## Subset by pharmacy type ---------------------------------------------------------
  message("Subsetting model ", g_rxcui_name, "...")
  
  original_sample_size <- nrow(DT_model)
  
  DT_model <- subset_DT_model(DT_model, model_name)
  
  new_sample_size <- nrow(DT_model)
  
  sample_size_prop <- signif(new_sample_size / original_sample_size, 4)
  if (sample_size_prop < 1)
  {
    message(model_name, " has subsetted to ", sample_size_prop)
  } else {
    message("No subsetting")
  }
 
  
  # Fit Model --------------------------------------------------------------------
  message("Fitting ", g_rxcui_name," model...")
  
  if (sum(is.na(DT_model[[outcome]])) > 0)
  {
    stop("Some outcomes are NA")
  }
  
  form_all <- as.formula(paste0(outcome, " ~ factor(lab_name) +",
                                covariates_form,
                                "| ", 
                                fe_form))
  
  if (model_name == "true_gen")
  {
    #This list of fake generics are typically brand name manufacturers who also
    #sell some of their drugs as generics. See "Authorized Generic Drugs:
    #Short-Term Effects and Long-Term Impact: A REPORT OF THE
    #FEDERAL TRADE COMMISSION", August 2011, Figure 2-3 
    
    fake_generics <- c("Greenstone", "Sandoz", "Patriot", "Watson", "Forest",
                       "MALLINCKRODT", "AstraZeneca",
                       "Merck", "Wyeth", "GlaxoSmithKline")
    
    DT_model[, true_generic := as.numeric(!(lab_name %in% fake_generics))]
    
    form_all <- as.formula(paste0(outcome, " ~ true_generic +",
                                  covariates_form,
                                  "| ", 
                                  fe_form))
    
  }
  
  if (model_name == "orig_manf")
  {
    form_all <- as.formula(paste0(outcome, " ~ orig_manf +",
                                  covariates_form,
                                  "| ", 
                                  fe_form))
    
  }
  
  if (model_name %in% c("true_orig", "true_orig_subset"))
  {
    fake_generics <- c("Greenstone", "Sandoz", "Patriot", "Watson", "Forest",
                       "MALLINCKRODT", "AstraZeneca",
                       "Merck", "Wyeth", "GlaxoSmithKline")
    
    DT_model[, manf_status := ifelse(orig_manf == 1, 
                                     "OriginalManf",
                                     ifelse(lab_name %in% fake_generics,
                                            "FakeGeneric",
                                            "TrueGeneric")) %>%
                               factor(levels = c("TrueGeneric",
                                                 "FakeGeneric",
                                                 "OriginalManf"))]
    
    form_all <- as.formula(paste0(outcome, " ~ manf_status +",
                                  covariates_form,
                                  "| ", 
                                  fe_form))
    
    if (model_name == "true_orig_subset")
    {
      DT_model[, fake_generic := ifelse(lab_name %in% fake_generics, 1, 0)]
      
      form_all <- as.formula(paste0(outcome, " ~ fake_generic + orig_manf + ",
                                    covariates_form,
                                    "| ", 
                                    fe_form))
    }
    
    # Maybe try just keeping drugs with at least two of the three possible manf_status levels
    drugs_to_keep <- DT_model[, .N, by = .(drug, manf_status)] %>%
      .[, .N, by = drug] %>%
      .[N > 1, drug]
    
    DT_model %<>% .[drug %in% drugs_to_keep, .(death_within_30days,
                                               manf_status,
                                               median_zip_income_log,
                                               age_in_mo, 
                                               dayssply_cat,
                                               female,
                                               white,
                                               dual_cstshr_ind,
                                               n_grxcui_year_log, 
                                               bnftphas, 
                                               pharmacy_month_str_drug,
                                               cntrct_pbp_rfrnc_yr, 
                                               region_month,
                                               fake_generic,
                                               orig_manf)]
  }
  
  message(form_all)
  fit <- feols(form_all, DT_model, se = "cluster", cluster = se_cluster_level)
  outcome_rate_per_1000_scripts <- mean(DT_model[[outcome]]) * 1000
  
  summary(fit)
  outcome_rate_per_1000_scripts
  
  #Save results
  if (save_model_results)
  {
    sink(paste0(lib_base_data, "regression_", outcome, "_", g_rxcui_name,
                "_", model_name, "_", pct, ".txt"))
    
    print(summary(fit))
    
    if (model_name == "true_gen")
    {
      print("Manfs in true generics:")
      print(DT_model[true_generic == 1, lab_name] %>% unique())
      print("Manfs in fake generics:")
      print(DT_model[true_generic == 0, lab_name] %>% unique())
    }
    
    sink()
  }
  
  if (add_to_balance_table)
  {
    dt_fit1 <- fit_to_dt(fit, "lab_name") %>%
      .[, lab_name := gsub("factor\\(lab_name\\)", "", term)] %>%
      rbind(data.table(lab_name = lab_names_by_size[1], #Adding a row for the dropped lab
                       term = "", estimate = 0, std.error = 0,
                       statistic = 0, p.value = 0, lb = 0, ub = 0))
    
    #Calculate the fstatistic
    lab_vars_all <- paste0("lab_name_", lab_names_by_size)
    baseline_lab <- lab_names_by_size[1]
    
    fstat_rhs <- paste0(lab_vars_all, collapse = " + ")
    
    # Fit Model --------------------------------------------------------------------
    message(paste0("Demeaning ", g_rxcui_name))
    DT_model_dm <- demean(DT_model[, c(outcome, lab_vars_all), with = FALSE],
                          c(DT_model[, fe_vars, with = FALSE]))
    
    # Sample mean
    sample_mean1 <- mean(unlist(DT_model[, outcome, with = FALSE]), na.rm = TRUE) %>%
      signif(digits = 3) %>%
      as.character()
    
    # F statistic (all)
    fstat_form <- as.formula(paste0(outcome, " ~ ", fstat_rhs))
    
    message(paste0("Fitting F-stat model for ", g_rxcui_name))
    fit_fstat <- lm(fstat_form, data = DT_model_dm)
    
    fstat1 <- round(summary(fit_fstat)$fstatistic["value"], 3)
    
    pval <- pf(summary(fit_fstat)$fstatistic["value"],
               summary(fit_fstat)$fstatistic["numdf"],
               summary(fit_fstat)$fstatistic["dendf"],
               lower.tail = FALSE) %>% signif(digits = 5)
    
    dt_fit1 %<>%
      .[, `:=`(outcome = outcome, sample_mean = sample_mean1, fstat = fstat1, pval = pval,
               o_lab = baseline_lab, sample_size_prop = sample_size_prop)]
    
    if (subset_data == FALSE) {
      file_name <- paste0(lib_base_data, "dem_balance_estimates_", g_rxcui_name,
                          '_', pct, '_', model_name, ".csv")
    } else {
      file_name <- paste0(lib_base_data, "dem_balance_estimates_", g_rxcui_name,
                          model_name, "_", pct, "_", min_arm_size, "minarm_",
                          timeframe.length.months, "monthtimeframe.csv")
    }
    
    while(!file.exists(file_name))
    {
      message(file_name, " does not yet exist. Checking every 1 minute.")
      Sys.sleep(60)
    }
    
    if(file.exists(file_name))
    {
      message("Adding outcome to ", model_name, " for ", g_rxcui_name, ".")
      dt_fit_orig <- fread(file_name)
      outcome1 <- outcome
      dt_fit_orig %<>% .[!(outcome == outcome1)]
      dt_fit <- rbind(dt_fit_orig, dt_fit1, fill = TRUE)
      fwrite(dt_fit, file_name)
    } else {
      message("No balance file for this model")
    }
  }
}

message("Done.")