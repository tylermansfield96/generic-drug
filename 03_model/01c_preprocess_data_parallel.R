#!/usr/bin/env Rscript

# Header -----------------------------------------------------------------------
# Proj: Bottle of Lies
# Author: Tyler Mansfield (tyler.mansfield96@gmail.com)
# Desc: Preprocessing of data before balance check or outcome regression
#       Because some calculations are intensive, the parallel structure of this
#       code splits the data into x groups (keeping bene_ids together), performs
#       the data manipulation, and then reassembles the data back together

# Start Script -----------------------------------------------------------------
package_list <- c("fastDummies", "stringi", "doParallel", "lubridate", 
                  "nnls", "SuperLearner")
source("../00_pre_process/start_script.R")

option_list = list(
  make_option(c("-p", "--pct"), type = 'character', default = "20pct"),
  make_option(c("-f", "--first_year"), type = 'integer', default = 2010),
  make_option(c("-m", "--first_month"), type = 'integer', default = 1),
  make_option(c("-l", "--last_year"), type = 'integer', default = 2013),
  make_option(c("-d", "--drop_small_manf"), type = 'logical', default = TRUE),
  make_option(c("-u", "--do_superlearner"), type = 'logical', default = FALSE)
  # Rather than using simple regression to predict mortality, Superlearner
  # provides a more robust methodology, but it is computationally intensive,
  # especially given the size of the data
)

unpack_opt(option_list)

classes <- fread(paste0(lib_base_data, "topdrugs_20pct.csv")) %>%
  .[1:50, .(name = key_word, g_rxcui)]

# Helper Functions -------------------------------------------------------------

#This function takes a vector of unordered dates and returns an equivalent length
#vector with the number of days between that date and the date before in
#chronologically
#Example: c("2010-01-30", "2010-02-01", "2010-01-15") would return
#         c(15, 1, NA)
time_to_previous_date <- function(dates)
{
  all_dates <- as.POSIXct(dates)
  ret_vec <- c()
  
  for (i in 1:length(dates))
  {
    diffs <- difftime(all_dates[i], all_dates, units = "days")
    
    if (sum(diffs > 0) == 0)
    {
      ret_vec <- c(ret_vec, NA)
    } else
    {
      ret_vec <- c(ret_vec, min(diffs[diffs > 0]))
    }
  }
  
  return(ret_vec)
}



#Perhaps we are interested if a patient has ever filled at a long-term pharmacy
#before. By setting label_of_interest = 'Suppliers/Pharmacy Long-term Care
#Pharmacy', this function will return 0 if none of the previous fills (including
#itself) contain the label 'Suppliers/Pharmacy Long-term Care Pharmacy' and 1
#otherwise
previous_label <- function(dates, labels, label_of_interest)
{
  all_dates <- as.POSIXct(dates)
  ret_vec <- rep(0, length(labels))

  for (i in 1:length(labels))
  {
    if(label_of_interest %in% labels[all_dates <= all_dates[i]])
    {
      ret_vec[i] <- 1
    }
  }

  return(ret_vec)
}

#Finds the most recent hospitalization AFTER a prescription is filled. Returns
#the index of the hospital event in hosp_data. If no hospitalization occurs
#after the fill, then -1 is returned
find_most_recent_hospitalization_index <- function(bene_id1,
                                                   srvc_dts,
                                                   hosp_data)
{
  hosp_data_subset <- hosp_data[bene_id == bene_id1[1]]
  
  #Check to see if there is any hospital data for this bene_id
  if (nrow(hosp_data_subset) == 0)
  {
    return(rep(-1, length(srvc_dts)))
  }
  
  return_vec <- c()

  for (my_srvc_dt in srvc_dts)
  {
    hosp_data_subset_i <- hosp_data_subset %>%
      .[, days_after_script := interval(ymd(my_srvc_dt), ymd(hosp_dt)) / days(1)] %>%
      .[days_after_script > 0]
    
    if (nrow(hosp_data_subset_i) == 0)
    {
      return_vec <- c(return_vec, -1L)
    } else
    {
      most_recent_index <- hosp_data_subset_i[order(days_after_script), ] %>%
        .[1, hosp_event_index]
      
      return_vec <- c(return_vec, most_recent_index) 
    }
  }

  return(return_vec)
}

# Iterate Over Classes ---------------------------------------------------------
message("Iterating over ", nrow(classes), " classes...")

for(g_rxcui_name in classes$name)
{
  # Read in Data ---------------------------------------------------------------
  message("Reading in ", g_rxcui_name," data...")
  
  file_name <- paste0(lib_base_data, "generic_sample_", g_rxcui_name, "_",
                      pct, ".csv")
  
  #Make sure file exists
  if(!file.exists(file_name))
  {
    message(file_name, " does not exist.")
    next
  }
  
  DT_model <- fread(file_name)

  # Prep Data ------------------------------------------------------------------
  message("Subsetting data ", g_rxcui_name,"...")

  # Subset to generic fills in period of interest
  DT_model %<>%
    .[branded == 0, ]
  
  # Some drugs have no generics
  if (nrow(DT_model) == 0)
  {
    message("!!! There are no generic branded drugs for ", g_rxcui_name)
    next
  }
  
  # Eliminate strengths with only one manufacturer
  strengths_to_keep <- DT_model %>%
    .[, .(num_manfs = length(unique(lab_name))), by = strength] %>%
    .[num_manfs > 1, strength]
  
  DT_model %<>% .[strength %in% strengths_to_keep]
  
  # Drop small manufacturers if requested
  if (drop_small_manf)
  {
    DT_model %<>%
      .[lab_name != "Small"]
  }

  message("Adding time and basic demographic variables ", g_rxcui_name,"...")
  DT_model %<>%
    .[, female := ifelse(sex == 2, 1, 0)] %>%
    .[, white := ifelse(race == 1, 1, 0)] %>%
    .[, quarter := ifelse(month <= 3, 1,
                          ifelse(month <= 6, 2,
                                 ifelse(month <= 9, 3, 4)))] %>%
    .[, runningweek := (rfrnc_yr - first_year)*53 + week] %>%
    .[, runningmonth := (rfrnc_yr - first_year)*12 + month] %>%
    .[, runningquarter := ceiling(runningmonth/3)] %>%
    .[, runningfortnight := ceiling(runningweek/2)] %>%
    .[, pharmacy_month := paste0("p", prvdr_id, "_m", runningmonth)] %>%
    .[, pharmacy_month_str := paste0(pharmacy_month, "_s", strength)] %>%
    .[, pharmacy_quarter := paste0("p", prvdr_id, "_q", runningquarter)] %>%
    .[, pharmacy_fortnight := paste0("p", prvdr_id, "_fn", runningfortnight)] %>%
    .[, pharmacy_week := paste0("p", prvdr_id, "_w", runningweek)] %>%
    .[, price_per_pill := totalcst / dayssply]

  # Make dayssply a categorical variable
  DT_model %<>%
    .[, dayssply_cat := cut(dayssply,
                            breaks=c(0, 13, 21, 40, 65, 1000),
                            labels=c('1-13', '14-21', '22-40', '41-65', '66+')) %>%
        relevel('22-40')] %>%
    .[!(dayssply_cat == '1-13')]

  DT_model %<>%
    .[, median_zip_income_log := log(median_zip_income)] %>%
    .[, n_grxcui_year_log := log(n_grxcui_year)] %>%
    .[, total_spending_year_log := log(total_spending_year + 1)] %>%
    .[, total_oop_year_log := log(total_oop_year + 1)] %>%
    .[, region := floor(DT_model[["zip5"]] / 10000)]

  # Combine small insurance plans
  DT_model %<>%
    .[, num_insurance := .N, by = cntrct_pbp_rfrnc_yr] %>%
    .[num_insurance < 10, cntrct_pbp_rfrnc_yr := "Small"] %>%
    .[, -c("num_insurance")]

  # Make interaction of region and timeframe
  DT_model %<>% .[, region_quarter := paste0("r", region, "_q", runningquarter)] %>%
    .[, region_month := paste0("r", region, "_m", runningmonth)] %>%
    .[, region_week := paste0("r", region, "_w", runningweek)] %>%
    .[, region_fortnight := paste0("r", region, "_fn", runningfortnight)]

  message("Adding pharmacy type classifications ", g_rxcui_name,"...")

  # Add NPI label and deactivation date
  xwalk <- fread(paste0(lib_base_data, "prvdr_label_xwalk.csv"))

  DT_model %<>% merge(xwalk, all.x = TRUE, by.x = "prvdr_id", by.y = "NPI") %>%
    .[, pharm_sample_size := .N, by = prvdr_id]

  # Make one small imputation for large pharmacies
  DT_model[NPI_label == "No Info, Deactivated" & pharm_sample_size >= 10000,
           NPI_label := "Suppliers/Pharmacy Mail Order Pharmacy"]

  # message("Adding indicator for if indiviudal has ",
  #       "purchased from long term care pharamcy previously (",
  #       g_rxcui_name,
  #       ")...")
  #
  # DT_model[, lt_now_or_before :=
  #            previous_label(srvc_dt,
  #                           NPI_label,
  #                           "Suppliers/Pharmacy Long-term Care Pharmacy"),
  #          by = bene_id]

  # Add random number for each bene_id 
  # (Can be used as a null value when checking significance)
  set.seed(1)
  bene_id_random_number <- DT_model[, .(random_num_beneid1 = rnorm(1),
                                        random_num_beneid2 = rbinom(1, 1, 0.5)),
                                    by = bene_id]

  DT_model %<>% merge(bene_id_random_number, by = "bene_id")

  # Create lab_num dummy variable
  message("Adding lab name dummy variables ", g_rxcui_name, "...")
  DT_model %<>%
    .[, lab_name := gsub("[^[:alnum:]]","",lab_name)] %>%
    dummy_cols("lab_name")

  lab_names_by_size <- DT_model[, .N, by = lab_name] %>% .[order(-N), lab_name]

  DT_model %<>%
    .[, lab_name := factor(lab_name, levels = lab_names_by_size)]
  
  # Add Phase Descriptions-------------------------------------------------------
  phase_xwalk <- list("DD" = "Deductible",
                      "DP" = "Deductible/PreGap",
                      "DI" = "Deductible/Gap",
                      "DC" = "Deductible/Catastrophic",
                      "PP" = "PreGap",
                      "PI" = "PreGap/Gap",
                      "PC" = "PreGap/Catastrophic",
                      "II" = "Gap",
                      "IC" = "Gap/Catastrophic",
                      "CC" = "Catastrophic",
                      " "	= "NotCovered")
  
  phase_xwalk <- data.table(code = names(phase_xwalk),
                            label = phase_xwalk) %>%
    .[code == " ", code := ""]
  
  DT_model[, bnftphas := factor(bnftphas, 
                                levels = phase_xwalk$code, 
                                labels = phase_xwalk$label)]

  # Add outcome(s) -----------------------------------------------------------------
  message("Adding death outcome ", g_rxcui_name, "...")

  # Death within 30 days
  deaths <- fread(paste0(lib_base_data, "sample_death_dates_", pct, ".csv"))

  DT_model %<>% merge(deaths, by = "bene_id", all.x = TRUE) %>%
    .[, death_on_record := as.numeric(!is.na(death_dt))] %>%
    .[, days_to_death := interval(ymd(srvc_dt), mdy(death_dt)) / days(1)] %>%
    .[, death_within_30days := as.numeric(days_to_death <= 30)] %>%
    .[is.na(death_within_30days), death_within_30days := 0]

  #If death occurred after the fill, eliminate the row
  message(nrow(DT_model[days_to_death <= 0, ]), " scripts were filled after their death")

  DT_model %<>% .[days_to_death > 0 | death_on_record == 0, ]

  message("Adding hospitalization outcome ", g_rxcui_name, "...")

  # Hospitalization within 30 days
  hosp_full <- read_and_combine(lib_base_data,
                                "sample_ip_primary_diags_long",
                                years,
                                pct) %>%
    .[, hosp_event_index := .I] %>%
    setnames("from_dt", "hosp_dt")
  
  # Make all time the same format
  DT_model[, srvc_dt := as.POSIXct(srvc_dt, 
                                   tz = "UTC", 
                                   time = 0, 
                                   origin = "1970-01-01") %>%
             as.character()]
  hosp_full[, hosp_dt := as.POSIXct(hosp_dt, 
                                    format = "%m/%d/%Y", 
                                    tz = "UTC", 
                                    time = 0,
                                    origin = "1970-01-01") %>%
              as.character()]
  
  # Also look at key hospitalizations
  target_hosp <- hosp_full[adr + ami + stroke > 0]
  
  # Now split the data and parallelize
  unique_bene_ids <- DT_model$bene_id %>% unique()
  num_cores_to_use <- min(detectCores() - 1, length(unique_bene_ids))
  bene_id_chunks <- split(unique_bene_ids, 
                          cut(seq_along(unique_bene_ids), 
                              num_cores_to_use, 
                              labels = FALSE))
  
  message("Beginning parallel ", g_rxcui_name)
  
  registerDoParallel(cores = num_cores_to_use)
  ret <- foreach(i = 1:num_cores_to_use) %dopar%
  {
    DT_model_subset <- DT_model[bene_id %in% bene_id_chunks[[i]]]
    
    message("Adding days since last fill ", g_rxcui_name," (Thread ", i, ")...")
    
    DT_model_subset[, days_since_last_fill := time_to_previous_date(srvc_dt), by = bene_id]
    DT_model_subset[, days_since_last_fill_log := log(days_since_last_fill)]
    
    message("Finding hospital index ", g_rxcui_name, " (Thread ", i, ")...")
    DT_model_subset[, hosp_event_index :=
               find_most_recent_hospitalization_index(bene_id1 = bene_id,
                                                      srvc_dts = srvc_dt,
                                                      hosp_data = hosp_full),
             by = bene_id]
    
    #Merge on hospital data
    message("Merging hospital outcomes ", g_rxcui_name, "(Thread ", i, ")...")
    
    DT_model_subset %<>% merge(hosp_full[, .(hosp_dt, hosp_code = dgnscd, hosp_event_index)],
                        by = "hosp_event_index",
                        all.x = TRUE,
                        sort = FALSE) %>%
      .[, days_to_hosp := interval(ymd(srvc_dt), ymd(hosp_dt)) / days(1)] %>%
      .[, hosp_within_30days := as.numeric(days_to_hosp <= 30)]
    
    DT_model_subset[is.na(hosp_within_30days), hosp_within_30days := 0]
    
    #Move "hosp_event_index" column to end
    col_names <- names(DT_model_subset)
    col_to_back <- "hosp_event_index"
    setcolorder(DT_model_subset, c(col_names[col_names != col_to_back], col_to_back))
    
    message("Adding key hospitalization outcome ", g_rxcui_name, " (Thread ", i, ")...")
    #A key hospitalization is either an adverse drug reaction, heart attack, or stroke
    
    DT_model_subset[, target_hosp_event_index :=
               find_most_recent_hospitalization_index(bene_id1 = bene_id,
                                                      srvc_dts = srvc_dt,
                                                      hosp_data = target_hosp),
             by = bene_id]
    
    #Merge on hospital data
    message("Merging key hospital outcomes ", g_rxcui_name, " (Thread ", i, ")...")
    
    DT_model_subset %<>% merge(target_hosp[, .(target_hosp_dt = hosp_dt,
                                        target_hosp_code = dgnscd,
                                        target_hosp_event_index = hosp_event_index)],
                        by = "target_hosp_event_index",
                        all.x = TRUE,
                        sort = FALSE) %>%
      .[, days_to_target_hosp := interval(ymd(srvc_dt), ymd(target_hosp_dt)) / days(1)] %>%
      .[, target_hosp_within_30days := as.numeric(days_to_target_hosp <= 30)]
    
    DT_model_subset[is.na(target_hosp_within_30days), target_hosp_within_30days := 0]
    
    #Move "target_hosp_event_index" column to end
    col_names <- names(DT_model_subset)
    col_to_back <- "target_hosp_event_index"
    setcolorder(DT_model_subset, c(col_names[col_names != col_to_back], col_to_back))
    
    message("Fininshed Thread ", i, " for ", g_rxcui_name, ".")
    
    return(DT_model_subset)
  }
  
  stopImplicitCluster()
  
  #Reassemble
  DT_model <- rbindlist(ret)
  rm(ret)
  
  # Adding OLS Predicted 30-day Mortality--------------------------------------------
  #Creates an OLS predicted mortality variable, but uses 5-fold cross validation 
  #to only use the prediction for the fold where the data was not trained on
  
  outcome <- "death_within_30days"
  
  covariates <- c("female",
                  "white",
                  "dual_cstshr_ind",
                  "age_in_mo",
                  "median_zip_income_log",
                  "n_grxcui_year_log", 
                  "dayssply_cat",
                  "bnftphas")
  
  my_library <- c("SL.lm")
  
  num_folds <- 5
  options(mc.cores = detectCores() - 1)
  set.seed(1, "L'Ecuyer-CMRG")
  
  message("Starting OLS SuperLearner ", g_rxcui_name)
  
  if (sd(DT_model[, get(outcome)]) != 0) # This condition will always be true as long as
                                         # we don't have every outcome the same
  {
    sl <- CV.SuperLearner(Y = DT_model[, get(outcome)], 
                          X = DT_model[, mget(covariates)],
                          family = gaussian(),
                          SL.library = my_library,
                          parallel = "multicore",
                          verbose = TRUE,
                          id = DT_model[, bene_id],
                          cvControl = list(V = num_folds),
                          innerCvControl = list(list(V = num_folds)))
    
    DT_model[, paste0("OLS_predicted_", outcome) := sl$SL.predict]
  } else {
    DT_model[, paste0("OLS_predicted_", outcome) := DT_model[, get(outcome)]]
  }
  
  # Export -----------------------------------------------------------------------
  message("Exporting ", g_rxcui_name,"...")
  
  fwrite(DT_model, paste0(lib_base_data, "preprocessDT_", g_rxcui_name, "_",
                          pct, ".csv"))
}

#Now add SuperLearner predicted mortality
if (do_superlearner)
{
  for (g_rxcui_name in classes$name)
  {
    DT_model <- fread(paste0(lib_base_data, "preprocessDT_", g_rxcui_name, "_",
                            pct, ".csv"))
    
    # Adding Predicted 30-day Mortality--------------------------------------------
    outcome <- "death_within_30days"
    
    covariates <- c("female",
                    "white",
                    "dual_cstshr_ind",
                    "age_in_mo",
                    "median_zip_income_log",
                    "n_grxcui_year_log",
                    "dayssply_cat")
    
    #Add some custom libraries
    SL.elastic_net1 = function(...) {
      SL.glmnet(..., alpha = 0)
    }
    
    SL.elastic_net2 = function(...) {
      SL.glmnet(..., alpha = 0.25)
    }
    
    SL.elastic_net3 = function(...) {
      SL.glmnet(..., alpha = 0.5)
    }
    
    SL.elastic_net4 = function(...) {
      SL.glmnet(..., alpha = 0.75)
    }
    
    my_library <- c("SL.glmnet", "SL.ranger", "SL.step.interaction", 
                    "SL.rpartPrune", "SL.mean", "SL.glm.interaction",
                    "SL.bayesglm", "SL.qda", "SL.elastic_net1",
                    "SL.elastic_net2", "SL.elastic_net3", "SL.elastic_net4")
    
    num_folds <- 5
    options(mc.cores = detectCores() - 1)
    set.seed(1, "L'Ecuyer-CMRG")
    
    message("Starting SuperLearner ", g_rxcui_name)
    
    sl <- CV.SuperLearner(Y = DT_model[, get(outcome)], 
                          X = DT_model[, mget(covariates)],
                          family = binomial(),
                          SL.library = my_library,
                          parallel = "multicore",
                          verbose = TRUE,
                          id = DT_model[, bene_id],
                          cvControl = list(V = num_folds),
                          innerCvControl = list(list(V = num_folds)))
    
    print(summary(sl))
    
    sink(paste0(lib_base_data, "superlearner_", g_rxcui_name, "_",
                outcome, "_fulldata_", pct, ".txt"))
    
    print(summary(sl))
    print(sl$whichDiscreteSL)
    
    sink()
    
    DT_model[, paste0("SL_predicted_", outcome) := sl$SL.predict]
    DT_model[, paste0("discrete_SL_predicted_", outcome) := sl$discreteSL.predict]
    
    # Export -----------------------------------------------------------------------
    message("Exporting ", g_rxcui_name,"...")
    
    fwrite(DT_model, paste0(lib_base_data, "preprocessDT_", g_rxcui_name, "_",
                            pct, ".csv"))
  }
}
