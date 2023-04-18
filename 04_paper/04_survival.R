#!/usr/bin/env Rscript

# Header -----------------------------------------------------------------------
# Proj: Bottle of Lies
# Author: Tyler Mansfield (tyler.mansfield96@gmail.com)
# Desc: Uses Kaplan-Meier to estimate the survival rate in our sample

# Start Script -----------------------------------------------------------------
package_list <- c("lme4", "fixest", "stringr", "DescTools", "survival")
source("../00_pre_process/start_script.R")

option_list = list(
  make_option(c("-p", "--pct"), type = 'character', default = "20pct"),
  make_option(c("-n", "--g_rxcui_name"), type = 'character',
              default = "atorvastatin") # Set to either atorvastatin or pool
)

unpack_opt(option_list)

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


# Analyzing survival times ----------------------------------------------------

## Crude Estimate
# Randomly take one script from each patient in our sample. 
# See how many died within 30 days of selected script
set.seed(1)
my_sample <- DT_model[DT_model[ , .I[sample(.N,1)] , by = bene_id]$V1]
mean(my_sample$death_within_30days)

## Kaplan-Meier raw -----------------------------------------------------------

set.seed(1)
my_sample <- DT_model[order(srvc_dt)] %>% .[, .SD[1], bene_id]
my_sample2 <- my_sample[, .(bene_id, srvc_dt, death_dt, death_on_record, days_to_death, dayssply_cat)]
my_sample2$days_in_sample <- as.IDate("12/31/2013", format = "%m/%d/%Y") - as.IDate(my_sample2$srvc_dt)
my_sample2[, days_in_sample := pmin(days_to_death, days_in_sample, na.rm = T)]

survmod1 <- survfit(Surv(days_in_sample, death_on_record)~1, data=my_sample2, 
                    conf.type="log-log")
survmod1.summary <- summary(survmod1)

plot(survmod1, 
     main="Kaplan-Meier Survival Estimates",
     xlab="Time (days)", ylab="Survival Probability", 
     bty="l", col=c("darkblue"))

cbind(survmod1$time, survmod1$surv)[31, ]

## Kaplan-Meier by days supply (just for fun) ----------------------------------

survmod1 <- survfit(Surv(days_in_sample, death_on_record)~dayssply_cat, data=my_sample2, 
                    conf.type="log-log")
survmod1.summary <- summary(survmod1)

plot(survmod1, 
     main="Kaplan-Meier Survival Estimates",
     xlab="Time (days)", ylab="Survival Probability", 
     bty="l", col=c("darkblue","darkorange", "red", "green"))

legend("bottomright",legend=c("14-21","22-40","41-65","66+"),
       lty=1, lwd=2, 
       col=c("darkblue","darkorange", "red", "green"),
       cex=0.8,bty="o")
