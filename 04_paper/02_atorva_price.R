#!/usr/bin/env Rscript

# Header -----------------------------------------------------------------------
# Proj: Bottle of Lies
# Author: Tyler Mansfield (tyler.mansfield96@gmail.com)
# Desc: Estimates manufacturer-month effects on the list price of a drug 

# Start Script -----------------------------------------------------------------
package_list <- c("lme4", "fixest", "ggplot2", "doParallel", 
                  "stringr", "DescTools", "broom", "ggthemes")
source("../00_pre_process/start_script.R")

option_list = list(
  make_option(c("-p", "--pct"), type = 'character', default = "20pct"),
  make_option(c("-n", "--g_rxcui_name"), type = 'character',
              default = "atorvastatin")
)

unpack_opt(option_list)

# Begin balance ---------------------------------------------------------------
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
message("Prepping model for ", g_rxcui_name, "...")
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

DT_model[, manf_month := as.factor(paste0(lab_name, '_', runningmonth)) %>%
           relevel(paste0(baseline_lab, '_', max(DT_model$runningmonth)))]

fe_vars <- c("prvdr_id",
             "cntrct_pbp_rfrnc_yr",
             "region",
             "bnftphas",
             "strength")

se_cluster_level <- "prvdr_id"

fe_form <- paste(fe_vars, collapse = " + ")

# Coefficients (all)
message("Fitting feols model ")
form_string <- paste0("listprice_30dayssply ~ manf_month | ", fe_form)
message("Form : ", form_string)
form_all <- as.formula(form_string)

fit_df <- data.table()

# Multiple plots for each strength (exclude strength from fixed effects)
# for (strength_i in sort(unique(DT_model$strength)))
# {
#   message("Fitting model for ", strength_i, " mg")
#   
#   DT_model_strength <- DT_model[strength == strength_i]
#   
#   fit <- feols(form_all, DT_model_strength, se = "cluster", cluster = se_cluster_level)
#   
#   fit_df_i <- tidy(fit) %>% data.table() %>% 
#     .[, running_month := gsub('\\D', '', term)] %>%
#     .[, manf := str_remove_all(gsub('manf_month', '', term), '[0-9_]')] %>%
#     .[, strength := paste0(strength_i, " mg")]
#   
#   fit_df <- rbind(fit_df, fit_df_i)
# }

# One plots for each strength

fit <- feols(form_all, DT_model, se = "cluster", cluster = se_cluster_level)

fit_df <- tidy(fit) %>% data.table() %>%
  .[, running_month := gsub('\\D', '', term)] %>%
  .[, manf := str_remove_all(gsub('manf_month', '', term), '[0-9_]')]# %>%
  #.[, strength := paste0(strength_i, " mg")]

ggplot(fit_df[running_month > 23], aes(x = as.numeric(running_month), y = estimate)) +
  geom_line(aes(col = manf)) +
  geom_point(aes(col = manf)) +
  scale_color_colorblind() +
  labs(y = "Month/Manufacturer Effect", x = "",
       title = "Month/Manufacturer Effect of Atorvastatin List Price 30 Day Supply",
       caption = "Baseline is Apotex 12/2013") +
  scale_x_continuous(breaks = c(0, 6, 12, 18, 24) + 24, 
                   labels = c("12/2011","06/2012", "12/2012", 
                              "06/2013", "12/2013")) +
  scale_y_continuous(labels=scales::dollar_format()) +
  #facet_wrap(~ strength) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), 
        panel.spacing = unit(2, "lines"))

message("Finished ", g_rxcui_name, ".")

DT_model[, .N, by = .(lab_name, runningmonth)] %>%
  .[order(runningmonth)]
