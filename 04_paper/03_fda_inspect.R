#!/usr/bin/env Rscript

# Header -----------------------------------------------------------------------
# Proj: Bottle of Lies
# Author: Tyler Mansfield (tyler.mansfield96@gmail.com)
# Desc: Compare estimated coefficients to proportion of bad inspections

# Start Script -----------------------------------------------------------------
package_list <- c("lme4", "fixest", "ggplot2", "doParallel", 
                  "stringr", "DescTools", "broom", "ggthemes")
source("../00_pre_process/start_script.R")

option_list = list(
  make_option(c("-p", "--pct"), type = 'character', default = "20pct"),
  make_option(c("-n", "--g_rxcui_name"), type = 'character',
              default = "atorvastatin"),
  make_option(c("-o", "--outcome"), type = 'character', default = "death_within_30days"),
  make_option(c("-b", "--model_name"), type = 'character',
              default = "full"),
  make_option(c("-s", "--start_date"), type = 'character',
              default = "2012-06-01"),
  make_option(c("-e", "--end_date"), type = 'character',
              default = "2014-01-01")
)

unpack_opt(option_list)

# Upload the regression results
regression_res <- read.delim(paste0(lib_base_data, "regression_", outcome, "_", 
                                    g_rxcui_name, "_", model_name, "_", 
                                    pct, ".txt"))[,1]

regression_res <- regression_res[str_detect(regression_res, "factor\\(lab_name\\)")] %>%
  str_remove_all("factor\\(lab_name\\)") %>%
  strsplit(" ") %>% 
  lapply(str_subset, ".{2,}")

regression_res <- do.call(rbind.data.frame, regression_res)
names(regression_res) <- c("Lab", "Estimate", "Std.Error", "t", "pval")
regression_res <- as.data.table(regression_res) %>%
  .[, Lab := tolower(Lab)] %>%
  .[, Estimate := as.numeric(Estimate)]

# Input the missing coefficient
preprocess_data_file_path <- paste0(lib_base_data, "preprocessDT_",
                                    g_rxcui_name, "_", pct, ".csv")
DT_model_labsize <- fread(preprocess_data_file_path, select = "lab_name")

lab_names_by_size <- DT_model_labsize[, .N, by = lab_name] %>% 
  .[order(N), tolower(lab_name)]

baseline_lab <- lab_names_by_size[!(lab_names_by_size %in% regression_res$Lab)]

assertthat::assert_that(length(baseline_lab) == 1)

regression_res %<>% rbind(data.table(Lab = baseline_lab, Estimate = 0, Std.Error = NA,
                                     t = NA, pval = NA))

regression_res[regression_res == "greenstone"] <- "pfizer"

# Upload the inspections data

DT_inspect <- fread(paste0(lib_base_data, "fda_inspections.csv"))
names(DT_inspect) <- make.names(names(DT_inspect), unique=TRUE)
DT_inspect %<>% .[Product.Type == "Drugs"] %>%
  .[, lab_name := tolower(str_remove_all(Legal.Name, "[^a-zA-Z]"))] %>%
  .[, lab_name_match := str_extract(lab_name, 
                                    paste0("(", paste(regression_res$Lab, 
                                                      collapse = "|"), 
                                           ")"))] %>%
  .[, Inspection.End.Date:= as.Date(Inspection.End.Date, format = "%m/%d/%Y")] %>%
  .[Inspection.End.Date < end_date & Inspection.End.Date > start_date] %>%
  .[!is.na(lab_name_match)]

data <- DT_inspect %>% .[, .(total_inspecs = .N,
                     total_OAI = sum(Classification == "Official Action Indicated (OAI)"),
                     prop_OAI = mean(Classification == "Official Action Indicated (OAI)"),
                     total_NAI = sum(Classification == "No Action Indicated (NAI)"),
                     prop_NAI = mean(Classification == "No Action Indicated (NAI)")), 
                 by = lab_name_match] %>%
  setnames("lab_name_match", "Lab")

data_merge <- data[regression_res, on = "Lab"] %>% .[order(Estimate)]

data_merge %>% dplyr::select(1:7)

ggplot(data_merge, aes(x = prop_OAI, y = Estimate, label = Lab)) +
  geom_point() +
  geom_text(hjust=0.5, vjust=-0.5) +
  scale_x_continuous(limits = c(0, 0.3)) +
  labs(y = "Estimated Death Within 30 Days Coef", 
       x = "Proportion of Official Action Indicated (OAI) Inspections") +
  theme_minimal()
