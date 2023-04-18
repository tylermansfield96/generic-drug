# Header -----------------------------------------------------------------------
# Proj: Bottle of Lies
# Author: Evan Flack (evanjflack@gmail.com)
# Desc: Script to wrtite data frame with classes of interest, their g_rxcui
#       codes, branded labelers, and sample start/end dates.

# Start Script -----------------------------------------------------------------
# package_list <- c("stringr", "foreach", "doParallel", "lubridate")
source("../00_pre_process/start_script.R")

# Start log file
# tart_log_file(paste0("log/07_make_make_compound_subsets_", pct))

classes <- readRDS(paste0(lib_base_data, "classes_of_interest.rds"))


classes <- data.table(name = c("metoprolol", "simvastatin", "lisinopril",
                               "amlodipine", "atorvastatin",
                               "hydrochlorothiazide", "atenolol",
                               "carvedilol", "diltiazem", "pravastatin",
                               "losartan"),
                      g_rxcui = c("6918", "36567", "29046", "17767", "83367",
                                  "5487", "1202", "20352", "3443" , "42463",
                                  "52175"),
                      brand_lab = c())

lab_names <- fread(paste0(lib_base_data, "ndc_lab_codes.csv")) %>%
  setnames(names(.), c("lab", "lab_name")) %>%
  .[, lab := str_pad(lab, 5, pad = "0")] %>%
  .[order(lab_name), ]

lab_names[grepl("Astra", lab_names$lab_name)]
