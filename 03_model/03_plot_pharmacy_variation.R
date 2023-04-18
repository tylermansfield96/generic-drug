# Header -----------------------------------------------------------------------
# Proj: Bottle of Lies
# Author: Evan Flack (evanjflack@gmail.com)
# Desc: Plots within pharmacy brand variation w/in sample and overall

# Start Script -----------------------------------------------------------------
package_list <- c("ggplot2", "stringr", "lubridate")
source("../00_pre_process/start_script.R")

option_list = list(
  make_option(c("-p", "--pct"), type = 'character', default = "20pct"),
  # make_option(c("-g", "--g_rxcui"), type = 'character', default = "29046"),
  make_option(c("-n", "--g_rxcui_name"), type = 'character',
              default = "atorvastatin"),
  make_option(c("-n", "--n_top"), type = 'character',
              default = "500") # Number of top pharamcies to plot 
  
)
unpack_opt(option_list)

# Start log file
#start_log_file("log/03_plot_pharmacy_variation")

# Read in Data -----------------------------------------------------------------
sample <- fread(paste0(lib_base_data, "generic_sample_", g_rxcui_name, "_", pct,
                        ".csv"))
# Format time and labeler variables
sample %<>%
  .[, srvc_dt := ymd(srvc_dt)] %>%
  .[, week := week(srvc_dt)] %>%
  .[week == 53, week := 52] %>%
  .[rfrnc_yr >= 2011] %>%
  .[, week1 := (rfrnc_yr - 2011)*52 + week] %>%
  .[, lab := str_pad(lab, 5, pad = "0")]

# Calculate top Manufacturers for Top Pharmacies -------------------------------
top_pharm <- sample %>%
  .[, .(.N), by = prvdr_id] %>%
  .[order(-N), ] %>%
  .[, pharm_ord := seq(1, .N)] %>%
  .[1:as.integer(n_top), ]

pharm_counts <- sample %>%
  .[prvdr_id %in% top_pharm$prvdr_id] %>%
  .[, .(obs = .N), by = .(prvdr_id, lab_name, week1, lab_name_long)] %>%
  .[, obs_pharm := sum(obs), by = .(prvdr_id, week1)] %>%
  .[, obs_pharm_total := sum(obs), by = prvdr_id] %>%
  .[, perc_pharm := obs/obs_pharm] %>%
  .[order(prvdr_id, week1, -obs)] %>%
  .[, .SD[1], by = .(prvdr_id, week1)] %>%
  merge(top_pharm, by = "prvdr_id")

# lab_names <- fread(paste0(lib_base_data, "ndc_lab_codes.csv")) %>%
#   setnames(names(.), c("lab", "lab_name_long")) %>%
#   .[, lab := str_pad(lab, 5, pad = "0")]

#pharm_counts %<>%
#  merge(lab_names, by = "lab")

# Plot within-pharmacy Variation -----------------------------------------------
ggplot(pharm_counts) +
  aes(y = pharm_ord, x = week1, fill = factor(lab_name)) +
  geom_tile() +
  scale_fill_brewer(palette = "Set3") +
  my_theme_paper +
  labs(x = "Weeks Since Jan 2011",
       y = paste0("Pharmacy"),
       fill = "Manufacturer",
       title = "Top Manufacturer for Each Week in Top 500 Pharmacies") +
  theme(axis.text.y = element_blank())

ggsave(paste0(lib_base, "plots/atorvastatin/pharmcy_lab_by_week.png"),
       width = 8, height = 3.7)

# Define branded vs "true" generic vs "branded" generic
sample %<>%
  .[, type := ifelse(lab %in% c("00781", "59762"), "'Branded' Generic",
                     ifelse(lab == "00071", "Branded", "'True' Generic"))] %>%
  .[, type := factor(type, levels = c("Branded", "'Branded' Generic",
                                      "'True' Generic"))]

pharm_counts1 <- sample %>%
  .[, .(obs = .N), by = .(prvdr_id, type, week1)] %>%
  .[, obs_pharm := sum(obs), by = .(prvdr_id, week1)] %>%
  .[, obs_pharm_total := sum(obs), by = prvdr_id] %>%
  .[, perc_pharm := obs/obs_pharm] %>%
  .[order(prvdr_id, week1, -obs)] %>%
  .[, .SD[1], by = .(prvdr_id, week1)] %>%
  # merge(my_lab_names) %>%
  merge(top_pharm, by = "prvdr_id") %>%
  .[, pharm_ord := factor(pharm_ord, levels = seq(1, 100))]

ggplot(pharm_counts1[obs_pharm >= 1 & pharm_ord %in% seq(1, 50) &
                       week1 >= 75, ]) +
  aes(y = factor(pharm_ord), x = week1, fill = factor(type)) +
  geom_tile() +
  scale_fill_brewer(palette = "Set3") +
  my_theme_paper +
  labs(x = "Weeks Since Jan 2011",
       y = "Pharmacy",
       fill = "Manufacturer") +
  theme(axis.text.y = element_blank())

#ggsave(paste0(lib_base, "plots/atorvastatin/pharmcy_type_by_week.png"),
#       width = 8, height = 3.7)

# End --------------------------------------------------------------------------

#end_log_file()
