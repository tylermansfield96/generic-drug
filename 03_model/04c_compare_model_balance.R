# Header -----------------------------------------------------------------------
# Proj: Bottle of Lies
# Author: Tyler Mansfield (tyler.mansfield96@gmail.com)
# Desc: Compares the balance charts of multiple models

# Start Script -----------------------------------------------------------------
package_list <- c("ggplot2", "ggthemes")
source("../00_pre_process/start_script.R")
source("../supporting_code/define_plot_theme.R")

option_list = list(
  make_option(c("-p", "--pct"), type = 'character', default = "20pct"),
  make_option(c("-n", "--g_rxcui_name"), type = 'character',
              default = "atorvastatin"),
  make_option(c("-x", "--exclude_small"), type = 'logical', default = TRUE),
  make_option(c("-s", "--subset_data"), type = 'logical', default = FALSE),
  
  #The following two options are only relevant if `subset_data` = TRUE
  make_option(c("-a", "--min_arm_size"), type = 'integer', default = 1),
  make_option(c("-t", "--timeframe.length.months"), type = 'integer', default = 1)
)
unpack_opt(option_list)


# Read in/Format Estimates -----------------------------------------------------
message("Reading in/formatting estimates...")

classes <- c("atorvastatin")
outcomes <- c("female",
              "white",
              "dual_cstshr_ind",
              "age_in_mo",
              "median_zip_income_log",
              "price_per_pill",
              "days_since_last_fill_log",
              "n_grxcui_year_log")

outcome_labs <- c("Female",
                  "White",
                  "Dual/LIS",
                  "Age (months)",
                  "Zip Code Income (log)",
                  "Price per Pill",
                  "Days since Last Fill (log)",
                  "Number of Unique Drugs (log)")

model_names <- c("onlymail", "large", "small")

model_labels <- c("Mail-In", "Large Pharms", "Small Pharms")
dt_fit <- data.table()

for (model_name in model_names)
{
  if (subset_data == FALSE) {
    dt_fit1 <- fread(paste0(lib_base_data, "dem_balance_estimates_", g_rxcui_name,
                            '_', pct, '_', model_name, ".csv")) %>%
      .[, g_rxcui_name := g_rxcui_name]
  } else {
    dt_fit1 <- fread(paste0(lib_base_data, "dem_balance_estimates_", g_rxcui_name,
                            model_name, "_", pct, "_", min_arm_size, "minarm_",
                            timeframe.length.months, "monthtimeframe.csv")) %>%
      .[, g_rxcui_name := g_rxcui_name]
  }
  
  if (exclude_small)
  {
    dt_fit1 %<>% .[lab_name != "Small"]
  }
  
  dt_fit1 %<>%
    .[, fstat := round(fstat, 2)] %>%
    .[, outcome := factor(outcome, levels = outcomes, labels = outcome_labs)] %>%
    .[, title1 := paste0(outcome, " (Mean = ", sample_mean, ")")] %>%
    .[, title2 := paste0(outcome, " (Mean = ", sample_mean,
                         ", ANOVA p-val = ", pval, ")")] %>%
    .[, model := model_name]

  dt_fit %<>% rbind(dt_fit1)
}

# Make Plot --------------------------------------------------------------------
message(paste0("Plotting coefficients for ", g_rxcui_name, "..."))

#Reorder
dt_fit %<>% .[, model := factor(model, levels = model_names, labels = model_labels)] %>%
  .[outcome %in% outcome_labs]

ggplot(dt_fit) +
  aes(x = lab_name, y = estimate, ymin = lb, ymax = ub, color = model) +
  geom_point() +
  geom_errorbar() +
  geom_hline(yintercept = 0) +
  facet_wrap(~ outcome, scales = "free_y", ncol = 2) +
  scale_color_colorblind() +
  my_theme_paper

dt_fit[pval < 1e-5, pval := 1e-5]

options(scipen=10000)
ggplot(dt_fit) + 
  geom_point(aes(x = model, y = pval, colour = pval > 0.01)) +
  geom_hline(yintercept = 0.05, linetype = "dashed", show.legend = T) +
  geom_hline(yintercept = 0.01, linetype = "dashed") +
  facet_wrap(~outcome, ncol = 2) +
  scale_y_log10() +
  theme_minimal()

if (subset_data == FALSE){
  ggsave(paste0(lib_base, "plots/", g_rxcui_name, "/dem_balance_all.png"),
         width = 8, height = 3.7)
} else {
  ggsave(paste0(lib_base, "plots/", g_rxcui_name, "/dem_balance_all_",
                min_arm_size, "minarm_",
                timeframe.length.months, "monthtimeframe.png"),
         width = 8, height = 3.7)
}

# End --------------------------------------------------------------------------
