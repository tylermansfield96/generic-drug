# Header -----------------------------------------------------------------------
# Proj: Bottle of Lies
# Author: Evan Flack (evanjflack@gmail.com) and
#         Tyler Mansfield (tyler.mansfield96@gmail.com)
# Desc: Plots covariate balance coefficient estimates using results from 
#       04a_estimate_covariate_balance.R

# Start Script -----------------------------------------------------------------
package_list <- c("ggplot2", "R.utils")
source("../00_pre_process/start_script.R")
source("../supporting_code/define_plot_theme.R")

option_list = list(
  make_option(c("-p", "--pct"), type = 'character', default = "20pct"),
  make_option(c("-n", "--g_rxcui_name"), type = 'character',
              default = "atorvastatin"),
  make_option(c("-u", "--plot_outcome"), type = 'character',
              default = "all"), #Which outcome coefficients to plot
  #Can use "all" on either g_rxcui_name or on plot_outcome
  make_option(c("-o", "--model_name"), type = 'character',
              default = "full")
)
unpack_opt(option_list)

# Read in/Format Estimates -----------------------------------------------------
message("Reading in/formatting estimates...")

# For multiple drugs -----------------------------------------------------------

if (g_rxcui_name == "all")
{
  classes <- fread(paste0(lib_base_data, "topdrugs_", pct, ".csv")) %>%
    .[1:10, .(name = key_word, g_rxcui)]
} else
{
  classes <- data.table(name = g_rxcui_name)
}


dt_fit_plot <- data.table()

for (g_rxcui_name in classes$name)
{
  file_name <- paste0(lib_base_data, "dem_balance_estimates_", g_rxcui_name,
                      '_', pct, '_', model_name, ".csv")
  
  dt_fit_plot1 <- fread(file_name) %>%
    .[, g_rxcui_name := g_rxcui_name]
  dt_fit_plot %<>% rbind(dt_fit_plot1)
}

#Display when the file was last modified
last_modified <- lastModified(file_name)

message("File last modified ", 
        difftime(Sys.time(), last_modified, units = "hours") %>% round(2),
        " hours ago.")

outcomes <- c("female",
              "white",
              "dual_cstshr_ind",
              "age_in_mo",
              "median_zip_income_log",
              "n_grxcui_year_log",
              "predicted_death_within_30days",
              "listprice_30dayssply",
              "oop_30dayssply",
              "death_within_30days",
              "total_spending_year_log",
              "total_oop_year_log",
              "days_since_last_fill_log")

outcomes_labs <- c("Female",
                  "White",
                  "Dual/LIS",
                  "Age (months)",
                  "Zip Code Income (log)",
                  "Number of Unique Drugs (log)",
                  "Mortality - Predicted",
                  "30 Day Supply List Price",
                  "30 Day Supply OOP Price",
                  "Mortality- True",
                  "Total Spending (log)",
                  "Total OOP (log)",
                  "Days since Last Fill (log)")

#If not plotting all outcomes, subset to the one we are plotting
if (plot_outcome != "all")
{
  index <- which(outcomes == plot_outcome)
  outcomes <- outcomes[index]
  outcomes_labs <- outcomes_labs[index]
}

if ("pval" %in% names(dt_fit_plot))
{
  dt_fit_plot %<>% .[, anova_pval := pval]
}

dt_fit_plot %<>%
  .[outcome %in% outcomes] %>%
  .[, outcome := factor(outcome, levels = outcomes, labels = outcomes_labs)] %>%
  .[, title1 := paste0(outcome, "\n (Mean = ", signif(sample_mean,3), ")")] %>%
  .[, title2 := paste0(outcome, "\n (Mean = ", signif(sample_mean,3),
                       ", ANOVA p-val = ", signif(anova_pval, 3), ")")] %>%
  .[, title3 := paste0(g_rxcui_name, "\n (Mean = ", signif(sample_mean,3),
                       ", ANOVA p-val = ", signif(anova_pval, 3), ")")]

# Make Plot --------------------------------------------------------------------
message(paste0("Plotting coefficients for ", g_rxcui_name, "..."))

if (nrow(classes) == 1 & length(outcomes) > 1) #One drug, many outcomes
{
  #Order the manufacturers by size
  preprocess_data_file_path <- paste0(lib_base_data, "preprocessDT_",
                                      g_rxcui_name, "_", pct, ".csv")
  DT_model_labsize <- fread(preprocess_data_file_path, select = "lab_name")
  
  lab_names_by_size <- DT_model_labsize[, .N, by = lab_name] %>% 
    .[order(N), lab_name]
  
  dt_fit_plot[, lab_name := factor(lab_name, levels = lab_names_by_size)]
  
  #Plot
  my_plot <- ggplot(dt_fit_plot) +
    aes(y = lab_name, x = estimate, xmin = lb, xmax = ub) +
    geom_point() +
    geom_errorbar() +
    geom_vline(xintercept = 0) +
    facet_wrap(~ title2, scales = "free", ncol = 4, dir = "v") +
    my_theme_paper
  
  my_plot
} else if (length(outcomes) == 1) #One outcome, potentially many drugs
{
  my_plot <- ggplot(dt_fit_plot) +
    aes(y = lab_name, x = estimate, xmin = lb, xmax = ub) +
    geom_point() +
    geom_errorbar() +
    geom_vline(xintercept = 0) +
    labs(x = outcomes) +
    facet_wrap(~ title3, scales = "free", ncol = 4) +
    my_theme_paper
  
  my_plot
}
