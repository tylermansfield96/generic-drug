# Header -----------------------------------------------------------------------
# Proj: Bottle of Lies
# Author: Tyler Mansfield (tyler.mansfield96@gmail.com)
# Desc: Plots within pharmacy brand variation over time. Plots look at long-term
#       pharmacies versus non-long term pharmacies, as well as stratifying a 
#       given pharmacy by strength

# Start Script -----------------------------------------------------------------
package_list <- c("ggplot2", "stringr", "lubridate")
source("../00_pre_process/start_script.R")

option_list = list(
  make_option(c("-p", "--pct"), type = 'character', default = "20pct"),
  make_option(c("-f", "--first_year"), type = 'integer', default = 2010),
  make_option(c("-n", "--g_rxcui_name"), type = 'character',
              default = "atorvastatin")
)
unpack_opt(option_list)

# Read in Data -----------------------------------------------------------------
sample <- fread(paste0(lib_base_data, "preprocessDT_", g_rxcui_name, "_",
                       pct, ".csv"))

# Format time and labeler variables
sample %<>%
  .[, srvc_dt := ymd(srvc_dt)] %>%
  .[, week := week(srvc_dt)] %>%
  .[week == 53, week := 52] %>%
  .[, running_week := (rfrnc_yr - first_year)*52 + week] %>%
  .[, lab := str_pad(lab, 5, pad = "0")] %>%
  .[, strength := paste0(strength, " mg")]

top_pharms <- sample %>%
  .[, .(.N), by = prvdr_id] %>%
  .[order(-N), ] %>%
  .[, pharm_ord := seq(1, .N)] %>%
  .[1:50, ]

top_phamr1 <- top_pharms[["prvdr_id"]][1]

# Plot ------------------------------------------------------------------------
plot_pharamcy_lab_trends <- function(num_to_plot, size_percentile, sample)
{
  #Extract pharamcies at specific quantile
  pharmacy_count <- sample %>%
    .[, .N, by = prvdr_id] 
  
  min_pharm_size <- pharmacy_count %>%
    .[, N] %>%
    quantile(size_percentile)
  
  pharms_to_plot <- pharmacy_count %>%
    .[N >= min_pharm_size] %>%
    .[order(N), prvdr_id] %>%
    .[1:100] %>%
    sample(num_to_plot)
  
  sample_week <- sample[, .N, by = .(prvdr_id, running_week, lab_name)]
  
  min_week <- sample_week[, running_week] %>% min()
  max_week <- sample_week[, running_week] %>% max()
  
  ggplot(sample_week[prvdr_id %in% pharms_to_plot]) +
    geom_point(aes(x = running_week, y = lab_name, size = N, color = lab_name)) +
    facet_wrap(~prvdr_id) +
    scale_x_continuous(breaks=seq(min_week, max_week, 4)) +
    labs(title = paste0("Pharmacies with about ", 
                        round(min_pharm_size),
                        " scripts in the whole sample (",
                        size_percentile * 100,
                        "th percentile)")) +
    theme_minimal() + 
    theme(plot.title = element_text(hjust = 0.5), 
          panel.grid.minor = element_blank())
}

plot_pharamcy_lab_trends(4, 0.9, sample = sample)

# Long term plotting -----------------------------------------------------------

plot_pharamcy_lab_trends_lt <- function(N, sample, min_sample_size = 0)
{
  pharms <- data.table()
  #Get similar sized non-longterm
  for (i in 1:N)
  {
    lt_pharm <- sample %>%
      .[NPI_label == "Suppliers/Pharmacy Long-term Care Pharmacy",
        .(sample_size = .N),
        by = prvdr_id] %>%
      .[sample_size >= min_sample_size] %>%
      .[, type := "LongTerm"] %>%
      .[sample(1:.N, 1)]

    pharms %<>% rbind(lt_pharm)

    non_lt_pharm <- sample %>%
      .[NPI_label != "Suppliers/Pharmacy Long-term Care Pharmacy",
        .(sample_size = .N),
        by = prvdr_id] %>%
      .[, distance_to_lt := abs(sample_size - lt_pharm[, sample_size])] %>%
      .[order(distance_to_lt)] %>%
      .[1, .(prvdr_id, sample_size, type = "NonLongTerm")]

    pharms %<>% rbind(non_lt_pharm)
  }

  new_labels <- paste(pharms$type,
                      pharms$sample_size,
                      pharms$prvdr_id,
                      sep = "_")

  pharms_to_plot_xwalk <- data.table(prvdr_id = pharms$prvdr_id,
                                     labels = new_labels)

  sample_subset <- sample %>% merge(pharms_to_plot_xwalk, by = "prvdr_id") %>%
    .[, prvdr_id := labels]

  sample_week <- sample_subset[, .N, by = .(prvdr_id, running_week, lab_name)]

  min_week <- sample_week[, running_week] %>% min()
  max_week <- sample_week[, running_week] %>% max()

  ggplot(sample_week) +
    geom_point(aes(x = running_week, y = lab_name, size = N, color = lab_name)) +
    facet_wrap(~prvdr_id, ncol = min(2, N)) +
    scale_x_continuous(breaks=seq(min_week, max_week, 4)) +
    labs(title = "Long Term vs Non-Long Term") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.minor = element_blank())
}

plot_pharamcy_lab_trends_lt(2, sample, min_sample_size = 1000)

# By strength Plot -------------------------------------------------------------
plot_pharamcy_lab_trends_strength <- function(size_percentile, 
                                              sample, 
                                              pharm_to_plot = NA,
                                              title_to_plot = NA)
{
  #Extract pharamcies at specific quantile
  pharmacy_count <- sample %>%
    .[, .N, by = prvdr_id] 
  
  min_pharm_size <- pharmacy_count %>%
    .[, N] %>%
    quantile(size_percentile)
  
  if (is.na(pharm_to_plot))
  {
    pharm_to_plot <- pharmacy_count %>%
      .[N >= min_pharm_size] %>%
      .[order(N), prvdr_id] %>%
      .[1:100] %>%
      sample(1)
  }

  sample_week <- sample[prvdr_id == pharm_to_plot, 
                        .N, 
                        by = .(prvdr_id, running_week, lab_name, strength, NPI_label)]
  
  NPI_label <- sample_week[1,NPI_label]
  
  min_week <- sample_week[, running_week] %>% min()
  max_week <- sample_week[, running_week] %>% max()
  
  if(is.na(title_to_plot))
  {
    title_to_plot <- paste0("Pharmacy ",
                            pharm_to_plot, 
                            ", ",
                            NPI_label)
  } else {
    title_to_plot <- paste0(title_to_plot, " (", pharm_to_plot, ")")
  }
  
  ggplot(sample_week) +
    geom_point(aes(x = running_week, y = lab_name, size = N, color = lab_name)) +
    facet_wrap(~strength, ncol = 1) +
    scale_x_continuous(breaks=seq(min_week, max_week, 12)) +
    labs(title = title_to_plot) +
    theme_minimal() + 
    theme(plot.title = element_text(hjust = 0.5))
}

plot_pharamcy_lab_trends_strength(0.95, sample = sample, pharm_to_plot = "1689689291",
                                  title_to_plot = "Bear Apothecary in NJ")
plot_pharamcy_lab_trends_strength(0.95, sample = sample)
