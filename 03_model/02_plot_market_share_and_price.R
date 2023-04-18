# Header -----------------------------------------------------------------------
# Proj: Bottle of Lies
# Author: Evan Flack (evanjflack@gmail.com) and
#         Tyler Mansfield (tyler.mansfield96@gmail.com)
# Desc: Plots market share and list/oop price of each generic manufacturer over
#       time, both individual manufacturer and by true vs branded generic status

# Start Script -----------------------------------------------------------------
package_list <- c("ggplot2", "RColorBrewer")
source("../00_pre_process/start_script.R")
source("../supporting_code/define_plot_theme.R")

option_list = list(
  make_option(c("-p", "--pct"), type = 'character', default = "20pct"),
  make_option(c("-n", "--g_rxcui_name"), type = 'character',
              default = "acetaminophen_hydrocodone_bitartrate")
)
unpack_opt(option_list)

# Read in Data -----------------------------------------------------------------
message("Reading in data...")

sample <- fread(paste0(lib_base_data, "preprocessDT_", g_rxcui_name, "_",
                       pct, ".csv"),
                select = c("bene_id", "rfrnc_yr", "month", "lab_name",
                           "strength", "dayssply", "totalcst", "ptpayamt"))

# Prep Data --------------------------------------------------------------------
message("Prepping data...")

# Subset variables, make running month variable, combine small labelers, define
# price by day variable
sample %<>%
  .[dayssply == 30] %>%
  .[, running_month := (rfrnc_yr - 2010)*12 + month] %>%
  .[, list_price30 := (totalcst/dayssply)*30] %>%
  .[, oop_price30 := (ptpayamt/dayssply)*30]

# Market Share/Price (Branded vs Generic) --------------------------------------
message("Plotting market share/price (true vs branded generics)...")

price_by_month_gen <- sample[, .(mean_list_price = mean(list_price30),
                                 mean_oop_price = mean(oop_price30),
                                 median_list_price = median(list_price30),
                                 median_oop_price = median(oop_price30),
                                 obs = .N),
                             by = .(brand, running_month, strength)] %>%
  .[, perc_lab := obs/sum(obs), by = .(running_month, strength)] %>%
  .[order(running_month, -perc_lab), ] %>%
  .[perc_lab >= .01, ] %>%
  .[, strength := paste(strength, "mg")]

# Market share
ggplot(price_by_month_gen) +
  aes(x = running_month, y = perc_lab, color = factor(brand)) +
  geom_line() +
  geom_point(size = .5) +
  labs(x = "Months Since Jan 2010", y = "Market Share",
       color = "Manufacturer") +
  facet_wrap(~ strength) +
  scale_color_brewer(palette = "Set3") +
  my_theme_paper

# ggsave(paste0(lib_base, "plots/", g_rxcui_name,
#               "/market_share_branded_gen.png"),
#        width = 8, height = 3.7)

# List price
ggplot(price_by_month_gen) +
  aes(x = running_month, y = mean_list_price, color = factor(brand)) +
  geom_line() +
  geom_point(size = .5) +
  labs(x = "Months Since Jan 2010", y = "List Price",
       color = "Manufacturer") +
  facet_wrap(~ strength) +
  scale_color_brewer(palette = "Set3") +
  my_theme_paper

# ggsave(paste0(lib_base, "plots/", g_rxcui_name,
#               "/list_price_true_branded_gen.png"),
#        width = 8, height = 3.7)

# Market Share Price (All Labs) ------------------------------------------------
message("Plotting market share/price (all labs)...")

price_by_month <- sample[, .(mean_list_price = mean(list_price30),
                             mean_oop_price = mean(oop_price30),
                             median_list_price = median(list_price30),
                             median_oop_price = median(oop_price30),
                             obs = .N),
                         by = .(lab_name, running_month, strength)] %>%
  .[order(running_month), ] %>%
  .[, strength := paste(strength, "mg")]

# Market share
# Make sure we have enough colors for the number of manufacturers
n.labs <- length(unique(price_by_month[["lab_name"]]))
mycolors <- colorRampPalette(brewer.pal(8, "Set3"))(n.labs)

ggplot(price_by_month) +
  aes(x = running_month, y = perc_lab, color = factor(lab_name)) +
  geom_line() +
  geom_point(size = .5) +
  labs(x = "Months Since Jan 2010", y = "Market Share",
       color = "Manufacturer") +
  facet_wrap(~ strength) +
  scale_fill_manual(values = mycolors) +
  my_theme_paper

# ggsave(paste0(lib_base, "plots/", g_rxcui_name,
#               "/market_share_all_labs.png"),
#        width = 8, height = 3.7)

strengths_multiple_manf <- price_by_month %>%
  .[, .(num_labs = length(unique(lab_name))), by = strength] %>% 
  .[num_labs > 1, strength]

price_by_month %<>% .[strength %in% strengths_multiple_manf]

# List price
ggplot(price_by_month) +
  aes(x = running_month, y = median_list_price, color = factor(lab_name)) +
  geom_line() +
  geom_point(size = .5) +
  labs(x = "Months Since Jan 2010", 
       y = "Median List Price for 30 Day Scripts",
       color = "Manufacturer",
       title = g_rxcui_name) +
  facet_wrap(~ strength, scales = "free_y") +
  scale_y_continuous(labels=scales::dollar_format()) +
  scale_fill_manual(values = mycolors) +
  my_theme_paper

# List price
ggplot(price_by_month) +
  aes(x = running_month, y = median_list_price, color = factor(lab_name)) +
  geom_line() +
  geom_point(size = .5) +
  labs(x = "Months Since Jan 2010", 
       y = "Median List Price for 30 Day Scripts",
       color = "Manufacturer",
       title = g_rxcui_name) +
  facet_wrap(~ strength) +
  scale_y_continuous(labels=scales::dollar_format(),
                     limits = c(0, max(price_by_month$median_list_price))) +
  scale_fill_manual(values = mycolors) +
  my_theme_paper

# upper_limit <- sample[running_month == 40, list_price30] %>%
#   quantile(0.99)
# 
# ggplot(sample[running_month == 40]) +
#   geom_density(aes(x = list_price30, color = lab_name)) +
#   xlim(c(0,upper_limit)) +
#   labs(x = "List Price for 30 Day Supply in May 2013 (Month 40)") +
#   theme_minimal()
# 
# ggplot(sample[running_month == 40]) +
#   geom_histogram(aes(x = list_price30, y=..density..)) +
#   facet_wrap(~lab_name, ncol = 1) +
#   theme_minimal()

# ggsave(paste0(lib_base, "plots/", g_rxcui_name,
#               "/list_price_all_labs.png"),
#        width = 8, height = 3.7)
