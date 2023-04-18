# Header -----------------------------------------------------------------------
# Proj: Bottle of Lies
# Author: Tyler Mansfield (tyler.mansfield96@gmail.com)
# Desc: Exports a subset of the original sample to only include scripts where
# every strata of (pharmacy, timeframe) has at least two different
# manufacturers


# Start Script -----------------------------------------------------------------
source("../00_pre_process/start_script.R")

option_list <- list(
  make_option(c("-p", "--pct"), type = 'character', default = "20pct"),
  make_option(c("-n", "--g_rxcui_name"), type = 'character',
              default = "atorvastatin"),
  make_option(c("-f", "--first_year"), type = 'integer', default = 2012),
  make_option(c("-m", "--first_month"), type = 'integer', default = 6),
  make_option(c("-l", "--last_year"), type = 'integer', default = 2013),
  make_option(c("-a", "--min_arm_size"), type = 'integer', default = 1),
  make_option(c("-m", "--timeframe.length.months"), type = 'double', default = 0.5),
  make_option(c("-d", "--min.days.supply"), type = 'integer', default = 14)
  
)
unpack_opt(option_list)

# Read in Data -----------------------------------------------------------------
message("Reading in data...")

# Index event sample and controls
orig.sample <- fread(paste0(lib_base_data, "generic_sample_", g_rxcui_name, "_",
                            pct, ".csv"))

# Subsetting Data --------------------------------------------------------------------
message("Subsetting data...")

# Select relevant columns, make running month variable, use that to find scripts 
# in same experimental time frame. Select only the generic scripts
sample <- orig.sample %>%
  .[, .(bene_id, rfrnc_yr, month, lab_name, srvc_dt,
        strength, prvdr_id, dayssply, year_month, week)] %>%
  .[, runningmonth := (rfrnc_yr - 2010)*12 + month] %>%
  .[, timeframe := ceiling(runningmonth/timeframe.length.months)]

if (timeframe.length.months < 1)
{
  sample %<>% .[, runningweek := (rfrnc_yr - 2010)*53 + week] %>%
    .[, timeframe := ceiling(runningweek/(timeframe.length.months*4))]
}

sample %<>%
  .[, brand := ifelse(lab_name == "Parke-Davis", "Branded", "Generic")] %>%
  .[brand == "Generic"]  %>%
  .[rfrnc_yr > first_year | (month >= first_month & rfrnc_yr == first_year), ] %>%
  .[dayssply >= min.days.supply, ]

#Each "unit" is a fixed pharmacy and time frame.
#Find out which units have at least 2 different generic brands
#with samples of size min_arm_size
sample.armlevel <- sample %>% 
  .[, .(num_in_arm = .N), keyby = .(prvdr_id, timeframe, lab_name)] %>%
  .[num_in_arm >= min_arm_size, ]

sample.experimentlevel <- sample.armlevel %>%
  .[, .(num_of_arms = .N, total_sample_size = sum(num_in_arm)), keyby = .(prvdr_id, timeframe)] %>%
  .[num_of_arms > 1] %>%
  .[, unit := paste0("p", as.character(prvdr_id),
      "t", as.character(timeframe))]

subset_unit_keys <- sample.experimentlevel[, unit]

# Subset
if (timeframe.length.months >= 1)
{
  subset.sample <- orig.sample %>%
    .[, runningmonth := (rfrnc_yr - 2010)*12 + month] %>%
    .[, timeframe := ceiling(runningmonth/timeframe.length.months)] %>%
    .[, unit := paste0("p", as.character(prvdr_id),
                       "t", as.character(timeframe))] %>%
    .[unit %in% subset_unit_keys, ]
} else
{
  subset.sample <- orig.sample %>%
    .[, runningweek := (rfrnc_yr - 2010)*53 + week] %>%
    .[, timeframe := ceiling(runningweek/(timeframe.length.months*4))] %>%
    .[, unit := paste0("p", as.character(prvdr_id),
                       "t", as.character(timeframe))] %>%
    .[unit %in% subset_unit_keys, ]

    #Change decimal to an underscore for file saving
    timeframe.length.months <- gsub("\\.", "_", as.character(timeframe.length.months))
}


# Exporting Data --------------------------------------------------------------------
message("Exporting data...")
fwrite(subset.sample, paste0(lib_base_data,
                             "generic_sample_subset_",
                             g_rxcui_name,
                             "_",
                             pct,
                             "_",
                             min_arm_size,
                             "minarm_",
                             gsub("\\.", "_", as.character(timeframe.length.months)),
                             "monthtimeframe.csv"))
