# ------------------------------------------------------------------------------
# Proj: Bottle of Lies
# Author: Evan Flack (evanjflack@gmail.com)
# Desc: Subsets the month level sample to only those months determined in
#       03b_make_month_level_sample_subsets.R
# ------------------------------------------------------------------------------

# Start Script -----------------------------------------------------------------
package_list <- c("stringr", "fastDummies", "foreach", "doParallel")
source("../00_pre_process/start_script.R")

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type = 'character', default = "0001pct"),
  make_option(c("-f", "--first_year"), type = 'integer', default = 2007),
  make_option(c("-l", "--last_year"), type = 'integer', default = 2013)
)
unpack_opt(option_list)

# Start log file
start_log_file(paste0("log/03c_make_month_level_sample_subsets_", pct))

# Sample Subsets ---------------------------------------------------------------

sample_benes <- fread(paste0(lib_base_data, "benes_for_sample_",
                             pct, ".csv")) %>%
  # Make continuous month variables (from Jan 2007 to Dec 2013)
  .[, first_month1 := first_mo + (first_yr - 2007)*12] %>%
  .[, last_month1 := last_mo + (last_yr - 2007)*12] %>%
  .[, .(bene_id, first_month1, last_month1)]

registerDoParallel(cores = length(years))
ret <- foreach(year = years) %dopar%
  {
    # Sample observations
    sample <- fread(paste0(lib_base_data, "month_level_sample_", year, "_",
                           pct, ".csv")) %>%
      .[, .(bene_id, rfrnc_yr, month)] %>%
      # Make continuous month variables (from Jan 2007 to Dec 2013)
      .[, month1 := month + (rfrnc_yr - 2007)*12]

    # Keep only months determined in subset
    sample %<>%
      merge(sample_benes, by = "bene_id") %>%
      .[month1 >= first_month1 & month1 <= last_month1] %>%
      .[, .(bene_id, rfrnc_yr, month, month1)]

    fwrite(sample, paste0(lib_base_data,
                          "covered_and_alive_month_level_sample_",
                          year, "_", pct, ".csv"))

    message("End ", year, ".")

  }

# End --------------------------------------------------------------------------

end_log_file()
