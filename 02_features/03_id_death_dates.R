################################################################################
################################################################################
################################################################################
# NEEDS WORK
################################################################################
################################################################################
################################################################################

# Header -----------------------------------------------------------------------
# Proj: Bottle of Lies
# Author: Evan Flack (evanjflack@gmail.com)
# Desc: Identifies death dates for every beneficary in the sample

# Start Script -----------------------------------------------------------------
package_list <- c("stringr", "foreach", "doParallel")
source("../00_pre_process/start_script.R")

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type = 'character', default = "20pct"),
  make_option(c("-f", "--first_year"), type = 'integer', default = 2007),
  make_option(c("-l", "--last_year"), type = 'integer', default = 2013)
)
unpack_opt(option_list)

# Start log file
start_log_file(paste0("log/03_id_death_dates_", pct))

# Reading in Data --------------------------------------------------------------
message("Reading in data...")

registerDoParallel(cores = length(years))
deaths <- foreach(year = years,
                  .combine = "rbind") %dopar%
  {

    # Read in bsf file
    deaths1 <- fread(paste0(lib_base_data, "sample_", year, "_", pct,
                             ".csv")) %>%
      .[, .(bene_id, death_dt)] %>%
      .[death_dt != ""]
  }
stopImplicitCluster()

# Only keep the first observation for each beneficiary
deaths %<>%
  .[, .SD[1], by = bene_id]

# Export -----------------------------------------------------------------------

fwrite(deaths, paste0(lib_base_data, "sample_death_dates_", pct, ".csv"))

end_log_file()
