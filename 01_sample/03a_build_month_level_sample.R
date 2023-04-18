# Header -----------------------------------------------------------------------
# Proj: Bottle of Lies
# Author: Evan Flack (evanjflack@gmail.com)
# Desc: Transforms wide variables to long, makes indicators for part d coverage,
#       (whether in a pdp or not), dual status, and whether a beneficiary is
#       alive at the start of the month

# Start Script -----------------------------------------------------------------
package_list <- c("stringr", "lubridate", "foreach", "doParallel")
source("../00_pre_process/start_script.R")

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type = 'character', default = "0001pct"),
  make_option(c("-f", "--first_year"), type = 'integer', default = 2007),
  make_option(c("-l", "--last_year"), type = 'integer', default = 2013)
)
unpack_opt(option_list)

# Start log file
start_log_file(paste0("log/03a_make_month_level_sample_", pct))

# Reading in Data --------------------------------------------------------------
message("Reading in data...")

registerDoParallel(cores = length(years))
ret <- foreach(year = years) %dopar%
  {
    # Read in bsf file (for beneficiaries with at least 1 month pdp coverage in
    # the year.)
    sample_wide <- fread(paste0(lib_base_data, "sample_", year, "_", pct,
                                ".csv"))

    # Reshape month level variables to long
    part_d_vars <- grep("part_d_ind", names(sample_wide), value = T)
    cntrct_vars <- grep("cntrct", names(sample_wide), value = T)
    pbp_vars <- grep("pbp", names(sample_wide), value = T)
    dual_vars <- grep("dual_ind", names(sample_wide), value = T)
    cstshr_vars <- grep("cstshr_ind", names(sample_wide), value = T)
    part_d_long <- reshape_month_level(sample_wide, c("bene_id"),
                                       part_d_vars, "part_d_ind")
    cntrct_long <- reshape_month_level(sample_wide, c("bene_id"),
                                       cntrct_vars, "cntrct")
    pbp_long <- reshape_month_level(sample_wide, c("bene_id"),
                                    pbp_vars, "pbp")
    dual_long <- reshape_month_level(sample_wide, c("bene_id"),
                                     dual_vars, "dual_ind")
    cstshr_long <- reshape_month_level(sample_wide, c("bene_id"),
                                       cstshr_vars, "cstshr_ind")

    # Death Dates
    deaths <- copy(sample_wide[, .(bene_id, death_dt)]) %>%
      .[death_dt != ""] %>%
      .[order(bene_id), ] %>%
      .[, .SD[1], by = bene_id] %>%
      .[, death_dt := mdy(death_dt)] %>%
      .[, `:=`(death_mo = month(death_dt), death_yr = year(death_dt))]

    # Age and birth month
    dt_age <- sample_wide[, .(bene_id, rfrnc_yr, birth_mo, age1)]

    rm(sample_wide)

    # Combine long data sets
    sample_long <- cbind(part_d_long,
                         pbp_long[, .(pbp)],
                         cntrct_long[, .(cntrct)],
                         dual_long[, .(dual_ind)],
                         cstshr_long[, .(cstshr_ind)]) %>%
      .[, rfrnc_yr := year]

    rm(part_d_long, cntrct_long, dual_long, cstshr_long)

    # Determine if alive at the beginning of the month
    sample_long %<>%
      merge(deaths, by = "bene_id", all.x = T) %>%
      .[is.na(death_dt), `:=`(death_yr = 9999, death_mo = 9999)] %>%
      .[, alive := ifelse(death_yr > rfrnc_yr, 1,
                          ifelse(death_mo >= month, 1, 0))]

    # Define age in months
    sample_long %<>%
      merge(dt_age, by = c("bene_id", "rfrnc_yr")) %>%
      .[, age_in_mo := age1*12 + (month - birth_mo)]

    # Make pbp indicator, and make variable subsets
    sample_long %<>%
      .[, pdp_ind := ifelse(substr(cntrct, 1, 1) == "S", 1, 0)] %>%
      .[, dual_cstshr_ind := ifelse(dual_ind == 1 | cstshr_ind == 1, 1, 0)] %>%
      .[, .(bene_id, rfrnc_yr, month, alive, cntrct, pbp, part_d_ind, pdp_ind,
            dual_cstshr_ind, age_in_mo)]

    # Export
    fwrite(sample_long, paste0(lib_base_data, "month_level_sample_", year, "_",
                               pct, ".csv"))

    message("Done ", year)
  }
stopImplicitCluster()

# End --------------------------------------------------------------------------

end_log_file()
