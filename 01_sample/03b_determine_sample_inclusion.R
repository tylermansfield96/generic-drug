# ------------------------------------------------------------------------------
# Proj: Bottle of Lies
# Author: Evan Flack (evanjflack@gmail.com)
# Desc: Creates month sample pf people that are alive, and covered in a PDP
# ------------------------------------------------------------------------------

# Start Script -----------------------------------------------------------------
package_list <- c("foreach", "doParallel", "lubridate")
source("../00_pre_process/start_script.R")

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type = 'character', default = "0001pct"),
  make_option(c("-f", "--first_year"), type = 'integer', default = 2007),
  make_option(c("-l", "--last_year"), type = 'integer', default = 2013)
)
unpack_opt(option_list)

# Start log file
start_log_file(paste0("log/03b_determine_sample_inclusion_", pct))

# Read in Data -----------------------------------------------------------------

registerDoParallel(cores = length(years))
obs_by_year <- foreach(year = years,
               .combine = "rbind",
               .multicombine = TRUE) %dopar%
  {
      sample <- fread(paste0(lib_base_data, "month_level_sample_", year,
                               "_", pct, ".csv")) %>%
        .[order(bene_id, rfrnc_yr, month), ]

      first_last_obs <- sample %>%
        .[part_d_ind == 1, ] %>%
        .[, .(first_mo = month[1], last_mo = month[.N],
              part_d_mo = sum(part_d_ind), pdp_mo = sum(pdp_ind)),
          by = .(bene_id, rfrnc_yr)]
  }
stopImplicitCluster()

# Make Subsets -----------------------------------------------------------------
# Want people who had continuous coverage from when they first appear to when
# they last appear.

# Caclculate the amount of months we would expect to see an individual, and the
# amount of months that they had part d (and pdp) coverage
sample_include <- obs_by_year %>%
  .[, .(first_yr = rfrnc_yr[1], first_mo = first_mo[1],
        last_yr = rfrnc_yr[.N], last_mo = last_mo[.N],
        part_d_mo = sum(part_d_mo), pdp_mo = sum(pdp_mo)), by = bene_id] %>%
  .[first_yr == last_yr, exp_mo := last_mo - first_mo + 1] %>%
  .[first_yr + 1 == last_yr, exp_mo := (12 - first_mo + 1) + last_mo] %>%
  .[is.na(exp_mo), exp_mo := (12 - first_mo + 1) + 12*(last_yr - first_yr - 1) +
      last_mo]

# Make the subsets (people who were in a pdp the whole time, for the expected
# time)
sample_include %<>%
  .[part_d_mo == pdp_mo, ] %>%
  .[part_d_mo == exp_mo, ] %>%
  .[, .(bene_id, first_yr, first_mo, last_yr, last_mo, part_d_mo)]

# Export -----------------------------------------------------------------------

fwrite(sample_include, paste0(lib_base_data, "benes_for_sample_",
                        pct, ".csv"))

# End --------------------------------------------------------------------------

end_log_file()
