# ------------------------------------------------------------------------------
# Proj: Bottle of Lies
# Autor: Evan Flack (evanjflack@gmail.com)
# Desc: Script to include at the beginning of all R scripts in the
#   enroll_month directory.
#     (1) Defines base directories for code and data
#     (2) Installs/updates/loads libraries in package_list, including the
#         behavioral hazard package from github
# ------------------------------------------------------------------------------


# Set base working directories
# lib_base <- "~/baicker-DUA50589/eflack-dua50589/bottle/"
lib_base_data <- "/homes/nber/eflack-dua50589/baicker-DUA50589/eflack-dua50589/bottle_data/"

# Installs/loads packages
source("../supporting_code/load_packages.R")

# Loads plot themes (if using ggplot)
if ("ggplot2" %in% package_list ) {
  source("../supporting_code/define_plot_theme.R")
}
