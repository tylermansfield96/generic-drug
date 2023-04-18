# ------------------------------------------------------------------------------
# Proj: Bottle of Lies
# Author: Evan Flack
# Desc: Downloads, updates, and loads packages needed by each script from
#       from CRAN. Downloads/updates/loads the cfo.behavioral package from
#       github
# ------------------------------------------------------------------------------

# Define Functions -------------------------------------------------------------

# Fxn: install_update_packages
# Desc: downloads uninstalled packages and updates installed packages
# Arg:
#   package_list - string vector of package names
#   package_path - string, path to where packages are downloaded
#   update - logical, if T then updatea all packages in package_path
# Example:
#   install_update_packages(package_list = c("ggplot2", "dplyr"),
#                           package_path = "~/baicker-DUA50589/eflack-dua50589/behavioral_hazard/Rpackages",
#                           update = TRUE)

install_update_packages <- function(package_list, package_path,
                                    update = FALSE) {

  # Update all packages already installed
  if (update == T) {
    update.packages(lib.loc = package_path, ask = FALSE)
  }
  # Install packages that are uninstalled
  installed <- installed.packages(lib.loc = package_path)[, 1]

  uninstalled <- unlist(package_list[!(package_list %in% installed)])
  if (length(uninstalled) >= 1) {
    installed_packages <- lapply(uninstalled, install.packages,
                                 lib = package_path)
    rm(installed_packages)
  }
}
# Fxn: load_packages
# Desc: Loads installed packages
# Arg:
#   package_list - string vector of package names
#   package_path - string, path to where packages are downloaded
# Example:
#   load_packages(package_list = c("ggplot2", "dplyr"),
#                 package_path = "~/baicker-DUA50589/eflack-dua50589/behavioral_hazard/Rpackages")
load_packages <- function(package_list, package_path) {
  # load all packages
  loaded_packages <- suppressMessages(lapply(package_list, library,
                                             lib.loc = package_path,
                                             character.only = T,
                                             quietly = T))
}

# Install/Update/Load Packages -------------------------------------------------
# path to packages
package_path <- "/homes/nber/eflack-dua50589/baicker-DUA50589/eflack-dua50589/Rpackages"

# packages required by every script
always_package_list <- c("rlang", "data.table", "magrittr", "tictoc",
                         "optparse", "devtools", "withr")
# adds more packages to be installed if asked for by the script
if (exists("package_list")) {
  package_list <- c(always_package_list, package_list)
} else {
  package_list <- always_package_list
}
package_list <- unique(package_list)

# Install and update packages
install_update_packages(package_list, package_path)
# Load packages
load_packages(package_list, package_path)

# Polypharmacy Package ---------------------------------------------------------

# Detach
if ("cfo.behavioral" %in% (.packages())) {
  detach("package:cfo.behavioral", unload=TRUE)
}

# Update
suppressMessages(with_libpaths(new = package_path,
                               install_github("evanjflack/cfo.behavioral@iv",
                                              dependencies = FALSE,
                                              quiet = FALSE)))


# Load
library("cfo.behavioral", lib.loc = package_path)

# Plot Theme -------------------------------------------------------------------
# if ggplot2 is loaded, defines the base theme

# if ("ggplot2" %in% package_list) {
#   source("define_plot_theme.R")
# }

# End --------------------------------------------------------------------------

remove(always_package_list, package_path, install_update_packages,
       load_packages)
