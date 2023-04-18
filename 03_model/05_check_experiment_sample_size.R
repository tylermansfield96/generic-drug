# Header -----------------------------------------------------------------------
# Proj: Bottle of Lies
# Author: Tyler Mansfield (tyler.mansfield96@gmail.com)
# Desc: Defines an experimental stratum, which is a fixed pharmacy, medication 
#   and time frame (length given in months). Within each experimental 
#   stratum, we check to see how many strata exist with at least two different 
#   treatment arms present. We then see how many strata exist with at least two 
#   different treatment arms when we require each arm size to be greater than 
#   some n. The same analysis is performed to see how this subsetting process 
#   affects the total number of pharmacies represented in our sample and the 
#   proportion of scripts kept in our subset


# Global Variables To Set ------------------------------------------------------
timeframe.length.months <- c(3) #Candidates for lengths of the 
                                      #timeframe for each stratum

#i.e. if timeframe.length = 3, then we will consider script A from pharmacy 2 in 
#Jan 2020 to be in the same stratum as script B from pharmacy 2 in March 2020, but
#but NOT in the same stratum as script C from pharmacy 2 in April 2020


min_arm_sizes <- c(1,2,3,4,5,6,7,8,9,10,20)       #Candidates for the minimum arm sizes
                            #to test when subsetting

# Start Script -----------------------------------------------------------------
package_list <- c("ggplot2")
source("../00_pre_process/start_script.R")

option_list <- list(
  make_option(c("-p", "--pct"), type = 'character', default = "20pct"),
  make_option(c("-n", "--g_rxcui_name"), type = 'character',
              default = "atorvastatin")
)

unpack_opt(option_list)


# Creating Output Dataframes  -----------------------------------------------------------------
message("Creating output dataframes...")

#Create object to store results
results <- list()

col_names <- paste(timeframe.length.months, "month(s)")

row_names <- c("Total Units",
               "Units with 2+ Arms",
               paste("Units with 2+ Arms with Size ", min_arm_sizes, "+", sep = ""))

results$strata <- matrix(ncol = length(timeframe.length.months), 
                     nrow = 2 + length(min_arm_sizes), 
                     dimnames = list(row_names, col_names))

row_names <- c("Total # Pharmacies",
               "# Phamarcies with Units that have 2+ Arms",
               paste("# Phamarcies with Units that have 2+ Arms with Size ", min_arm_sizes, "+", sep = ""))

results$prvdr <- matrix(ncol = length(timeframe.length.months), 
                        nrow = 2 + length(min_arm_sizes), 
                        dimnames = list(row_names, col_names))

row_names <- c("Total Scripts",
               "# of Scripts in Units with 2+ Arms",
               paste("# of Scripts in Units that have 2+ Arms with size ", min_arm_sizes, "+", sep = ""))

results$scripts <- matrix(ncol = length(timeframe.length.months), 
                          nrow = 2 + length(min_arm_sizes), 
                          dimnames = list(row_names, col_names))

nth_largest_value <- function(x, n = 2)
{
  sort(x)[length(x) - n + 1]
}

# Read in Data -----------------------------------------------------------------
message("Reading in data...")

# Index event sample and controls
orig.sample <- fread(paste0(lib_base_data, "generic_sample_", g_rxcui_name, "_",
                       pct, ".csv"))

# Analyzing Data --------------------------------------------------------------------
message("Analyzing data...")

for (i in 1:length(timeframe.length.months))
{
  # Set timeframe length
  timeframe.length <- timeframe.length.months[i]
  
  # Subset variables, make running month variable, use that to find scripts in same
  # experimental time frame. Select only the generic scripts
  sample <- orig.sample %>%
    .[, .(bene_id, rfrnc_yr, month, lab_name, srvc_dt,
          strength, prvdr_id, dayssply, year_month)] %>%
    .[, runningmonth := (rfrnc_yr - 2010)*12 + month] %>%
    .[, timeframe := ceiling(runningmonth/timeframe.length)]
  
  sample %<>%
    .[, brand := ifelse(lab_name == "Parke-Davis", "Branded", "Generic")] %>%
    .[brand == "Generic"]
  
  #Each "strata" is a fixed pharmacy and time frame.
  #Find out which strata have at least 2 different generic brands
  sample.armlevel <- sample %>% 
    .[, .N, keyby = .(prvdr_id, timeframe, lab_name)] %>%
    .[, num_in_arm := N] %>%
    .[, -c("N")]
  
  sample.stratalevel <- sample.armlevel %>%
    .[, .N, keyby = .(prvdr_id, timeframe)] %>%
    .[, num_of_arms := N] %>%
    .[, -c("N")]
  
  sample.prvdrlevel <- sample.stratalevel %>%
    .[, .N, keyby = .(prvdr_id)]
  
  # Store results
  results$strata[1,i] <- nrow(sample.stratalevel)
  results$prvdr[1,i] <- nrow(sample.prvdrlevel)
  results$scripts[1,i] <- nrow(sample)

  
  # Now only select those with switches
  sample.stratalevel %<>% 
    .[num_of_arms > 1]
  sample.armlevel %<>% 
    .[, num_of_arms := .N, keyby = .(prvdr_id, timeframe)] %>% 
    .[num_of_arms > 1]
  sample.prvdrlevel <- sample.stratalevel %>%
    .[, .N, keyby = .(prvdr_id)]
  
  # Store results
  
  results$strata[2,i] <- nrow(sample.stratalevel)
  results$prvdr[2,i] <- nrow(sample.prvdrlevel)
  results$scripts[2,i] <- sum(sample.armlevel[["num_in_arm"]])
  
  # Now filter based on the size of the arm
  
  for (j in 1:length(min_arm_sizes))
  {
    min_arm_size <- min_arm_sizes[j]
    
    sample.armlevel <- sample %>% 
      #Include the following line if we want each patient to only appear once in
      #each strata when calculating size
      #.[, .N, keyby = .(prvdr_id, timeframe, lab_name, bene_id)] %>%
      .[, .N, keyby = .(prvdr_id, timeframe, lab_name)] %>%
      .[, num_in_arm := N] %>%
      .[num_in_arm >= min_arm_size, -c("N")]
    
    sample.stratalevel <- sample.armlevel %>%
      .[, .(.N, sum(num_in_arm), max(num_in_arm), nth_largest_value(num_in_arm)), 
        keyby = .(prvdr_id, timeframe)] %>%
      .[, .(prvdr_id, timeframe, num_of_arms = N, 
            total_sample_size = V2, largest_arm = V3, second_largest_arm = V4)] %>%
      .[num_of_arms > 1]
    
    sample.prvdrlevel <- sample.stratalevel %>%
      .[, .N, keyby = .(prvdr_id)]
    
    # Store results
    
    results$strata[2+j,i] <- nrow(sample.stratalevel)
    results$prvdr[2+j,i] <- nrow(sample.prvdrlevel)
    results$scripts[2+j,i] <- sum(sample.stratalevel[["total_sample_size"]])
  }
}

#Show results
options(scipen=100)
results

### Make plots to analyze the second-largest arm size

timeframe.length <- 1
min_arm_size <- 1

# Subset variables, make running month variable, use that to find scripts in same
# experimental time frame. Select only the generic scripts
sample <- orig.sample %>%
  .[, .(bene_id, rfrnc_yr, month, lab_name, srvc_dt,
        strength, prvdr_id, dayssply, year_month)] %>%
  .[, runningmonth := (rfrnc_yr - 2010)*12 + month] %>%
  .[, timeframe := ceiling(runningmonth/timeframe.length)]

sample %<>%
  .[, brand := ifelse(lab_name == "Parke-Davis", "Branded", "Generic")] %>%
  .[brand == "Generic"]

sample.armlevel <- sample %>% 
  .[, .N, keyby = .(prvdr_id, timeframe, lab_name)] %>%
  .[, num_in_arm := N] %>%
  .[num_in_arm >= 1, -c("N")]

sample.stratalevel <- sample.armlevel %>%
  .[, .(.N, sum(num_in_arm), max(num_in_arm), nth_largest_value(num_in_arm)), 
    keyby = .(prvdr_id, timeframe)] %>%
  .[, .(prvdr_id, timeframe, num_of_arms = N, 
        total_sample_size = V2, largest_arm = V3, second_largest_arm = V4)] %>%
  .[num_of_arms > 1]

#Set a max to display on the histogram (since we have an extreme right skew)
max_display <- 20
data <- data.frame(val = sample.stratalevel[["second_largest_arm"]])
max(data['val'])
data['val'][data['val'] > max_display] <- max_display
max(data['val'])
labels <- c(as.character(1:(max_display-1)), paste0(max_display, "+"))

#Plot!
ggplot(data) +
  geom_histogram(
    aes(x = val,
        y = after_stat(density)),
    breaks = 1:(max_display + 1) - 0.5, colour = "black"
  ) +
  stat_bin(
    aes(x = val,
        y = after_stat(density),
        label = after_stat(ifelse(count == 0, "", count))),
    breaks = 1:(max_display + 1) - 0.5, geom = "text", vjust = -1
  ) +
  scale_x_continuous(breaks=1:(max_display), labels = labels) + 
  labs(x = "Number of Scripts in Second Largest Arm in Strata",
       y = "Proportion of Total Strata with At Least 2 Arms",
       title = paste0("Second-Largest Arm Size Distribution, ", g_rxcui_name, 
                      ", ", timeframe.length, " month strata")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


## NOW make the arm size the number of unique patients rather than the number of scripts

sample.armlevel <- sample %>% 
  #Collapse so each patient only occurs once in each arm
  .[, .N, keyby = .(prvdr_id, timeframe, lab_name, bene_id)] %>%
  .[, .N, keyby = .(prvdr_id, timeframe, lab_name)] %>%
  .[, num_in_arm := N] %>%
  .[num_in_arm >= min_arm_size, -c("N")]

sample.stratalevel <- sample.armlevel %>%
  .[, .(.N, sum(num_in_arm), max(num_in_arm), nth_largest_value(num_in_arm)), 
    keyby = .(prvdr_id, timeframe)] %>%
  .[, .(prvdr_id, timeframe, num_of_arms = N, 
        total_sample_size = V2, largest_arm = V3, second_largest_arm = V4)] %>%
  .[num_of_arms > 1]

max_display <- 20
data <- data.frame(val = sample.stratalevel[["second_largest_arm"]])
max(data['val'])
data['val'][data['val'] > max_display] <- max_display
max(data['val'])
labels <- c(as.character(1:(max_display-1)), paste0(max_display, "+"))

ggplot(data) +
  geom_histogram(
    aes(x = val,
        y = after_stat(density)),
    breaks = 1:(max_display + 1) - 0.5, colour = "black"
  ) +
  stat_bin(
    aes(x = val,
        y = after_stat(density),
        label = after_stat(ifelse(count == 0, "", count))),
    breaks = 1:(max_display + 1) - 0.5, geom = "text", vjust = -1
  ) +
  scale_x_continuous(breaks=1:(max_display), labels = labels) + 
  labs(x = "Number of Unique Patients in Second Largest Arm in Strata",
       y = "Proportion of Total Strata with At Least 2 Arms",
       title = paste0("Second-Largest Arm Patient Size Distribution, ", g_rxcui_name, 
                      ", ", timeframe.length, " month strata")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

