# Header -----------------------------------------------------------------------
# Proj: Bottle of Lies
# Author: Evan Flack (evanjflack@gmail.com)
# Desc: Makes xwalk of ndc9 to g_rxcuis and pill strength

# Start Script -----------------------------------------------------------------
package_list <- c("stringr", "httr", "readr")
source("../00_pre_process/start_script.R")

# Start log file
start_log_file("log/06_make_ndc_g_rxcui_xwalk")

# Classes of interest and their generic rxcui codes
classes <- data.table(name = c("metoprolol", "simvastatin", "lisinopril",
                               "amlodipine", "atorvastatin",
                               "hydrochlorothiazide", "atenolol", "carvedilol",
                               "diltiazem", "pravastatin", "losartan"),
                      g_rxcui = c("6918",  "36567", "29046", "17767", "83367",
                                  "5487",  "1202",  "20352", "3443",  "42463",
                                  "52175"))

# brand_labs <- data.table(g_rxcui_name = c("amlodipine",
#                                           "atorvastatin",
#                                           "atenolol"),
#                          brand_labs = list(c("Greenstone"),
#                                            c("Greenstone", "Sandoz"),
#                                            c("Sandoz")))
# classes %<>%
#   merge(brand_labs, by.x = "name", by.y = "g_rxcui_name", all.x = T)

# Data Read In -----------------------------------------------------------------
message("Reading in data...")

# # Xwalk from ndc9 to rxcui, atc4, generic rxcui (atc5) and name
# ndc9_atc_xwalk <- fread(paste0(lib_base_data, "../enroll_month/ndc9_atc_xwalk_",
#                                "20pct", ".csv"))

ndc9_atc_xwalk <- fread(paste0(lib_base_data, "ndc9_atc4_xwalk_20pct.csv")) %>%
  .[, lab_prod := str_pad(lab_prod, 9, pad = "0")] %>%
  .[, ndc := str_pad(ndc, 11, pad = "0")]

# Find if any of the NDC9 with missing g_rxcui might be of interest to us
missing <- ndc9_atc_xwalk %>%
  .[atc4 == "", ] %>%
  .[, concept_name := tolower(concept_name)]

missing_by_class <- data.table(name = classes$name,
                               n_missing = 0)
for (i in 1:nrow(classes)) {
  missing_by_class[i, "n_missing"] <- sum(grepl(missing_by_class$name[i],
                                                missing$concept_name))
}
message("Piotential missing NDC9s:")
print(missing_by_class)

# # Xwalk from rxcui to branded status
# rxcui_brand_xwalk <- fread(paste0(lib_base_data,
#                                   "../enroll_month/brand_indicator_20pct",
#                                   ".csv"))

# Define Functions -------------------------------------------------------------

# Fxn: order_g_rxcui
# Desc: Some lab_prod/rxcui correspond to multiple g_rxcuis. This function
#       combines those g_rxcuis by ordering them, and then placing an underscore
#       between them
# Arg: g_rxcui - character, with different codes separated by "; "
# Ret: combined g_rxcui code
order_g_rxcui <- function(g_rxcui) {
  ret <- unlist(str_split(g_rxcui, "; ")) %>%
    unique() %>%
    .[order(.)] %>%
    paste(., collapse = "_")
  return(ret)
}

# Fxn: get_pill_strength_and_name
# Desc: Queries RxTerms to get the pill strength and full name of an rxcui
# Arg: rxcui - character or numeric
# Ret: list with two character elements, strength and full name
# get_pill_strength_and_name <- function(rxcui) {
#   result <- GET(
#     url = "https://rxnav.nlm.nih.gov/",
#     path = paste0("REST/RxTerms/rxcui/", rxcui, "/allinfo")
#   )
#   strength <- content(result)$rxtermsProperties$strength
#   full_name <- content(result)$rxtermsProperties$fullGenericName
#   list(strength = strength, full_name = full_name)
# }

# Make Unique Xwalk ------------------------------------------------------------
message("Making unique xwalk...")

# Subset to variables/classes of interest
ndc9_g_rxcui_xwalk <- copy(ndc9_atc_xwalk) %>%
  .[, g_rxcui := sapply(g_rxcui, order_g_rxcui)] %>%
  .[, .(lab_prod,  rxcui, g_rxcui, concept_name, branded)] %>%
  merge(classes, by = "g_rxcui", all = TRUE)

# Get strength (from concept name)
extract_strength <- function(concept_name)
{
  #Replace non-numeric numbers with commas
  strings <- gsub("[^0-9.]", ",", concept_name)
  
  #Collapse commas to a single comma
  strings <- gsub('(,)\\1+', '\\1', strings)
  
  #Remove ending and beginning commas
  strings <- gsub('^\\,|\\,$', '', strings)
  
  strings_pt2 <- str_extract_all(concept_name, "succinate", simplify = TRUE)[,1]
  
  return(paste0(strings, strings_pt2))
}

ndc9_g_rxcui_xwalk %<>%
  .[, mg_ind := grepl("MG", concept_name)] %>%
  .[, mg := extract_strength(concept_name)]

message(mean(ndc9_g_rxcui_xwalk$mg_ind)*100, " percent with valid strength")

# query for pill strength and name
# rxcui_strength_name_xwalk <- copy(ndc9_g_rxcui_xwalk) %>%
#   .[, .(rxcui)] %>%
#   unique() %>%
#   .[, strength_name := lapply(rxcui, get_pill_strength_and_name)] %>%
#   .[, strength := lapply(strength_name, function(x) x$strength)] %>%
#   .[, full_name := lapply(strength_name, function(x) x$full_name)]
#   .[, strength_name := NULL]

# # Merge in strength and full name
# ndc9_g_rxcui_xwalk %<>%
#   merge(rxcui_strength_name_xwalk, by = "rxcui")

# # Merge in branded xwalk
# ndc9_g_rxcui_xwalk %<>%
#   merge(rxcui_brand_xwalk, by = "rxcui")

# Export -----------------------------------------------------------------------

fwrite(ndc9_g_rxcui_xwalk, paste0(lib_base_data, "ndc9_g_rxcui_xwalk.csv"))

saveRDS(classes, paste0(lib_base_data, "classes_of_interest.rds"))

end_log_file()
