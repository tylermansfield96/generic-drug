#!/usr/bin/env Rscript

# Header -----------------------------------------------------------------------
# Proj: Bottle of Lies
# Author: Tyler Mansfield (tyler.mansfield96@gmail.com)
# Desc: Finds the drugs in our sample with the highest script count

# Start Script -----------------------------------------------------------------
package_list <- c("stringi", "foreach", "doParallel", "lubridate", "DescTools", "stringr")
source("../00_pre_process/start_script.R")

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type = 'character', default = "20pct"),
  make_option(c("-f", "--first_year"), type = 'integer', default = 2010),
  make_option(c("-l", "--last_year"), type = 'integer', default = 2013),
  make_option(c("-n", "--num_to_label"), type = 'integer', default = 100),
  
  make_option(c("-b", "--model_name"), type = 'character',
              default = "full")
)
unpack_opt(option_list)

# Data Read In -----------------------------------------------------------------
message("Reading in data...")

# NDC9 to g_rxcui xwalk
ndc9_g_rxcui_xwalk <- fread(paste0(lib_base_data, "ndc9_g_rxcui_xwalk.csv")) %>%
  .[, lab_prod := str_pad(lab_prod, 9,pad = "0")] %>%
  setnames("mg", "strength") %>%
  .[, .(g_rxcui, rxcui, lab_prod, strength, branded, concept_name)]

# Labeler code -> name xwalk
lab_names <- fread(paste0(lib_base_data, "ndc_lab_codes_2016.csv")) %>%
  setnames(names(.), c("lab", "lab_name")) %>%
  .[, lab := str_pad(lab, 5, pad = "0")]

if(!file.exists(paste0(lib_base_data, "pde_full_", pct, ".csv")))
{
  pde <- data.table()
  
  registerDoParallel(cores = length(years))
  ret <- foreach(year = years) %dopar%
    {
      message("Starting ", year)
      
      # Read in pde claims, subset to classes of interest
      pde_year <- fread(paste0(lib_base_data, "sample_pde_", year, "_", pct,
                               ".csv"),
                        select = c("lab_prod", "totalcst", "dayssply", "srvc_dt")) %>%
        .[, list_price30 := (totalcst/dayssply)*30] %>%
        .[, lab_prod := str_pad(lab_prod, 9, pad = "0")] %>%
        .[, lab := substr(lab_prod, 1, 5)] %>%
        merge(ndc9_g_rxcui_xwalk, by = "lab_prod") %>%
        .[, srvc_yr := year]
      
      message("Finished ", year)
      return(pde_year)
    }
  
  #Combine results
  pde <- rbindlist(ret)
  
  # Merge lab names --------------------------------------------------------------
  message("Merging lab names...")
  
  #Get lab names
  lab_names_edit <- pde %>%
    .[, .(.N), by = .(lab)] %>%
    unique() %>%
    merge(lab_names, by = "lab", all.x = T) %>%
    .[, lab_name_short := str_split_fixed(lab_name, " ", 2)[, 1]] %>%
    .[order(lab_name_short, -N), ] %>%
    .[, lab_comb := lab[1], by = lab_name_short] %>%
    setnames("lab_name", "lab_name_long") %>%
    setnames("lab_name_short", "lab_name") %>%
    .[, .(lab, lab_name, lab_name_long, lab_comb)] %>%
    .[lab_name == "Dr.", lab_name := "Dr. Reddy's"] %>%
    .[, lab_name := gsub("[^[:alnum:] ]", "", lab_name)]
  
  pde %<>%
    merge(lab_names_edit, by = "lab")
  
  fwrite(pde, paste0(lib_base_data, "pde_full_", pct, ".csv"))
  
} else {
  pde <- fread(paste0(lib_base_data, "pde_full_", pct, ".csv"))
}

# Collapse table --------------------------------------------------------------
message("Collapsing table..")

FindMode <- function(x) {
  u <- unique(x)
  u[which.max(tabulate(match(x, u)))]
}
pde_sample_size <- pde[, .(sample_size = .N, 
                           example_concept = FindMode(concept_name),
                           n_manf = length(unique(lab_name))), 
                       by = g_rxcui] %>%
  .[order(-sample_size)]


# Label each g_rxcui -----------------------------------------------------------
message("Creating label for each g_rxcui...")

#This is extremely costly to do, so just do it for our top ones
all_g_rxcui <- pde_sample_size$g_rxcui
top_grxcuis <- all_g_rxcui[1:num_to_label]
top_grxcuis_mixtures <- top_grxcuis[str_detect(top_grxcuis, "_")]
top_grxcuis_partials <- top_grxcuis_mixtures %>% 
  str_split("_") %>% 
  unlist()

#Some medications have suffixes, append them to the first word
suffix_list <- c(" sodium", 
                 " hydrochloride", 
                 " chloride", 
                 " glycol",
                 " acid", 
                 " calcium", 
                 " propionate", 
                 " mononitrate",
                 " bitartrate",
                 " potassium")

suffix_replacement <- gsub(' ', '_', suffix_list)

#Create a word bank that has all of the words in our concept notes along with the 
#number of unique g_rxcuis that have that word in one of the concept notes
message("Building word bank...")
word_bank <- pde %>%
  .[, .(concept_name, g_rxcui)] %>%
  .[, lapply(.SD, function(x) toString(unique(x))), by = g_rxcui] %>%
  #Fix suffixes
  .[, concept_name := stri_replace_all_regex(concept_name, 
                                             pattern = suffix_list,
                                             replacement = suffix_replacement,
                                             vectorize_all = FALSE)] %>%
  #Remove digits and lowercase everything
  .[, concept_words := stri_extract_all_words(gsub('[[:digit:]]+', 
                                                   '', 
                                                   concept_name), 
                                              simplify = TRUE) %>%
      stri_trans_tolower() %>%
      unique() %>%
      paste(collapse = ' '),
    by = .(concept_name, g_rxcui)]

word_bank_counts <- data.table(words = word_bank$concept_words %>% 
                                 stri_extract_all_words() %>%
                                 unlist()) %>%
  .[, .N, by = words]

pde_concept_level <- pde[, .N, by = .(g_rxcui, concept_name)]

#This function takes the concept words associated with a specific g_rxcui
#and chooses a key word that is highly specialized to that g_rxcui compared
#to the rest of the g_rxcuis in our dataset
get_key_word <- function(concept_words, word_bank_counts = word_bank_counts)
{
  concept_words_dt <- data.table(words = concept_words %>% 
                                   stri_extract_all_words() %>%
                                   unlist()) %>%
    .[, .(num_in_grxcui = .N), by = words] %>% 
    merge(word_bank_counts, by = "words") %>%
    .[, information := num_in_grxcui / N]
  
  key_word <- concept_words_dt[information == max(information), words][1]
  return(key_word)
}

message("Finding name for each g_rxcui...")
pde_grxcui_concept_words <- pde_concept_level %>%
  .[g_rxcui %in% c(top_grxcuis, top_grxcuis_partials)] %>%
  #Attach suffixes
  .[, concept_name := stri_replace_all_regex(concept_name, 
                                             pattern = suffix_list,
                                             replacement = suffix_replacement,
                                             vectorize_all = FALSE)] %>%
  #Remove brand names, numbers, and make everything lowercase
  .[, concept_name := gsub("\\[[^()]*\\]", '', concept_name)] %>%
  .[, concept_words := stri_extract_all_words(gsub('[[:digit:]]+', 
                                                   '', 
                                                   concept_name), 
                                              simplify = TRUE) %>%
      stri_trans_tolower() %>%
      paste(collapse = ' '),
    by = .(concept_name, g_rxcui)] %>%
  #Flatten everything down to the g_rxcui level
  .[,lapply(.SD, function(x) toString(x)), by = g_rxcui] %>%
  .[, .(g_rxcui, concept_words = gsub(',','',concept_words))] %>%
  #Get key word
  .[, key_word := get_key_word(concept_words, word_bank_counts = word_bank_counts), 
    by = g_rxcui] %>%
  #For those that have multiple g_rxcui, see if the individual g_rxcuis are in our data
  .[g_rxcui %in% top_grxcuis_mixtures,
    rename_possible := all((str_split(g_rxcui, "_") %>% unlist()) %in% all_g_rxcui),
    by = g_rxcui] %>%
  .[, .(g_rxcui, key_word, rename_possible)]

#If a g_rxcui is a combination, name it a concatenation of the individual g_rxcuis
message("Concatenating mixture names...")
for (g_rxcui_long in pde_grxcui_concept_words[rename_possible == TRUE, g_rxcui])
{
  g_rxcuis <- str_split(g_rxcui_long, "_") %>% unlist()
  
  new_name <- c()
  
  for (g1 in g_rxcuis)
  {
    new_name <- c(new_name, pde_grxcui_concept_words[g_rxcui == g1, key_word])
  }
  
  
  pde_grxcui_concept_words[g_rxcui == g_rxcui_long, 
                           key_word := new_name %>% paste(collapse="_")]
}

# Additional variables ---------------------------------------------------------
message("Finding other variables...")

pde_sample_size_generic <- pde[branded == 0, 
                               .(n_manf_generic = length(unique(lab_name)),
                                 generic_sample_size = .N), 
                               by = g_rxcui]

pde_sample_size_generic2 <- pde[branded == 0 & srvc_yr == first_year, 
                               .(n_scripts_first_year = .N), 
                               by = g_rxcui]

pde_sample_size_generic3 <- pde[branded == 0 & srvc_yr == last_year, 
                                .(n_scripts_last_year = .N), 
                                by = g_rxcui]

pde_sample_size_branded <- pde[branded == 1, .N, by = .(lab_name, g_rxcui)] %>%
  .[order(-N)] %>%
  .[,lapply(.SD, function(x) toString(unique(x))), by = g_rxcui] %>%
  .[, .(brand_labs = lab_name, g_rxcui)]

pde_most_pop_gen_strength <- pde[!is.na(strength) & branded == 0, 
                                 .N, 
                                 by = .(g_rxcui, strength)] %>% 
  .[order(-N)] %>%
  .[match(unique(g_rxcui), g_rxcui)]

pde_sample_size_price_sd <- merge(pde, pde_most_pop_gen_strength, by = c("g_rxcui", "strength")) %>%
  .[branded == 0, .(sd_30day_supply_mps = sd(Winsorize(list_price30[is.finite(list_price30)], 
                                                       probs = c(0.025, 0.975),
                                                       na.rm = TRUE)
                                             ),
        mean_30day_supply_mps = mean(list_price30[is.finite(list_price30)], 
                                     na.rm = TRUE),
        mps = strength[1],
        n_mps = .N), by = g_rxcui]

#Merge together all the different variables
pde_sample_size %<>% 
  merge(pde_sample_size_generic, 
        by = "g_rxcui",
        all.x = TRUE) %>%
  merge(pde_sample_size_generic2, 
        by = "g_rxcui",
        all.x = TRUE) %>%
  merge(pde_sample_size_generic3, 
        by = "g_rxcui",
        all.x = TRUE) %>%
  merge(pde_sample_size_branded, 
        by = "g_rxcui",
        all.x = TRUE) %>%
  merge(pde_sample_size_price_sd,
        by = "g_rxcui",
        all.x = TRUE) %>% 
  merge(pde_grxcui_concept_words,
        by = "g_rxcui",
        all.x = TRUE) %>%
  setcolorder(c("key_word", names(pde_sample_size)[names(pde_sample_size) != "key_word"])) %>%
  .[g_rxcui != "",] %>%
  .[order(-sample_size)]

# Add in details from models  -------------------------------------------------

for (g_rxcui_name in pde_sample_size$key_word[1:num_to_label])
{
  file_name <- paste0(lib_base_data, "dem_balance_estimates_", g_rxcui_name,
                      '_', pct, '_', model_name, ".csv")
  
  if (!file.exists(file_name))
  {
    next
  }
  
  balance_results <- fread(file_name)
  tot_manfs <- length(unique(c(balance_results$lab_name, balance_results$olab)))
  tot_similar_OOP_price <- balance_results[outcome == "oop_30dayssply" & term != ""] %>%
    .[ub > 0 & lb < 0] %>%
    nrow()
  tot_similar_list_price <- balance_results[outcome == "listprice_30dayssply" & term != ""] %>%
    .[ub > 0 & lb < 0] %>%
    nrow()
  
  #Save results
  pde_sample_size[key_word == g_rxcui_name, total_gen_manfs_big_enough := tot_manfs]
  pde_sample_size[key_word == g_rxcui_name, total_gen_manfs_same_OOP := tot_similar_OOP_price]
  pde_sample_size[key_word == g_rxcui_name, total_gen_manfs_same_list := tot_similar_list_price]
}

# Exporting -------------------------------------------------------------------
message("Exporting...")
fwrite(pde_sample_size, paste0(lib_base_data, "topdrugs_", pct, ".csv"))

message("Done.")
