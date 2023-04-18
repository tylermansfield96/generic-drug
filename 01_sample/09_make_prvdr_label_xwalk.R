# Header -----------------------------------------------------------------------
# Proj: Bottle of Lies
# Author: Tyler Mansfield (tyler.mansfield96@gmail.com)
# Desc: Makes a cross walk between the prvdr_id and the taxonomy classification
#       for the provider. Since many pharmacies have multiple taxonomies, 
#       we list the taxonomy descriptions in order from what we care most about 
#       to care about least and select the taxonomy that is highest on our 
#       priority list

# Start Script -----------------------------------------------------------------
source("../00_pre_process/start_script.R")

# First build crosswalk between the pharmacy taxonomy codes and labels
taxonomy_to_name <- fread(paste0(lib_base_data,
                                 "Medicare_Provider_and_Supplier_Taxonomy_Crosswalk_June_2022.csv"))
names(taxonomy_to_name) <- c("specialty_code", "type_desc", "tax_code", "tax_label")

#Make one minor fix
bad_label <- "Suppliers Pharmacy Home Infusion Therapy Services"
good_label <- "Suppliers/Pharmacy Home Infusion Therapy Pharmacy"
taxonomy_to_name[taxonomy_to_name==bad_label] <- good_label

pharamcy_taxonomies <- taxonomy_to_name %>%
  .[grepl("harmac", type_desc) | grepl("harmac", tax_label)] %>%
  .[, .N, by = .(tax_code, tax_label)] %>%
  .[, tax_code := gsub(" ", "", tax_code, fixed = TRUE)] %>% #Remove whitespace
  .[, -c("N")]

#Add a generic pharmacy label that wasn't included in the taxonomy_to_name file
pharamcy_taxonomies <- rbind(pharamcy_taxonomies,
                          list(tax_code = '183500000X',
                               tax_label = 'Pharmacist'))

#Make sure our pharamcy_taxonomies xwalk is 1 to 1
if(length(unique(pharamcy_taxonomies[["tax_code"]])) !=
   length(pharamcy_taxonomies[["tax_code"]]))
{
  stop("We have some codes with multiple labels")
}

if(length(unique(pharamcy_taxonomies[["tax_label"]])) !=
   length(pharamcy_taxonomies[["tax_label"]]))
{
  stop("We have some labels with multiple codes")
}

#Since deactivated pharmacies are deleted from the NPI data, we can look
#at files from multiple years and keep the first unique label we find for
#each pharmacy

header_files <- c("npidata_20050523-20151213FileHeader.csv",
                  "npidata_pfile_20050523-20220612_FileHeader.csv",
                  "npidata_20150608-20150614FileHeader.csv")

data_files <- c("npidata_20050523-20151213.csv",
                "npidata_pfile_20050523-20220612.csv",
                "npidata_20150608-20150614.csv")

full_xwalk <- data.table()

for (i in 1:length(data_files))
{
  # Read in Data ----------------------------------------------------------------
  message("Reading in data...")
  
  data_file <- data_files[i]
  header_file <- header_files[i]
  
  data_cols <- fread(paste0(lib_base_data, header_file)) %>%
    names()
  taxonomy_cols <- data_cols[grepl("axonomy Code", data_cols, fixed = TRUE)]
  
  xwalk <- fread(paste0(lib_base_data, data_file), select = c("NPI", taxonomy_cols))
  
  
  # Process Data  --------------------------------------------------------------
  message("Processing data...")
  xwalk %<>% melt.data.table(id.vars = "NPI", measure.vars = taxonomy_cols) %>%
    .[!(value == ""), .(NPI, Label_Number = variable, tax_code = value)] %>%
    .[, .(label_num = Label_Number[1]), by = .(NPI, tax_code)]
  
  xwalk %<>% merge(pharamcy_taxonomies, by = "tax_code")
  
  # Selecting best label for each pharmacy -------------------------------------
  
  #Level of importance in labels
  #These ones will take priority, in the order shown
  most_important <- c("Suppliers/Pharmacy Mail Order Pharmacy",
                      "Suppliers/Pharmacy Long-term Care Pharmacy",
                      "Suppliers/Pharmacy Institutional Pharmacy",
                      "Suppliers/Pharmacy Clinic Pharmacy",
                      "Suppliers/Pharmacy Community/Retail Pharmacy",
                      "Suppliers/Pharmacy Managed Care Organization Pharmacy",
                      "Suppliers/Pharmacy Specialty Pharmacy",
                      "Suppliers/Pharmacy Compounding Pharmacy",
                      "Suppliers/Pharmacy Nuclear Pharmacy")
  
  #These will always be replaced by another label if available
  least_important <- c("Suppliers/Pharmacy", 
                       "Pharmacist")
  
  tax_label_levels <- most_important
  
  for (level in unique(xwalk[["tax_label"]]))
  {
    if (!(level %in% c(most_important, least_important)))
    {
      tax_label_levels <- c(tax_label_levels, level)
    }
  }
  
  tax_label_levels <- c(tax_label_levels, least_important)
  xwalk[, tax_label := factor(tax_label, levels = tax_label_levels)]
  
  #Just get most important label of each pharmacy  
  xwalk %<>% .[order(tax_label)] %>%
    .[match(unique(NPI), NPI)]
  
  full_xwalk %<>% rbind(xwalk)
}

#Delete duplicates, keep earliest occurrence
full_xwalk %<>% .[match(unique(NPI), NPI), .(NPI, tax_code, tax_label)]

# Deactivated Info -------------------------------------------------------------

deactivate_file <- "npidata_pfile_20050523-20220612.csv"
deactivate_file_header <- "npidata_pfile_20050523-20220612_FileHeader.csv"

data_cols <- fread(paste0(lib_base_data, deactivate_file_header)) %>%
  names()

#Only get deactivation information for the ones we have no info about
deact_dt <- fread(paste0(lib_base_data, deactivate_file),
                  select = c("NPI", "NPI Deactivation Date")) %>%
  .[!(NPI %in% full_xwalk[["NPI"]]) & get("NPI Deactivation Date") != ""] %>%
  .[, .(NPI,
        tax_code = NA,
        tax_label = "No Info, Deactivated",
        deact_date = get("NPI Deactivation Date"))]

full_xwalk %<>% .[, deact_date := NA] %>%
  rbind(deact_dt) %>%
  .[match(unique(NPI), NPI)]


# Export -----------------------------------------------------------------------

full_xwalk_export <- full_xwalk[, .(NPI,
                                    NPI_label = tax_label,
                                    NPI_deact_date = deact_date)]

fwrite(full_xwalk_export, paste0(lib_base_data, "prvdr_label_xwalk.csv"))
