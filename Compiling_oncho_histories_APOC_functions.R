# ==================================================================================================================== #
#                 Functions to compile and run the APOC IU onchocerciasis histories                                    #
# ==================================================================================================================== #

# ============================================================================ #
#     Function to Update ESPEN 2022 Data for Specific Countries                #
# ============================================================================ #

update_espen_data <- function(df, country_code, file_path) {
  # Load ESPEN updated data
  espen_data <- read.csv(file_path)
  espen_data_mda <- subset(espen_data, EpiCov > 0)
  
  # Subset the main dataframe for year 2022
  df_subset <- df[df$Year == 2022, ]
  matching_indices <- match(df_subset$IU_ID_MAPPING, espen_data_mda$IU_ID)
  matched <- !is.na(matching_indices)
  
  # Update relevant columns
  cols_to_update <- c("EpiCov", "Endemicity", "Cum_MDA", "Cov")
  for (col in cols_to_update) {
    df_subset[[col]][matched] <- espen_data_mda[[col]][matching_indices[matched]]
  }
  
  # Replace original rows with updated ones
  df[df$Year == 2022, ] <- df_subset
  return(df)
}

# ===================================================================================== #
#   Function to extract & process pre-control IU-level prevalences for decision - tree  #
# ===================================================================================== #

extract_baseline_geostat_mfp_func <- function(apoc_file, liberia_file, ocp_file) {
  # Load the datasets
  load(apoc_file)
  load(liberia_file)
  load(ocp_file)
  
  # =========================================================================== #
  #      1)  Filter out APOC countries                                          #
  # =========================================================================== #
  OCP <- c("Mali", "Niger", "Burkina Faso", "Ghana", "Togo", "Benin",
           "Cote d'Ivoire", "Guinea", "Sierra Leone", "Senegal",
           "Guinea-Bissau")
  
  # Filter out the OCP countries from dfAPOC
  dfAPOC <- dfAPOC[-which((dfAPOC$ADMIN0 %in% OCP)),]
  
  # Check how many rows there are for 2022
  dfAPOC_checkn <- subset(dfAPOC, Year == 2022)
  cat("Number of rows for 2022:", nrow(dfAPOC_checkn), "\n")
  
  # =========================================================================== #
  #              Make column with matched pre-control mf prevalence             #
  # =========================================================================== #
  # Convert the summary data to dataframes
  APOC_baseline <- as.data.frame(summaries_apoc)
  liberia_baseline <- as.data.frame(summaries_liberia)
  
  # New in March 2025: Handle OCP baseline
  OCP_baseline <- as.data.frame(summaries_ocp)
  OCP_baseline$Country <- substr(OCP_baseline$IU_CODE, 1, 3)
  
  # Filter for Nigeria (NGA) in the OCP baseline
  APOC_baseline_additional <- subset(OCP_baseline, Country == "NGA")
  APOC_baseline_additional <- APOC_baseline_additional %>% select(-Country)
  
  # ============================================== #
  # Step to remove duplicates in NGA (April 25')   #
  # ============================================== #
  # Find duplicated IU_ID_MAPPING values
  duplicate_ids <- intersect(
    unique(APOC_baseline_additional$IU_CODE),
    unique(APOC_baseline$IU_CODE)
  )
  
  # Remove rows from APOC_baseline_additional where IU_ID_MAPPING is duplicated
  APOC_baseline_additional_cleaned <- APOC_baseline_additional %>%
    filter(!IU_CODE %in% duplicate_ids)
  
  # Continue with merging datasets
  APOC_liberia_baseline <- rbind(APOC_baseline, liberia_baseline, APOC_baseline_additional_cleaned)
  
  # Create a variable to map to IU_ID_mapping (shortened IU_CODE)
  APOC_liberia_baseline$ADMIN0 <- substr(APOC_liberia_baseline$IU_CODE, 1, 3)
  
  # Handle special case for certain countries with four-character codes
  first_4_chars <- c("AGO", "BDI", "CMR", "CAF")
  
  APOC_liberia_baseline$IU_ID_MAPPING <- ifelse(
    APOC_liberia_baseline$ADMIN0 %in% first_4_chars,
    substr(APOC_liberia_baseline$IU_CODE, nchar(APOC_liberia_baseline$IU_CODE) - 3, nchar(APOC_liberia_baseline$IU_CODE)),
    substr(APOC_liberia_baseline$IU_CODE, nchar(APOC_liberia_baseline$IU_CODE) - 4, nchar(APOC_liberia_baseline$IU_CODE))
  )
  
  # Return the cleaned dataframe
  return(list(dfAPOC,APOC_liberia_baseline))
}

# =========================================================================== #
#      Function to determine whether IU classified as Endemic at any point    #
# =========================================================================== #

clean_EndemicIUs_alignment_func <- function(df) {
  # =========================================================================== #
  #      1) Subset all IUs with any endemicity character in Max_endemicity column #
  # =========================================================================== #
  IU_endemic_toselect <- c("Endemic (MDA not delivered)", "Endemic (under MDA)", "Endemic (under post-intervention surveillance)")
  
  # Subset the dataframe based on the condition
  df_endemic <- df[df$MAX_Endemicity %in% IU_endemic_toselect, ]
  cat("Number of unique IUs for selected endemicity:", length(unique(df_endemic$IU_ID_MAPPING)), "\n")
  
  # =========================================================================== #
  #      2) Assign parent or child status and check non-endemic children         #
  # =========================================================================== #
  # Label whether parent or child IU
  df_endemic <- df_endemic %>%
    group_by(IU_ID_MAPPING) %>%
    mutate(has_child = ifelse(any(Parent1_IU_Name != ""), "yes", "no")) %>%
    ungroup()
  
  IU_endemic_codes <- unique(df_endemic$IU_ID_MAPPING)
  
  # Define parent-child relationship
  df_endemic$parent_child_inverse <- ifelse(is.na(df_endemic$Parent1_IU_Name) | df_endemic$Parent1_IU_Name == "", "parent", "child")
  df_endemic$parent_child_subset <- ifelse(grepl("child", df_endemic$parent_child_inverse, ignore.case = TRUE), "parent",
                                           ifelse(grepl("parent", df_endemic$parent_child_inverse, ignore.case = TRUE), "child", NA))
  
  df_endemic$parent_child <- ifelse(df_endemic$Parent1_IU_Name != "" & df_endemic$parent_child_subset == "parent", "parent",
                                    ifelse(df_endemic$Parent1_IU_Name == "" & df_endemic$has_child == "yes", "child",
                                           ifelse(df_endemic$has_child == "no", "parent", df_endemic$parent_child_inverse)))
  
  # Remove temporary columns that are not needed anymore
  df_endemic <- df_endemic[, !(names(df_endemic) %in% c("parent_child_inverse", "parent_child_subset"))]
  
  # =========================================================================== #
  #      3) Check if any child IUs are non-endemic while parent IU is endemic    #
  # =========================================================================== #
  unique_non_endemic_child <- df_endemic %>%
    filter(parent_child == "child" & Endemicity == "Non-endemic") %>%
    select(IUs_NAME_MAPPING) %>%
    unique()
  
  # Use print to display the results instead of cat
  print("Non-endemic child IUs found:")
  print(unique_non_endemic_child)
  
  # =========================================================================== #
  #      4) Check if child IUs are unknown/not reported while parent is endemic  #
  # =========================================================================== #
  unique_unknown_child <- df_endemic %>%
    filter(parent_child == "child" & !(Endemicity %in% IU_endemic_toselect)) %>%
    select(IUs_NAME_MAPPING) %>%
    unique()
  
  print("Unknown child IUs (parent endemic) found:")
  print(unique_unknown_child)
  
  # =========================================================================== #
  #      5) Handle IUs that do not have exactly 10 rows (2013-2022)              #
  check_result1 <- df_endemic %>%
    group_by(IU_ID_MAPPING) %>%
    filter(n() != 10) %>%
    ungroup()
  
  cat("Number of IUs with less than 10 rows:", length(unique(check_result1$IU_ID_MAPPING)), "\n")
  
  # Subset for the year 2022
  check_df <- subset(df_endemic, Year == 2022)
  cat("Rows for 2022:", nrow(check_df), "\n")
  cat("Unique IUs for 2022:", length(unique(check_df$IU_ID_MAPPING)), "\n")
  
  # Return the cleaned and processed dataframe
  return(list(df_endemic, IU_endemic_codes))
}

# ============================================================== #
#    Function to merge the baseline mfp and classify endemicity  #
# ============================================================== #

# function to merge baseline mfp data and define inclusion / endemicity
merge_and_process_data <- function(df, APOC_liberia_baseline, endemicity) {
  # Merge the two data frames based on the 'IU_ID_MAPPING' column
  df_merged <- merge(df, APOC_liberia_baseline, by.x = "IU_ID_MAPPING", by.y = "IU_ID_MAPPING", all.x = TRUE)
  
  # Omit last 7 columns with merged dataframe
  df_merged <- df_merged[, 1:(ncol(df_merged) - 8)]
  
  # Create the 'endemicity_baseline' column based on the 'mean' column values
  df_merged <- df_merged %>%
    mutate(endemicity_baseline = case_when(
      is.na(mean) ~ "non-endemic",
      mean < 0.40 ~ "hypoendemic",
      mean < 0.60 ~ "mesoendemic",
      mean < 0.80 ~ "hyperendemic",
      TRUE ~ "holoendemic"
    ))
  
  if(endemicity == "endemic"){
    # Add 'included' and 'classification' columns
    df_merged$included <- "included"
    df_merged$classification <- "endemic under ESPEN"
  }
  
  # Check the number of rows and unique IU_ID_MAPPING values for 2022
  check_df <- subset(df_merged, Year == 2022)
  cat("Rows for 2022:", nrow(check_df), "\n")
  cat("Unique IUs for 2022:", length(unique(check_df$IU_ID_MAPPING)), "\n")
  
  # Return the modified dataframe and check result
  return(list(df_merged = df_merged, check_result = check_result))
}


# =============================================================================================================== #
#   Function to process IUs classified as Non-endemic IUs: classified as "unknown / not-reported " at any point   #
# =============================================================================================================== #

process_unknown_nonendemic_IUs_func <- function(df, IU_endemic_codes) {
  
  # =========================================================================== #
  #      1) Remove all IUs with endemic status                                  #
  
  df_unknowns <- df[!(df$IU_ID_MAPPING %in% IU_endemic_codes), ]
  cat("Number of IUs with non-endemic or unknown status in 2022:", nrow(subset(df_unknowns, Year == 2022)), "\n")
  
  # =========================================================================== #
  #      2) Subset out all IUs where non-endemic for every year (excluding 2013 and 2014) #
  
  df_unknowns2 <- df_unknowns %>%
    filter(!(Year %in% c(2013, 2014)))
  
  # Remove IUs where non-endemic in every year from 2014 onwards
  df_nonendemic <- df_unknowns2 %>%
    group_by(IU_ID_MAPPING) %>%
    filter(all(Endemicity == "Non-endemic")) %>%
    ungroup()
  
  # Check the number of IUs in 2022 that are non-endemic
  check_df <- subset(df_nonendemic, Year == 2022)
  cat("Number of IUs with non-endemic status in 2022:", nrow(check_df), "\n")
  cat("Unique IUs with non-endemic status in 2022:", length(unique(check_df$IU_ID_MAPPING)), "\n")
  
  # Get unique codes for non-endemic IUs
  IU_nonendemic_codes <- unique(df_nonendemic$IU_ID_MAPPING)
  cat("Number of unique non-endemic IUs:", length(IU_nonendemic_codes), "\n")
  
  # =========================================================================== #
  #      3) Mark non-endemic IUs as "excluded" and "non-endemic in ESPEN"      #
  
  df_nonendemic$included <- "excluded"
  df_nonendemic$classification <- "non-endemic in ESPEN"
  
  # =========================================================================== #
  #      4) Remove all non-endemic IUs from the unknowns dataframe               #
  
  df_unknowns <- df_unknowns[!(df_unknowns$IU_ID_MAPPING %in% IU_nonendemic_codes), ]
  cat("Number of IUs remaining in unknowns after excluding non-endemic:", nrow(subset(df_unknowns, Year == 2022)), "\n")
  
  # =========================================================================== #
  #      5) Check IUs with less than 10 rows                                    #
  
  check_result <- df_unknowns %>%
    group_by(IU_ID_MAPPING) %>%
    filter(n() != 10) %>%
    ungroup()
  
  cat("Number of IUs with less than 10 rows:", length(unique(check_result$IU_ID_MAPPING)), "\n")
  
  # =========================================================================== #
  #      6) Find IUs with both 'Not reported' and 'Non-endemic' in all years    #
  
  df_notreported_nonendemic <- df_unknowns %>%
    group_by(IU_ID_MAPPING) %>%
    filter(all(c("Not reported", "Non-endemic") %in% Endemicity) & n_distinct(Endemicity) == 2) %>%
    ungroup()
  
  IU_notreported_nonendemic <- unique(df_notreported_nonendemic$IU_ID_MAPPING)
  cat("Number of IUs with 'Not reported' and 'Non-endemic' in all years:", length(IU_notreported_nonendemic), "\n")
  
  # =========================================================================== #
  #      7) Remove these IUs from the non-endemic dataset if necessary          #
  
  df_nonendemic2 <- df_nonendemic[!(df_nonendemic$IU_ID_MAPPING %in% IU_notreported_nonendemic), ]
  IU_notreported_nonendemic2 <- unique(df_nonendemic2$IU_ID_MAPPING)
  cat("Number of IUs left after removing those with 'Not reported' and 'Non-endemic':", length(IU_notreported_nonendemic2), "\n")
  
  # =========================================================================== #
  #      8) Return the processed dataframes                                      #
  # =========================================================================== #
  return(list(
    df_unknowns = df_unknowns,
    df_nonendemic = df_nonendemic2,
    IU_nonendemic_codes = IU_nonendemic_codes
  ))
}

# ============================================================================================== #
#   Function to process Unknown IUs using baseline endemicity inputs converted from Zoure et al. #
# ============================================================================================== #

process_baseline_conditions <- function(df, endemicity) {
  
  # a) Mesoendemic baseline: include
  df_mesoendemic <- df %>%
    group_by(IU_ID_MAPPING) %>%
    filter(any(endemicity_baseline == "mesoendemic")) %>%
    ungroup()
  
  IU_meso_codes <- unique(df_mesoendemic$IU_ID_MAPPING)
  cat("Number of IUs with mesoendemic baseline:", length(IU_meso_codes), "\n")
  
  df_mesoendemic$included <- "included"
  df_mesoendemic$classification <- "unknown with mesoendemic baseline"
  
  # b) Hypoendemic baseline & Cum_MDA > 0: include
  df_hypoendemic <- df %>%
    group_by(IU_ID_MAPPING) %>%
    filter(any(endemicity_baseline == "hypoendemic")) %>%
    ungroup()
  
  IU_hypo_codes <- unique(df_hypoendemic$IU_ID_MAPPING)
  cat("Number of IUs with hypoendemic baseline:", length(IU_hypo_codes), "\n")
  
  df_hypoendemic_with_mda <- df_hypoendemic %>%
    group_by(IU_ID_MAPPING) %>%
    filter(any(Cum_MDA > 0)) %>%
    ungroup()
  
  IU_hypo_with_mda_codes <- unique(df_hypoendemic_with_mda$IU_ID_MAPPING)
  cat("Number of IUs with hypoendemic baseline and Cum_MDA > 0:", length(IU_hypo_with_mda_codes), "\n")
  
  df_hypoendemic_with_mda$included <- "included"
  df_hypoendemic_with_mda$classification <- "unknown with hypoendemic baseline & Cum_MDA > 0"
  
  # # c) check the remaining n = 530 IUs 
  # df_NOT_hypo_meso_endemic <- df %>%
  #   group_by(IU_ID_MAPPING) %>%
  #   filter(any(!endemicity_baseline %in% c("hypoendemic", "mesoendemic"))) %>%
  #   ungroup() # subset all those IUs with hypoendemic
  # 
  # IU_unknowns_NOT_hypomeso_codes <- unique(df_NOT_hypo_meso_endemic$IU_ID_MAPPING)
  # cat("Number of unknown IUs not hypo or meso:", length(IU_unknowns_NOT_hypomeso_codes), "\n")
  
  # D) Unknown with hypo baseline and Cum_MDA = 0 
  df_hypoendemic_no_mda <- df_hypoendemic %>%
    group_by(IU_ID_MAPPING) %>%
    filter(all(Cum_MDA == 0)) %>%
    ungroup()
  
  IU_unknowns_hypo_no_mda <- unique(df_hypoendemic_no_mda$IU_ID_MAPPING)
  cat("Number of unknown IUs that are hypo without MDAs:", length(IU_unknowns_hypo_no_mda), "\n")
  # find unique codes = 747 IUs (in flowchart = 1158 - 411 = 747 IUs)
  
  df_hypoendemic_no_mda$included <- as.character("excluded")
  df_hypoendemic_no_mda$classification <- as.character("unknown with hypoendemic baseline
  & Cum_MDA = 0 in ESPEN")
  
  # E) Non-endemic baseline: exclude
  df_nonendemic <- df %>%
    group_by(IU_ID_MAPPING) %>%
    filter(any(endemicity_baseline == "non-endemic")) %>%
    ungroup()
  
  IU_nonendemic_codes <- unique(df_nonendemic$IU_ID_MAPPING)
  cat("Number of IUs with non-endemic baseline:", length(IU_nonendemic_codes), "\n")
  
  df_nonendemic$included <- "excluded"
  df_nonendemic$classification <- "unknown with no baseline"
  
  # Return results
  return(list(
    df_mesoendemic = df_mesoendemic,
    df_hypoendemic_with_mda = df_hypoendemic_with_mda,
    #df_NOT_hypo_meso_endemic = df_NOT_hypo_meso_endemic,
    df_hypoendemic_no_mda = df_hypoendemic_no_mda,
    df_nonendemic = df_nonendemic
  ))
}

# ============================================================== #
# Function to combine daaframes on endemicity status and process #
# ============================================================== #

combine_apoc_sub_data_with_checks <- function(df_endemic, df_unknowns_mesoendemic, df_unknowns_hypoendemic_with_mda,
                                              df_unknowns_hypoendemic_no_mda, df_unknowns_nonendemic, df_nonendemic) {
  
  # Check the number of rows for the year 2022 in each dataframe
  check_rows <- function(df, year_val) {
    return(nrow(subset(df, Year == year_val)))
  }
  
  # Perform checks for 2022
  check_endemic <- check_rows(df_endemic, 2022)
  check_unknowns_mesoendemic <- check_rows(df_unknowns_mesoendemic, 2022)
  check_unknowns_hypoendemic_with_mda <- check_rows(df_unknowns_hypoendemic_with_mda, 2022)
  check_unknowns_hypoendemic_no_mda <- check_rows(df_unknowns_hypoendemic_no_mda, 2022)
  check_unknowns_nonendemic <- check_rows(df_unknowns_nonendemic, 2022)
  check_nonendemic <- check_rows(df_nonendemic, 2022)
  
  # Print the results of the checks
  cat("Rows for year 2022 in each subset:\n")
  cat("Endemic: ", check_endemic, "\n")
  cat("Unknown + Mesoendemic: ", check_unknowns_mesoendemic, "\n")
  cat("Unknown + Hypo (with MDA): ", check_unknowns_hypoendemic_with_mda, "\n")
  cat("Unknown + Hypo (no MDA): ", check_unknowns_hypoendemic_no_mda, "\n")
  cat("Unknown + Non-endemic: ", check_unknowns_nonendemic, "\n")
  cat("Non-endemic: ", check_nonendemic, "\n")
  
  # Remove unwanted columns from df_endemic
  df_endemic <- df_endemic[, -c(59, 60)]
  
  # Rename columns as necessary
  names(df_endemic)[names(df_endemic) == "ADMIN0.x"] <- "ADMIN0"
  names(df_endemic)[names(df_endemic) == "IU_CODE.x"] <- "IU_CODE"
  
  names(df_unknowns_mesoendemic)[names(df_unknowns_mesoendemic) == "ADMIN0.x"] <- "ADMIN0"
  names(df_unknowns_mesoendemic)[names(df_unknowns_mesoendemic) == "IU_CODE.x"] <- "IU_CODE"
  
  names(df_unknowns_hypoendemic_with_mda)[names(df_unknowns_hypoendemic_with_mda) == "ADMIN0.x"] <- "ADMIN0"
  names(df_unknowns_hypoendemic_with_mda)[names(df_unknowns_hypoendemic_with_mda) == "IU_CODE.x"] <- "IU_CODE"
  
  names(df_unknowns_hypoendemic_no_mda)[names(df_unknowns_hypoendemic_no_mda) == "ADMIN0.x"] <- "ADMIN0"
  names(df_unknowns_hypoendemic_no_mda)[names(df_unknowns_hypoendemic_no_mda) == "IU_CODE.x"] <- "IU_CODE"
  
  names(df_unknowns_nonendemic)[names(df_unknowns_nonendemic) == "ADMIN0.x"] <- "ADMIN0"
  names(df_unknowns_nonendemic)[names(df_unknowns_nonendemic) == "IU_CODE.x"] <- "IU_CODE"
  
  # Add missing columns to df_nonendemic
  df_nonendemic$IU_CODE.y <- NA
  df_nonendemic$mean <- NA
  df_nonendemic$endemicity_baseline <- NA
  
  # Combine all dataframes into one
  df_complete <- rbind(df_endemic, df_unknowns_mesoendemic, df_unknowns_hypoendemic_with_mda,
                       df_unknowns_hypoendemic_no_mda, df_unknowns_nonendemic, df_nonendemic)
  
  check_all <- check_rows(df_complete, 2022)
  cat("Re-combined dataframe (should recover 4161 IUs): ", check_all, "\n") # check
  
  return(df_complete)
}

# =============================================================================================== #
#     Function to isolate final endemic IUs in APOC (incl. excluding IUs in specific countries)   #
# =============================================================================================== #

Exclude_select_endemic_IUs <- function(dfAPOC_complete, ETH_exclude, ETH_exclude2, ETH_exclude3, ETH_include, 
                                          SDN_include, TZN_include, year_val = 2022) {
  
  # Get the last year data
  dfAPOC_complete_lastyr <- subset(dfAPOC_complete, Year == year_val)
  
  # Check and print the number of rows for the selected year
  cat("\nNumber of rows in dfAPOC_complete_lastyr for Year", year_val, ":", nrow(dfAPOC_complete_lastyr), "\n")
  
  # Initialize classification and included columns
  dfAPOC_complete_lastyr$classification2 <- factor(dfAPOC_complete_lastyr$classification, levels = c(
    "endemic under ESPEN", "unknown with mesoendemic baseline", "unknown with hypoendemic baseline & Cum_MDA > 0",
    "unknown with hypoendemic baseline\n  & Cum_MDA = 0 in ESPEN", "unknown with no baseline", "non-endemic in ESPEN", NA))
  
  dfAPOC_complete_lastyr$included2 <- ifelse(
    dfAPOC_complete_lastyr$classification == "unknown with hypoendemic baseline\n  & Cum_MDA = 0 in ESPEN", 
    "included as treatment naive/hypoendemic or impact from LF MDA/hypoendemic", dfAPOC_complete_lastyr$included
  )
  
  # Exclude certain IUs for Ethiopia (ETH)
  dfAPOC_complete_lastyr$included2 <- ifelse(
    dfAPOC_complete_lastyr$IUs_NAME_MAPPING %in% ETH_exclude & dfAPOC_complete_lastyr$ADMIN0ISO3 == "ETH", "excluded", dfAPOC_complete_lastyr$included2
  )
  dfAPOC_complete_lastyr$included2 <- ifelse(
    dfAPOC_complete_lastyr$IUs_NAME_MAPPING %in% ETH_exclude2 & dfAPOC_complete_lastyr$ADMIN0ISO3 == "ETH", "excluded", dfAPOC_complete_lastyr$included2
  )
  dfAPOC_complete_lastyr$included2 <- ifelse(
    dfAPOC_complete_lastyr$IUs_NAME_MAPPING %in% ETH_exclude3 & dfAPOC_complete_lastyr$ADMIN0ISO3 == "ETH", "excluded", dfAPOC_complete_lastyr$included2
  )
  
  dfAPOC_complete_lastyr$included2 <- ifelse(
    dfAPOC_complete_lastyr$included2 == "included as treatment naive/hypoendemic or impact from LF MDA/hypoendemic"
    & dfAPOC_complete_lastyr$ADMIN0ISO3 == "ETH", "excluded", dfAPOC_complete_lastyr$included2
  )
  
  # Include certain IUs for Ethiopia (ETH) 
  dfAPOC_complete_lastyr$included2 <- ifelse(
    dfAPOC_complete_lastyr$IUs_NAME_MAPPING %in% ETH_include, "included as treatment naive/hypoendemic or impact from LF MDA/hypoendemic", dfAPOC_complete_lastyr$included2
  )
  
  # Include certain IUs for Sudan (SDN)
  dfAPOC_complete_lastyr$included2 <- ifelse(
    dfAPOC_complete_lastyr$IUs_NAME_MAPPING %in% SDN_include & dfAPOC_complete_lastyr$ADMIN0ISO3 == "SDN",
    "included as treatment naive/hypoendemic or impact from LF MDA/hypoendemic",
    ifelse(dfAPOC_complete_lastyr$ADMIN0ISO3 != "SDN", dfAPOC_complete_lastyr$included2, "excluded")
  )
  
  # Exclude for Mozambique (MOZ) and specific countries (KEN, ZMB)
  dfAPOC_complete_lastyr$included2 <- ifelse(
    dfAPOC_complete_lastyr$classification2 != "unknown with mesoendemic baseline" & dfAPOC_complete_lastyr$ADMIN0ISO3 == "MOZ", "excluded", dfAPOC_complete_lastyr$included2
  )
  
  dfAPOC_complete_lastyr$included2 <- ifelse(
    dfAPOC_complete_lastyr$ADMIN0ISO3 %in% c("ZMB", "KEN"), "excluded", dfAPOC_complete_lastyr$included2
  )
  
  # Reclassify endemicity based on inclusion/exclusion
  dfAPOC_complete_lastyr$endemicity_reclassified <- ifelse(
    dfAPOC_complete_lastyr$included2 %in% c("included", "included as treatment naive/hypoendemic or impact from LF MDA/hypoendemic"),
    "endemic", "non-endemic"
  )
  
  # # check unknowns
  # dfAPOC_complete_lastyr_check <- subset(dfAPOC_complete_lastyr, classification == "unknown with hypoendemic baseline\n  & Cum_MDA = 0 in ESPEN")
  # cat("\nNumber of IUs unknown with hypoendemic baseline\n& Cum_MDA = 0 in ESPEN:", length(unique(dfAPOC_complete_lastyr_check$IU_ID_MAPPING)), "\n")
  
  
  # Select endemic IUs for the final dataset
  dfAPOC_complete_lastyr_endemic <- dfAPOC_complete_lastyr[which(dfAPOC_complete_lastyr$endemicity_reclassified == "endemic"),]
  
  # Output the number of IUs after filtering for endemic
  cat("\nNumber of IUs classified as endemic (note, not neccessarily included):", length(unique(dfAPOC_complete_lastyr_endemic$IU_ID_MAPPING)), "\n")
  
  # Extract final_APOC_IUs_vec and df_APOC_endemic_minimal
  final_APOC_IUs_vec <- dfAPOC_complete_lastyr_endemic$IU_ID_MAPPING
  df_APOC_endemic_minimal <- dfAPOC_complete_lastyr_endemic[, c("ADMIN0", "ADMIN0ISO3", "ADMIN1", "IU_ID_MAPPING", "IUs_NAME_MAPPING", "IU_CODE_MAPPING", "endemicity_baseline",
                                                                "included2", "endemicity_reclassified")]
  
  # Output the number of IUs after filtering for endemic
  
  dfAPOC_complete_lastyr_check <- subset(dfAPOC_complete_lastyr, Year == 2022)
  
  # 1463 (1463 IUs in April 25)
  cat("\nNumber of IUs endemic under ESPEN & included:", nrow(subset(dfAPOC_complete_lastyr_check, classification == "endemic under ESPEN" & included == "included")), "\n") 
  
  # 858 (858 IUs in April 25) - EXCLUDED
  cat("\nNumber of IUs non-endemic in ESPEN & excluded:", nrow(subset(dfAPOC_complete_lastyr_check, classification == "non-endemic in ESPEN" & included == "excluded")), "\n") 
  
  # 545 (530 IUs in April 25) - EXCLUDED
  cat("\nNumber of IUs unknown with no baseline & excluded:", nrow(subset(dfAPOC_complete_lastyr_check, classification == "unknown with no baseline" & included == "excluded")), "\n") 
  
  # 150 (152 IUs in April 25)
  cat("\nNumber of IUs unknown with mesoendemic baseline & included:", nrow(subset(dfAPOC_complete_lastyr_check, classification == "unknown with mesoendemic baseline" & included == "included")), "\n") 
  
  # 410 (411 IUs in April 25)
  cat("\nNumber of IUs unknown with hypoendemic baseline + & Cum_MDA > 0 in ESPEN & included:", nrow(subset(dfAPOC_complete_lastyr_check, classification == "unknown with hypoendemic baseline & Cum_MDA > 0" & included == "included")), "\n") 
  
  # 735 (747 IUs in April 25) - EXCLUDED
  cat("\nNumber of IUs unknown with hypoendemic baseline + & Cum_MDA = 0 in ESPEN & excluded:", nrow(subset(dfAPOC_complete_lastyr_check,  classification == "unknown with hypoendemic baseline\n  & Cum_MDA = 0 in ESPEN" & included == "excluded")), "\n") 
  
  # total included
  cat("\nNumber of IUs included & endemic:", nrow(subset(dfAPOC_complete_lastyr_check, included == "included")), "\n") 
  
  # total excluded
  cat("\nNumber of IUs excluded:", nrow(subset(dfAPOC_complete_lastyr_check, included == "excluded")), "\n") 
  
  # calculate total excluded 
  # check to include (should be # 858 + 530 + 747 = 2135 excluded) : #
  cat("\nNumber of IUs excluded (adding together all excluded CHECK):", nrow(subset(dfAPOC_complete_lastyr_check, classification == "non-endemic in ESPEN" & included == "excluded")) + 
        nrow(subset(dfAPOC_complete_lastyr_check, classification == "unknown with no baseline" & included == "excluded")) +
        nrow(subset(dfAPOC_complete_lastyr_check, classification == "unknown with hypoendemic baseline\n  & Cum_MDA = 0 in ESPEN" & included == "excluded")), "\n")
  
  # 858 + 545 + 735 = 2138 excluded 
  # 858 + 530 + 758 = 2146 excluded (April 2025)
  # 858 + 530 + 747 = 2135 excluded (April 2025 correct)
  
  # SIZE of final dataframe with endemic IUs (not all will be included!)
  cat("\nNumber of IUs included (as endemic) in output dataframe:", length(unique(dfAPOC_complete_lastyr_endemic$IU_ID_MAPPING)), "\n")
  
  
  # Return results
  return(list(final_APOC_IUs_vec = unique(final_APOC_IUs_vec), df_APOC_endemic_minimal = df_APOC_endemic_minimal))
}

# ======================================================================= #
#  Function to perform final endemic IUs section and do further checks   #
# ======================================================================= #

filter_select_ALL_endemicIUs_and_check_func <- function(dfAPOC_complete, final_APOC_IUs_vec) {
  
  # rename one value due to formatting
  dfAPOC_complete$classification <- gsub("unknown with hypoendemic baseline\n  & Cum_MDA = 0 in ESPEN", 
                                         "unknown with hypoendemic baseline & Cum_MDA = 0", 
                                         dfAPOC_complete$classification)
  
  # Filter dfAPOC_included based on final_APOC_IUs_vec
  dfAPOC_included <- subset(dfAPOC_complete, IU_ID_MAPPING %in% final_APOC_IUs_vec)
  
  # Check rows for 2022 data
  dfAPOC_included_check <- subset(dfAPOC_included, Year == 2022)
  cat("\nNumber of IUs for Year 2022:", nrow(dfAPOC_included_check), "\n")
  cat("Length of unique IU_ID_MAPPING:", length(unique(dfAPOC_included_check$IU_ID_MAPPING)), "\n")
  
  # Check unique overlaps
  check_result4 <- dfAPOC_included %>%
    group_by(IU_ID_MAPPING) %>%
    filter(n() != 10) %>%
    ungroup() # check if any IUs with less than 10 rows
  cat("\nNumber of IUs with less than 10 rows:", length(unique(check_result4$IU_ID_MAPPING)), "\n")
  
  # Take out the incomplete IUs and create full history
  not_complete_IUs <- unique(check_result4$IU_ID_MAPPING)
  dfAPOC_included_temp <- dfAPOC_included[!(dfAPOC_included$IU_ID_MAPPING %in% not_complete_IUs), ]
  dfAPOC_included_notcomplete <- check_result4
  
  # Generate missing years for each IU
  all_combinations <- expand.grid(
    IU_ID_MAPPING = unique(dfAPOC_included_notcomplete$IU_ID_MAPPING),
    Year = 2013:2022
  )
  
  merged_df <- left_join(all_combinations, dfAPOC_included_notcomplete, by = c("IU_ID_MAPPING", "Year"))
  merged_df <- merged_df %>% arrange(IU_ID_MAPPING, Year)
  merged_df$Endemicity <- ifelse(is.na(merged_df$Endemicity), "Augmented", merged_df$Endemicity)
  
  # Replacing missing Cum_MDA values (just as an example update)
  updated_Cum_MDA <- c(13,13,14,15,16,17,18,19,20,21,15,15,16,17,18,19,20,20,21,22,0,0,0,0,0,0,0,0,0,0,14,14,14,14,14,14,14,14,14,14,0,0,0,0,0,0,0,0,0,0,2,2,2,2,2,3,3,3,3,3)
  merged_df$Cum_MDA <- updated_Cum_MDA
  
  # Add this back into the main dataframe
  dfAPOC_included <- rbind(dfAPOC_included_temp, merged_df)
  
  # Ensure all IUs have 2013-2022 history
  check_result5 <- dfAPOC_included %>%
    group_by(IU_ID_MAPPING) %>%
    filter(n() != 10) %>%
    ungroup()
  cat("\nNumber of IUs without complete history (2013-2022):", length(unique(check_result5$IU_ID_MAPPING)), "\n")
  
  # Check again if all years are present
  dfAPOC_included <- dfAPOC_included %>% arrange(IU_ID_MAPPING, Year)
  
  dfAPOC_included$endemicity_reclassified <- "endemic"
  
  # now check full dataset with all IUs : #
  
  check <- dfAPOC_complete
  
  check$incl_updated <- ifelse(check$IU_ID_MAPPING %in% final_APOC_IUs_vec, "included", "excluded")
  
  check <- subset(check, Year == 2022)
  
  # ========================================== #
  # Additional checks for updated classification and inclusion
  
  cat("\nCheck for 'endemic under ESPEN' classification:\n")
  cat("  Rows for 'endemic under ESPEN':", nrow(subset(check, classification == "endemic under ESPEN")), "\n")
  cat("  Rows for 'endemic under ESPEN' & included:", nrow(subset(check, classification == "endemic under ESPEN" & incl_updated == "included")), "\n")
  cat("  Rows for 'endemic under ESPEN' & excluded:", nrow(subset(check, classification == "endemic under ESPEN" & incl_updated == "excluded")), "\n")
  
  cat("\nCheck for 'non-endemic in ESPEN' classification:\n")
  cat("  Rows for 'non-endemic in ESPEN':", nrow(subset(check, classification == "non-endemic in ESPEN")), "\n")
  cat("  Rows for 'non-endemic in ESPEN' & excluded:", nrow(subset(check, classification == "non-endemic in ESPEN" & incl_updated == "excluded")), "\n")
  
  cat("\nCheck for 'unknown with no baseline' classification:\n")
  cat("  Rows for 'unknown with no baseline':", nrow(subset(check, classification == "unknown with no baseline")), "\n")
  cat("  Rows for 'unknown with no baseline' & excluded:", nrow(subset(check, classification == "unknown with no baseline" & incl_updated == "excluded")), "\n")
  
  cat("\nCheck for 'unknown with mesoendemic baseline' classification:\n")
  cat("  Rows for 'unknown with mesoendemic baseline':", nrow(subset(check, classification == "unknown with mesoendemic baseline")), "\n")
  cat("  Rows for 'unknown with mesoendemic baseline' & included:", nrow(subset(check, classification == "unknown with mesoendemic baseline" & incl_updated == "included")), "\n")
  
  cat("\nCheck for 'unknown with hypoendemic baseline & Cum_MDA > 0' classification:\n")
  cat("  Rows for 'unknown with hypoendemic baseline & Cum_MDA > 0':", nrow(subset(check, classification == "unknown with hypoendemic baseline & Cum_MDA > 0")), "\n")
  cat("  Rows for 'unknown with hypoendemic baseline & Cum_MDA > 0' & included:", nrow(subset(check, classification == "unknown with hypoendemic baseline & Cum_MDA > 0" & incl_updated == "included")), "\n")
  
  cat("\nCheck for 'unknown with hypoendemic baseline & Cum_MDA = 0' classification:\n")
  cat("  Rows for 'unknown with hypoendemic baseline & Cum_MDA = 0':", nrow(subset(check, classification == "unknown with hypoendemic baseline & Cum_MDA = 0")), "\n")
  cat("  Rows for 'unknown with hypoendemic baseline & Cum_MDA = 0' & excluded:", nrow(subset(check, classification == "unknown with hypoendemic baseline & Cum_MDA = 0" & incl_updated == "excluded")), "\n")
  cat("  Rows for 'unknown with hypoendemic baseline & Cum_MDA = 0' & included:", nrow(subset(check, classification == "unknown with hypoendemic baseline & Cum_MDA = 0" & incl_updated == "included")), "\n")
  
  cat("\nCheck all baseline hypoendemic IUs\n")
  cat("  Rows for 'all hypoendemic baseline IUs not classified as endemic in ESPEN':", nrow(subset(check, endemicity_baseline == "hypoendemic" & !classification == "endemic under ESPEN")), "\n")
  
  # Return final results
  return(list(dfAPOC_included = dfAPOC_included))
}

# ======================================================== #
#   Function to create rows going back to 1975 for each IU #
# ======================================================== #

# Define the function to process the data with checks for CMR-specific IU_CODES
process_apoc_IUs_tobaseline_func <- function(dfAPOC_included, final_APOC_IUs_vec, 
                                             cmr_iu_codes = c("CMR0081907441", "CMR0081807424")) {
  
  # Process CMR-specific IU_CODES first
  dfAPOC_included_subset_CMR <- subset(dfAPOC_included, IU_CODE.y %in% cmr_iu_codes)
  dfls_CMR <- split(dfAPOC_included_subset_CMR, dfAPOC_included_subset_CMR$IU_ID_MAPPING)
  
  FinalESPENyr_CMR <- 2014  # Year for CMR IUs
  newdf_CMR <- vector("list", length(dfls_CMR))
  
  # Sub-loop for CMR-specific IUs
  for (i in 1:length(dfls_CMR)) {
    newdf_CMR[[i]] <- subset(dfls_CMR[[i]], select = colnames(dfAPOC_included_subset_CMR))
    
    FinalESPENyr_CMR <- head(newdf_CMR[[i]]$Year, 1)
    start <- min(newdf_CMR[[i]]$Year) - FinalESPENyr_CMR
    IUStartMDA <- max(min(newdf_CMR[[i]]$Year) - min(newdf_CMR[[i]]$Cum_MDA) - start)
    
    IUstart <- 1975
    newdf_CMR[[i]]$IUstart <- IUstart
    Prioryrs <- FinalESPENyr_CMR - IUstart
    PriorMDA <- FinalESPENyr_CMR - IUStartMDA
    
    newdf_CMR[[i]]$lsID <- i
    
    if (IUstart < FinalESPENyr_CMR) {
      tmp <- newdf_CMR[[i]][rep(1, each = (min(newdf_CMR[[i]]$Year) - IUstart)), ]
      tmp$Year <- seq(IUstart, min(newdf_CMR[[i]]$Year) - 1)
      
      if (any(head(newdf_CMR[[i]]$Cum_MDA) > 0)) {
        val_totest <- head(newdf_CMR[[i]]$Cum_MDA, 1)
        if (val_totest > 1) {
          leading_zeros <- rep(0, 37 - head(newdf_CMR[[i]]$Cum_MDA, 1) + 1)
          tmp$Cum_MDA <- c(leading_zeros, seq(from = 0, to = head(newdf_CMR[[i]]$Cum_MDA, 1), by = 1))
        }
      }
      
      tmp$Endemicity <- "Augmented"
      tmp$MDA_scheme <- "Augmented"
      tmp[, 22:25] <- 0
      tmp[, 27:30] <- 0
      tmp[, 32:40] <- 0
      
      newdf_CMR[[i]] <- rbind(tmp, newdf_CMR[[i]])
    }
  }
  
  # Combine the processed CMR data
  newdf_CMR <- do.call(rbind, newdf_CMR)
  dfAPOC_included2_CMR <- newdf_CMR
  
  # Now process the rest of the data (non-CMR IU_CODES)
  FinalESPENyr <- 2013
  dfls <- split(dfAPOC_included, dfAPOC_included$IU_ID_MAPPING)
  
  newdf <- vector("list", length(dfls))
  lsid <- split(dfAPOC_included$IU_ID_MAPPING, dfAPOC_included$IU_ID_MAPPING)
  lsid <- unique(unlist(lsid))
  
  # Main loop for all IU_ID_MAPPING excluding the CMR-specific ones
  for (i in 1:length(dfls)) {
    # Skip the CMR-specific IU_CODES
    if (any(dfls[[i]]$IU_CODE.y %in% cmr_iu_codes)) {
      next
    }
    
    newdf[[i]] <- subset(dfls[[i]], select = colnames(dfAPOC_included))
    FinalESPENyr <- head(newdf[[i]]$Year, 1)
    start <- min(newdf[[i]]$Year) - FinalESPENyr
    IUStartMDA <- max(min(newdf[[i]]$Year) - min(newdf[[i]]$Cum_MDA) - start)
    
    IUstart <- 1975
    newdf[[i]]$IUstart <- IUstart
    Prioryrs <- FinalESPENyr - IUstart
    PriorMDA <- FinalESPENyr - IUStartMDA
    
    newdf[[i]]$lsID <- i
    
    if (IUstart < FinalESPENyr) {
      tmp <- newdf[[i]][rep(1, each = (min(newdf[[i]]$Year) - IUstart)), ]
      tmp$Year <- seq(IUstart, min(newdf[[i]]$Year) - 1)
      
      if (any(head(newdf[[i]]$Cum_MDA) > 0)) {
        val_totest <- head(newdf[[i]]$Cum_MDA, 1)
        if (val_totest > 1) {
          leading_zeros <- rep(0, 37 - head(newdf[[i]]$Cum_MDA, 1) + 1)
          tmp$Cum_MDA <- c(leading_zeros, seq(from = 0, to = head(newdf[[i]]$Cum_MDA, 1) - 1, by = 1))
        }
      }
      
      tmp$Endemicity <- "Augmented"
      tmp$MDA_scheme <- "Augmented"
      tmp[, 22:25] <- 0
      tmp[, 27:30] <- 0
      tmp[, 32:40] <- 0
      
      newdf[[i]] <- rbind(tmp, newdf[[i]])
    }
  }
  
  # Combine the processed data for non-CMR-specific IU_CODES
  newdf <- do.call(rbind, newdf)
  dfAPOC_included2 <- newdf
  
  # Combine both CMR and non-CMR dataframes
  dfAPOC_included2_combined <- rbind(dfAPOC_included2, dfAPOC_included2_CMR)
  dfAPOC_included2_combined <- dfAPOC_included2_combined[order(dfAPOC_included2_combined$IU_ID_MAPPING), ]
  
  cat("\nCheck for number of IUs after processing")
  cat(" IUs in processed dataframe:", nrow(subset(dfAPOC_included2_combined, Year == 2022)), "\n")
  
  
  # Return the final dataframe
  return(dfAPOC_included2_combined)
}

# ==================================================================================== #
#     Function for defining inclusion of histories (ESPEN yrs 2013 - 2022)             #
# ==================================================================================== #

# Define the function for Pre-ESPEN and Post-ESPEN history processing
define_pre_post_espen_inclusion_func <- function(df) {
  
  # Pre-ESPEN history to include
  df <- df %>%
    group_by(IU_ID_MAPPING) %>%
    mutate(
      Pre_ESPEN_MDA_history = ifelse(
        (any(MAX_Endemicity %in% c("Endemic (MDA not delivered)", "Endemic (under MDA)", "Endemic (under post-intervention surveillance)")) &
           any(Cum_MDA > 0 & Year == 2013)) |
          (any(MAX_Endemicity %in% c("Unknown (consider Oncho Elimination Mapping)", "Unknown (under LF MDA)", "Not reported")) &
             any(endemicity_baseline %in% c("mesoendemic", "hypoendemic")) &
             any(Cum_MDA > 0 & Year == 2013)),
        "Include",
        "Exclude"
      )
    ) %>%
    ungroup()  # Remove grouping information
  
  # Post-ESPEN history to include
  df <- df %>%
    group_by(IU_ID_MAPPING) %>%
    mutate(
      ESPEN_MDA_history = ifelse(
        (any(MAX_Endemicity %in% c("Endemic (MDA not delivered)", "Endemic (under MDA)", "Endemic (under post-intervention surveillance)")) &
           any(Cum_MDA > 0 & Year > 2013)) |
          (any(MAX_Endemicity %in% c("Unknown (consider Oncho Elimination Mapping)", "Unknown (under LF MDA)", "Not reported")) &
             any(endemicity_baseline %in% c("mesoendemic", "hypoendemic")) &
             any(Cum_MDA > 0 & Year > 2013)),
        "Include",
        "Exclude"
      )
    ) %>%
    ungroup()  # Remove grouping information
  
  # checks (1) :
  cat(" check labels included/excluded made in 'Pre_ESPEN_MDA_history'col:", unique(df$Pre_ESPEN_MDA_history), "\n")
  cat(" check labels included/excluded made in 'ESPEN_MDA_history'col:", unique(df$ESPEN_MDA_history), "\n")
  
  # checks (2) :
  check1 <- df %>%
    filter(
      (Pre_ESPEN_MDA_history == "Include" & ESPEN_MDA_history == "Exclude") ) # its always "exclude" pre-ESPEN and "include" ESPEN, not otherway ( n = 26 IUs) = IUs where no pre-ESPEN only ESPEN (new?)
  cat(" check if any IUs have pre-ESPEN MDA to include BUT no ESPEN MDA to include:", length(unique(check1$IU_ID_MAPPING)), "\n")
  
  
  check2 <- df %>%
    filter(
      (Pre_ESPEN_MDA_history == "Exclude" & ESPEN_MDA_history == "Include") ) # its always "exclude" pre-ESPEN and "include" ESPEN, not otherway ( n = 26 IUs) = IUs where no pre-ESPEN only ESPEN (new?)
  cat(" check if any IUs have ESPEN MDA to include BUT no pre-ESPEN MDA to include:", length(unique(check2$IU_ID_MAPPING)), "\n")
  
  check3 <- df %>%
    filter(
      (Pre_ESPEN_MDA_history == "Exclude" & ESPEN_MDA_history == "Exclude") ) # its always "exclude" pre-ESPEN and "include" ESPEN, not otherway ( n = 26 IUs) = IUs where no pre-ESPEN only ESPEN (new?)
  cat(" check if any IUs are BOTH pre-ESPEN & ESPEN MDA to EXCLUDE (i.e., treatment naive?):", length(unique(check3$IU_ID_MAPPING)), "\n")
  
  # Return the updated dataframe
  return(df)
}

# ==================================================================================== #
#    Function to extract and process information on IUs with biannual MDA              #
# ==================================================================================== #

get_biannual_ius <- function(file_path) {
  # Read the Biannual treatment data from the CSV
  Biannual_APOC_IUs <- read.csv(file_path)
  
  # Filter out rows where Biannual is NA
  Biannual_APOC_IUs2 <- Biannual_APOC_IUs[!is.na(Biannual_APOC_IUs$Biannual), ]
  
  # Create Biannual_IUs_vec
  Biannual_IUs_vec <- unique(Biannual_APOC_IUs2$IUs_NAME)
  
  # Filter Sudan-specific data for Biannual treatment
  Biannual_Sudan_IUs <- subset(Biannual_APOC_IUs2, ADMIN0ISO3 == "SDN")
  
  # Create Biannual_Sudan_IUs_vec
  Biannual_Sudan_IUs_vec <- unique(Biannual_Sudan_IUs$IUs_NAME2)
  
  # Check the results
  cat(" head of biannual in APOC vector:", unique(head(Biannual_IUs_vec)), "\n")
  cat(" length of biannual in APOC vector:", length(unique(Biannual_IUs_vec)), "\n")
  
  cat(" head of biannual in SDN vector:", unique(head(Biannual_Sudan_IUs_vec)), "\n")
  cat(" length of biannual in SDN vector:", length(unique(Biannual_Sudan_IUs_vec)), "\n")
  
  # Return the two vectors
  return(list(Biannual_IUs_vec = Biannual_IUs_vec, Biannual_Sudan_IUs_vec = Biannual_Sudan_IUs_vec))
}

# ================================================================================================ #
#                   Function to process pre-ESPEN MDA for the countries                            #
# "AGO", "BDI", "CMR", "CAF", "TCD", "COG", "COD", "GNQ", "ETH", "GAB", "LBR", "MWI", "SSD", "TZA" #
# ================================================================================================ #


# Define the function to process MDA coverage assignment for each country
process_mda_coverage <- function(df, split_vectors, country_code, coverage_data, start_year, condition_year_start, condition_year_end) {
  
  # Extract the coverage data for the specific country
  coverages <- coverage_data
  coverages <- coverages[start_year:length(coverages)]  # Slice the coverage data based on the start year
  coverages <- coverages / 100  # Convert to proportion (percentage -> decimal)
  
  # Apply the coverage condition
  coverages2 <- ifelse(is.na(coverages), NA, ifelse(coverages < 0.65, 0.25, 0.65))  # Apply threshold for coverage
  
  # Create a condition for the specific country and years
  condition <- with(df, ADMIN0ISO3 == country_code & trimws(Pre_ESPEN_MDA_history) == "Include" & Year > condition_year_start & Year < condition_year_end)
  indices <- which(condition)
  
  # Create the necessary columns if they don't exist (on first iteration only)
  if (!"MDA_CDTI_raw" %in% colnames(df)) {
    df$MDA_CDTI_raw <- NA_real_
    df$cov_source <- NA
    df$cov_specific_source <- NA
    df$MDA_CDTI <- NA_real_
  }
  
  # Use indexing to assign values based on the condition
  df$MDA_CDTI_raw[indices] <- rep(coverages, length.out = length(indices))
  df$cov_source[indices] <- rep("national-level coverage data", length.out = length(indices))
  df$cov_specific_source[indices] <- rep("APOC report, 2015", length.out = length(indices))
  df$MDA_CDTI[indices] <- rep(coverages2, length.out = length(indices))
  
  return(df)
}

# ================================================================================ #
# Function to iterate over country_info list to update the dataframe with MDA data #
# ================================================================================ #

process_all_countries <- function(dfAPOC_included2, split_vectors, country_info) {
  
  # Start with the original dataframe
  dfAPOC_included3 <- dfAPOC_included2
  
  # Loop over all countries in country_info
  for (country in country_info) {
    
    # Run the process_mda_coverage function with the current state of the dataframe
    dfAPOC_included3 <- process_mda_coverage(
      dfAPOC_included3,  # Pass the current state of the dataframe (modified by previous countries)
      split_vectors, 
      country_code = country$country_code,
      coverage_data = country$coverage_data, 
      start_year = country$start_year,
      condition_year_start = country$condition_year_start,
      condition_year_end = country$condition_year_end
    )
    
    # If the country is Tanzania (TZA), apply the IU filtering logic (extra step for March 2025)
    if (country$country_code == "TZA") {
      dfAPOC_included3 <- dfAPOC_included3[!dfAPOC_included3$IU_CODE_MAPPING %in% c(
        "TZA0478846626", "TZA0479246628", "TZA0480046634", "TZA0480046635", 
        "TZA0480346638", "TZA0480846644", "TZA0481146645"
      ), ]
    }
    
    # After each country is processed, you can either save or further manipulate dfAPOC_included3
    # For example: store the result or return a list of processed dataframes
    # For now, we'll just print the number of rows processed for each country:
    cat("Processed country:", country$country_code, "\n")
    cat("Rows in 2022:", nrow(subset(dfAPOC_included3, Year == 2022)), "\n")
    cat("Unique IU_ID_MAPPING count:", length(unique(dfAPOC_included3$IU_ID_MAPPING)), "\n")
  }
  
  # Return the final processed dataframe (after all countries are processed)
  return(dfAPOC_included3)
}

# ======================================================================================== #
#         Function to extract, process and integrate oncho snapshot data for Nigeria       #
# ======================================================================================== #

process_nigeria_data <- function(df, split_vectors, oncho_snapshot_path) {
  
  # Subset the dataframe for Nigeria (NGA)
  df_NGA <- subset(df, ADMIN0ISO3 == "NGA")
  
  # APPROACH 1: Count rows by IU_ID_MAPPING
  row_counts <- df_NGA %>%
    group_by(IU_ID_MAPPING) %>%
    summarise(row_count = n())
  
  # APPROACH 2: Process national oncho snapshot data
  oncho_snapshot <- read.csv(oncho_snapshot_path)
  oncho_snapshot <- oncho_snapshot %>% rename_with(~gsub("^X", "", .), starts_with("X1990"):ends_with("X2021"))
  
  columns_to_update <- as.character(seq(1990, 2021))
  oncho_snapshot <- oncho_snapshot %>%
    mutate(across(all_of(columns_to_update),
                  ~ case_when(
                    . == 0 ~ 0,
                    . < 65 ~ 0.25,
                    . >= 65 ~ 0.65,
                    TRUE ~ .  # Keep the original value if none of the conditions match
                  )
    ))
  
  oncho_snapshot_long <- oncho_snapshot %>%
    pivot_longer(cols = starts_with("1990"):ends_with("2021"),
                 names_to = "Year",
                 values_to = "Coverage_value_tmp")
  
  # Adding raw coverages and cov data source cols
  oncho_snapshot_long_raw <- oncho_snapshot %>%
    pivot_longer(cols = starts_with("1990"):ends_with("2021"),
                 names_to = "Year",
                 values_to = "Coverage_value_tmp_raw")
  
  oncho_snapshot_long$Coverage_value_tmp_raw <- oncho_snapshot_long_raw$Coverage_value_tmp_raw / 100
  
  oncho_snapshot_long$Coverage_source_temp <- ifelse(!is.na(oncho_snapshot_long$Coverage_value_tmp_raw), "IU-level coverage", NA)
  oncho_snapshot_long$Coverage_source_temp2 <- ifelse(!is.na(oncho_snapshot_long$Coverage_value_tmp_raw), "FMOH national oncho snapshot", NA)
  
  oncho_snapshot_long$Year <- as.numeric(oncho_snapshot_long$Year)
  
  # Merge with dfAPOC_included_NGA
  df_NGA_merged <- df_NGA %>%
    left_join(oncho_snapshot_long, by = c("ADMIN0", "ADMIN1", "ADMIN2", "Year"))
  
  # Fill missing coverage values
  df_NGA_merged$MDA_CDTI[is.na(df_NGA_merged$MDA_CDTI)] <- df_NGA_merged$Coverage_value_tmp[is.na(df_NGA_merged$MDA_CDTI)]
  
  # For raw coverage values and cov data source columns
  df_NGA_merged$MDA_CDTI_raw[is.na(df_NGA_merged$MDA_CDTI_raw)] <- df_NGA_merged$Coverage_value_tmp_raw[is.na(df_NGA_merged$MDA_CDTI_raw)]
  df_NGA_merged$cov_source[is.na(df_NGA_merged$cov_source)] <- df_NGA_merged$Coverage_source_temp[is.na(df_NGA_merged$cov_source)]
  df_NGA_merged$cov_specific_source[is.na(df_NGA_merged$cov_specific_source)] <- df_NGA_merged$Coverage_source_temp2[is.na(df_NGA_merged$cov_specific_source)]
  
  # Remove extra columns from snapshot to match the original dfAPOC_included structure
  df_NGA_merged <- df_NGA_merged %>% select(1:72)
  
  # Identify rows that are in dfAPOC_included but not in the merged dataframe
  rows_to_keep <- df %>%
    anti_join(df_NGA_merged, by = c("ADMIN0", "ADMIN1", "ADMIN2", "Year"))
  
  # Update the original dataframe by including the merged rows
  df_updated <- bind_rows(rows_to_keep, df_NGA_merged)
  
  # Now handle any extra rows to remove based on conditions
  oncho_snapshot_long_nonendemic <- subset(
    oncho_snapshot_long, 
    Endemic_classification %in% c("Not Endemic", "Not Endemic ") & 
      !ADMIN2 %in% c("Enugu North", "Yola North")
  )
  
  NGA_noneendemic <- unique(oncho_snapshot_long_nonendemic$ADMIN2)
  
  # Remove non-endemic IUs from dfAPOC_included_updated
  df_updated <- df_updated[!df_updated$IUs_NAME_MAPPING %in% NGA_noneendemic, ]
  
  # Additional manual rows to remove
  extra_to_remove <- c("Quan'Pam", "Langtan North", "Langtan South", "Kanem")
  df_updated <- df_updated[!df_updated$IUs_NAME_MAPPING %in% extra_to_remove, ]
  df_updated <- df_updated[!(df_updated$IUs_NAME_MAPPING == "Obi" & df_updated$ADMIN1 == "Nasarawa"), ]
  
  # Check number of IUs :
  cat("Rows in 2022 after processing Nigeria IUs:", nrow(subset(df_updated, Year == 2022)), "\n")
  cat("Unique IU_ID_MAPPING count after processing Nigeria IUs:", length(unique(df_updated$IU_ID_MAPPING)), "\n")
  
  # Return the updated dataframe for Nigeria
  return(df_updated)
}


# ====================================================================================================== #
#      Function to extract, process and integrate MDA data for Uganda based on Katabarwa for Uganda      #
# ====================================================================================================== #

process_uganda_data <- function(df, UGA_strt_stp_path, split_vectors) {
  
  # get UGA MDa data from Katabarwa et al. 2018 & group districts:
  UGA_strt_stp <- read.csv(UGA_strt_stp_path)
  
  UGA_strt_stp <- subset(UGA_strt_stp, District_updated != "Obongi") # remove and manually include as the Obongi foci
  
  UGA_strt_stp$RB_MDA_Start <- as.numeric(UGA_strt_stp$RB_MDA_Start)
  UGA_strt_stp$RB_MDA_End <- as.numeric(UGA_strt_stp$RB_MDA_End)
  
  UGA_strt_stp$YearRange <- paste(UGA_strt_stp$RB_MDA_Start, UGA_strt_stp$RB_MDA_End, sep = ',')
  
  UGA_grouped_district <- UGA_strt_stp %>%
    group_by(YearRange) %>%
    summarise(UniqueDistricts = toString(unique(District_updated))) # group LGAs
  
  UGA_grouped_district <- UGA_grouped_district %>%
    separate(YearRange, into = c("RB_strt_yr", "RB_end_yr"), sep = ",", convert = TRUE) # split into 2 numeric val columns for strt and end yr
  
  UGA_grouped_district
  
  
  # Coverage vector and adjustments
  coverages_UGA <- split_vectors$Uganda
  coverages_UGA <- coverages_UGA / 100
  coverages_UGA_all <- ifelse(is.na(coverages_UGA), NA, ifelse(coverages_UGA < 0.65, 0.25, 0.65))
  
  # Add 9 x 0.25 values at the start for pre-1999 years
  coverages_UGA1 <- c(rep(0.25, 9), coverages_UGA_all)
  
  # Function to apply coverage to districts
  apply_coverage <- function(df, districts, coverages, coverage_type, start_year, end_year) {
    condition <- with(df, ADMIN0ISO3 == "UGA" & Year > start_year & Year < end_year & ADMIN2 %in% districts)
    indices <- which(condition)
    df$MDA_CDTI[indices] <- rep(coverages, length.out = length(indices))
    df$cov_source[indices] <- rep(coverage_type, length.out = length(indices))
    return(df)
  }
  
  # Apply coverage data for different district groups
  # First group of districts
  districts1 <- unlist(strsplit(as.character(UGA_grouped_district[1, 3]), ",\\s*"))
  df_updated <- apply_coverage(df, districts1, coverages_UGA1, "assumed", 1989, 1999)
  
  # Years covered by APOC report (1999-2012)
  df_updated <- apply_coverage(df_updated, districts1, coverages_UGA, "national-level coverage data", 1998, 2013)
  
  # for years covered by APOC, 2015 report
  condition2 <- with(df_updated, ADMIN0ISO3 == "UGA" & Year > 1998 & Year < 2013 & ADMIN2 %in% districts1)
  indices <- which(condition2)
  df_updated$MDA_CDTI_raw[indices] <- rep(coverages_UGA, length.out = length(indices))
  df_updated$cov_source[indices] <- rep("national-level coverage data", length.out = length(indices))
  df_updated$cov_specific_source[indices] <- rep("APOC report, 2015", length.out = length(indices))
  
  # for years pre-APOC, 2015 report; Katabarwa indicates coverage (assumed non-CDTI)
  condition3 <- with(df_updated, ADMIN0ISO3 == "UGA" & Year > 1989 & Year < 1999 & ADMIN2 %in% districts1)
  indices <- which(condition3)
  df_updated$cov_source[indices] <- rep("assumed", length.out = length(indices))
  df_updated$cov_specific_source[indices] <- rep("Katabarwa et al. 2018 indicates MDA (no coverage; assumed non-CDTI)", length.out = length(indices))
  
  # ======================== #
  # second group of districts
  coverages_UGA2 <- coverages_UGA1[-c(1, length(coverages_UGA1))] # remove first and last element
  districts2 <- unlist(strsplit(as.character(UGA_grouped_district[2, 3]), ",\\s*"))
  df_updated <- apply_coverage(df_updated, districts2, coverages_UGA2, "assumed", 1990, 1999)
  
  # Years covered by APOC report (1999-2012)
  df_updated <- apply_coverage(df_updated, districts2, coverages_UGA, "national-level coverage data", 1998, 2013)
  
  # for years covered by APOC, 2015 report
  condition2 <- with(df_updated, ADMIN0ISO3 == "UGA" & Year > 1998 & Year < 2013 & ADMIN2 %in% districts2)
  indices <- which(condition2)
  df_updated$MDA_CDTI_raw[indices] <- rep(coverages_UGA, length.out = length(indices))
  df_updated$cov_source[indices] <- rep("national-level coverage data", length.out = length(indices))
  df_updated$cov_specific_source[indices] <- rep("APOC report, 2015", length.out = length(indices))
  
  # for years pre-APOC, 2015 report; Katabarwa indicates coverage (assumed non-CDTI)
  condition3 <- with(df_updated, ADMIN0ISO3 == "UGA" & Year > 1990 & Year < 1999 & ADMIN2 %in% districts2)
  indices <- which(condition3)
  df_updated$cov_source[indices] <- rep("assumed", length.out = length(indices))
  df_updated$cov_specific_source[indices] <- rep("Katabarwa et al. 2018 indicates MDA (no coverage; assumed non-CDTI)", length.out = length(indices))
  
  # ======================== #
  # third group of districts
  coverages_UGA3 <- coverages_UGA1[-1] # remove first element
  districts3a <- unlist(strsplit(as.character(UGA_grouped_district[3, 3]), ",\\s*"))
  districts3b <- unlist(strsplit(as.character(UGA_grouped_district[4, 3]), ",\\s*"))
  districts3 <- c(districts3a, districts3b)
  df_updated <- apply_coverage(df_updated, districts3, coverages_UGA3, "assumed", 1990, 1999)
  
  # Years covered by APOC report (1999-2012)
  df_updated <- apply_coverage(df_updated, districts3, coverages_UGA, "national-level coverage data", 1998, 2013)
  
  # for years covered by APOC, 2015 report
  condition2 <- with(df_updated, ADMIN0ISO3 == "UGA" & Year > 1998 & Year < 2013 & ADMIN2 %in% districts3)
  indices <- which(condition2)
  df_updated$MDA_CDTI_raw[indices] <- rep(coverages_UGA, length.out = length(indices))
  df_updated$cov_source[indices] <- rep("national-level coverage data", length.out = length(indices))
  df_updated$cov_specific_source[indices] <- rep("APOC report, 2015", length.out = length(indices))
  
  # for years pre-APOC, 2015 report; Katabarwa indicates coverage (assumed non-CDTI)
  condition3 <- with(df_updated, ADMIN0ISO3 == "UGA" & Year > 1990 & Year < 1999 & ADMIN2 %in% districts3)
  indices <- which(condition3)
  df_updated$cov_source[indices] <- rep("assumed", length.out = length(indices))
  df_updated$cov_specific_source[indices] <- rep("Katabarwa et al. 2018 indicates MDA (no coverage; assumed non-CDTI)", length.out = length(indices))
  
  # ======================== #
  # fourth group of districts
  coverages_UGA4 <- coverages_UGA1[-c(1:3, (length(coverages_UGA1) - 1):length(coverages_UGA1))] # remove first 3 and last 2 elements from cov vector
  districts4 <- unlist(strsplit(as.character(UGA_grouped_district[5, 3]), ",\\s*"))
  df_updated <- apply_coverage(df_updated, districts4, coverages_UGA4, "assumed", 1992, 1999)
  
  # Years covered by APOC report (1999-2012)
  df_updated <- apply_coverage(df_updated, districts4, coverages_UGA, "national-level coverage data", 1998, 2011)
  
  # for years covered by APOC, 2015 report
  condition2 <- with(df_updated, ADMIN0ISO3 == "UGA" & Year > 1998 & Year < 2011 & ADMIN2 %in% districts4)
  indices <- which(condition2)
  df_updated$MDA_CDTI_raw[indices] <- rep(coverages_UGA, length.out = length(indices))
  df_updated$cov_source[indices] <- rep("national-level coverage data", length.out = length(indices))
  df_updated$cov_specific_source[indices] <- rep("APOC report, 2015", length.out = length(indices))
  
  # for years pre-APOC, 2015 report; Katabarwa indicates coverage (assumed non-CDTI)
  condition3 <- with(df_updated, ADMIN0ISO3 == "UGA" & Year > 1992 & Year < 1999 & ADMIN2 %in% districts4)
  indices <- which(condition3)
  df_updated$cov_source[indices] <- rep("assumed", length.out = length(indices))
  df_updated$cov_specific_source[indices] <- rep("Katabarwa et al. 2018 indicates MDA (no coverage; assumed non-CDTI)", length.out = length(indices))
  
  # ======================== #
  # fifth group of districts
  coverages_UGA5 <- coverages_UGA1[-c(1:3)] # remove first 3
  districts5a <- unlist(strsplit(as.character(UGA_grouped_district[6, 3]), ",\\s*"))
  districts5b <- unlist(strsplit(as.character(UGA_grouped_district[7, 3]), ",\\s*"))
  districts5 <- c(districts5a, districts5b)
  df_updated <- apply_coverage(df_updated, districts5, coverages_UGA5, "assumed", 1992, 1999)
  
  # Years covered by APOC report (1999-2012)
  df_updated <- apply_coverage(df_updated, districts5, coverages_UGA, "national-level coverage data", 1998, 2013)
  
  # for years covered by APOC, 2015 report
  condition2 <- with(df_updated, ADMIN0ISO3 == "UGA" & Year > 1998 & Year < 2013 & ADMIN2 %in% districts5)
  indices <- which(condition2)
  df_updated$MDA_CDTI_raw[indices] <- rep(coverages_UGA, length.out = length(indices))
  df_updated$cov_source[indices] <- rep("national-level coverage data", length.out = length(indices))
  df_updated$cov_specific_source[indices] <- rep("APOC report, 2015", length.out = length(indices))
  
  # for years pre-APOC, 2015 report; Katabarwa indicates coverage (assumed non-CDTI)
  condition3 <- with(df_updated, ADMIN0ISO3 == "UGA" & Year > 1992 & Year < 1999 & ADMIN2 %in% districts5)
  indices <- which(condition3)
  df_updated$cov_source[indices] <- rep("assumed", length.out = length(indices))
  df_updated$cov_specific_source[indices] <- rep("Katabarwa et al. 2018 indicates MDA (no coverage; assumed non-CDTI)", length.out = length(indices))
  
  # ======================== #
  # sixth group of districts
  coverages_UGA6 <- coverages_UGA1[-c(1:4)] # remove first 4
  districts6a <- unlist(strsplit(as.character(UGA_grouped_district[8, 3]), ",\\s*"))
  districts6b <- unlist(strsplit(as.character(UGA_grouped_district[9, 3]), ",\\s*"))
  districts6 <- c(districts6a, districts6b)
  df_updated <- apply_coverage(df_updated, districts6, coverages_UGA6, "assumed", 1993, 1999)
  
  # Years covered by APOC report (1999-2012)
  df_updated <- apply_coverage(df_updated, districts6, coverages_UGA, "national-level coverage data", 1998, 2013)
  
  # for years covered by APOC, 2015 report
  condition2 <- with(df_updated, ADMIN0ISO3 == "UGA" & Year > 1998 & Year < 2013 & ADMIN2 %in% districts6)
  indices <- which(condition2)
  df_updated$MDA_CDTI_raw[indices] <- rep(coverages_UGA, length.out = length(indices))
  df_updated$cov_source[indices] <- rep("national-level coverage data", length.out = length(indices))
  df_updated$cov_specific_source[indices] <- rep("APOC report, 2015", length.out = length(indices))
  
  # for years pre-APOC, 2015 report; Katabarwa indicates coverage (assumed non-CDTI)
  condition3 <- with(df_updated, ADMIN0ISO3 == "UGA" & Year > 1993 & Year < 1999 & ADMIN2 %in% districts6)
  indices <- which(condition3)
  df_updated$cov_source[indices] <- rep("assumed", length.out = length(indices))
  df_updated$cov_specific_source[indices] <- rep("Katabarwa et al. 2018 indicates MDA (no coverage; assumed non-CDTI)", length.out = length(indices))
  
  # Check number of IUs :
  cat("Rows in 2022 after processing Ugandan IUs:", nrow(subset(df_updated, Year == 2022)), "\n")
  cat("Unique IU_ID_MAPPING count after processing Ugandan IUs:", length(unique(df_updated$IU_ID_MAPPING)), "\n")
  
  
  return(list(df_updated, coverages_UGA2))
}


# ==================================================================================== #
#                   Function to integrate Sudan MDA using The Carter Center data       #
# ==================================================================================== #

process_sudan_data <- function(df, split_vectors) {
  
  # Coverage vectors for Sudan foci
  coverages_SDN_foci1 <- c(0.25, 0.25, 0.25, 0.25, 0.65, 0.25, 0.25, 0.25, 0.65, 0.25, 0.65, 0.65, 0.25, 0.65, 0.65, 0.65, 0.65)
  coverages_SDN_foci2 <- c(0.65, 0.65, 0.25, 0.65, 0.65, 0.65, 0.65, 0.65, 0.65, 0.65, 0.65)
  coverages_SDN_foci3 <- c(0.65, 0.65, 0.65, 0.65, 0.65, 0.65, 0.65, 0.65, 0.65, 0.65, 0.65)
  
  # Helper function to apply coverage for a specific foci
  apply_coverage_sudan <- function(df, districts, coverages, coverage_type, start_year, end_year) {
    condition <- df$ADMIN0ISO3 == "SDN" & df$Year > start_year & df$Year < end_year & df$IUs_NAME_MAPPING %in% districts
    indices <- which(condition)
    df$MDA_CDTI[indices] <- rep(coverages, length.out = length(indices))
    df$cov_source[indices] <- rep(coverage_type, length.out = length(indices))
    return(df)
  }
  
  # Apply coverage for first foci (e.g., Abu Hamed, Marawai, Berber, El Quresha)
  districts1 <- c("Abu Hamed", "Marawai", "Berber", "El Quresha")
  df_updated <- apply_coverage_sudan(df, districts1, coverages_SDN_foci1, "assumed", 1995, 1999) # Pre-APOC
  df_updated <- apply_coverage_sudan(df_updated, districts1, coverages_SDN_foci1, "national-level coverage data", 1999, 2013) # Years covered by APOC
  
  # Specific coverage source for foci1
  condition2 <- df_updated$ADMIN0ISO3 == "SDN" & df_updated$Year > 1998 & df_updated$Year < 2013 & df_updated$IUs_NAME_MAPPING %in% districts1
  indices <- which(condition2)
  df_updated$cov_source[indices] <- rep("national-level coverage data", length.out = length(indices))
  df_updated$cov_specific_source[indices] <- rep("APOC report, 2015", length.out = length(indices))
  
  # Assumed coverage for pre-APOC
  condition3 <- df_updated$ADMIN0ISO3 == "SDN" & df_updated$Year > 1995 & df_updated$Year < 1999 & df_updated$IUs_NAME_MAPPING %in% districts1
  indices <- which(condition3)
  df_updated$cov_source[indices] <- rep("assumed", length.out = length(indices))
  df_updated$cov_specific_source[indices] <- rep("The Carter Centre info indicates MDA: no coverage, assume non-CDTI", length.out = length(indices))
  
  # Apply coverage for second foci (e.g., Eastern El Qalabat)
  districts2 <- c("Eastern El Qalabat")
  df_updated <- apply_coverage_sudan(df_updated, districts2, coverages_SDN_foci2, "assumed", 2006, 2018)
  
  # For years covered by APOC report
  df_updated <- apply_coverage_sudan(df_updated, districts2, coverages_SDN_foci2, "national-level coverage data", 2006, 2013)
  
  # Specific coverage source for foci2
  condition2 <- df_updated$ADMIN0ISO3 == "SDN" & df_updated$Year > 2006 & df_updated$Year < 2013 & df_updated$IUs_NAME_MAPPING %in% districts2
  indices <- which(condition2)
  df_updated$cov_source[indices] <- rep("national-level coverage data", length.out = length(indices))
  df_updated$cov_specific_source[indices] <- rep("APOC report, 2015", length.out = length(indices))
  
  # Assumed coverage after APOC report (2013-2017)
  condition3 <- df_updated$ADMIN0ISO3 == "SDN" & df_updated$Year > 2012 & df_updated$Year < 2018 & df_updated$IUs_NAME_MAPPING %in% districts2
  indices <- which(condition3)
  df_updated$cov_source[indices] <- rep("assumed", length.out = length(indices))
  df_updated$cov_specific_source[indices] <- rep("The Carter Centre info indicates MDA: no coverage in ESPEN, assume CDTI with 65% total population coverage", length.out = length(indices))
  
  # Apply coverage for third foci (e.g., El Radoom)
  districts3 <- c("El Radoom")
  df_updated <- apply_coverage_sudan(df_updated, districts3, coverages_SDN_foci3, "assumed", 2011, 2018)
  
  # For years covered by APOC report
  df_updated <- apply_coverage_sudan(df_updated, districts3, coverages_SDN_foci3, "national-level coverage data", 2011, 2013)
  
  # Specific coverage source for foci3
  condition2 <- df_updated$ADMIN0ISO3 == "SDN" & df_updated$Year > 2011 & df_updated$Year < 2013 & df_updated$IUs_NAME_MAPPING %in% districts3
  indices <- which(condition2)
  df_updated$cov_source[indices] <- rep("national-level coverage data", length.out = length(indices))
  df_updated$cov_specific_source[indices] <- rep("APOC report, 2015", length.out = length(indices))
  
  # Assumed coverage after APOC report (2013-2017)
  condition3 <- df_updated$ADMIN0ISO3 == "SDN" & df_updated$Year > 2012 & df_updated$Year < 2018 & df_updated$IUs_NAME_MAPPING %in% districts3
  indices <- which(condition3)
  df_updated$cov_source[indices] <- rep("assumed", length.out = length(indices))
  df_updated$cov_specific_source[indices] <- rep("The Carter Centre info indicates MDA: no coverage in ESPEN, assume CDTI with 65% total population coverage", length.out = length(indices))
  
  # Check number of IUs :
  cat("Rows in 2022 after processing Sudanese IUs:", nrow(subset(df_updated, Year == 2022)), "\n")
  cat("Unique IU_ID_MAPPING count after processing Sudanese IUs:", length(unique(df_updated$IU_ID_MAPPING)), "\n")
  
  return(df_updated)
}

# ================================================================================ #
#               Function to update MDA cov in ESPEN years (2013-2022)              #
# ================================================================================ #

update_espen_mda_years <- function(df) {
  
  df_updated <- df # df to update
  
  # Update for epi coverage > 65%
  condition_high_cov <- df_updated$EpiCov >= 65 &
    df_updated$Year %in% 2013:2022 &
    df_updated$ADMIN0ISO3 != "SDN"
  indices_high_cov <- which(condition_high_cov)
  
  df_updated$MDA_CDTI[indices_high_cov] <- rep(0.65, length.out = length(indices_high_cov))
  df_updated$MDA_CDTI_raw[indices_high_cov] <- df_updated$EpiCov[indices_high_cov] / 100
  df_updated$cov_source[indices_high_cov] <- rep("IU-level coverage", length.out = length(indices_high_cov))
  df_updated$cov_specific_source[indices_high_cov] <- rep("ESPEN", length.out = length(indices_high_cov))
  
  # Update for epi coverage < 65% and > 0%
  condition_low_cov <- df_updated$EpiCov > 0 &
    df_updated$EpiCov < 65 &
    df_updated$Year %in% 2013:2022 &
    df_updated$ADMIN0ISO3 != "SDN"
  indices_low_cov <- which(condition_low_cov)
  
  df_updated$MDA_CDTI[indices_low_cov] <- rep(0.25, length.out = length(indices_low_cov))
  df_updated$MDA_CDTI_raw[indices_low_cov] <- df_updated$EpiCov[indices_low_cov] / 100
  df_updated$cov_source[indices_low_cov] <- rep("IU-level coverage", length.out = length(indices_low_cov))
  df_updated$cov_specific_source[indices_low_cov] <- rep("ESPEN", length.out = length(indices_low_cov))
  
  # check number of IUs at this stage:
  cat("Rows in 2022 after processing (filling MDA in ESPEN  years 2013-22):", nrow(subset(df_updated, Year == 2022)), "\n")
  cat("Unique IU_ID_MAPPING count after processing (filling MDA in ESPEN  years 2013-22):", length(unique(df_updated$IU_ID_MAPPING)), "\n")
  
  
  return(df_updated)
}

# =================================================================== #
#           Function to update MDA in years 2013/14                   #
# =================================================================== #

update_mda_coverage_2013_2014 <- function(df) {
  
  df_updated <- df #
  
  # Check for IUs with Cum_MDA > 0 but no EpiCov > 0 during 2013-2022
  result <- df_updated %>%
    filter(Year >= 2013 & Year <= 2022) %>%
    group_by(IU_ID_MAPPING) %>%
    summarise(has_positive_Cum_MDA = any(Cum_MDA > 0),
              no_positive_EpiCov = all(EpiCov == 0))
  
  # Filter rows where Cum_MDA > 0 but no positive EpiCov
  result_filtered <- result %>%
    filter(has_positive_Cum_MDA & no_positive_EpiCov)
  
  unique_noEpiCov_vec <- result_filtered$IU_ID_MAPPING
  
  # ESPEN MDA years (2013 and 2014) for countries that need 65% coverage
  condition_65 <- df_updated$ESPEN_MDA_history == "Include" &
    (df_updated$Year == 2013 | df_updated$Year == 2014) &
    (df_updated$Endemicity == "Not reported" | df_updated$Endemicity == "Augmented") &
    (df_updated$ADMIN0ISO3 %in% c("BDI", "CMR", "TCD", "COG", "COD", "ETH", "LBR", "MWI", "NGA", "TZA", "UGA")) &
    !(df_updated$IU_ID_MAPPING %in% unique_noEpiCov_vec)
  
  indices_65 <- which(condition_65)
  df_updated$MDA_CDTI[indices_65] <- rep(0.65, length.out = length(indices_65))
  
  # Add columns for raw coverage, and source of info
  df_updated$cov_source[indices_65] <- rep("assumed", length.out = length(indices_65))
  df_updated$cov_specific_source[indices_65] <- rep("assumed (continuous MDA in 2013 and/or 2014)", length.out = length(indices_65))
  
  # ESPEN MDA years (2013 and 2014) for countries that need 25% coverage
  condition_25 <- df_updated$ESPEN_MDA_history == "Include" &
    (df_updated$Year == 2013 | df_updated$Year == 2014) &
    (df_updated$Endemicity == "Not reported" | df_updated$Endemicity == "Augmented") &
    (df_updated$ADMIN0ISO3 %in% c("GNQ", "SSD")) &
    !(df_updated$IU_ID_MAPPING %in% unique_noEpiCov_vec)
  
  indices_25 <- which(condition_25)
  df_updated$MDA_CDTI[indices_25] <- rep(0.25, length.out = length(indices_25))
  
  # Add columns for raw coverage, and source of info for 25% coverage
  df_updated$cov_source[indices_25] <- rep("national-level coverage data", length.out = length(indices_25))
  df_updated$cov_specific_source[indices_25] <- rep("APOC report, 2015", length.out = length(indices_25))
  
  # check number of IUs at this stage:
  cat("Rows in 2022 after processing (updating 2013/2014 MDA years):", nrow(subset(df_updated, Year == 2022)), "\n")
  cat("Unique IU_ID_MAPPING count after processing (updating 2013/2014 MDA years):", length(unique(df_updated$IU_ID_MAPPING)), "\n")
  
  
  return(df_updated)
}

# ======================================================================================== #
#             Function to update IUs in Nigeria for 2022 with TCC & Sightsavers info       #
# ======================================================================================== #

update_nigeria_2022_data <- function(df, NGA_strt_stp_LGA_path, NGA_snapshot_LGAnames_path, Sightsavers_data_path, NGA_2022_EPSEN_path) {
  
  df_updated <- df 
  
  # Load data
  NGA_strt_stp_LGA <- read.csv(NGA_strt_stp_LGA_path)
  NGA_snapshot_LGAnames <- read.csv(NGA_snapshot_LGAnames_path)
  NGA_sightsavers_2022 <- read.csv(Sightsavers_data_path)
  NGA_2022_EPSEN <- read.csv(NGA_2022_EPSEN_path)
  
  # Convert MDA start and end years to numeric
  NGA_strt_stp_LGA$RB_MDA_Start <- as.numeric(NGA_strt_stp_LGA$RB_MDA_Start)
  NGA_strt_stp_LGA$RB_MDA_End <- as.numeric(NGA_strt_stp_LGA$RB_MDA_End)
  
  # Create YearRange for grouping
  NGA_strt_stp_LGA$YearRange <- paste(NGA_strt_stp_LGA$RB_MDA_Start, NGA_strt_stp_LGA$RB_MDA_End, sep = ',')
  
  # Group LGAs
  NGA_grouped_LGA <- NGA_strt_stp_LGA %>%
    group_by(YearRange) %>%
    summarise(UniqueLGAs = toString(unique(LGA_updated))) # group LGAs
  
  NGA_grouped_LGA <- NGA_grouped_LGA %>%
    separate(YearRange, into = c("RB_strt_yr", "RB_end_yr"), sep = ",", convert = TRUE) # split into 2 numeric val columns for strt and end yr
  
  # Set LGAs for 2022
  LGAs1_22 <- unlist(strsplit(as.character(NGA_grouped_LGA[3, 3]), ",\\s*"))
  LGAs2_22 <- unlist(strsplit(as.character(NGA_grouped_LGA[6, 3]), ",\\s*"))
  LGAs3_22 <- unlist(strsplit(as.character(NGA_grouped_LGA[8, 3]), ",\\s*"))
  LGAs4_22 <- unlist(strsplit(as.character(NGA_grouped_LGA[10, 3]), ",\\s*"))
  LGAs_22_MDA <- c(LGAs1_22, LGAs2_22, LGAs3_22, LGAs4_22)
  
  # Check for IUs with Cum_MDA > 0 but no EpiCov > 0 in 2022 (ESPEN)
  NGA_2022_ESPEN_covless65 <- subset(NGA_2022_EPSEN, EpiCov > 0 & EpiCov < 65)
  IUs_covless65_NGA_2022 <- unique(NGA_2022_ESPEN_covless65$ADMIN2)
  
  # Remove IUs already in ESPEN data with coverage < 65%
  to_remove <- intersect(IUs_covless65_NGA_2022, LGAs_22_MDA)
  LGAs_22_MDA <- LGAs_22_MDA[!LGAs_22_MDA %in% to_remove]
  
  # Update MDA with CC data (2022)
  condition_MDA22_NGA <- with(df_updated, ADMIN0ISO3 == "NGA" & Year == 2022 & ADMIN2 %in% LGAs_22_MDA)
  indices <- which(condition_MDA22_NGA)
  df_updated$MDA_CDTI[indices] <- rep(0.65, length.out = length(indices))
  
  # Add coverage columns
  df_updated$cov_source[indices] <- rep("assumed", length.out = length(indices))
  df_updated$cov_specific_source[indices] <- rep("The Carter Centre info indicates MDA: no coverage in ESPEN, assume CDTI with 65% total population coverage", length.out = length(indices))
  
  # Update with Sightsavers data (2022)
  LGAs_2022_SS <- NGA_sightsavers_2022$LGA_mappable
  condition <- (df_updated$Year == 2022) &
    (df_updated$ADMIN0ISO3 == "NGA") &
    (df_updated$ADMIN1 %in% c("Benue", "Kogi", "Kwara")) &
    (df_updated$IUs_NAME_MAPPING %in% LGAs_2022_SS)
  indices <- which(condition)
  df_updated$MDA_CDTI[indices] <- rep(0.65, length.out = length(indices)) # All EpiCov > 0.6499 for 2022 from Sightsavers data
  
  # Add coverage columns for Sightsavers data
  df_updated$cov_source[indices] <- rep("IU-level coverage", length.out = length(indices))
  df_updated$cov_specific_source[indices] <- rep("Sightsavers info", length.out = length(indices))
  
  # check number of IUs at this stage:
  cat("Rows in 2022 after processing (updating 2022 MDA in Nigeria with TCC/Sightsavers data):", nrow(subset(df_updated, Year == 2022)), "\n")
  cat("Unique IU_ID_MAPPING count after processing (updating 2022 MDA in Nigeria with TCC/Sightsavers data):", length(unique(df_updated$IU_ID_MAPPING)), "\n")
  
  
  return(df_updated)
}

# ============================================================================================== #
#   Function to create new coverage (Cov.in2) col and update CDTI cols & ETH (MDA in 2022)       #
# ============================================================================================== #

update_mda_coverage_cols_ETH_update <- function(df, ETH_2022_EPSEN_path) {
  
  df_updated <- df
  
  # Create new column for Cov.in2 based on MDA_CDTI
  df_updated$Cov.in2 <- df_updated$MDA_CDTI
  
  # first find new IUs in EtHiopia with latest 2022 data indicating first MDA round in 2022 
  ETH_2022_EPSEN <- read.csv(ETH_2022_EPSEN_path)
  ETH_MDA_2022 <- subset(ETH_2022_EPSEN, EpiCov > 0) #find those IUs with epicov > 0
  
  new_MDA_ETH_2022 <- subset(ETH_MDA_2022, Cum_MDA == 1)
  new_IUs_ETH <- new_MDA_ETH_2022$IU_ID
  new_IUs_ETH_name <- new_MDA_ETH_2022$ADMIN3
  
  dfAPOC_included4_subset_ETH2022 <- dfAPOC_included4[dfAPOC_included4$Year == 2022 & dfAPOC_included4$ADMIN0ISO3 == "ETH", ] # which Ius already included in 2022 in ETH
  dfAPOC_included4_subset_ETH2022_IUs <- dfAPOC_included4_subset_ETH2022$IU_ID_MAPPING # currently in histories
  
  # check if they arein the histories currently?
  new_IUs_ETH # all IUs with MDA for first time in ETH
  IU_ID_notinhistories <- setdiff(new_IUs_ETH, dfAPOC_included4_subset_ETH2022_IUs) # find Ius that are not in the histories currently for 2022
  IU_ID_notinhistories
  
  # make a vector of new IUs (minus those without baseline mf prev)
  IU_ID_new_toinclude_ETH <- setdiff(new_IUs_ETH, IU_ID_notinhistories)
  IU_ID_new_toinclude_ETH
  
  # Remove MDA in 2013 or 2014 where new_MDA_IUs_2022 == "ESPEN update: first MDA round in 2022" for ETH
  condition <- (df_updated$Year %in% c(2013)) &
    (df_updated$ADMIN0ISO3 == "ETH") &
    (df_updated$IU_ID %in% IU_ID_new_toinclude_ETH)
  indices <- which(condition)
  df_updated$Cov.in2[indices] <- rep(NA, length.out = length(indices))
  
  # Remove coverage source and specific source for these indices
  df_updated$cov_source[indices] <- rep(NA, length.out = length(indices))
  df_updated$cov_specific_source[indices] <- rep(NA, length.out = length(indices))
  
  # 1) Update MDA_CDTI to state if annual CDTI in each year
  df_updated$MDA_CDTI <- NA  # Reset MDA_CDTI column to NA
  
  # Set MDA_CDTI to 1 where Cov.in2 > 0
  condition <- df_updated$Cov.in2 > 0
  indices <- which(condition)
  df_updated$MDA_CDTI[indices] <- rep(1, length.out = length(indices))
  
  # check number of IUs at this stage:
  cat("Rows in 2022 after processing (updating 2022 MDA in ETH & general coverage cols e.g. Cov.in2):", nrow(subset(df_updated, Year == 2022)), "\n")
  cat("Unique IU_ID_MAPPING count after processing (updating 2022 MDA in ETH & general coverage cols e.g. Cov.in2):", length(unique(df_updated$IU_ID_MAPPING)), "\n")
  
  
  return(df_updated)
}


# ================================================================ #
#              Function to update biannual MDA in Uganda           #
# ================================================================ #

update_biannual_uganda_mda <- function(df, biannual_file_path) {
  
  df_updated <- df 
  
  # Load the Biannual MDA data
  Biannual_APOC_IUs <- read.csv(biannual_file_path)
  Biannual_APOC_IUs2 <- Biannual_APOC_IUs[!is.na(Biannual_APOC_IUs$Biannual), ]
  
  # Filter data for Uganda
  unique_IUs_UGA <- Biannual_APOC_IUs2 %>%
    filter(ADMIN0ISO3 == "UGA")
  
  unique_IUs_UGA_vec <- as.vector(unique_IUs_UGA$IUs_NAME)
  
  # Group Uganda IUs by Biannual years
  grouped_IUs_biannual_UGA <- unique_IUs_UGA %>%
    group_by(Biannual) %>%
    summarise(unique_IUs = list(unique(IUs_NAME))) # Group IUs by Biannual years
  
  grouped_IUs_biannual_UGA_vec <- grouped_IUs_biannual_UGA$unique_IUs
  
  # Create a new column for Biannual MDA coverage
  df_updated$MDA_CDTI_Biannual <- NA  # Only need this once to create an empty column
  
  # Define the year ranges for each biannual group manually
  year_ranges <- list(
    c(2006, 2011),
    c(2007, 2012),
    c(2007, 2013),
    c(2007, 2022),
    c(2011, 2022),
    c(2012, 2022),
    c(2014, 2022)
  )
  
  # Loop through each group of biannual IUs and apply the conditions
  for (i in 1:length(grouped_IUs_biannual_UGA_vec)) {
    condition <- df_updated$Cov.in2 > 0 &
      df_updated$Year > year_ranges[[i]][1] & df_updated$Year < year_ranges[[i]][2] &
      df_updated$ADMIN0ISO3 == "UGA" &
      df_updated$IUs_NAME_MAPPING %in% grouped_IUs_biannual_UGA_vec[[i]]
    
    indices <- which(condition)
    df_updated$MDA_CDTI_Biannual[indices] <- rep(1, length.out = length(indices))
  }
  
  # check number of IUs at this stage:
  cat("Rows in 2022 after processing (biannual MDA in Uganda):", nrow(subset(df_updated, Year == 2022)), "\n")
  cat("Unique IU_ID_MAPPING count after processing (biannual MDA in Uganda):", length(unique(df_updated$IU_ID_MAPPING)), "\n")
  
  # Return the updated dataframe
  return(df_updated)
}


# ================================================================ #
#              Function to update biannual MDA in Ethiopia         #
# ================================================================ #

update_biannual_ethiopia_mda <- function(df, biannual_file_path) {
  
  df_updated <- df
  
  # Load the Biannual MDA data
  Biannual_APOC_IUs <- read.csv(biannual_file_path)
  Biannual_APOC_IUs2 <- Biannual_APOC_IUs[!is.na(Biannual_APOC_IUs$Biannual), ]
  
  # Filter data for Ethiopia
  unique_IUs_ETH <- Biannual_APOC_IUs2 %>%
    filter(ADMIN0ISO3 == "ETH")
  
  # Group Ethiopia IUs by Biannual years
  grouped_IUs_biannual_ETH <- unique_IUs_ETH %>%
    group_by(Biannual) %>%
    summarise(unique_IUs = list(unique(IUs_NAME))) # Group IUs by Biannual years
  
  grouped_IUs_biannual_ETH_vec <- grouped_IUs_biannual_ETH$unique_IUs
  
  # Create a new column for Biannual MDA coverage
  df_updated$MDA_CDTI_Biannual <- NA  # Only need this once to create an empty column
  
  # Define the year ranges for each biannual group
  year_ranges <- list(
    c(2013, 2023),
    c(2014, 2023)
  )
  
  # Loop through each group of biannual IUs and apply the conditions
  for (i in 1:length(grouped_IUs_biannual_ETH_vec)) {
    condition <- df_updated$Cov.in2 > 0 &
      df_updated$Year > year_ranges[[i]][1] & df_updated$Year < year_ranges[[i]][2] &
      df_updated$ADMIN0ISO3 == "ETH" &
      df_updated$IUs_NAME_MAPPING %in% grouped_IUs_biannual_ETH_vec[[i]]
    
    # Extract the IUs for the current biannual group
    years <- as.numeric(year_ranges[[i]])
    IUs <- grouped_IUs_biannual_ETH_vec[[i]] # for label
    
    indices <- which(condition)
    df_updated$MDA_CDTI_Biannual[indices] <- rep(1, length.out = length(indices))
    # Print a label for the biannual IUs (grouped by years and IUs)
    cat("Biannual IUs for years", paste(years, collapse = "-"), ":", paste(IUs, collapse = "; "), "\n")
  }
  
  # check number of IUs at this stage:
  cat("Rows in 2022 after processing (biannual MDA in Ethiopia):", nrow(subset(df_updated, Year == 2022)), "\n")
  cat("Unique IU_ID_MAPPING count after processing (biannual MDA in Ethiopia):", length(unique(df_updated$IU_ID_MAPPING)), "\n")
  
  # Return the updated dataframe
  return(df_updated)
}

# ================================================================ #
#              Function to update biannual MDA in Nigeria          #
# ================================================================ #

update_biannual_nigeria_mda <- function(df, cc_data_file_path) {
  
  df_updated <- df
  
  # Load the CC data for Nigeria (post 2017)
  NGA_post2017 <- read.csv(cc_data_file_path)
  
  # Rename the columns from the CC data
  columns_to_update <- names(NGA_post2017)[6:21]
  new_column_names <- sub("^.", "", columns_to_update)
  names(NGA_post2017)[6:21] <- new_column_names # Rename the columns
  
  # Group the LGAs by biannual years
  NGA_grouped_LGA_biannual <- NGA_post2017 %>%
    group_by(biannual_years) %>%
    summarise(UniqueLGAs = toString(unique(LGA_updated))) # Group LGAs
  
  # Manually define the year ranges for each biannual group
  biannual_years <- list(
    c(2016, 2017, 2018, 2021, 2022),
    c(2017),
    c(2017, 2018),
    c(2017, 2018, 2021, 2022),
    c(2018),
    c(2022)
  )
  
  # Loop through each biannual group to update MDA_CDTI_Biannual
  for (i in 1:nrow(NGA_grouped_LGA_biannual)) {
    # Extract the LGAs for the current biannual group
    LGAs <- unlist(strsplit(as.character(NGA_grouped_LGA_biannual[i, 2]), ",\\s*"))
    
    # Get the year range for the current group
    years <- biannual_years[[i]]
    
    # Create the condition for filtering
    condition <- with(df_updated, 
                      ADMIN0ISO3 == "NGA" & 
                        Year %in% years & 
                        ADMIN2 %in% LGAs)
    
    # Find the indices of the rows that meet the condition
    indices <- which(condition)
    
    # Update the MDA_CDTI_Biannual column for the filtered rows
    df_updated$MDA_CDTI_Biannual[indices] <- rep(1, length.out = length(indices))
    
    # Print a label for the biannual IUs (grouped by LGAs)
    cat("Biannual LGAs for years", paste(years, collapse = "-"), ":", paste(LGAs, collapse = "; "), "\n")
  }
  
  # check number of IUs at this stage:
  cat("Rows in 2022 after processing (biannual MDA in Nigeria):", nrow(subset(df_updated, Year == 2022)), "\n")
  cat("Unique IU_ID_MAPPING count after processing (biannual MDA in Nigeria):", length(unique(df_updated$IU_ID_MAPPING)), "\n")
  
  # Return the updated dataframe
  return(df_updated)
}

# ================================================================ #
#              Function to update biannual MDA in Sudan            #
# ================================================================ #

update_biannual_mda_sudan <- function(df, biannual_data_file_path) {
  
  df_updated <- df
  
  # Load the Biannual APOC IUs data for Sudan
  Biannual_APOC_IUs <- read.csv(biannual_data_file_path)
  Biannual_APOC_IUs2 <- Biannual_APOC_IUs[!is.na(Biannual_APOC_IUs$Biannual), ]
  
  # Filter and group unique IUs by their biannual years
  unique_IUs_SDN <- Biannual_APOC_IUs2 %>%
    filter(ADMIN0ISO3 == "SDN")
  
  grouped_IUs_biannual_SDN <- unique_IUs_SDN %>%
    group_by(Biannual) %>%
    summarise(unique_IUs = list(unique(IUs_NAME))) # Find grouping of unique IUs with same biannual years
  
  # Extract the vectors of grouped IUs for each biannual group
  grouped_IUs_biannual_SDN_vec <- grouped_IUs_biannual_SDN$unique_IUs
  
  # Manually define the year ranges for each biannual group in Sudan
  biannual_years <- list(
    c(2006, 2012),    # First group of IUs
    c(2009, 2017),    # Second group of IUs
    c(2021, 2022)     # Third group of IUs
  )
  
  # Loop through each biannual group and update MDA_CDTI_Biannual for the corresponding IUs
  for (i in 1:nrow(grouped_IUs_biannual_SDN)) {
    # Extract the IUs for the current biannual group
    IUs <- unlist(strsplit(as.character(grouped_IUs_biannual_SDN[i, 2]), ",\\s*"))
    
    # Get the year range for the current biannual group
    years <- biannual_years[[i]]
    
    # Create the condition for filtering based on the biannual group
    condition <- with(df_updated, 
                      ADMIN0ISO3 == "SDN" & 
                        Year %in% years & 
                        IUs_NAME_MAPPING %in% IUs)
    
    # Find the indices of the rows that meet the condition
    indices <- which(condition)
    
    # Update the MDA_CDTI_Biannual column for the filtered rows
    df_updated$MDA_CDTI_Biannual[indices] <- rep(1, length.out = length(indices))
    
    cat("Biannual IUs in Sudan:\n", paste(years, collapse = "-"), paste(unlist(grouped_IUs_biannual_SDN_vec[[1]]), collapse = "; "), "\n")
  }
  
  # check number of IUs at this stage:
  cat("Rows in 2022 after processing (biannual MDA in Sudan):", nrow(subset(df_updated, Year == 2022)), "\n")
  cat("Unique IU_ID_MAPPING count after processing (biannual MDA in Sudan):", length(unique(df_updated$IU_ID_MAPPING)), "\n")
  
  # Return the updated dataframe
  return(df_updated)
}

# ================================================================ #
#              Function to update biannual MDA in Tanzania         #
# ================================================================ #

update_biannual_mda_tanzania <- function(df, biannual_file_path) {
  
  df_updated <- df
  
  # Read in the Biannual data
  Biannual_APOC_IUs <- read.csv(biannual_file_path)
  Biannual_APOC_IUs2 <- Biannual_APOC_IUs[!is.na(Biannual_APOC_IUs$Biannual), ]
  
  # Filter for unique IUs in Tanzania (TZA)
  unique_IUs_TZA <- Biannual_APOC_IUs2 %>%
    filter(ADMIN0ISO3 == "TZA")
  
  # Group by Biannual years
  grouped_IUs_biannual_TZA <- unique_IUs_TZA %>%
    group_by(Biannual) %>%
    summarise(unique_IUs = list(unique(IUs_NAME)))  # Group unique IUs by Biannual
  
  # Get the vector of IUs
  grouped_IUs_biannual_TZA_vec <- grouped_IUs_biannual_TZA$unique_IUs
  
  # Loop through the groups and update MDA_CDTI_Biannual for each group
  for (i in 1:length(grouped_IUs_biannual_TZA_vec)) {
    
    # Apply conditions for each biannual group
    condition <- df_updated$Cov.in2 > 0 &
      df_updated$Year > 2018 &
      df_updated$ADMIN0ISO3 == "TZA" &
      df_updated$IUs_NAME_MAPPING %in% grouped_IUs_biannual_TZA_vec[[i]]
    
    indices <- which(condition)
    df_updated$MDA_CDTI_Biannual[indices] <- rep(1, length.out = length(indices))
    
    # Print the label with the years and IU names
    cat("Biannual IUs for years", paste("> 2018"), ":", 
        paste(grouped_IUs_biannual_TZA_vec[[i]], collapse = "; "), "\n")
  }
  
  # check number of IUs at this stage:
  cat("Rows in 2022 after processing (biannual MDA in Tanzania):", nrow(subset(df_updated, Year == 2022)), "\n")
  cat("Unique IU_ID_MAPPING count after processing (biannual MDA in Tanzania):", length(unique(df_updated$IU_ID_MAPPING)), "\n")
  
  # Return the updated dataframe
  return(df_updated)
}

# ================================================================ #
#              Function to update biannual MDA in South Sudan      #
# ================================================================ #

update_biannual_mda_southsudan <- function(df, biannual_file_path) {
  
  df_updated <- df
  
  # Read in the Biannual data
  Biannual_APOC_IUs <- read.csv(biannual_file_path)
  Biannual_APOC_IUs2 <- Biannual_APOC_IUs[!is.na(Biannual_APOC_IUs$Biannual), ]
  
  # Filter for unique IUs in South Sudan (SSD)
  unique_IUs_SSD <- Biannual_APOC_IUs2 %>%
    filter(ADMIN0ISO3 == "SSD")
  
  # Group by Biannual years
  grouped_IUs_biannual_SSD <- unique_IUs_SSD %>%
    group_by(Biannual) %>%
    summarise(unique_IUs = list(unique(IUs_NAME)))  # Group unique IUs by Biannual
  
  # Get the vector of IUs
  grouped_IUs_biannual_SSD_vec <- grouped_IUs_biannual_SSD$unique_IUs
  
  # Loop through the groups and update MDA_CDTI_Biannual for each group
  for (i in 1:length(grouped_IUs_biannual_SSD_vec)) {
    
    # Apply conditions for each biannual group
    condition <- df_updated$Cov.in2 > 0 &
      df_updated$Year > 2020 &
      df_updated$ADMIN0ISO3 == "SSD" &
      df_updated$IUs_NAME_MAPPING %in% grouped_IUs_biannual_SSD_vec[[i]]
    
    indices <- which(condition)
    df_updated$MDA_CDTI_Biannual[indices] <- rep(1, length.out = length(indices))
    
    # Print the label with the years and IU names
    cat("Biannual IUs for years", paste("> 2020"), ":", 
        paste(grouped_IUs_biannual_SSD_vec[[i]], collapse = "; "), "\n")
  }
  
  # check number of IUs at this stage:
  cat("Rows in 2022 after processing (biannual MDA in South Sudan):", nrow(subset(df_updated, Year == 2022)), "\n")
  cat("Unique IU_ID_MAPPING count after processing (biannual MDA in South Sudan):", length(unique(df_updated$IU_ID_MAPPING)), "\n")
  
  # Return the updated dataframe
  return(df_updated)
}

# ================================================================ #
#           Function to update vector control inn Uganda           #
# ================================================================ #

update_vector_control_UGA <- function(df, biannual_file_path) {
  
  df_updated <- df
  
  # Read in the Biannual data
  Biannual_APOC_IUs <- read.csv(biannual_file_path)
  Biannual_APOC_IUs2 <- Biannual_APOC_IUs[!is.na(Biannual_APOC_IUs$Biannual), ]
  
  # Filter for unique IUs in Uganda (UGA)
  unique_IUs_UGA <- Biannual_APOC_IUs2 %>%
    filter(ADMIN0ISO3 == "UGA")
  
  # Group by vector control
  grouped_IUs_vectorcntrl_UGA <- unique_IUs_UGA %>%
    group_by(vector_control) %>%
    summarise(unique_IUs = list(unique(IUs_NAME)))  # Group unique IUs by vector control
  
  # Get the vector of IUs
  grouped_IUs_vectorcntrl_UGA_vec <- grouped_IUs_vectorcntrl_UGA$unique_IUs
  
  df_updated$vector_control <- NA # only need this once to create empty col
  
  # Update vector control for each foci manually specifying years
  
  # Foci 1: Mpamba-Nkusi; 2003-2007 VC and vector elimination after 2007
  years_foci_1 <- 2003:2007
  IUs_foci_1 <- grouped_IUs_vectorcntrl_UGA_vec[[2]]
  cat("Foci 1 - Mpamba-Nkusi IUs:", paste(IUs_foci_1, collapse = "; "), "Years:", paste(years_foci_1, collapse = "-"), "\n")
  condition <- df_updated$Year %in% years_foci_1 &
    df_updated$ADMIN0ISO3 == "UGA" &
    df_updated$IUs_NAME_MAPPING %in% IUs_foci_1 # Mpamba-Nkusi foci (Kibaale) VC
  indices <- which(condition)
  df_updated$vector_control[indices] <- rep(1, length.out = length(indices))
  
  condition <- df_updated$Year > 2007 & df_updated$Year < 2023 &
    df_updated$ADMIN0ISO3 == "UGA" &
    df_updated$IUs_NAME_MAPPING %in% IUs_foci_1 # Mpamba-Nkusi foci (Kibaale) VE
  indices <- which(condition)
  df_updated$vector_control[indices] <- rep(2, length.out = length(indices))
  
  # Foci 2: Mt Elgon; 2006-2009 VC and vector elimination after 2009
  years_foci_2 <- 2006:2009
  IUs_foci_2 <- grouped_IUs_vectorcntrl_UGA_vec[[3]]
  cat("Foci 2 - Mt Elgon IUs:", paste(IUs_foci_2, collapse = "; "), "Years:", paste(years_foci_2, collapse = "-"), "\n")
  condition <- df_updated$Year %in% years_foci_2 &
    df_updated$ADMIN0ISO3 == "UGA" &
    df_updated$IUs_NAME_MAPPING %in% IUs_foci_2 # Mt Elgon foci (Mbale etc) VC
  indices <- which(condition)
  df_updated$vector_control[indices] <- rep(1, length.out = length(indices))
  
  condition <- df_updated$Year > 2009 & df_updated$Year < 2023 &
    df_updated$ADMIN0ISO3 == "UGA" &
    df_updated$IUs_NAME_MAPPING %in% IUs_foci_2 # Mt Elgon foci (Mbale etc) VE
  indices <- which(condition)
  df_updated$vector_control[indices] <- rep(2, length.out = length(indices))
  
  # Foci 3: Kashoya-Kitomi & Wambabya-Rwamarongo; 2008-2010 VC and vector elimination after 2010
  years_foci_3 <- 2008:2010
  IUs_foci_3 <- grouped_IUs_vectorcntrl_UGA_vec[[4]]
  cat("Foci 3 - Kashoya-Kitomi & Wambabya-Rwamarongo IUs:", paste(IUs_foci_3, collapse = "; "), "Years:", paste(years_foci_3, collapse = "-"), "\n")
  condition <- df_updated$Year %in% years_foci_3 &
    df_updated$ADMIN0ISO3 == "UGA" &
    df_updated$IUs_NAME_MAPPING %in% IUs_foci_3 # Kashoya-Kitomi foci (Ibanda etc) & Wambabya-Rwamarongo (Hoima)
  indices <- which(condition)
  df_updated$vector_control[indices] <- rep(1, length.out = length(indices))
  
  condition <- df_updated$Year > 2010 & df_updated$Year < 2023 &
    df_updated$ADMIN0ISO3 == "UGA" &
    df_updated$IUs_NAME_MAPPING %in% IUs_foci_3 # Kashoya-Kitomi foci (Ibanda etc) & Wambabya-Rwamarongo (Hoima)
  indices <- which(condition)
  df_updated$vector_control[indices] <- rep(2, length.out = length(indices))
  
  # Foci 4: Nyagak-Bondo; 2012-2013 VC and vector elimination after 2013
  years_foci_4 <- 2012:2013
  IUs_foci_4 <- grouped_IUs_vectorcntrl_UGA_vec[[5]]
  cat("Foci 4 - Nyagak-Bondo IUs:", paste(IUs_foci_4, collapse = "; "), "Years:", paste(years_foci_4, collapse = "-"), "\n")
  condition <- df_updated$Year %in% years_foci_4 &
    df_updated$ADMIN0ISO3 == "UGA" &
    df_updated$IUs_NAME_MAPPING %in% IUs_foci_4 # Nyagak-Bondo foci (Zombo etc)
  indices <- which(condition)
  df_updated$vector_control[indices] <- rep(1, length.out = length(indices))
  
  condition <- df_updated$Year > 2013 & df_updated$Year < 2023 &
    df_updated$ADMIN0ISO3 == "UGA" &
    df_updated$IUs_NAME_MAPPING %in% IUs_foci_4 # Nyagak-Bondo foci (Zombo etc)
  indices <- which(condition)
  df_updated$vector_control[indices] <- rep(2, length.out = length(indices))
  
  # Foci 5: Budongo; 2012-2014 VC and vector elimination after 2014
  years_foci_5 <- 2012:2014
  IUs_foci_5 <- grouped_IUs_vectorcntrl_UGA_vec[[6]]
  cat("Foci 5 - Budongo IUs:", paste(IUs_foci_5, collapse = "; "), "Years:", paste(years_foci_5, collapse = "-"), "\n")
  condition <- df_updated$Year %in% years_foci_5 &
    df_updated$ADMIN0ISO3 == "UGA" &
    df_updated$IUs_NAME_MAPPING %in% IUs_foci_5 # Budongo foci (Masindi etc)
  indices <- which(condition)
  df_updated$vector_control[indices] <- rep(1, length.out = length(indices))
  
  condition <- df_updated$Year > 2014 & df_updated$Year < 2023 &
    df_updated$ADMIN0ISO3 == "UGA" &
    df_updated$IUs_NAME_MAPPING %in% IUs_foci_5 # Budongo foci (Masindi etc)
  indices <- which(condition)
  df_updated$vector_control[indices] <- rep(2, length.out = length(indices))
  
  # Foci 6: Madi Mid North; 2012-current VC and NO vector elimination
  years_foci_6 <- 2012:2022
  IUs_foci_6 <- grouped_IUs_vectorcntrl_UGA_vec[[7]]
  cat("Foci 6 - Madi Mid North IUs:", paste(IUs_foci_6, collapse = "; "), "Years:", paste(years_foci_6, collapse = "-"), "\n")
  condition <- df_updated$Year %in% years_foci_6 &
    df_updated$ADMIN0ISO3 == "UGA" &
    df_updated$IUs_NAME_MAPPING %in% IUs_foci_6 # Madi Mid North foci (Gulu etc)
  indices <- which(condition)
  df_updated$vector_control[indices] <- rep(1, length.out = length(indices))
  
  # Foci 7: Lhubirha; 2015-current VC and NO vector elimination
  years_foci_7 <- 2015:2022
  IUs_foci_7 <- grouped_IUs_vectorcntrl_UGA_vec[[8]]
  cat("Foci 7 - Lhubirha IUs:", paste(IUs_foci_7, collapse = "; "), "Years:", paste(years_foci_7, collapse = "-"), "\n")
  condition <- df_updated$Year %in% years_foci_7 &
    df_updated$ADMIN0ISO3 == "UGA" &
    df_updated$IUs_NAME_MAPPING %in% IUs_foci_7 # Lhubirha foci (Kasese; not also Nyamugasani foci here)
  indices <- which(condition)
  df_updated$vector_control[indices] <- rep(1, length.out = length(indices))
  
  # Foci 8: Itwara; 1993 - 2003 VC and vector elimination after 2003
  years_foci_8 <- 1993:2003
  IUs_foci_8 <- c("Bunyangabu", "KABAROLE", "KYENJOJO")  # Itwara foci (Kabarole etc)
  cat("Foci 8 - Itwara IUs:", paste(IUs_foci_8, collapse = "; "), "Years:", paste(years_foci_8, collapse = "-"), "\n")
  condition <- df_updated$Year %in% years_foci_8 &
    df_updated$ADMIN0ISO3 == "UGA" &
    df_updated$IUs_NAME_MAPPING %in% IUs_foci_8 # Itwara foci (Kabarole etc)
  indices <- which(condition)
  df_updated$vector_control[indices] <- rep(1, length.out = length(indices))
  
  condition <- df_updated$Year > 2003 & df_updated$Year < 2023 &
    df_updated$ADMIN0ISO3 == "UGA" &
    df_updated$IUs_NAME_MAPPING %in% IUs_foci_8 # Itwara foci (Kabarole etc)
  indices <- which(condition)
  df_updated$vector_control[indices] <- rep(2, length.out = length(indices))
  
  # check number of IUs at this stage:
  cat("Rows in 2022 after processing (vector control in Uganda):", nrow(subset(df_updated, Year == 2022)), "\n")
  cat("Unique IU_ID_MAPPING count after processing (vector control in Uganda):", length(unique(df_updated$IU_ID_MAPPING)), "\n")
  
  
  return(df_updated)
}

# ================================================================ #
#     Function to update vector control in Eq. Guinea              #
# ================================================================ #

update_vector_control_mda_GNQ <- function(df, biannual_file_path) {
  
  df_updated <- df
  
  # Read in the Biannual data
  Biannual_APOC_IUs <- read.csv(biannual_file_path)
  Biannual_APOC_IUs2 <- Biannual_APOC_IUs[!is.na(Biannual_APOC_IUs$Biannual), ]
  
  # Vector Control (VC) from 2001-2005
  # Vector control (ABR 80% proportional reduction = 1)
  condition <- df_updated$Year > 2000 & df_updated$Year < 2006 &
    df_updated$ADMIN0ISO3 == "GNQ" &
    df_updated$ADMIN1 %in% c("Bioko Norte", "Bioko Sur")
  indices <- which(condition)
  if (length(indices) > 0) {
    df_updated$vector_control[indices] <- 1
    cat("Vector Control for Bioko Norte / Bioko Sur (2001-2005):\n", 
        paste(df_updated$IUs_NAME_MAPPING[indices], collapse = "; "), 
        "\nYears:", paste(min(df_updated$Year[indices]), max(df_updated$Year[indices]), sep = "-"), "\n")
  }
  
  # Vector Elimination (ABR to 0 = 2) from 2006-2022
  condition <- df_updated$Year > 2005 & df_updated$Year < 2023 &
    df_updated$ADMIN0ISO3 == "GNQ" &
    df_updated$ADMIN1 %in% c("Bioko Norte", "Bioko Sur")
  indices <- which(condition)
  if (length(indices) > 0) {
    df_updated$vector_control[indices] <- 2
    cat("Vector Elimination for Bioko Norte / Bioko Sur (2006-2022):\n", 
        paste(df_updated$IUs_NAME_MAPPING[indices], collapse = "; "), 
        "\nYears:", paste(min(df_updated$Year[indices]), max(df_updated$Year[indices]), sep = "-"), "\n")
  }
  
  # Bioko MDA information from 1989 to 1998 (low coverage = 25%)
  condition <- df_updated$Year > 1988 & df_updated$Year < 1999 &
    df_updated$ADMIN0ISO3 == "GNQ" &
    df_updated$IUs_NAME_MAPPING %in% c("Malabo", "Baney", "Luba", "Riaba")
  indices <- which(condition)
  if (length(indices) > 0) {
    df_updated$MDA_CDTI[indices] <- 1
    df_updated$Cov.in2[indices] <- 0.25
    df_updated$cov_source[indices] <- rep("Subnational coverage data (literature)", length.out = length(indices))
    df_updated$cov_specific_source[indices] <- rep("Hernández González et al. 2016", length.out = length(indices))
    cat("Bioko MDA (1989-1998):\n", 
        paste(df_updated$IUs_NAME_MAPPING[indices], collapse = "; "), 
        "\nYears:", paste(min(df_updated$Year[indices]), max(df_updated$Year[indices]), sep = "-"), "\n")
  }
  
  # Extend MDA from 2011-2016 for 3 IUs (Baney, Luba, Riaba)
  condition <- df_updated$Year > 2010 & df_updated$Year < 2017 &
    df_updated$ADMIN0ISO3 == "GNQ" &
    df_updated$IUs_NAME_MAPPING %in% c("Baney", "Luba", "Riaba")
  indices <- which(condition)
  if (length(indices) > 0) {
    df_updated$MDA_CDTI[indices] <- 1
    df_updated$Cov.in2[indices] <- 0.25
    df_updated$cov_source[indices] <- rep("Subnational coverage data (literature)", length.out = length(indices))
    df_updated$cov_specific_source[indices] <- rep("Hernández González et al. 2016", length.out = length(indices))
    cat("Bioko MDA (2011-2016):\n", 
        paste(df_updated$IUs_NAME_MAPPING[indices], collapse = "; "), 
        "\nYears:", paste(min(df_updated$Year[indices]), max(df_updated$Year[indices]), sep = "-"), "\n")
  }
  
  # Update MDA for Malabo from 2010-2012
  condition <- df_updated$Year > 2010 & df_updated$Year < 2013 &
    df_updated$ADMIN0ISO3 == "GNQ" &
    df_updated$IUs_NAME_MAPPING %in% c("Malabo")
  indices <- which(condition)
  if (length(indices) > 0) {
    df_updated$MDA_CDTI[indices] <- 1
    df_updated$Cov.in2[indices] <- 0.25
    df_updated$cov_source[indices] <- rep("Subnational coverage data (literature)", length.out = length(indices))
    df_updated$cov_specific_source[indices] <- rep("Hernández González et al. 2016", length.out = length(indices))
    cat("Malabo MDA (2010-2012):\n", 
        paste(df_updated$IUs_NAME_MAPPING[indices], collapse = "; "), 
        "\nYears:", paste(min(df_updated$Year[indices]), max(df_updated$Year[indices]), sep = "-"), "\n")
  }
  
  # check number of IUs at this stage:
  cat("Rows in 2022 after processing (vector control in Eq. Guinea):", nrow(subset(df_updated, Year == 2022)), "\n")
  cat("Unique IU_ID_MAPPING count after processing (vector control in Eq. Guinea):", length(unique(df_updated$IU_ID_MAPPING)), "\n")
  
  # Returning the updated dataframe
  return(df_updated)
}


# =============================================================================== #
#         Function to create intervention label column (biannual_VC_mapping)      #
# =============================================================================== #

create_intervention_mapping_variable <- function(df) {
  
  df_updated <- df
  
  # Group by IU_ID_MAPPING and calculate biannual_included (if MDA_CDTI_Biannual == 1 exists)
  df_updated <- df_updated %>%
    group_by(IU_ID_MAPPING) %>%
    mutate(biannual_included = any(MDA_CDTI_Biannual == 1))
  
  # Group by IU_ID_MAPPING and calculate VC_included (if vector_control == 1 exists)
  df_updated <- df_updated %>%
    group_by(IU_ID_MAPPING) %>%
    mutate(VC_included = any(vector_control == 1))
  
  # Group by IU_ID_MAPPING and calculate annual_only_included (if biannual_included is NA and MDA_CDTI == 1 exists)
  df_updated <- df_updated %>%
    group_by(IU_ID_MAPPING) %>%
    mutate(annual_only_included = all(is.na(biannual_included)) & any(MDA_CDTI == 1))
  
  # Group by IU_ID_MAPPING and calculate biannual_VC_mapping using case_when
  df_updated <- df_updated %>%
    group_by(IU_ID_MAPPING) %>%
    mutate(
      biannual_VC_mapping = case_when(
        all(biannual_included) & (all(is.na(annual_only_included)) | all(annual_only_included == FALSE)) & all(is.na(VC_included | VC_included == FALSE)) ~ "biannual",
        all(biannual_included) & (all(is.na(annual_only_included)) | all(annual_only_included == FALSE)) & all(VC_included) ~ "biannual & vector control",
        all(is.na(biannual_included)) & all(annual_only_included) & (all(is.na(VC_included | VC_included == FALSE)) | all(VC_included == FALSE)) ~ "annual only",
        all(is.na(biannual_included)) & all(annual_only_included) & all(VC_included) ~ "annual & vector control",
        TRUE ~ "treatment naive"
      )
    )
  
  # Print frequency of each label in biannual_VC_mapping
  cat("Frequency of each biannual_VC_mapping label:\n")
  freq_table <- table(subset(df_updated, Year == 2022)$biannual_VC_mapping)
  print(freq_table)
  
  cat("Sum across biannual_VC_mapping label:\n")
  print(sum(freq_table))
  
  # check number of IUs at this stage:
  cat("Rows in 2022 after creating biannual_VC_mapping column:", nrow(subset(df_updated, Year == 2022)), "\n")
  cat("Unique IU_ID_MAPPING count after creating biannual_VC_mapping column:", length(unique(df_updated$IU_ID_MAPPING)), "\n")
  
  return(df_updated)
}

# ===================================================================== #
#            Function to remove IUs without baseline mapped samples     #
# ===================================================================== #

remove_non_endemic_IUs <- function(df, baseline_file_path) {
  
  # store pre-processed number of IUs
  pre_processed_nIUs <- length(unique(df$IU_ID_MAPPING))
  
  # Load the "no_baseline_IUs" Rdata file
  load(baseline_file_path)
  
  # Filter non-endemic IUs and select required columns
  non_endemic_IUs <- df %>%
    filter(endemicity_baseline == "non-endemic") %>%
    select(IUs_NAME_MAPPING, Endemicity, MAX_Endemicity, MAX_Endemicity_Parent, endemicity_baseline, mean) %>%
    distinct()
  
  # Get the list of IUs to remove
  non_endemic_IUs_baseline_vec <- non_endemic_IUs$IUs_NAME_MAPPING
  
  # Remove non-endemic IUs from the dataframe
  df_updated <- df %>%
    filter(!(IUs_NAME_MAPPING %in% non_endemic_IUs_baseline_vec))
  
  # Check the number of IUs remaining in 2022
  cat("Number of IUs  2022 before processing: ", pre_processed_nIUs, "\n")
  
  cat("Number of IUs remaining (after removing those without baseline mapped samples): ", nrow(subset(df_updated, Year == 2022)), "\n")
  
  cat("Number of IUs  2022 removed b/c no baseline mapped samples: ", pre_processed_nIUs - nrow(subset(df_updated, Year == 2022)), "\n")
  
  
  # Return the updated dataframe
  return(df_updated)
}

# ======================================================================= #
#          Function to augment (extend) each Iu from 2022 - 2025          #
# ======================================================================= #

augment_mda_to2025_func <- function(df, FinalESPENyr = 2022, IUend = 2025) {
  
  # Make a copy of the dataframe
  df_updated <- df
  
  # Split the data by IU_ID_MAPPING
  dfls <- split(df_updated, df_updated$IU_ID_MAPPING)
  
  # Initialize the list to store new dataframes
  newdf <- vector("list", length(dfls))
  
  # Loop over each split dataframe (grouped by IU_ID_MAPPING)
  for (i in 1:length(dfls)) {
    
    newdf[[i]] <- subset(dfls[[i]], select = colnames(df_updated))
    
    # Assigning the last year to IUend (2025 in this case)
    newdf[[i]]$IUend <- IUend
    
    # Calculate the number of future years to include
    Futureyrs <- IUend - FinalESPENyr
    
    # Assign the index of the list to the IU data frame
    newdf[[i]]$lsID <- i
    
    # Check if IUend is not NA
    if (!is.na(IUend)) {
      # If the IUend year is greater than the FinalESPEN year (i.e., there are missing years after FinalESPENyr)
      if (IUend > FinalESPENyr) {
        
        # Repeat the first row to create new rows for the missing years
        tmp <- newdf[[i]][rep(1, each = (min(IUend - newdf[[i]]$Year))), ]
        
        # Fill the new rows with the correct years starting after FinalESPENyr
        tmp$Year <- seq(FinalESPENyr + 1, IUend)
        
        # Set the endemicity and MDA scheme for the augmented rows
        tmp$Endemicity <- "Augmented"
        tmp$MDA_scheme <- "Augmented"
        
        # Append the augmented rows to the original dataframe
        newdf[[i]] <- rbind(newdf[[i]], tmp)  # Add the augmented rows at the end
      }
    }
  }
  
  # Combine the list of augmented dataframes back into a single dataframe
  df_updated <- do.call(rbind, newdf)
  
  # Check the result:
  cat("Rows in 2022 after augmenting MDA data (adding 2022-2025 rows):", nrow(subset(df_updated, Year == 2022)), "\n")
  cat("Unique IU_ID_MAPPING count after augmenting MDA data (adding 2022-2025 rows):", length(unique(df_updated$IU_ID_MAPPING)), "\n")
  
  # Return the updated dataframe
  return(df_updated)
}


# ============================================= #
#    Function to update single IU (IU 49752)    # 
# ============================================= #

update_iu_mda_data <- function(df, IU_ID, coverage_input, year_range = c(1999, 2012)) {
  
  # Create a copy of the input dataframe to avoid modifying the original
  df_updated <- df
  
  # Define the condition to select the rows for the specific IU and year range
  condition <- with(df_updated, IU_ID_MAPPING == 49752 & trimws(Pre_ESPEN_MDA_history) == "Include" & Year >= year_range[1] & Year <= year_range[2])
  
  # Find the indices that meet the condition
  indices <- which(condition)
  
  # Define the replacement MDA_CDTI values (assuming 1 for each year)
  replace_MDA_CDTI <- rep(1, length(indices))
  
  # Update MDA_CDTI values
  df_updated$MDA_CDTI[indices] <- replace_MDA_CDTI
  
  # Set Cov.in2 values (coverage data, passed as parameter)
  df_updated$Cov.in2[indices] <- rep(coverage_input, length.out = length(indices))
  
  # For years covered by APOC, 2015 report, set MDA_CDTI_raw and source columns
  df_updated$MDA_CDTI_raw[indices] <- rep(coverage_input, length.out = length(indices))
  df_updated$cov_source[indices] <- rep("national-level coverage data", length.out = length(indices))
  df_updated$cov_specific_source[indices] <- rep("APOC report, 2015", length.out = length(indices))
  
  # Check the updated dataframe
  cat("Rows updated for IU 49752:", range(indices), "\n")
  
  # Return the updated dataframe
  return(df_updated)
}

# ============================================================================= #
# Function to create trt_status_2022 variable and print frequency of categories #
# ============================================================================= #

create_trt_status_col <- function(df) {
  
  # Update trt_status_2022
  df_updated <- df %>%
    group_by(IU_ID_MAPPING) %>%
    mutate(
      trt_status_2022 = case_when(
        all(
          any(Endemicity %in% c("Endemic (MDA not delivered)", "Endemic (under MDA)") & Year %in% c(2021, 2022) & !all(is.na(Cov.in2)))
        ) ~ "MDA continues",
        all(
          any(MAX_Endemicity %in% c("Unknown (under LF MDA)", "Unknown (consider Oncho Elimination Mapping)", "Not reported")) &
            any(EpiCov > 0) &
            any(Cum_MDA > 0)
        ) ~ "MDA continues",
        any(Endemicity == "Unknown (under LF MDA)" & Year %in% c(2021:2022)) & any(Cov.in2 > 0 & Year %in% c(2013:2022)) ~ "MDA stopped",
        all(any(MAX_Endemicity %in% c("Unknown (under LF MDA)", "Unknown (consider Oncho Elimination Mapping)", "Not reported")) &
              all(EpiCov == 0) &
              any(Cum_MDA > 0) &
              !all(is.na(Cov.in2))
        ) ~ "MDA stopped",
        any(Endemicity == "Endemic (under post-intervention surveillance)" & Year %in% c(2021, 2022) & !all(is.na(Cov.in2))) ~ "MDA stopped",
        any(Endemicity == "Non-endemic" & Year %in% c(2021, 2022) & !all(is.na(Cov.in2))) ~ "MDA stopped",
        all(Endemicity %in% c("Non-endemic", "Not reported")) & length(unique(Endemicity)) == 2 ~ "MDA stopped", # check
        all(ADMIN0ISO3 == "SDN" & !all(is.na(Cov.in2)) & IUs_NAME_MAPPING != "El Radoom") ~ "MDA stopped",
        all(ADMIN0ISO3 == "SDN" & IUs_NAME_MAPPING == "El Radoom") ~ "MDA continues",
        all(is.na(Cov.in2)) ~ "Treatment naive",
        TRUE ~ NA
      )
    )
  
  # Update trt_status_2022_v2
  df_updated <- df_updated %>%
    group_by(IU_ID_MAPPING) %>%
    mutate(
      trt_status_2022_v2 = case_when(
        all(
          any(Endemicity %in% c("Endemic (MDA not delivered)", "Endemic (under MDA)") & Year %in% c(2021, 2022) & !all(is.na(Cov.in2)))
        ) ~ "MDA continues",
        all(
          any(MAX_Endemicity %in% c("Unknown (under LF MDA)", "Unknown (consider Oncho Elimination Mapping)", "Not reported")) &
            any(EpiCov > 0) &
            any(Cum_MDA > 0)
        ) ~ "MDA continues",
        any(Endemicity == "Unknown (under LF MDA)" & Year %in% c(2021:2022)) & any(Cov.in2 > 0 & Year %in% c(2013:2022)) ~ "MDA stopped: no MDA in 2021/2022 ESPEN",
        all(any(MAX_Endemicity %in% c("Unknown (under LF MDA)", "Unknown (consider Oncho Elimination Mapping)", "Not reported")) &
              all(EpiCov == 0) &
              any(Cum_MDA > 0) &
              !all(is.na(Cov.in2))
        ) ~ "MDA stopped: no MDA in ESPEN years",
        any(Endemicity == "Endemic (under post-intervention surveillance)" & Year %in% c(2021, 2022) & !all(is.na(Cov.in2))) ~ "MDA stopped: under PIS (ESPEN)",
        any(Endemicity == "Non-endemic" & Year %in% c(2021, 2022) & !all(is.na(Cov.in2))) ~ "MDA stopped: non-endemic in 2021/2022 ESPEN",
        all(Endemicity %in% c("Non-endemic", "Not reported")) & length(unique(Endemicity)) == 2 ~ "MDA stopped: non/endemic & not reported in ESPEN", # check
        all(ADMIN0ISO3 == "SDN" & !all(is.na(Cov.in2)) & IUs_NAME_MAPPING != "El Radoom") ~ "MDA stopped: under PIS (other sources) or eliminated",
        all(ADMIN0ISO3 == "SDN" & IUs_NAME_MAPPING == "El Radoom") ~ "MDA continues: remove SDN",
        all(ADMIN0ISO3 == "SEN") ~ "MDA stopped: under PIS (other sources) or eliminated",
        all(is.na(Cov.in2)) ~ "Treatment naive",
        TRUE ~ trt_status_2022
      )
    )
  
  # Print the frequency of each category for trt_status_2022 and trt_status_2022_v2 for Year 2022
  cat("Frequency of trt_status_2022 in 2022:\n")
  status_2022_table <- table(subset(df_updated, Year == 2022)$trt_status_2022)
  for (category in names(status_2022_table)) {
    cat(category, ":", status_2022_table[category], "\n")
  }
  
  cat("\nFrequency of trt_status_2022_v2 in 2022:\n")
  status_2022_v2_table <- table(subset(df_updated, Year == 2022)$trt_status_2022_v2)
  for (category in names(status_2022_v2_table)) {
    cat(category, ":", status_2022_v2_table[category], "\n")
  }
  
  # Check the result:
  cat("Rows in 2022 after creating trt_status_2022 variable:", nrow(subset(df_updated, Year == 2022)), "\n")
  cat("Unique IU_ID_MAPPING count after creating trt_status_2022 variable:", length(unique(df_updated$IU_ID_MAPPING)), "\n")
  
  
  # Return the updated dataframe
  return(df_updated)
}


# ==================================================================== #
#             Function for updating MDA in 2023 - 2025 period          #
# ==================================================================== #

update_mda_status_to2025 <- function(df) {
  
  # Create a copy of the original dataframe
  df_updated <- df
  
  # make Cov.in2 a numeric column
  df_updated$Cov.in2 <- as.numeric(df_updated$Cov.in2)
  
  # Mutate to update MDA_CDTI, Cov.in2, and cov_source for years 2023-2025 where MDA continues
  df_updated <- df_updated %>%
    mutate(
      MDA_CDTI = if_else(
        trt_status_2022 == "MDA continues" & Year %in% c(2023:2025),
        1,
        MDA_CDTI
      ),
      Cov.in2 = if_else(
        trt_status_2022 == "MDA continues" & Year %in% c(2023:2025),
        0.65,
        Cov.in2
      ),
      cov_source = if_else(
        trt_status_2022 == "MDA continues" & Year %in% c(2023:2025),
        "assumed (projected)",
        cov_source
      )
    )
  
  # Check if MDA stopped
  df_updated_check_MDAstopped <- subset(df_updated, trt_status_2022 == "MDA stopped")
  cat("Unique IUs with MDA stopped:", length(unique(df_updated_check_MDAstopped$IU_ID_MAPPING)), "\n")
  df_updated_check_MDAstopped <- subset(df_updated_check_MDAstopped, Year %in% c(2023:2025))
  
  # Check if MDA continues
  df_updated_check_MDAcont <- subset(df_updated, trt_status_2022 == "MDA continues")
  cat("Unique IUs with MDA continues:", length(unique(df_updated_check_MDAcont$IU_ID_MAPPING)), "\n")
  df_updated_check_MDAcont <- subset(df_updated_check_MDAcont, Year %in% c(2023:2025))
  
  # Check if treatment naive
  df_updated_check_trtnaive <- subset(df_updated, trt_status_2022 == "Treatment naive")
  cat("Unique IUs with Treatment naive:", length(unique(df_updated_check_trtnaive$IU_ID_MAPPING)), "\n")
  
  # Check number of IUs:
  cat("Rows in 2022 after updated MDA in 2023-2025:", nrow(subset(df_updated, Year == 2022)), "\n")
  cat("Unique IU_ID_MAPPING count after updated MDA in 2023-2025:", length(unique(df_updated$IU_ID_MAPPING)), "\n")
  
  
  # Return the updated dataframe
  return(df_updated)
}

# ========================================================================== #
#       Function to update biannual MDA and vector control in 2023 - 2025    #
# ========================================================================== #

update_biannual_and_vector_control_to2025 <- function(df) {
  # Create a copy of the original dataframe
  df_updated <- df
  
  # Filter IUs where MDA was biannual in 2020-2022 and continues in 2023-2025
  filtered_ids <- df_updated %>%
    group_by(IU_ID_MAPPING) %>%
    filter(
      any(Year %in% c(2020, 2021, 2022) & MDA_CDTI_Biannual == 1) &
        any(Year %in% c(2023, 2024, 2025) & MDA_CDTI == 1) &
        trt_status_2022 == "MDA continues"
    ) %>%
    distinct(IU_ID_MAPPING) %>%
    pull(IU_ID_MAPPING)
  
  # Update biannual MDA for 2023-2025
  df_updated$MDA_CDTI_Biannual <- ifelse(df_updated$IU_ID_MAPPING %in% filtered_ids & df_updated$Year %in% c(2023, 2024, 2025), 1, df_updated$MDA_CDTI_Biannual)
  
  # Update vector control for 2023-2025 (continue VC status from 2022)
  # If vector control was 1 in 2022, continue VC=1 in 2023-2025
  condition_vc1 <- df_updated$vector_control == 1 & df_updated$Year == 2022
  indices_vc1 <- which(condition_vc1)
  selected_rows_vc1 <- df_updated[indices_vc1, ]
  unique_IDIU_vc1 <- unique(selected_rows_vc1$IU_ID_MAPPING)
  df_updated$vector_control <- ifelse(df_updated$IU_ID_MAPPING %in% unique_IDIU_vc1 & df_updated$Year %in% c(2023, 2024, 2025), 1, df_updated$vector_control)
  
  # If vector control was 2 in 2022, continue VC=2 in 2023-2025
  condition_vc2 <- df_updated$vector_control == 2 & df_updated$Year == 2022
  indices_vc2 <- which(condition_vc2)
  selected_rows_vc2 <- df_updated[indices_vc2, ]
  unique_IDIU_vc2 <- unique(selected_rows_vc2$IU_ID_MAPPING)
  df_updated$vector_control <- ifelse(df_updated$IU_ID_MAPPING %in% unique_IDIU_vc2 & df_updated$Year %in% c(2023, 2024, 2025), 2, df_updated$vector_control)
  
  # Check number of IUs:
  cat("Rows in 2022 after updating biannual MDA & vector control in 2023-2025:", nrow(subset(df_updated, Year == 2022)), "\n")
  cat("Unique IU_ID_MAPPING count after updating biannual MDA & vector control in 2023-2025:", length(unique(df_updated$IU_ID_MAPPING)), "\n")
  
  
  # Return the updated dataframe
  return(df_updated)
}

# ============================================================== #
#       Function to remove a) non-endemic IUs in Uganda and      #
#       b) treatment naive IUs which are not-reported/ unknown   #
#       except in SDN Blue Nile Admin1                           #
# ============================================================== #

remove_non_endemic_and_trtnaive_IUs <- function(df) {
  df_updated <- df
  
  cat("Rows in 2022 after BEFORE non-endemic IUs (UGA) & treatment-naive (unknown/not-reported in ESPEN) step:", nrow(subset(df_updated, Year == 2022)), "\n")
  cat("Unique IU_ID_MAPPING count BEFORE removing non-endemic IUs (UGA) & treatment-naive (unknown/not-reported in ESPEN) step:", length(unique(df_updated$IU_ID_MAPPING)), "\n")
  
  nIUs_prestep <- length(unique(df_updated$IU_ID_MAPPING)) # store
  
  # # Update treatment naive IUs in Uganda to "non-endemic"
  # df_updated$trt_status_2022 <- ifelse(
  #   df_updated$ADMIN0ISO3 == "UGA" & df_updated$trt_status_2022 == "Treatment naive", 
  #   "non-endemic", 
  #   df_updated$trt_status_2022
  # )
  
  condition <- df_updated$ADMIN0ISO3 == "UGA" &
    df_updated$trt_status_2022 == "Treatment naive"
  indices <- which(condition)
  df_updated$trt_status_2022[indices] <- "non-endemic"
  
  # Remove non-endemic IUs (from Uganda)
  df_updated <- subset(df_updated, !(df_updated$trt_status_2022 == "non-endemic"))
  
  cat("Rows in 2022 AFTER removing non-endemic IUs (UGA):", nrow(subset(df_updated, Year == 2022)), "\n")
  cat("Unique IU_ID_MAPPING count AFTER removing non-endemic IUs (UGA) :", length(unique(df_updated$IU_ID_MAPPING)), "\n")
  cat("Unique IU_ID_MAPPING removed because non-endemic IUs (UGA):", nIUs_prestep - length(unique(df_updated$IU_ID_MAPPING)), "\n")
  
  
  nIUs_prestep2 <- length(unique(df_updated$IU_ID_MAPPING)) # store
  
  # Remove treatment naive IUs with Not reported, Unknown (under LF MDA), or Unknown (consider Oncho Elimination Mapping)
  condition <- df_updated$ADMIN1 != "Blue Nile" & df_updated$trt_status_2022 == "Treatment naive" &
    df_updated$MAX_Endemicity %in% c("Not reported", "Unknown (under LF MDA)", "Unknown (consider Oncho Elimination Mapping)")
  
  indices <- which(condition)
  df_updated$exclude[indices] <- "exclude"
  
  # Filter out the rows marked as "exclude"
  df_updated <- subset(df_updated, is.na(df_updated$exclude))
  
  # Check number of IUs:
  cat("Rows in 2022 AFTER removing treatment-naive (unknown/not-reported in ESPEN):", nrow(subset(df_updated, Year == 2022)), "\n")
  cat("Unique IU_ID_MAPPING count AFTER removing treatment-naive (unknown/not-reported in ESPEN):", length(unique(df_updated$IU_ID_MAPPING)), "\n")
  cat("Unique IU_ID_MAPPING removed because treatment-naive (unknown/not-reported in ESPEN) step:", nIUs_prestep2 - length(unique(df_updated$IU_ID_MAPPING)), "\n")
  
  nIUs_prestep3 <- length(unique(df_updated$IU_ID_MAPPING)) # store
  
  # final filtering of treatment naive (not in Gabon or Sudan)
  
  df_updated <- df_updated %>%
    filter(
      # Keep "Treatment naive" only if ADMIN0ISO3 is "GAB" or "SDN"
      (trt_status_2022 == "Treatment naive" & ADMIN0ISO3 %in% c("GAB", "SDN")) |
        # Keep any row where trt_status_2022 is not "Treatment naive"
        trt_status_2022 != "Treatment naive",
      # Exclude a specific IU_ID
      !IU_ID_MAPPING %in% c("19529", "37017") # these two are both revised_cum_MDA == 0 so treatment naive 
    )
  
  # Check number of IUs:
  cat("Rows in 2022 AFTER final removal of treatment-naive (except GAB and SDN):", nrow(subset(df_updated, Year == 2022)), "\n")
  cat("Unique IU_ID_MAPPING count AFTER final removal of treatment-naive (except GAB and SDN):", length(unique(df_updated$IU_ID_MAPPING)), "\n")
  cat("Unique IU_ID_MAPPING removed because treatment-naive (except GAB and SDN) step:", nIUs_prestep3 - length(unique(df_updated$IU_ID_MAPPING)), "\n")
  
  
  
  # Return the updated dataframe
  return(df_updated)
}

# ============================================================== #
#            Function to make final columns & tidying            #
# ============================================================== #

create_final_columns <- function(df) {
  
  df_updated <- df
  
  # 1) Create adherence_par column based on MDA_CDTI
  df_updated <- df_updated %>%
    mutate(
      adherence_par = ifelse(MDA_CDTI == 1, 0.3, NA_real_)
    ) 
  
  # 2) Create number_rnds column (sum of MDA_CDTI and MDA_CDTI_Biannual)
  df_updated$number_rnds <- rowSums(df_updated[c("MDA_CDTI", "MDA_CDTI_Biannual")], na.rm = TRUE)
  
  # 3) Create any_MDA column (1 if number_rnds > 0, else 0)
  df_updated$any_MDA <- ifelse(df_updated$number_rnds > 0, 1, 0)
  
  # 4) Create CUM_MDA_modelled column (cumulative sum of number_rnds grouped by IU_ID_MAPPING)
  df_updated <- df_updated %>%
    group_by(IU_ID_MAPPING) %>%
    mutate(CUM_MDA_modelled = cumsum(number_rnds))
  
  # 5) Create Cum_MDA_ESPEN column
  df_updated$Cum_MDA_ESPEN <- df_updated$Cum_MDA
  
  # 6) Create PHASE, SIZ_label, and MDA_nonCDTI columns with NA values
  df_updated$PHASE <- NA
  df_updated$SIZ_label <- NA
  df_updated$MDA_nonCDTI <- NA
  
  # 7) Create IUID column
  df_updated$IUID <- df_updated$IU_ID_MAPPING
  df_updated$IUID[nchar(df_updated$IU_ID_MAPPING) == 4] <- paste(0, df_updated$IUID[nchar(df_updated$IU_ID_MAPPING) == 4], sep = "")
  df_updated$IUID <- paste(df_updated$ADMIN0ISO3, df_updated$IUID, sep = "")
  
  # 8) Set Control_prog to "APOC"
  df_updated$Control_prog <- "APOC"
  
  # 9) Create Cov_raw column from MDA_CDTI_raw
  df_updated$Cov_raw <- df_updated$MDA_CDTI_raw
  
  # Check number of IUs:
  cat("Rows in 2022 after creating final columns and tidying:", nrow(subset(df_updated, Year == 2022)), "\n")
  cat("Unique IU_ID_MAPPING count after creating final columns and tidying:", length(unique(df_updated$IU_ID_MAPPING)), "\n")
  
  # Return the updated dataframe
  return(df_updated)
}

# ============================================================================== #
#                 Function to create a co-endemic column for oncho-LF-loa        #
# ============================================================================== #

create_co_endemicity_column <- function(df, co_endemicity_file_path) {
  
  df_updated <- df
  
  # Read the co-endemicity data
  co_endemic_IUs <- read.csv(co_endemicity_file_path)
  
  # Create subsets for each co-endemicity category
  co_endemic_IUs_oncho_LF_loa <- subset(co_endemic_IUs, co_endemicity == "oncho,LF,loa")
  co_endemic_IUs_oncho_LF_loa_vec <- unique(co_endemic_IUs_oncho_LF_loa$IU_ID_MAPPING)
  
  co_endemic_IUs_oncho_LF <- subset(co_endemic_IUs, co_endemicity == "oncho,LF")
  co_endemic_IUs_oncho_LF_vec <- unique(co_endemic_IUs_oncho_LF$IU_ID_MAPPING)
  
  co_endemic_IUs_oncho_loa <- subset(co_endemic_IUs, co_endemicity == "oncho,loa")
  co_endemic_IUs_oncho_loa_vec <- unique(co_endemic_IUs_oncho_loa$IU_ID_MAPPING)
  
  co_endemic_IUs_oncho <- subset(co_endemic_IUs, co_endemicity == "oncho")
  co_endemic_IUs_oncho_vec <- unique(co_endemic_IUs_oncho$IU_ID_MAPPING)
  
  # Update co_endemicity column based on IU_ID_MAPPING
  df_updated$co_endemicity <- ifelse(df_updated$IU_ID_MAPPING %in% co_endemic_IUs_oncho_LF_loa_vec, "oncho,LF,loa",
                                     ifelse(df_updated$IU_ID_MAPPING %in% co_endemic_IUs_oncho_LF_vec, "oncho,LF",
                                            ifelse(df_updated$IU_ID_MAPPING %in% co_endemic_IUs_oncho_loa_vec, "oncho,loa",
                                                   ifelse(df_updated$IU_ID_MAPPING %in% co_endemic_IUs_oncho_vec, "oncho", "assumed oncho only"))))
  
  # Check number of IUs:
  cat("Rows in 2022 after creating co-endemicity column:", nrow(subset(df_updated, Year == 2022)), "\n")
  cat("Unique IU_ID_MAPPING count after creating co-endemicity column:", length(unique(df_updated$IU_ID_MAPPING)), "\n")
  
  # Return the updated dataframe
  return(df_updated)
}

# ====================================================================== #
#            Function to make final, minimal & single year dataframes    #
# ====================================================================== #

create_apoc_histories_dataframes <- function(df) {
  # Create Full_APOC_histories_df_popinfo
  Full_df_popinfo <- df[, c("IU_ID_MAPPING","IUs_NAME_MAPPING","IU_CODE_MAPPING", "IUID", "ADMIN0ISO3","Endemicity", 
                            "MAX_Endemicity", "PHASE", "SIZ_label", "endemicity_baseline", "Year", "PopTot", "PopPreSAC", 
                            "PopSAC", "PopAdult", "PopReq", "PopTrg", "PopTreat", "MDA_scheme", "Cum_MDA_ESPEN", "Cov", 
                            "EpiCov", "vector_control", "biannual_VC_mapping", "MDA_nonCDTI", "MDA_CDTI", "MDA_CDTI_Biannual", 
                            "number_rnds", "Cov.in2", "Cov_raw", "cov_source", "cov_specific_source", "CUM_MDA_modelled", 
                            "trt_status_2022", "trt_status_2022_v2", "adherence_par", "co_endemicity", "Control_prog")]
  
  # Create Full_APOC_histories_df_minimal
  Full_df_minimal <- df[, c("IU_ID_MAPPING", "IUs_NAME_MAPPING", "IU_CODE_MAPPING", "IUID", "ADMIN0ISO3", "Endemicity", 
                            "MAX_Endemicity", "PHASE", "SIZ_label", "endemicity_baseline", "Year", "MDA_scheme", 
                            "Cum_MDA_ESPEN", "Cov", "EpiCov", "vector_control", "biannual_VC_mapping", "MDA_nonCDTI", 
                            "MDA_CDTI", "MDA_CDTI_Biannual", "number_rnds", "Cov.in2", "Cov_raw", "cov_source", 
                            "cov_specific_source", "CUM_MDA_modelled", "trt_status_2022", "trt_status_2022_v2", "adherence_par", 
                            "co_endemicity", "Control_prog")]
  
  # Create Full_APOC_histories_df_minimal_lastyr_2022
  Full_df_minimal_lastyr_2022 <- subset(Full_df_popinfo, Year == 2022)
  
  # Create Full_APOC_histories_df_minimal_lastyr_2025
  Full_df_minimal_lastyr_2025 <- subset(Full_df_popinfo, Year == 2025)
  
  # Return all dataframes as a list
  return(list(
    Full_df_popinfo,
    Full_df_minimal,
    Full_df_minimal_lastyr_2022,
    Full_df_minimal_lastyr_2025
  ))
}


