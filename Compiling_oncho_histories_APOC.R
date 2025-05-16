# ================================================================================================================================== #
#                                           New APOC histories (OCT-NOV 2023)                                                        #
# ================================================================================================================================== #

library(dplyr)
library(sf)
library(sp)
library(tidyverse)
library(broom)
library(ggplot2)

# ========================================================================================================= #
#                  Load Cleaned IU Dataframe from Paul - based on preliminary ESPEN data from J Cano (2023) #
# ========================================================================================================= #

final_df_oncho_Parent_Child <- read.csv(
  "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Cleaning IUs (Paul B)/final_df_oncho_Parent_Child.csv"
)

# ============================================================================ #
#            Update ESPEN 2022 Data for Specific Countries (MAY 2024)         #
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

# -------------------- #
#   ETHIOPIA (ETH)     #
# -------------------- #
final_df_oncho_Parent_Child <- update_espen_data(
  final_df_oncho_Parent_Child,
  "ETH",
  "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/ESPEN updates to check/ESPEN updated data for 2022/data-ETH-Oncho-iu-2022.csv"
)

# -------------------- #
#    NIGERIA (NGA)     #
# -------------------- #
final_df_oncho_Parent_Child <- update_espen_data(
  final_df_oncho_Parent_Child,
  "NGA",
  "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/ESPEN updates to check/ESPEN updated data for 2022/data-NGA-Oncho-iu-2022.csv"
)

# -------------------- #
#     UGANDA (UGA)     #
# -------------------- #
final_df_oncho_Parent_Child <- update_espen_data(
  final_df_oncho_Parent_Child,
  "UGA",
  "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/ESPEN updates to check/ESPEN updated data for 2022/data-UG-Oncho-iu-2022.csv"
)

# check 1 : check if any IUs with less than 10 rows 
check_result <- final_df_oncho_Parent_Child %>%
  group_by(IU_ID_MAPPING) %>%
  filter(n() != 10) %>%
  ungroup() 
length(unique(check_result$IU_ID_MAPPING)) # total of 11 Ius without 10 years/rows (2013 - 2022)

dfAPOC <- final_df_oncho_Parent_Child # RESET HERE

# check #
check_df <- subset(final_df_oncho_Parent_Child, Year == 2022)
nrow(check_df)
length(unique(check_df$IU_ID_MAPPING)) # 5121 IUs

# ============================================================================ #
#            Pre-control IU-level prevalences for decision - tree              #
# ============================================================================ #

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

# Example usage:
extracted_data <- extract_baseline_geostat_mfp_func(
  "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/IU pre-control fitted prevalence/summaries_apoc.Rdata",
  "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/IU pre-control fitted prevalence/summaries_liberia.Rdata",
  "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/IU pre-control fitted prevalence/summaries_ocp.Rdata"
)

dfAPOC <- extracted_data[[1]]
APOC_liberia_baseline <- extracted_data[[2]] # rename here

# =========================================================================== #
#      2)  Determine whether IU classified as Endemic at any point            #
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

# call function :
result <- clean_EndemicIUs_alignment_func(dfAPOC)

dfAPOC_endemic <- result[[1]]
IU_endemic_codes <- result[[2]]

# ====================================================== #
#        Merge the baseline mfp and classify endemicity  #

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

# call function
result <- merge_and_process_data(dfAPOC_endemic, APOC_liberia_baseline, endemicity = "endemic")

# The processed dataframe
dfAPOC_endemic <- result$df_merged


# =================================================================================== #
#      3) Non-endemic IUs: classified as "unknown / not-reported " at any point       #
# =================================================================================== #

# # first remove all IUs with endemic status
# 
# dfAPOC_unknowns <- dfAPOC[!(dfAPOC$IU_ID_MAPPING %in% IU_endemic_codes), ]
# nrow(subset(dfAPOC_unknowns, Year == 2022)) # 2698 IUs are non-endemic or unknown/not-reported
# 
# # subset out all IUs where non-endemic for every year #
# 
# # note almost all IUs "not-reported" in 2013 so do not select on 2013, only > 2013
# # dfAPOC_unknowns2 <- dfAPOC_unknowns %>%
# #   filter(Year != 2013)
# 
# dfAPOC_unknowns2 <- dfAPOC_unknowns %>%
#   filter(!(Year %in% c(2013, 2014)))
# 
# # remove all where non-endemic from 2014
# dfAPOC_nonendemic <- dfAPOC_unknowns2 %>%
#   group_by(IU_ID_MAPPING) %>%
#   filter(all(Endemicity == "Non-endemic")) %>%
#   ungroup() # subset all those IUs with non-endemic every year from 2014
# 
# # check #
# check_df <- subset(dfAPOC_nonendemic, Year == 2022)
# nrow(check_df)
# length(unique(check_df$IU_ID_MAPPING)) # 858 IUs (all yrs "non-endemic" in ESPEN)
# 
# IU_nonendemic_codes <- unique(dfAPOC_nonendemic$IU_ID_MAPPING)
# length(IU_nonendemic_codes) # find unique codes (n = 859 IUs); 858 in April 25'
# 
# dfAPOC_nonendemic <- dfAPOC_unknowns[(dfAPOC_unknowns$IU_ID_MAPPING %in% IU_nonendemic_codes), ] #final non-endemic from unknowns (with 2013 + 2014 years back in)
# 
# dfAPOC_nonendemic$included <- as.character("excluded")
# dfAPOC_nonendemic$classification <- as.character("non-endemic in ESPEN")
# 
# nrow(subset(dfAPOC_unknowns, Year == 2022)) # 2698 IUs are non-endemic or unknown/not-reported
# 
# # now filter out all non-endemic IUs from unknowns dataframe to leave unknowns
# dfAPOC_unknowns <- dfAPOC_unknowns[!(dfAPOC_unknowns$IU_ID_MAPPING %in% IU_nonendemic_codes), ]
# 
# nrow(subset(dfAPOC_unknowns, Year == 2022)) # 1840 IUs remaining after removing non-endemic (2698 - 858)
# 
# # check length/ number of IUs of unknowns remaining
# IU_unknowns_codes <- unique(dfAPOC_unknowns$IU_ID_MAPPING)
# length(IU_unknowns_codes) # find unique codes (n = 1840 IUs) # April 25' = 1840
# 
# check_result2 <- dfAPOC_unknowns %>%
#   group_by(IU_ID_MAPPING) %>%
#   filter(n() != 10) %>%
#   ungroup() # check if any IUs with less than 10 rows
# 
# length(unique(check_result2$IU_ID_MAPPING)) # 1 IU
# 
# # total of 1 IU without 10 years/rows (2013 - 2022)
# 
# # find IUs and number of IUs with only not-reported/non-endemic combination in all years
# dfAPOC_notreported_nonendemic <- dfAPOC_unknowns %>%
#   group_by(IU_ID_MAPPING) %>%
#   filter(all(c("Not reported", "Non-endemic") %in% Endemicity) & n_distinct(Endemicity) == 2) %>%
#   ungroup()
# 
# IU_notreported_nonendemic <- unique(dfAPOC_notreported_nonendemic$IU_ID_MAPPING)
# length(IU_notreported_nonendemic) # n = 146 IUs
# 
# # see if non-endemic + not-reported in df_nonendemic df
# dfAPOC_nonendemic2 <- dfAPOC_nonendemic[!(dfAPOC_nonendemic$IU_ID_MAPPING %in% IU_notreported_nonendemic), ]
# IU_notreported_nonendemic2 <- unique(dfAPOC_nonendemic2$IU_ID_MAPPING)
# length(IU_notreported_nonendemic2) # n = 859  -->
# length(IU_nonendemic_codes) # 859 IUs
# 
# identical(IU_notreported_nonendemic2, IU_nonendemic_codes) # these are identifcal 


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

# call function and extract outputs:
result <- process_unknown_nonendemic_IUs_func(dfAPOC, IU_endemic_codes)
dfAPOC_unknowns <- result$df_unknowns
dfAPOC_nonendemic <- result$df_nonendemic
IU_nonendemic_codes <- result$IU_nonendemic_codes


# ========================================================================= #
# now add a column with the baseline endemicity for next selection criteria #

# nrow(subset(dfAPOC_unknowns, Year == 2022)) # 1840 IUs remaining after removing non-endemic (2698 - 858)
# # merge pre-endemic mf prevalence data
# 
# # Merge the two data frames based on the 'IU_CODE' column
# dfAPOC_unknowns  <- merge(dfAPOC_unknowns, APOC_liberia_baseline, by.x = "IU_ID_MAPPING", by.y = "IU_ID_MAPPING", all.x = TRUE)
# 
# # omit last 7 cols with merged dataframe (unless want quantiles for pre-control prev)
# dfAPOC_unknowns  <- dfAPOC_unknowns[, 1:(ncol(dfAPOC_unknowns) - 8)]
# 
# dfAPOC_unknowns <- dfAPOC_unknowns %>%
#   mutate(endemicity_baseline = case_when(
#     is.na(mean) ~ "non-endemic",
#     mean < 0.40 ~ "hypoendemic",
#     mean < 0.60 ~ "mesoendemic",
#     mean < 0.80 ~ "hyperendemic",
#     TRUE ~ "holoendemic"
#   ))
# 
# 
# # # CMR #
# # dfAPOC_unknowns_CMR <- subset(dfAPOC_unknowns, ADMIN0.x == "Cameroon")
# #
# # count_by_endemicity <- dfAPOC_unknowns_CMR %>%
# #   group_by(endemicity_baseline, IU_ID_MAPPING) %>%
# #   summarise(n = n()) %>%
# #   group_by(endemicity_baseline) %>%
# #   summarise(count_IU_ID_MAPPING = n_distinct(IU_ID_MAPPING))
# 
# nrow(subset(dfAPOC_unknowns, Year == 2022)) # 1840 IUs remaining after removing non-endemic (2698 - 858)

# ========================================= #
# merge baseline mfp with unknown dataframe #

# call function
result <- merge_and_process_data(dfAPOC_unknowns, APOC_liberia_baseline, endemicity = "unknown")

# The processed dataframe
dfAPOC_unknowns <- result$df_merged

# # ===================================== #
# # a) if mesoendemic baseline = include  #
# 
# dfAPOC_unknowns_mesoendemic <- dfAPOC_unknowns %>%
#   group_by(IU_ID_MAPPING) %>%
#   filter(any(endemicity_baseline == "mesoendemic")) %>%
#   ungroup() # subset all those IUs with mesoendemic
# 
# IU_unknowns_meso_codes <- unique(dfAPOC_unknowns_mesoendemic$IU_ID_MAPPING)
# length(IU_unknowns_meso_codes) # find unique codes = 150 IUs; 152 (April 25')
# 
# dfAPOC_unknowns_mesoendemic$included <- as.character("included")
# dfAPOC_unknowns_mesoendemic$classification <- as.character("unknown with mesoendemic baseline")
# 
# nrow(subset(dfAPOC_unknowns_mesoendemic, Year == 2022)) # 152 IUs 
# 
# # =================================================== #
# # b) if hypoendemic baseline & Cum_MDA > 0 = include  #
# 
# dfAPOC_unknowns_hypoendemic <- dfAPOC_unknowns %>%
#   group_by(IU_ID_MAPPING) %>%
#   filter(any(endemicity_baseline == "hypoendemic")) %>%
#   ungroup() # subset all those IUs with hypoendemic
# 
# IU_unknowns_hypo_codes <- unique(dfAPOC_unknowns_hypoendemic$IU_ID_MAPPING)
# length(IU_unknowns_hypo_codes) # find unique codes = 1145 IUs
#                                # April 25' = 1158 IUs
# 
# # check the remaining n = 530 IUs 
# dfAPOC_unknowns_NOT_hypo_meso_endemic <- dfAPOC_unknowns %>%
#   group_by(IU_ID_MAPPING) %>%
#   filter(any(!endemicity_baseline %in% c("hypoendemic", "mesoendemic"))) %>%
#   ungroup() # subset all those IUs with hypoendemic
# 
# IU_unknowns_NOT_hypomeso_codes <- unique(dfAPOC_unknowns_NOT_hypo_meso_endemic$IU_ID_MAPPING)
# length(IU_unknowns_NOT_hypomeso_codes) # find unique codes = 530 IUs remaining (which are not "unknown" meso + hypo)
# 
# unique(dfAPOC_unknowns_NOT_hypo_meso_endemic$endemicity_baseline) # all 530 IUs are "non-endemic" based on Zoure et al. 
# 
# # DO WE STILL WANT TO FILTER BASED ON WHETHER CUM_MDA > 0 ??
# # IF considered baseline endemic, and IU experienced LF MDA (with IVM), then additional benefit on oncho?
# # or treat as treatment naive ?
# 
# dfAPOC_unknowns_hypoendemic2 <- dfAPOC_unknowns_hypoendemic %>%
#   group_by(IU_ID_MAPPING) %>%
#   filter(any(Cum_MDA > 0)) %>%
#   ungroup()
# 
# IU_unknowns_hypo_codes2 <- unique(dfAPOC_unknowns_hypoendemic2$IU_ID_MAPPING)
# length(IU_unknowns_hypo_codes2) # find unique codes = 410 IUs ; 411 IUs April 25'
# 
# dfAPOC_unknowns_hypoendemic2
# 
# dfAPOC_unknowns_hypoendemic2$included <- as.character("included")
# dfAPOC_unknowns_hypoendemic2$classification <- as.character("unknown with hypoendemic baseline
# & Cum_MDA > 0 in ESPEN")
# 
# # check those Cum_MDA = 0  + Unknown + hypo baseline
# 
# # dfAPOC_unknowns_hypoendemic3 <- dfAPOC_unknowns_hypoendemic %>%
# #   group_by(IU_ID_MAPPING) %>%
# #   filter(any(Cum_MDA == 0)) %>%
# #   ungroup()
# # 
# # nrow(subset(dfAPOC_unknowns_hypoendemic3, Year == 2022)) # 749 (should be 747 b/c 1158 - 411 = 747?)
# 
# # # CMR #
# # dfAPOC_unknowns_hypoendemic2_CMR <- subset(dfAPOC_unknowns_hypoendemic2, ADMIN0.x == "Cameroon")
# #
# # count_by_classification <- dfAPOC_unknowns_hypoendemic2_CMR %>%
# #   group_by(classification, IU_ID_MAPPING) %>%
# #   summarise(n = n()) %>%
# #   group_by(classification) %>%
# #   summarise(count_IU_ID_MAPPING = n_distinct(IU_ID_MAPPING))
# 
# # all of those with Cum_MDA = 0
# dfAPOC_unknowns_hypoendemic3 <- dfAPOC_unknowns_hypoendemic %>%
#   group_by(IU_ID_MAPPING) %>%
#   filter(all(Cum_MDA == 0)) %>%
#   ungroup()
# 
# IU_unknowns_hypo_codes3 <- unique(dfAPOC_unknowns_hypoendemic3$IU_ID_MAPPING)
# length(IU_unknowns_hypo_codes3) # find unique codes = 735 IUs (in flowchart = 1158 - 411 = 747)
#                                 # find unique codes = 747 IUs (in flowchart = 1158 - 411 = 747 IUs)
# 
# dfAPOC_unknowns_hypoendemic3$included <- as.character("excluded")
# dfAPOC_unknowns_hypoendemic3$classification <- as.character("unknown with hypoendemic baseline
# & Cum_MDA = 0 in ESPEN")
# 
# # # CMR #
# # dfAPOC_unknowns_hypoendemic3_CMR <- subset(dfAPOC_unknowns_hypoendemic3, ADMIN0.x == "Cameroon")
# #
# # count_by_classification <- dfAPOC_unknowns_hypoendemic3_CMR %>%
# #   group_by(classification, IU_ID_MAPPING) %>%
# #   summarise(n = n()) %>%
# #   group_by(classification) %>%
# #   summarise(count_IU_ID_MAPPING = n_distinct(IU_ID_MAPPING))
# 
# 
# # c) if non-endemic baseline = exclude
# dfAPOC_unknowns_nonendemic <- dfAPOC_unknowns %>%
#   group_by(IU_ID_MAPPING) %>%
#   filter(any(endemicity_baseline == "non-endemic")) %>%
#   ungroup() # subset all those IUs with hypoendemic
# 
# IU_unknowns_nonendemic_codes <- unique(dfAPOC_unknowns_nonendemic$IU_ID_MAPPING)
# length(IU_unknowns_nonendemic_codes) # find unique codes = 545 IUs; 530 IUs in April 25' (1688 - 1158)
# 
# dfAPOC_unknowns_nonendemic$included <- as.character("excluded")
# dfAPOC_unknowns_nonendemic$classification <- as.character("unknown with no baseline")


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

# Example usage:
result <- process_baseline_conditions(dfAPOC_unknowns, endemicity = "unknown")
dfAPOC_unknowns_mesoendemic <- result$df_mesoendemic
dfAPOC_unknowns_hypoendemic_with_mda <- result$df_hypoendemic_with_mda
dfAPOC_unknowns_hypoendemic_no_mda <- result$df_hypoendemic_no_mda
#dfAPOC_unknowns_nonendemic <- result$df_NOT_hypo_meso_endemic
dfAPOC_unknowns_nonendemic <- result$df_nonendemic


# Compare the column names
colnames(dfAPOC_unknowns_nonendemic) # Columns in dfAPOC_unknowns_nonendemic
colnames(dfAPOC_nonendemic) # Columns in dfAPOC_nonendemic

# Check if there are any columns that are present in one but not the other
setdiff(colnames(dfAPOC_unknowns_nonendemic), colnames(dfAPOC_nonendemic)) # Columns in dfAPOC_unknowns_nonendemic but not in dfAPOC_nonendemic
setdiff(colnames(dfAPOC_nonendemic), colnames(dfAPOC_unknowns_nonendemic)) # Columns in dfAPOC_nonendemic but not in dfAPOC_unknowns_nonendemic


# =================================================================================== #
#      4) map non-endemic/unknown/ not-reported IUs                                   #



# # IUs without endemic or Unknown/Not reported for at least 1 year, classified as:Non-endemic = exclude
# 
# # # check
# # nrow(subset(dfAPOC_endemic, Year == 2022)) # 1463 : endemic 
# # nrow(subset(dfAPOC_unknowns_mesoendemic, Year == 2022)) # 152 : unknown + mesoendemic (zoure)
# # nrow(subset(dfAPOC_unknowns_hypoendemic2, Year == 2022)) # 411 : unknown + hypo (zoure) + cum_MDA > 0
# # nrow(subset(dfAPOC_unknowns_hypoendemic3, Year == 2022)) # 747 : unknown  + hypo (zoure) + Cum_MDA = 0
# # nrow(subset(dfAPOC_unknowns_nonendemic, Year == 2022)) # 530 : unknown + non-endemic (zoure)
# # nrow(subset(dfAPOC_nonendemic, Year == 2022)) # 858: non-endemic (espen)
# # # 1463 + 152 + 411 + 747 + 530 + 858 = 4161 IUs in former APOC
# 
# # check
# nrow(subset(dfAPOC_endemic, Year == 2022)) # 1463 : endemic 
# nrow(subset(dfAPOC_unknowns_mesoendemic, Year == 2022)) # 152 : unknown + mesoendemic (zoure)
# nrow(subset(dfAPOC_unknowns_hypoendemic_with_mda, Year == 2022)) # 411 : unknown + hypo (zoure) + cum_MDA > 0
# nrow(subset(dfAPOC_unknowns_hypoendemic_no_mda, Year == 2022)) # 747 : unknown  + hypo (zoure) + Cum_MDA = 0
# nrow(subset(dfAPOC_unknowns_nonendemic, Year == 2022)) # 530 : unknown + non-endemic (zoure)
# nrow(subset(dfAPOC_nonendemic, Year == 2022)) # 858: non-endemic (espen)
# # 1463 + 152 + 411 + 747 + 530 + 858 = 4161 IUs in former APOC
# 
# # Combine all subsetted dataframes
# 
# ncol(dfAPOC_endemic)
# dfAPOC_endemic <- dfAPOC_endemic[, -c(59, 60)]
# ncol(dfAPOC_unknowns_nonendemic)
# 
# dfAPOC_nonendemic$IU_CODE.y <- NA
# dfAPOC_nonendemic$mean <- NA
# dfAPOC_nonendemic$endemicity_baseline <- NA
# 
# names(dfAPOC_endemic)[names(dfAPOC_endemic) == "ADMIN0.x"] <- "ADMIN0"
# names(dfAPOC_endemic)[names(dfAPOC_endemic) == "IU_CODE.x"] <- "IU_CODE"
# 
# names(dfAPOC_unknowns_mesoendemic)[names(dfAPOC_unknowns_mesoendemic) == "ADMIN0.x"] <- "ADMIN0"
# names(dfAPOC_unknowns_mesoendemic)[names(dfAPOC_unknowns_mesoendemic) == "IU_CODE.x"] <- "IU_CODE"
# 
# names(dfAPOC_unknowns_hypoendemic_with_mda)[names(dfAPOC_unknowns_hypoendemic_with_mda) == "ADMIN0.x"] <- "ADMIN0"
# names(dfAPOC_unknowns_hypoendemic_with_mda)[names(dfAPOC_unknowns_hypoendemic_with_mda) == "IU_CODE.x"] <- "IU_CODE"
# 
# names(dfAPOC_unknowns_hypoendemic_no_mda)[names(dfAPOC_unknowns_hypoendemic_no_mda) == "ADMIN0.x"] <- "ADMIN0"
# names(dfAPOC_unknowns_hypoendemic_no_mda)[names(dfAPOC_unknowns_hypoendemic_no_mda) == "IU_CODE.x"] <- "IU_CODE"
# 
# names(dfAPOC_unknowns_nonendemic)[names(dfAPOC_unknowns_nonendemic) == "ADMIN0.x"] <- "ADMIN0"
# names(dfAPOC_unknowns_nonendemic)[names(dfAPOC_unknowns_nonendemic) == "IU_CODE.x"] <- "IU_CODE"
# 
# dfAPOC_complete <- rbind(dfAPOC_endemic, dfAPOC_unknowns_mesoendemic, dfAPOC_unknowns_hypoendemic_with_mda,
#                          dfAPOC_unknowns_hypoendemic_no_mda, dfAPOC_unknowns_nonendemic, dfAPOC_nonendemic)
# 
# 
# APOC_countries <- unique(dfAPOC_complete$ADMIN0ISO3)
# 
# # # sdiff between original dataframe and reconstructred (200 IUs?)
# # unique_IUs_allAPOC1 <- unique(dfAPOC_complete$IU_ID_MAPPING)
# # length(unique_IUs_allAPOC1)
# #
# # unique_IUs_allAPOC2 <- unique(dfAPOC$IU_ID_MAPPING)
# # length(unique_IUs_allAPOC2)
# #
# # # SAME NUMBER OF IUS = 4162 across APOC countries
# #
# # difference_rows <- anti_join(dfAPOC, dfAPOC_complete)
# 
# dfAPOC_complete_2022 <- subset(dfAPOC_complete, Year == 2022) # all including non-endemic (n = 4161)
# 
# nrow(dfAPOC_complete_2022) # 4161
# length(unique(dfAPOC_complete_2022$IU_ID_MAPPING)) # 4161 IUs (April 25' accounting for NGA duplicates)
# 
# table(dfAPOC_complete_2022$included)
# table(dfAPOC_complete_2022$classification)


# Function to combine daaframes on endemicity status and process 
combine_apoc_data_with_checks <- function(df_endemic, df_unknowns_mesoendemic, df_unknowns_hypoendemic_with_mda,
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

# Usage example:
dfAPOC_complete <- combine_apoc_data_with_checks(dfAPOC_endemic, dfAPOC_unknowns_mesoendemic,
                                                 dfAPOC_unknowns_hypoendemic_with_mda, dfAPOC_unknowns_hypoendemic_no_mda,
                                                 dfAPOC_unknowns_nonendemic, dfAPOC_nonendemic)

# # Further analysis
# dfAPOC_complete_2022 <- subset(dfAPOC_complete, Year == 2022)
# nrow(dfAPOC_complete_2022) # Number of rows for year 2022
# length(unique(dfAPOC_complete_2022$IU_ID_MAPPING)) # Number of unique IUs

# ============================================================================================== #
#     Final inclusion step for APOC "endemic" IUs (after review of IUs in ETH, SDN, TZN)         #
# ============================================================================================== #

# ====================================================== #
#  ETH specific review of IUs                            #

# new - March 25 (removing 2 IUs from the list that should be endemic + MDA in ESPEN)
ETH_exclude <- c("Seweyna", "Ginir", "Dawe Ketchen", "Gura Damole", "Mena (Bale)", "Harena Buluk", "Nenesebo (Wereka)",
                 "Deka Suftu", "Arda Jila", "Golocha", "Burqua Dhintu", "Meyu Muleke",
                 "Boke", "Kuni", "Melka Balo", "Mesela", "Gemechis", 
                 "Chiro Zuria", "Tulo (Oromia)", "Doba", "Goro Gutu", "Deder", "Bedeno", "Meta", "Kersa (East Hararge)",
                 "Kurfa Chele", "Kombolcha", "Jarso (East Hararghe)", "Gursum (Oromia)", "Meyu Muleke",
                 "Amibara", "Dulecha", "Awash Fentale") # new for May 24 with IUs starting MDA in 2022 ; "Degeluna Tijo" re-included


# updated March 2025 to remove Habro from this list
ETH_exclude2 <- c("Argoba", "Liben Chukala", "Aleltu", "Lemmi", "Mareko") # new for May 24 with IUs starting MDA in 2022


ETH_exclude3 <- c("Gonder Zuria", "East Dembia", "West Dembiya", "Dera (Amhara)",
                  "Yilmana Densa", "Jabi Tehnan", "Dembecha", "Michakel", "Guzamn") # new for May 24 with IUs starting MDA in 2022
# "Quarit", "West Esite", "East Esite" re-included


ETH_include <- c("Jimma Rare", "Abeshege","Takusa","Jore","Lare","Makuey","Jikawo")

# ====================================================== #
#  SDN specific review of IUs (Carter Centre data 2023)  #

SDN_include <- c("El Jabaleen", "El Salam (White Nile)", "Kosti", "Rabak", "EL Qeteena", "El Dwaeem", "Um Ramtta", "Jebal Aulya",
                 "Um Durman", "Bahri (Khartoum North)", "Karari", "El matama", "Shendi", "Eldamar", "Berber", "Abu Hamed",
                 "Marawai","Eldamar","Atbara River","Arouma","Halfa El gedeeda","Western Kasala","Wad EL Helew","El Fashaga",
                 "El Quresha","El Radoom","Buram","Sunta","El Ferdous","Bahr El Arab","Abu Jabra","Dimsu","Katayla","Tullus",
                 "Abyei","Keilak","Eastern El Qalabat","El Kurmuk","Qessan")

SDN_MDA <- c("Marawai","Abu Hamed","Berber","El Quresha","Eastern El Qalabat")
SDN_MDA_future <- c("El Radoom")

to_include_TCC <- c(SDN_MDA, SDN_MDA_future)

# ====================================================== #
#  TZN specific review of IUs                            #

TZN_include <- c("Mahenge", "Malinyi")

# ====================================================== #
# function to isolate final endemic IUs in APOC:         #

process_apoc_data_with_checks <- function(dfAPOC_complete, ETH_exclude, ETH_exclude2, ETH_exclude3, ETH_include, 
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

# Usage example:
result <- process_apoc_data_with_checks(dfAPOC_complete, ETH_exclude, ETH_exclude2, ETH_exclude3, ETH_include, SDN_include, TZN_include)

# Output the results
final_APOC_IUs_vec <- result$final_APOC_IUs_vec
df_APOC_endemic_minimal <- result$df_APOC_endemic_minimal

# Check the results
length(final_APOC_IUs_vec) # Check the length of the unique IUs
nrow(df_APOC_endemic_minimal) # Check the number of rows in the final dataframe


# ==================================================== #
#  Final endemic IUs seelction and do further checks   #

filter_and_check_apoc_data <- function(dfAPOC_complete, final_APOC_IUs_vec) {
  
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

# Usage example:
result <- filter_and_check_apoc_data(dfAPOC_complete, final_APOC_IUs_vec)

# Output the results
dfAPOC_included <- result$dfAPOC_included

# ===================================================== #
#   Load in MDA start years based on APOC report (2015) #

# load APOC national coverage data
dfAPOC_report <- read.csv("C:/Users/mad206/OneDrive/Endgame/APOC/APOC_data_97_13.csv")
colnames(dfAPOC_report) <- c("ADMIN0", "EpiCov", "Year")

# find national start years
#dfAPOC=na.omit(dfAPOC)
NationalStartYear <- data.frame(Year=with(na.omit(dfAPOC_report), tapply(Year, ADMIN0, min)))
NationalStartYear$ADMIN0 <- row.names(NationalStartYear)
row.names(NationalStartYear) <- NULL

# ============================================ #
#   Create rows going back to 1975 for each IU #

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

# Usage example:
dfAPOC_included2 <- process_apoc_IUs_tobaseline_func(dfAPOC_included, final_APOC_IUs_vec)

# ==================================================================================== #
#            Defining inclusion of histories (ESPEN yrs 2013 - 2022)                   #
# ==================================================================================== #

# # ============================== #
# #  Pre-ESPEN history to include  #
# 
# dfAPOC_included2 <- dfAPOC_included2 %>%
#   group_by(IU_ID_MAPPING) %>%
#   mutate(
#     Pre_ESPEN_MDA_history = ifelse(
#       (any(MAX_Endemicity %in% c("Endemic (MDA not delivered)", "Endemic (under MDA)", "Endemic (under post-intervention surveillance)")) &
#          any(Cum_MDA > 0 & Year == 2013)) |
#         (any(MAX_Endemicity %in% c("Unknown (consider Oncho Elimination Mapping)", "Unknown (under LF MDA)", "Not reported")) &
#            any(endemicity_baseline %in% c("mesoendemic", "hypoendemic")) &
#            any(Cum_MDA > 0 & Year == 2013)),
#       "Include",
#       "Exclude"
#     )
#   ) %>%
#   ungroup()  # Remove grouping information
# 
# # dfAPOC_included2 <- dfAPOC_included2 %>%
# #   mutate(
# #     CheckColumn = ifelse(
# #       Pre_ESPEN_MDA_history == "Exclude pre-ESPEN history" & (!is.na(Pre_ESPEN_MDA_history1) | !is.na(Pre_ESPEN_MDA_history2)),
# #       "Condition Met",
# #       "Condition Not Met"
# #     ),
# #     AnyConditionMet = ifelse(any(CheckColumn == "Condition Met"), "Yes", "No")
# #   )
# 
# # dfAPOC_included2 <- dfAPOC_included2 %>%
# #   group_by(IU_ID_MAPPING) %>%
# #   mutate(
# #     Pre_ESPEN_MDA_history1 = ifelse(
# #       any(MAX_Endemicity %in% c("Endemic (MDA not delivered)", "Endemic (under MDA)", "Endemic (under post-intervention surveillance)")) &
# #         any(Cum_MDA > 0 & Year == 2013),
# #       "Include pre-ESPEN history",
# #       NA
# #     )
# #   ) %>%
# #   ungroup()  # Remove grouping information
# #
# # dfAPOC_included2 <- dfAPOC_included2 %>%
# #   group_by(IU_ID_MAPPING) %>%
# #   mutate(
# #     Pre_ESPEN_MDA_history2 = ifelse(
# #       any(MAX_Endemicity %in% c("Unknown (consider Oncho Elimination Mapping)", "Unknown (under LF MDA)", "Not reported")) &
# #       any(endemicity_baseline %in% c("mesoendemic", "hypoendemic")) &
# #         any(Cum_MDA > 0 & Year == 2013),
# #       "Include pre-ESPEN history",
# #       NA
# #     )
# #   ) %>%
# #   ungroup()  # Remove grouping information
# 
# # ==============================  #
# #  Post-ESPEN history to include  #
# 
# dfAPOC_included2 <- dfAPOC_included2 %>%
#   group_by(IU_ID_MAPPING) %>%
#   mutate(
#     ESPEN_MDA_history = ifelse(
#       (any(MAX_Endemicity %in% c("Endemic (MDA not delivered)", "Endemic (under MDA)", "Endemic (under post-intervention surveillance)")) &
#          any(Cum_MDA > 0 & Year > 2013)) |
#         (any(MAX_Endemicity %in% c("Unknown (consider Oncho Elimination Mapping)", "Unknown (under LF MDA)", "Not reported")) &
#            any(endemicity_baseline %in% c("mesoendemic", "hypoendemic")) &
#            any(Cum_MDA > 0 & Year > 2013)),
#       "Include",
#       "Exclude"
#     )
#   ) %>%
#   ungroup()  # Remove grouping information


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
  
  # Return the updated dataframe
  return(df)
}

# Usage example:
dfAPOC_included2 <- define_pre_post_espen_inclusion_func(dfAPOC_included2)

# Check the result
head(dfAPOC_included2$Pre_ESPEN_MDA_history)
head(dfAPOC_included2$ESPEN_MDA_history)

# ============================= #
#   CONTINUE FROM HERE          #


# check if any pre-ESPEN and ESPEN histories not both include

check_histories <- dfAPOC_included2 %>%
  filter(
    (Pre_ESPEN_MDA_history == "Include" & ESPEN_MDA_history == "Exclude") |
      (Pre_ESPEN_MDA_history == "Exclude" & ESPEN_MDA_history == "Include")
  ) # its always "exclude" pre-ESPEN and "include" ESPEN, not otherway ( n = 26 IUs) = IUs where no pre-ESPEN only ESPEN (new?)

check_histories2 <- dfAPOC_included2 %>%
  filter(
    (Pre_ESPEN_MDA_history == "Include" & ESPEN_MDA_history == "Exclude") |
      (Pre_ESPEN_MDA_history == "Exclude" & ESPEN_MDA_history == "Include")
  ) # its always "exclude" pre-ESPEN and "include" ESPEN, not otherway ( n = 26 IUs) = IUs where no pre-ESPEN only ESPEN (new?)


length(unique(check_histories$IU_ID_MAPPING)) # 41 IUs (42 in April 25')

check_histories_trtnaive <- dfAPOC_included2 %>%
  filter(
    (Pre_ESPEN_MDA_history == "Exclude" & ESPEN_MDA_history == "Exclude")
  ) # Treatment naive n = 586

length(unique(check_histories_trtnaive$IU_ID_MAPPING)) # 586 IUs (596 in April 25')


check_histories_trtnaive_lastyr <- subset(check_histories_trtnaive, Year == 2022)
all_trtnaive_IUs <- ESPEN_IUs_ALL %>%
  left_join(check_histories_trtnaive_lastyr, by = c("IU_ID" = "IU_ID_MAPPING"))

cbPalette_endemicity <- c("#CC79A7","#E69F00","#009E73")

# check
ggplot() +
  geom_sf(data = all_trtnaive_IUs, aes(fill = endemicity_baseline), colour = NA, alpha = 0.7) +
  geom_sf(data = ESPEN_IUs_APOC, aes(), colour = NA, size = 1, fill = NA, alpha = 0.1) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
  coord_sf(xlim = c(-20, 50), ylim = c(38, -35)) +
  theme_bw() +
  scale_fill_manual(values = cbPalette_endemicity, na.value = "gray") +
  scale_colour_manual(na.value="gray")+
  labs(fill='') +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal")

frequency_trtnaive_endemicity <- table(check_histories_trtnaive_lastyr$endemicity_baseline)
print(frequency_trtnaive_endemicity)

# ====================================================================================================== #
#              Subset MAX_Endemicity  == any(endemic) to figure out n's                                  #

# ====================================================================================================== #
#              Subset MAX_Endemicity  == any(unknown/reported) to figure out n's                         #

# ========================== #
# Biannual treatment in APOC #

Biannual_APOC_IUs <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Biannual in APOC/Biannual_APOC_IUs.csv")

Biannual_APOC_IUs2 <- Biannual_APOC_IUs[!is.na(Biannual_APOC_IUs$Biannual), ]

Biannual_IUs_vec <- unique(Biannual_APOC_IUs2$IUs_NAME)

# map these #
biannual_IUs_spatial <- ESPEN_IUs_ALL %>%
  left_join(Biannual_APOC_IUs2, by = c("IUs_NAME" = "IUs_NAME"))

biannual_IUs_spatial <- biannual_IUs_spatial[!is.na(biannual_IUs_spatial$Biannual),]

#all_IUs$Biannual_MDA_present <- ifelse(dfAPOC_complete_lastyr2$included2 %in% c("included","included as\ntreatment naive/hypoendemic or impact from LF MDA/hypoendemic"),"endemic", "non-endemic")

# ggplot() +
#   geom_sf(data = biannual_IUs_spatial, aes(fill = Biannual_startyr), colour = "black", alpha = 0.7) +
#   geom_sf(data = ESPEN_IUs_APOC, aes(), colour = NA, size = 1, fill = NA, alpha = 0.1) +
#   geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
#   coord_sf(xlim = c(20, 40), ylim = c(-3, 22)) +
#   theme_bw() +
#   #scale_fill_manual(values = cbPalette3, na.value = "gray") +
#   scale_fill_viridis(option = "C", direction = -1, na.value = "gray") +
#   scale_colour_manual(na.value="gray")+
#   labs(fill='') +
#   theme(
#     legend.position = "bottom",  # Place the legend at the bottom
#     legend.direction = "horizontal")

ggplot() +
  geom_sf(data = ESPEN_IUs_APOC, aes(), colour = NA, size = 1, fill = "gray", alpha = 0.4) +
  geom_sf(data = biannual_IUs_spatial, aes(fill = Biannual_startyr), colour = "black", alpha = 0.9) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
  coord_sf(xlim = c(20, 40), ylim = c(-10, 22)) +
  theme_bw() +
  scale_fill_viridis(option = "C", direction = -1) +
  labs(fill='Starting year of biannual MDA') +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal"
  )
# ====================================================================================================== #
#    need to manually recode foci (Sudan only?) where "treatment naive" but actually have historical MDA #

Biannual_Sudan_IUs <- subset(Biannual_APOC_IUs2, ADMIN0ISO3 == "SDN")

Biannual_Sudan_IUs_vec <- unique(Biannual_Sudan_IUs$IUs_NAME2)

dfAPOC_included2$Pre_ESPEN_MDA_history <- ifelse(dfAPOC_included2$IUs_NAME %in% Biannual_Sudan_IUs_vec, "Include", dfAPOC_included2$Pre_ESPEN_MDA_history)
dfAPOC_included2$ESPEN_MDA_history <- ifelse(dfAPOC_included2$IUs_NAME %in% Biannual_Sudan_IUs_vec, "Include", dfAPOC_included2$ESPEN_MDA_history)


# check if any pre-ESPEN and ESPEN histories not both include

check_histories2 <- dfAPOC_included2 %>%
  filter(
    (Pre_ESPEN_MDA_history == "Include" & ESPEN_MDA_history == "Exclude") |
      (Pre_ESPEN_MDA_history == "Exclude" & ESPEN_MDA_history == "Include")
  ) # its always "exclude" pre-ESPEN and "include" ESPEN, not otherway ( n = 26 IUs) = IUs where no pre-ESPEN only ESPEN (new?)

length(unique(check_histories2$IU_ID_MAPPING)) # 42 IUs (April 25')

check_histories_trtnaive2 <- dfAPOC_included2 %>%
  filter(
    (Pre_ESPEN_MDA_history == "Exclude" & ESPEN_MDA_history == "Exclude")
  ) # Treatment naive n = 586

length(unique(check_histories_trtnaive2$IU_ID_MAPPING)) # 590 IUs (April 25')


check_histories_trtnaive_lastyr2 <- subset(check_histories_trtnaive2, Year == 2022)
all_trtnaive_IUs <- ESPEN_IUs_ALL %>%
  left_join(check_histories_trtnaive_lastyr2, by = c("IU_ID" = "IU_ID_MAPPING"))

cbPalette_endemicity <- c("#CC79A7","#E69F00","#009E73")

# check
ggplot() +
  geom_sf(data = all_trtnaive_IUs, aes(fill = endemicity_baseline), colour = NA, alpha = 0.7) +
  geom_sf(data = ESPEN_IUs_APOC, aes(), colour = NA, size = 1, fill = NA, alpha = 0.1) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
  coord_sf(xlim = c(-20, 50), ylim = c(38, -35)) +
  theme_bw() +
  scale_fill_manual(values = cbPalette_endemicity, na.value = "gray") +
  scale_colour_manual(na.value="gray")+
  labs(fill='') +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal")

frequency_trtnaive_endemicity2 <- table(check_histories_trtnaive_lastyr2$endemicity_baseline)
print(frequency_trtnaive_endemicity2)

#====================================================#
#    Tanzania non-endemic IUs to remove - March 2025 #
#====================================================#

# Remove rows where IU_CODE_MAPPING is one of the specified values
dfAPOC_included2 <- dfAPOC_included2[!dfAPOC_included2$IU_CODE_MAPPING %in% c("TZA0478846626", "TZA0479246628", 
                                                                              "TZA0480046634", "TZA0480046635", 
                                                                              "TZA0480346638", "TZA0480846644", 
                                                                              "TZA0481146645"), ]


dfAPOC_included2_NGA <- subset(dfAPOC_included2, ADMIN0ISO3 == "NGA")

row_counts <- dfAPOC_included2_NGA %>%
  group_by(IU_ID_MAPPING) %>%
  summarise(row_count = n())
row_counts


# ===================================================================================================================#
#                             Backfill histories based on pre-ESPEN/ ESPEN inclusion labels                          #
# ===================================================================================================================#

dfAPOC_included3 <- dfAPOC_included2 # speciy new dataframe so can easily reset

nrow(subset(dfAPOC_included3, Year == 2022)) # check n IUs = 2346; March 2025: 2357 IUs (see below removed 7 TZN IUs)
length(unique(dfAPOC_included3$IU_ID_MAPPING)) # 2357 IUs (April 25'- because 7 removed from TZN above: before 2364 )

# ======================== #
#     PROCEED FROM HERE    #

view(dfAPOC_report) # check coverages & start years

dfAPOC_report2 <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/APOC/APOC_data_97_13_v2.csv")
colnames(dfAPOC_report2) <- c("ADMIN0", "EpiCov", "Year")

split_vectors <- setNames(split(dfAPOC_report2$EpiCov, dfAPOC_report2$ADMIN0), unique(dfAPOC_report2$ADMIN0))

dfAPOC_included3$MDA_CDTI <- NA

# ====== #
# Angola #
# ====== #

coverages_Angola <- split_vectors$Angola
coverages_Angola 
start_year_AGO <- 7
coverages_Angola <- coverages_Angola[start_year_AGO:length(coverages_Angola)]
coverages_Angola <- coverages_Angola/100
coverages_Angola2 <- ifelse(is.na(coverages_Angola), NA, ifelse(coverages_Angola < 0.65, 0.25, 0.65))
#coverages_Angola2 <- c(0.25, 0.25, 0.25, 0.25, 0.25, 0.65, 0.25, 0.25)

# Your original condition
condition <- with(dfAPOC_included3, ADMIN0ISO3 == "AGO" & trimws(Pre_ESPEN_MDA_history) == "Include" & Year > 2004 & Year < 2013)
# Get the indices where the condition is true
indices <- which(condition)

# new #
# make col for raw Cov values (for final dataframe to publish)
dfAPOC_included3$MDA_CDTI_raw <- NA_real_
# Use indexing to assign values based on the condition
dfAPOC_included3$MDA_CDTI_raw[indices] <- rep(coverages_Angola, length.out = length(indices))

# make col to indcate if cov based on IU-level specific coverage, national-level coverage, assumption, ESPEN - data
dfAPOC_included3$cov_source <- NA
# Use indexing to assign values based on the condition
dfAPOC_included3$cov_source[indices] <- rep("national-level coverage data", length.out = length(indices))

# make col to indcate if cov based on IU-level specific coverage, national-level coverage, assumption, ESPEN - data
dfAPOC_included3$cov_specific_source <- NA
# Use indexing to assign values based on the condition
dfAPOC_included3$cov_specific_source[indices] <- rep("APOC report, 2015", length.out = length(indices))

# Use indexing to assign values based on the condition
dfAPOC_included3$MDA_CDTI[indices] <- rep(coverages_Angola2, length.out = length(indices))


# ========#
# Burundi #
# ========#

coverages_Burundi <- split_vectors$Burundi
coverages_Burundi
start_year_Burundi <- 7
coverages_Burundi <- coverages_Burundi[start_year_Burundi:length(coverages_Burundi)]
coverages_Burundi <- coverages_Burundi/100
coverages_Burundi2 <- ifelse(is.na(coverages_Burundi), NA, ifelse(coverages_Burundi < 0.65, 0.25, 0.65))

condition <- with(dfAPOC_included3, ADMIN0ISO3 == "BDI" & trimws(Pre_ESPEN_MDA_history) == "Include" & Year > 2004 & Year < 2013)
indices <- which(condition)

# Use indexing to assign values based on the condition
dfAPOC_included3$MDA_CDTI_raw[indices] <- rep(coverages_Burundi, length.out = length(indices))

# Use indexing to assign values based on the condition
dfAPOC_included3$cov_source[indices] <- rep("national-level coverage data", length.out = length(indices))

# Use indexing to assign values based on the condition
dfAPOC_included3$cov_specific_source[indices] <- rep("APOC report, 2015", length.out = length(indices))

# Use indexing to assign values based on the condition
dfAPOC_included3$MDA_CDTI[indices] <- rep(coverages_Burundi2, length.out = length(indices))



# ========= #
# Cameroon  #
# ========= #
coverages_Cameroon <- split_vectors$Cameroon
coverages_Cameroon
start_year_Cameroon <- 0
coverages_Cameroon <- coverages_Cameroon[start_year_Cameroon:length(coverages_Cameroon)]
coverages_Cameroon <- coverages_Cameroon/100
coverages_Cameroon2 <- ifelse(is.na(coverages_Cameroon), NA, ifelse(coverages_Cameroon < 0.65, 0.25, 0.65))

condition <- with(dfAPOC_included3, ADMIN0ISO3 == "CMR" & trimws(Pre_ESPEN_MDA_history) == "Include" & Year > 1998 & Year < 2013)
indices <- which(condition)

# Use indexing to assign values based on the condition
dfAPOC_included3$MDA_CDTI_raw[indices] <- rep(coverages_Cameroon, length.out = length(indices))

# Use indexing to assign values based on the condition
dfAPOC_included3$cov_source[indices] <- rep("national-level coverage data", length.out = length(indices))

# Use indexing to assign values based on the condition
dfAPOC_included3$cov_specific_source[indices] <- rep("APOC report, 2015", length.out = length(indices))

# Use indexing to assign values based on the condition
dfAPOC_included3$MDA_CDTI[indices] <- rep(coverages_Cameroon2, length.out = length(indices))

# ========= #
# CAR       #
# ========= #

coverages_CAR <- split_vectors$`Central African Republic`
coverages_CAR
start_year_CAR <- 0
coverages_CAR <- coverages_CAR[start_year_CAR:length(coverages_CAR)]
coverages_CAR <- coverages_CAR/100
coverages_CAR2 <- ifelse(is.na(coverages_CAR), NA, ifelse(coverages_CAR < 0.65, 0.25, 0.65))

condition <- with(dfAPOC_included3, ADMIN0ISO3 == "CAF" & trimws(Pre_ESPEN_MDA_history) == "Include" & Year > 1998 & Year < 2013)
indices <- which(condition)

# Use indexing to assign values based on the condition
dfAPOC_included3$MDA_CDTI_raw[indices] <- rep(coverages_CAR, length.out = length(indices))

# Use indexing to assign values based on the condition
dfAPOC_included3$cov_source[indices] <- rep("national-level coverage data", length.out = length(indices))

# Use indexing to assign values based on the condition
dfAPOC_included3$cov_specific_source[indices] <- rep("APOC report, 2015", length.out = length(indices))

# Use indexing to assign values based on the condition
dfAPOC_included3$MDA_CDTI[indices] <- rep(coverages_CAR2, length.out = length(indices))

# ========= #
# Chad (TCD)#
# ========= #

coverages_TCD <- split_vectors$Chad
coverages_TCD
start_year_TCD <- 0
coverages_TCD <- coverages_TCD[start_year_TCD:length(coverages_TCD)]
coverages_TCD <- coverages_TCD/100
coverages_TCD2 <- ifelse(is.na(coverages_TCD), NA, ifelse(coverages_TCD < 0.65, 0.25, 0.65))

condition <- with(dfAPOC_included3, ADMIN0ISO3 == "TCD" & trimws(Pre_ESPEN_MDA_history) == "Include" & Year > 1998 & Year < 2013)
indices <- which(condition)

# Use indexing to assign values based on the condition
dfAPOC_included3$MDA_CDTI_raw[indices] <- rep(coverages_CAR, length.out = length(indices))

# Use indexing to assign values based on the condition
dfAPOC_included3$cov_source[indices] <- rep("national-level coverage data", length.out = length(indices))

# Use indexing to assign values based on the condition
dfAPOC_included3$cov_specific_source[indices] <- rep("APOC report, 2015", length.out = length(indices))

# Use indexing to assign values based on the condition
dfAPOC_included3$MDA_CDTI[indices] <- rep(coverages_CAR2, length.out = length(indices))

# ============ #
# Congo (COG)  #
# ============ #

coverages_COG <- split_vectors$Congo
coverages_COG
start_year_COG <- 3
coverages_COG <- coverages_COG[start_year_COG:length(coverages_COG)]
coverages_COG <- coverages_COG/100
coverages_COG2 <- ifelse(is.na(coverages_COG), NA, ifelse(coverages_COG < 0.65, 0.25, 0.65))

condition <- with(dfAPOC_included3, ADMIN0ISO3 == "COG" & trimws(Pre_ESPEN_MDA_history) == "Include" & Year > 2000 & Year < 2013)
indices <- which(condition)

# Use indexing to assign values based on the condition
dfAPOC_included3$MDA_CDTI_raw[indices] <- rep(coverages_COG, length.out = length(indices))

# Use indexing to assign values based on the condition
dfAPOC_included3$cov_source[indices] <- rep("national-level coverage data", length.out = length(indices))

# Use indexing to assign values based on the condition
dfAPOC_included3$cov_specific_source[indices] <- rep("APOC report, 2015", length.out = length(indices))

# Use indexing to assign values based on the condition
dfAPOC_included3$MDA_CDTI[indices] <- rep(coverages_COG2, length.out = length(indices))


# ============ #
# DRC (COD)  #
# ========== #

coverages_COD <- split_vectors$`Democratic Republic of the Congo`
coverages_COD
start_year_COD <- 3
coverages_COD <- coverages_COD[start_year_COD:length(coverages_COD)]
coverages_COD <- coverages_COD/100
coverages_COD2 <- ifelse(is.na(coverages_COD), NA, ifelse(coverages_COD < 0.65, 0.25, 0.65))

condition <- with(dfAPOC_included3, ADMIN0ISO3 == "COD" & trimws(Pre_ESPEN_MDA_history) == "Include" & Year > 2000 & Year < 2013)
indices <- which(condition)
# Use indexing to assign values based on the condition
dfAPOC_included3$MDA_CDTI_raw[indices] <- rep(coverages_COD, length.out = length(indices))

# Use indexing to assign values based on the condition
dfAPOC_included3$cov_source[indices] <- rep("national-level coverage data", length.out = length(indices))

# Use indexing to assign values based on the condition
dfAPOC_included3$cov_specific_source[indices] <- rep("APOC report, 2015", length.out = length(indices))

# Use indexing to assign values based on the condition
dfAPOC_included3$MDA_CDTI[indices] <- rep(coverages_COD2, length.out = length(indices))

# ================ #
# Eq Guinea (GNQ)  #

coverages_GNQ <- split_vectors$`Equatorial Guinea`
coverages_GNQ
start_year_GNQ <- 0
coverages_GNQ <- coverages_GNQ[start_year_GNQ:length(coverages_GNQ)]
coverages_GNQ <- coverages_GNQ/100
coverages_GNQ2 <- ifelse(is.na(coverages_GNQ), NA, ifelse(coverages_GNQ < 0.65, 0.25, 0.65))

# need to make seperate vectors for the cov_soruce col due to NAs
cov_source_GNQ <- ifelse(!is.na(coverages_GNQ), "national-level coverage data", NA)
cov_source_GNQ2 <- ifelse(!is.na(coverages_GNQ), "APOC report, 2015", NA)

condition <- with(dfAPOC_included3, ADMIN0ISO3 == "GNQ" & trimws(Pre_ESPEN_MDA_history) == "Include" & Year > 1998 & Year < 2013)
indices <- which(condition)
# Use indexing to assign values based on the condition
dfAPOC_included3$MDA_CDTI_raw[indices] <- rep(coverages_GNQ, length.out = length(indices))

# Use indexing to assign values based on the condition
dfAPOC_included3$cov_source[indices] <- rep(cov_source_GNQ, length.out = length(indices))

# Use indexing to assign values based on the condition
dfAPOC_included3$cov_specific_source[indices] <- rep(cov_source_GNQ2, length.out = length(indices))

# Use indexing to assign values based on the condition
dfAPOC_included3$MDA_CDTI[indices] <- rep(coverages_GNQ2, length.out = length(indices))


# ================ #
# Ethiopia (ETH)  #

coverages_ETH <- split_vectors$Ethiopia
coverages_ETH
start_year_ETH <- 3
coverages_ETH <- coverages_ETH[start_year_ETH:length(coverages_ETH)]
coverages_ETH <- coverages_ETH/100
coverages_ETH2 <- ifelse(is.na(coverages_ETH), NA, ifelse(coverages_ETH < 0.65, 0.25, 0.65))

condition <- with(dfAPOC_included3, ADMIN0ISO3 == "ETH" & trimws(Pre_ESPEN_MDA_history) == "Include" & Year > 2000 & Year < 2013)
indices <- which(condition)
# Use indexing to assign values based on the condition
dfAPOC_included3$MDA_CDTI_raw[indices] <- rep(coverages_ETH, length.out = length(indices))

# Use indexing to assign values based on the condition
dfAPOC_included3$cov_source[indices] <- rep("national-level coverage data", length.out = length(indices))

# Use indexing to assign values based on the condition
dfAPOC_included3$cov_specific_source[indices] <- rep("APOC report, 2015", length.out = length(indices))

# Use indexing to assign values based on the condition
dfAPOC_included3$MDA_CDTI[indices] <- rep(coverages_ETH2, length.out = length(indices))

# ================ #
# Gabon (GAB)     #

coverages_GAB <- split_vectors$Gabon
coverages_GAB
start_year_GAB <- 0
coverages_GAB <- coverages_GAB[start_year_GAB:length(coverages_GAB)]
coverages_GAB <- coverages_GAB/100
coverages_GAB2 <- ifelse(is.na(coverages_GAB), NA, ifelse(coverages_GAB < 0.65, 0.25, 0.65))

# need to make seperate vectors for the cov_soruce col due to NAs
cov_source_GAB <- ifelse(!is.na(coverages_GAB), "national-level coverage data", NA)
cov_source_GAB2 <- ifelse(!is.na(coverages_GAB), "APOC report, 2015", NA)

condition <- with(dfAPOC_included3, ADMIN0ISO3 == "GAB" & trimws(Pre_ESPEN_MDA_history) == "Include" & Year > 1998 & Year < 2013)
indices <- which(condition)
# Use indexing to assign values based on the condition
dfAPOC_included3$MDA_CDTI_raw[indices] <- rep(coverages_GAB, length.out = length(indices))

# Use indexing to assign values based on the condition
dfAPOC_included3$cov_source[indices] <- rep(cov_source_GAB, length.out = length(indices))

# Use indexing to assign values based on the condition
dfAPOC_included3$cov_specific_source[indices] <- rep(cov_source_GAB2, length.out = length(indices))

# Use indexing to assign values based on the condition
dfAPOC_included3$MDA_CDTI[indices] <- rep(coverages_GAB2, length.out = length(indices))

# ================ #
# Liberia (LBR)   #

coverages_LBR <- split_vectors$Liberia
coverages_LBR
start_year_LBR <- 0
coverages_LBR <- coverages_LBR[start_year_LBR:length(coverages_LBR)]
coverages_LBR <- coverages_LBR/100
coverages_LBR2 <- ifelse(is.na(coverages_LBR), NA, ifelse(coverages_LBR < 0.65, 0.25, 0.65))

# need to make seperate vectors for the cov_soruce col due to NAs
cov_source_LBR <- ifelse(!is.na(coverages_LBR), "national-level coverage data", NA)
cov_source_LBR2 <- ifelse(!is.na(coverages_LBR), "APOC report, 2015", NA)

condition <- with(dfAPOC_included3, ADMIN0ISO3 == "LBR" & trimws(Pre_ESPEN_MDA_history) == "Include" & Year > 1998 & Year < 2013)
indices <- which(condition)
# Use indexing to assign values based on the condition
dfAPOC_included3$MDA_CDTI_raw[indices] <- rep(coverages_LBR, length.out = length(indices))

# Use indexing to assign values based on the condition
dfAPOC_included3$cov_source[indices] <- rep(cov_source_LBR, length.out = length(indices))

# Use indexing to assign values based on the condition
dfAPOC_included3$cov_specific_source[indices] <- rep(cov_source_LBR2, length.out = length(indices))

# Use indexing to assign values based on the condition
dfAPOC_included3$MDA_CDTI[indices] <- rep(coverages_LBR2, length.out = length(indices))

# ================ #
# Malawi (MWI)   #

coverages_MWI <- split_vectors$Malawi
coverages_MWI
start_year_MWI <- 0
coverages_MWI <- coverages_MWI[start_year_MWI:length(coverages_MWI)]
coverages_MWI <- coverages_MWI/100
coverages_MWI2 <- ifelse(is.na(coverages_MWI), NA, ifelse(coverages_MWI < 0.65, 0.25, 0.65))

condition <- with(dfAPOC_included3, ADMIN0ISO3 == "MWI" & trimws(Pre_ESPEN_MDA_history) == "Include" & Year > 1998 & Year < 2013)
indices <- which(condition)
# Use indexing to assign values based on the condition
dfAPOC_included3$MDA_CDTI_raw[indices] <- rep(coverages_MWI, length.out = length(indices))

# Use indexing to assign values based on the condition
dfAPOC_included3$cov_source[indices] <- rep("national-level coverage data", length.out = length(indices))

# Use indexing to assign values based on the condition
dfAPOC_included3$cov_specific_source[indices] <- rep("APOC report, 2015", length.out = length(indices))

# Use indexing to assign values based on the condition
dfAPOC_included3$MDA_CDTI[indices] <- rep(coverages_MWI2, length.out = length(indices))


# ================ #
# Nigeria (NGA)   #

# == check ESPEN 2022 update (May 2024) ======= #
#  Now IUs updated at start in parent_child_df  #
#
# NGA_2022_EPSEN <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/ESPEN updates to check/ESPEN updated data for 2022/data-NGA-Oncho-iu-2022.csv")
# NGA_MDA_2022 <- subset(NGA_2022_EPSEN, EpiCov > 0) #find those IUs with epicov > 0
# nrow(NGA_MDA_2022) # 224 IUs with MDA in 2022 with new ESPEN update
#
# IUs_NGA_MDA_2022_ESPEN <- NGA_MDA_2022$IU_ID
# length(IUs_NGA_MDA_2022_ESPEN) # 224 IU_IDs
#
# # new_MDA_NGA_2022 <- subset(NGA_MDA_2022, Cum_MDA == 1) # there no IUs with MDA for first time in 2022!
# # new_IUs_NGA <- new_MDA_NGA_2022$IU_ID
#
# dfAPOC_NGA_2022_withMDA <- subset(dfAPOC_included4, ADMIN0ISO3 == "NGA" & Year == 2022 & EpiCov > 0) # currently 0 IUs with MDA in 2022
# nrow(dfAPOC_NGA_2022_withMDA)
#
# dfAPOC_NGA_2022 <- subset(dfAPOC_included4, ADMIN0ISO3 == "NGA" & Year == 2022) # currently 0 IUs with MDA in 2022
# nrow(dfAPOC_NGA_2022) # 732 IUs present in 2022
# IUs_present_NGA_2022 <- dfAPOC_NGA_2022$IU_ID
#
# Any_IUs_not_present_histories_ESPEN2022update_NGA_withMDA <- setdiff(IUs_NGA_MDA_2022_ESPEN, IUs_present_NGA_2022)
# dfAPOC_NGA_MDAin2022 <- subset(dfAPOC_NGA_2022, IU_ID %in% IUs_NGA_MDA_2022_ESPEN) # all 224 IUs updated in ESPEN are found in our histories
#                                                                                    # but without MDA in 2022 = UPDATE at start of this script!


# APPROACH 1 : based on APOC 2015 report #
# coverages_NGA <- split_vectors$Nigeria
# start_year_NGA <- 0
# coverages_NGA <- coverages_NGA[start_year_NGA:length(coverages_NGA)]
# coverages_NGA <- coverages_NGA/100
# coverages_NGA2 <- ifelse(is.na(coverages_NGA), NA, ifelse(coverages_NGA < 0.65, 0.25, 0.65))
#
# condition <- with(dfAPOC_included3, ADMIN0ISO3 == "NGA" & trimws(Pre_ESPEN_MDA_history) == "Include" & Year > 1998 & Year < 2013)
# indices <- which(condition)
# dfAPOC_included3$MDA_CDTI[indices] <- rep(coverages_NGA2, length.out = length(indices))

dfAPOC_included3_NGA <- subset(dfAPOC_included3, ADMIN0ISO3 == "NGA")

row_counts <- dfAPOC_included3_NGA %>%
  group_by(IU_ID_MAPPING) %>%
  summarise(row_count = n())
row_counts


# APPROACH 2: based on national oncho snapshot data #
oncho_snapshot_NGA <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/data from CC/oncho_snapshot_NGA.csv")
oncho_snapshot_NGA <- oncho_snapshot_NGA %>%   rename_with(~gsub("^X", "", .), starts_with("X1990"):ends_with("X2021"))

columns_to_update <- as.character(seq(1990, 2021))
oncho_snapshot_NGA2 <- oncho_snapshot_NGA %>%
  mutate(across(all_of(columns_to_update),
                ~ case_when(
                  . == 0 ~ 0,
                  . < 65 ~ 0.25,
                  . >= 65 ~ 0.65,
                  TRUE ~ .  # Keep the original value if none of the conditions match
                )
  ))

dfAPOC_included3_NGA <- subset(dfAPOC_included3, ADMIN0 == "Nigeria") # test in this : dfAPOC_included3_NGA

oncho_snapshot_long <- oncho_snapshot_NGA2 %>%
  pivot_longer(cols = starts_with("1990"):ends_with("2021"),
               names_to = "Year",
               values_to = "Coverage_value_tmp") # Reshape the oncho_snapshot_NGA dataframe from wide to long format

# Adding raw coverages and cov data source cols - Feb 2025 #
# needed for raw cov values #
oncho_snapshot_long_raw <- oncho_snapshot_NGA %>%
  pivot_longer(cols = starts_with("1990"):ends_with("2021"),
               names_to = "Year",
               values_to = "Coverage_value_tmp") # Reshape the oncho_snapshot_NGA dataframe from wide to long format

oncho_snapshot_long$Coverage_value_tmp_raw <- oncho_snapshot_long_raw$Coverage_value_tmp/100 # new col with raw cov values
# make cols for cov_source and cov_specific_source with ifelse statements
oncho_snapshot_long$Coverage_source_temp <- ifelse(!is.na(oncho_snapshot_long$Coverage_value_tmp_raw), "IU-level coverage", NA)
oncho_snapshot_long$Coverage_source_temp2 <- ifelse(!is.na(oncho_snapshot_long$Coverage_value_tmp_raw), "FMOH national oncho snapshot", NA)

# continue as before
oncho_snapshot_long$Year <- as.numeric(oncho_snapshot_long$Year) # make numeric to merge

dfAPOC_included3_NGA_merged <- dfAPOC_included3_NGA %>%
  left_join(oncho_snapshot_long, by = c("ADMIN0", "ADMIN1", "ADMIN2", "Year")) # Merge the dataframes based on common columns

dfAPOC_included3_NGA_merged$MDA_CDTI[is.na(dfAPOC_included3_NGA_merged$MDA_CDTI)] <- dfAPOC_included3_NGA_merged$Coverage_value_tmp[is.na(dfAPOC_included3_NGA_merged$MDA_CDTI)]

# for raw cov and cov data source cols - feb 25
dfAPOC_included3_NGA_merged$MDA_CDTI_raw[is.na(dfAPOC_included3_NGA_merged$MDA_CDTI_raw)] <- dfAPOC_included3_NGA_merged$Coverage_value_tmp_raw[is.na(dfAPOC_included3_NGA_merged$MDA_CDTI_raw)]
dfAPOC_included3_NGA_merged$cov_source[is.na(dfAPOC_included3_NGA_merged$cov_source)] <- dfAPOC_included3_NGA_merged$Coverage_source_temp[is.na(dfAPOC_included3_NGA_merged$cov_source)]
dfAPOC_included3_NGA_merged$cov_specific_source[is.na(dfAPOC_included3_NGA_merged$cov_specific_source)] <- dfAPOC_included3_NGA_merged$Coverage_source_temp2[is.na(dfAPOC_included3_NGA_merged$cov_specific_source)]

#dfAPOC_included3_NGA_merged <- dfAPOC_included3_NGA_merged %>% select(1:69) # remove additional new cols from snapshot to same cols as dfAPOC_included3_NGA
dfAPOC_included3_NGA_merged <- dfAPOC_included3_NGA_merged %>% select(1:72) # remove additional new cols from snapshot to same cols as dfAPOC_included3_NGA

rows_to_keep <- dfAPOC_included3 %>%
  anti_join(dfAPOC_included3_NGA_merged, by = c("ADMIN0", "ADMIN1", "ADMIN2", "Year")) # identify rows in dfAPOC_included3 that are not in merged_df

dfAPOC_included3 <- bind_rows(rows_to_keep, dfAPOC_included3_NGA_merged) # update dfAPOC_included3 by including updated nigeria rows with covs

extra_rows <- setdiff(dfAPOC_included3_NGA_merged, dfAPOC_included3_NGA)

check_df <- subset(dfAPOC_included3, Year == 2022)
nrow(check_df) # 2357 - (April 25')
length(unique(dfAPOC_included3$IU_ID_MAPPING)) # 2357 in April 25'

# ===================================================================================== #
#     March 2025 - remove any NGA Ius newly introdcued that are non-endemic in snapshot 
#    (re-introduced because now have a baseline-endemicity)                             #

#oncho_snapshot_long_nonendemic <- subset(oncho_snapshot_long, Endemic_classification %in% c("Not Endemic", "Not Endemic "))
oncho_snapshot_long_nonendemic <- subset(
  oncho_snapshot_long, 
  Endemic_classification %in% c("Not Endemic", "Not Endemic ") & 
    !ADMIN2 %in% c("Enugu North", "Yola North") # these two are considered "Non-endemic" in oncho snapshot, 
                                                # however have values in "Recommended treatment plan" and/or "MDA ever started"/
                                                # "Year MDA started" + coverage values. Not other "Non-endemic" LGAs have coverage value
                                                # but Recommended treatment plan" and/or "MDA ever started" do not have values so not
                                                # included with treatment (Oncho snapshot 2022 check: 07-04-2025)
)
NGA_noneendemic <- unique(oncho_snapshot_long_nonendemic$ADMIN2)

missing_values <- setdiff(NGA_noneendemic, dfAPOC_included3$IUs_NAME_MAPPING) # check if any non-endemic IUs in snapshot not found in ESPEN
missing_values
# Remove rows where IU_CODE_MAPPING is one of the specified values
NGA_to_removesubset <- subset(dfAPOC_included3, IUs_NAME_MAPPING %in% NGA_noneendemic)
IU_ID_NGA_toremove <- unique(NGA_to_removesubset$IU_CODE_MAPPING)
length(IU_ID_NGA_toremove) # 248 IUs

# find duplicates #
NGA_to_removesubset_2022 <- subset(NGA_to_removesubset, Year == 2012) # 249 IUs (ITs actually 244 IUs in step below)
value_counts <- table(NGA_to_removesubset_2022$IUs_NAME_MAPPING)
value_counts

NGA_noneendemic <- NGA_noneendemic[NGA_noneendemic != "Obi"] # remove this for now

#dfAPOC_included3_NGAtoremove <- dfAPOC_included3[dfAPOC_included3$IUs_NAME_MAPPING %in% NGA_noneendemic, ]
#dfAPOC_included3_NGAtoremove_2022 <- subset(dfAPOC_included3_NGAtoremove, Year == 2022)
dfAPOC_included3 <- dfAPOC_included3[!dfAPOC_included3$IUs_NAME_MAPPING %in% NGA_noneendemic, ]



# extra to remove (names mismatch in ESPEN/snapshot) =- 6 in total 
extra_to_remove_NGA <- c("Quan'Pam", "Langtan North","Langtan South", "Kanem")
dfAPOC_included3 <- dfAPOC_included3[!dfAPOC_included3$IUs_NAME_MAPPING %in% extra_to_remove_NGA, ]
dfAPOC_included3 <- dfAPOC_included3[!(dfAPOC_included3$IUs_NAME_MAPPING == "Obi" & dfAPOC_included3$ADMIN1 == "Nasarawa"), ]

# SO IN TOTAL 244 + 6 removed = 250 IUs in NGA to remove b/c non-endemic in SS

# dfAPOC_included3_NGA_merged_Toungo <- subset(dfAPOC_included3_NGA_merged, ADMIN2 == "Toungo")
# dfAPOC_included3_NGA_Toungo <- subset(dfAPOC_included3_NGA, ADMIN2 == "Toungo")
admin2_counts_df <- dfAPOC_included3_NGA_merged %>%
  group_by(ADMIN2) %>%
  summarise(Count = n())

# APPROACH 3: based on CC data #  - note APPROACH #2 supplants this
# load in CC MDA history data #
NGA_strt_stp_LGA <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/data from CC/NGA_strt_stp_LGA.csv")
#dfAPOC_included3_NGA <- subset(dfAPOC_included3, ADMIN0 == "Nigeria")

# # make sure all LGA names match ADMIN1 names in main dataframe (or find closest matches)
# # Extract the 'ADMIN2' and 'LGA' columns from the data frames
# NGA_snapshot_LGAnames <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/data from CC/oncho_snapshot_NGA_LGAnames.csv")
# admin2_values <- na.omit(unique(dfAPOC_included3_NGA$ADMIN2))
# lga_values <- na.omit(unique(NGA_strt_stp_LGA$LGA))
#
# # Function to find the closest match for each 'LGA' value in 'ADMIN2'
# find_closest_match <- function(value, choices) {
#   distances <- stringdist::stringdistmatrix(value, choices)
#   min_index <- which.min(distances)
#   return(list(match = choices[min_index], score = 1 - distances[min_index]))
# }
#
# # Create a data frame to store the results
# result_df <- data.frame(LGA = character(), Closest_ADMIN2 = character(), Score = numeric(), stringsAsFactors = FALSE)
#
# # Iterate through each 'LGA' value and find the closest match in 'ADMIN2'
# for (lga_value in lga_values) {
#   match_info <- find_closest_match(lga_value, admin2_values)
#   result_df <- rbind(result_df, c(lga_value, match_info$match, match_info$score))
# }
#
# # Rename the columns in the result data frame
# colnames(result_df) <- c('LGA', 'Closest_ADMIN2', 'Score')

# ============================================ #
# matching names from the oncho snapshot data  #
NGA_snapshot_LGAnames <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/data from CC/oncho_snapshot_NGA_LGAnames.csv")
NGA_snapshot_LGAnames <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/data from CC/LGAs_names_missing_tocheck.csv")
# admin2_values <- na.omit(unique(dfAPOC_included3_NGA$ADMIN2))
# lga_values <- na.omit(unique(NGA_snapshot_LGAnames$LGA))
# # Function to find the closest match for each 'LGA' value in 'ADMIN2'
# find_closest_match <- function(value, choices) {
#   distances <- stringdist::stringdistmatrix(value, choices)
#   min_index <- which.min(distances)
#   return(list(match = choices[min_index], score = 1 - distances[min_index]))
# }
# # Create a data frame to store the results
# result_df <- data.frame(LGA = character(), Closest_ADMIN2 = character(), Score = numeric(), stringsAsFactors = FALSE)
# # Iterate through each 'LGA' value and find the closest match in 'ADMIN2'
# for (lga_value in lga_values) {
#   match_info <- find_closest_match(lga_value, admin2_values)
#   result_df <- rbind(result_df, c(lga_value, match_info$match, match_info$score))
# }
# # Rename the columns in the result data frame
# colnames(result_df) <- c('LGA', 'Closest_ADMIN2', 'Score')
# # write.csv(result_df, file = "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/data from CC/LGAs_names_closestmatch.csv", row.names = FALSE)
# # write.csv(result_df, file = "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/data from CC/LGAs_names_closestmatch2.csv", row.names = FALSE)

# # APPROACH 3 USING CC DATA FROM HERE #
#
# # group LGAs on basis of MDA start and MDA end years
NGA_strt_stp_LGA$RB_MDA_Start <- as.numeric(NGA_strt_stp_LGA$RB_MDA_Start)
NGA_strt_stp_LGA$RB_MDA_End <- as.numeric(NGA_strt_stp_LGA$RB_MDA_End)

NGA_strt_stp_LGA$YearRange <- paste(NGA_strt_stp_LGA$RB_MDA_Start, NGA_strt_stp_LGA$RB_MDA_End, sep = ',')

NGA_grouped_LGA <- NGA_strt_stp_LGA %>%
  group_by(YearRange) %>%
  summarise(UniqueLGAs = toString(unique(LGA_updated))) # group LGAs

NGA_grouped_LGA <- NGA_grouped_LGA %>%
  separate(YearRange, into = c("RB_strt_yr", "RB_end_yr"), sep = ",", convert = TRUE) # split into 2 numeric val columns for strt and end yr
#
# # updated vector of coverage values (accounting for CC data from 1992)
# coverages_NGA <- split_vectors$Nigeria
# start_year_NGA <- 0
# coverages_NGA <- coverages_NGA[start_year_NGA:length(coverages_NGA)]
# coverages_NGA <- coverages_NGA/100
# coverages_NGA_all <- ifelse(is.na(coverages_NGA), NA, ifelse(coverages_NGA < 0.65, 0.25, 0.65))
#
# coverages_NGA1 <- c(rep(0.25, 7), coverages_NGA_all) # add 7 x 0.25 values at front of vector for 1992 - 1998 years (CC data)
#
# # find conditions for each combination of RB_start_yr/RB_end_yr by LGA/ADMIN2
# # Extract the element at location [1, 3]
# # first group of LGAs
# LGAs1 <- unlist(strsplit(as.character(NGA_grouped_LGA[1, 3]), ",\\s*")) # Split the string into a character vector
# condition1 <- with(dfAPOC_included3, ADMIN0ISO3 == "NGA" & Year > 1991 & Year < 2013 & ADMIN2 %in% LGAs1)
# indices <- which(condition1)
# dfAPOC_included3$MDA_CDTI[indices] <- rep(coverages_NGA1, length.out = length(indices))
#
# # 2nd group of LGAs
# LGAs2a <- unlist(strsplit(as.character(NGA_grouped_LGA[2, 3]), ",\\s*")) # Split the string into a character vector
# LGAs2b <- unlist(strsplit(as.character(NGA_grouped_LGA[3, 3]), ",\\s*")) # Split the string into a character vector
# LGAs2 <- c(LGAs2a,LGAs2b) # add vectors together
# condition2 <- with(dfAPOC_included3, ADMIN0ISO3 == "NGA" & Year > 1992 & Year < 2013 & ADMIN2 %in% LGAs2)
# indices <- which(condition2)
# coverages_NGA2 <- coverages_NGA1[-1] # update coverage vector for this set of LGAs (remove first cov value)
# dfAPOC_included3$MDA_CDTI[indices] <- rep(coverages_NGA2, length.out = length(indices))
#
# # 3rd group of LGAs
# LGAs3a <- unlist(strsplit(as.character(NGA_grouped_LGA[4, 3]), ",\\s*")) # Split the string into a character vector
# LGAs3b <- unlist(strsplit(as.character(NGA_grouped_LGA[5, 3]), ",\\s*")) # Split the string into a character vector
# LGAs3c <- unlist(strsplit(as.character(NGA_grouped_LGA[6, 3]), ",\\s*")) # Split the string into a character vector
# LGAs3 <- c(LGAs3a,LGAs3b,LGAs3c) # add vectors together
# condition3 <- with(dfAPOC_included3, ADMIN0ISO3 == "NGA" & Year > 1993 & Year < 2013 & ADMIN2 %in% LGAs3)
# indices <- which(condition3)
# coverages_NGA3 <- coverages_NGA2[-1] # update coverage vector for this set of LGAs
# dfAPOC_included3$MDA_CDTI[indices] <- rep(coverages_NGA3, length.out = length(indices))
#
# # 4th group of LGAs
# LGAs4a <- unlist(strsplit(as.character(NGA_grouped_LGA[7, 3]), ",\\s*")) # Split the string into a character vector
# LGAs4b <- unlist(strsplit(as.character(NGA_grouped_LGA[8, 3]), ",\\s*")) # Split the string into a character vector
# LGAs4c <- unlist(strsplit(as.character(NGA_grouped_LGA[9, 3]), ",\\s*")) # Split the string into a character vector
# LGAs4 <- c(LGAs4a,LGAs4b,LGAs4c) # add vectors together
# condition4 <- with(dfAPOC_included3, ADMIN0ISO3 == "NGA" & Year > 1994 & Year < 2013 & ADMIN2 %in% LGAs4)
# indices <- which(condition4)
# coverages_NGA4 <- coverages_NGA3[-1] # update coverage vector for this set of LGAs
# dfAPOC_included3$MDA_CDTI[indices] <- rep(coverages_NGA4, length.out = length(indices))

# 5th group of LGAs
# start in 2018 - Abakaliki, Ohaukwu; just check if represented in ESPEN database

# # remove MDA in 2020 and 2021 for certain LGAs (CC data)
# NGA_2020_2021_noMDA <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/data from CC/NGA_2020_2021_noMDA.csv")
#
# # 2020 LGAs to remove MDA
# excl_2020_NGA <- as.character(NGA_2020_2021_noMDA$X2020)
# condition_2020 <- with(dfAPOC_included3, ADMIN0ISO3 == "NGA" & Year == 2020 & ADMIN2 %in% excl_2020_NGA)
# indices <- which(condition_2020)
# dfAPOC_included3$MDA_CDTI[indices] <- rep(NA, length.out = length(indices))
#
# # 2021 LGAs to remove MDA
# excl_2021_NGA <- as.character(NGA_2020_2021_noMDA$X2021)
# excl_2021_NGA <- excl_2021_NGA[nzchar(excl_2021_NGA)] # remove empty ""
# condition_2021 <- with(dfAPOC_included3, ADMIN0ISO3 == "NGA" & Year == 2020 & ADMIN2 %in% excl_2021_NGA)
# indices <- which(condition_2021)
# dfAPOC_included3$MDA_CDTI[indices] <- rep(NA, length.out = length(indices))

# # non-CC States #
# NGA_states_MDA <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/data from CC/NGA_states_MDA.csv")
#
# NGA_states_MDA_nonCC <- subset(NGA_states_MDA, Pre.ESPEN.MDA == 1)
# NGA_MDA_nonCC_vec <- unique(NGA_states_MDA_nonCC$State2)
#
# # need to assign non-CC states (identified as + MDA pre-ESPEN) with standard NGA MDA history
# # if classified as 'included' in the Pre-ESEPN label
# coverages_NGA_APOC15 <- split_vectors$Nigeria # based on APOC 2015 report #
# start_year_NGA <- 0
# coverages_NGA_APOC15 <- coverages_NGA_APOC15[start_year_NGA:length(coverages_NGA_APOC15)]
# coverages_NGA_APOC15 <- coverages_NGA_APOC15/100
# coverages_NGA_APOC15 <- ifelse(is.na(coverages_NGA_APOC15), NA, ifelse(coverages_NGA_APOC15 < 0.65, 0.25, 0.65))
#
# condition <- with(dfAPOC_included3, ADMIN0ISO3 == "NGA" & trimws(Pre_ESPEN_MDA_history) == "Include" &
#                     ADMIN1 %in% NGA_MDA_nonCC_vec & Year > 1998 & Year < 2013)
# indices <- which(condition)
# dfAPOC_included3$MDA_CDTI[indices] <- rep(coverages_NGA_APOC15, length.out = length(indices))

#write.csv(dfAPOC_included3_NGA_merged, "NGA_epsen_snapshot.csv") # csv for NGA with snapshot info (before merging fully)

# # ================================================================#
# #   identified duplicate rows in some IUs in NGA in certain years #
# 
# # ========================================= #
# #   duplicates based on endemicity_baseline #
# dfAPOC_included3_NGA <- subset(dfAPOC_included3, ADMIN0ISO3 == "NGA")
# 
# row_counts <- dfAPOC_included3_NGA %>%
#   group_by(IU_ID_MAPPING) %>%
#   summarise(row_count = n())
# row_counts # find IU_ID_MAPPING where number of rows exceed expected (>48)
# 
# row_counts_subset <- subset(row_counts, row_count > 51)
# IUs_NGAs_duplicates <- row_counts_subset$IU_ID_MAPPING # 66 IUs are duplicated
# 
# dfAPOC_included3_NGA_duplicates <- subset(dfAPOC_included3_NGA, IU_ID_MAPPING %in% IUs_NGAs_duplicates) 
# 
# # find those duplicates with incorrect baseline endemicity (check the 2012 endemicity)
# 
# dfAPOC_included3_NGA_duplicates_2012 <- subset(dfAPOC_included3_NGA_duplicates, Year == 2012)
# 
# lookup_NGA_duplicates <- data.frame(IU_ID_MAPPING = dfAPOC_included3_NGA_duplicates_2012$IU_ID_MAPPING,
#                                     endemicity_baseline_check = dfAPOC_included3_NGA_duplicates_2012$endemicity_baseline)
# 
# # Perform a left join between df and lookup_NGA_duplicates based on IU_ID
# dfAPOC_included3 <- dfAPOC_included3 %>%
#   left_join(lookup_NGA_duplicates, by = "IU_ID_MAPPING")  # Adjust the column name if necessary
# 
# # Filter out rows where the endemicity_baseline values do not match
# dfAPOC_included3 <- dfAPOC_included3 %>%
#   dplyr::filter(is.na(endemicity_baseline_check) | endemicity_baseline == endemicity_baseline_check) %>%  # Use endemicity_baseline.y after the join
#   select(-endemicity_baseline_check)  # Remove the duplicate column after the join
# 
# # ========================================= #
# #   duplicates based on mean                #
# dfAPOC_included3_NGA <- subset(dfAPOC_included3, ADMIN0ISO3 == "NGA")
# 
# row_counts <- dfAPOC_included3_NGA %>%
#   group_by(IU_ID_MAPPING) %>%
#   summarise(row_count = n())
# row_counts # find IU_ID_MAPPING where number of rows exceed expected (>48)
# 
# row_counts_subset <- subset(row_counts, row_count > 51)
# IUs_NGAs_duplicates <- row_counts_subset$IU_ID_MAPPING # 66 IUs are duplicated
# 
# dfAPOC_included3_NGA_duplicates <- subset(dfAPOC_included3_NGA, IU_ID_MAPPING %in% IUs_NGAs_duplicates) 
# 
# # find those duplicates with incorrect mean (check the 2012 endemicity)
# dfAPOC_included3_NGA_duplicates_2012 <- subset(dfAPOC_included3_NGA_duplicates, Year == 2012)
# 
# lookup_NGA_duplicates <- data.frame(IU_ID_MAPPING = dfAPOC_included3_NGA_duplicates_2012$IU_ID_MAPPING,
#                                     mean_check = dfAPOC_included3_NGA_duplicates_2012$mean)
# 
# # Perform a left join between df and lookup_NGA_duplicates based on IU_ID
# dfAPOC_included3 <- dfAPOC_included3 %>%
#   left_join(lookup_NGA_duplicates, by = "IU_ID_MAPPING")  # Adjust the column name if necessary
# 
# # Filter out rows where the endemicity_baseline values do not match
# dfAPOC_included3 <- dfAPOC_included3 %>%
#   dplyr::filter(is.na(mean_check) | mean == mean_check) %>%  # Use endemicity_baseline.y after the join
#   select(-mean_check)  # Remove the duplicate column after the join
# 
# # now check
# dfAPOC_included3_NGA <- subset(dfAPOC_included3, ADMIN0ISO3 == "NGA")
# dfAPOC_included3_NGA$year_tmp <- dfAPOC_included3_NGA$Year
# 
# row_counts <- dfAPOC_included3_NGA %>%
#   group_by(IU_ID_MAPPING) %>%
#   summarise(row_count = n())
# row_counts

# row_counts <- dfAPOC_included3 %>%
#   group_by(IU_ID_MAPPING) %>%
#   summarise(row_count = n())
# row_counts

# # Select rows 97 and 98
# df_selected <- dfAPOC_included3_NGA_duplicates %>% slice(97, 98)
# are_identical <- identical(df_selected[1, ], df_selected[2, ])
# are_identical
# differences <- df_selected[1, ] != df_selected[2, ]# Compare rows 97 and 98 and show differences
# differences_columns <- names(differences[differences == TRUE]) # Show columns where values differ

# check number of IUs after removing NGA duplicates

check_df <- subset(dfAPOC_included3, Year == 2022)
nrow(check_df) # 2107 IUs (this is because 2357 IUs - 250 classified as non-endemic in NGA national oncho snapshot = 2107 IUs)
length(unique(check_df$IU_ID_MAPPING)) # 2107 IUs

nrow(check_df)

# ================  #
# South Sudan (SSD) #

coverages_SSD <- split_vectors$`South Sudan`
coverages_SSD # NAs found
start_year_SSD <- 0
coverages_SSD <- coverages_SSD[start_year_SSD:length(coverages_SSD)]
coverages_SSD <- coverages_SSD/100
coverages_SSD2 <- ifelse(is.na(coverages_SSD), NA, ifelse(coverages_SSD < 0.65, 0.25, 0.65))

# need to make seperate vectors for the cov_soruce col due to NAs
cov_source_SSD <- ifelse(!is.na(coverages_SSD), "national-level coverage data", NA)
cov_source_SSD2 <- ifelse(!is.na(coverages_SSD), "APOC report, 2015", NA)

condition <- with(dfAPOC_included3, ADMIN0ISO3 == "SSD" & trimws(Pre_ESPEN_MDA_history) == "Include" & Year > 1998 & Year < 2013)
indices <- which(condition)
# Use indexing to assign values based on the condition
dfAPOC_included3$MDA_CDTI_raw[indices] <- rep(coverages_SSD, length.out = length(indices))

# Use indexing to assign values based on the condition
dfAPOC_included3$cov_source[indices] <- rep(cov_source_SSD, length.out = length(indices))

# Use indexing to assign values based on the condition
dfAPOC_included3$cov_specific_source[indices] <- rep(cov_source_SSD2, length.out = length(indices))

# Use indexing to assign values based on the condition
dfAPOC_included3$MDA_CDTI[indices] <- rep(coverages_SSD2, length.out = length(indices))

check_df <- subset(dfAPOC_included3, Year == 2022)
nrow(check_df) # 2107 IUs 
length(unique(check_df$IU_ID_MAPPING)) # 2107 IUs

# ================  #
# Tanzania (TZA)    #

coverages_TZA <- split_vectors$`Tanzania (Mainland)`
coverages_TZA
start_year_TZA <- 0
coverages_TZA <- coverages_TZA[start_year_TZA:length(coverages_TZA)]
coverages_TZA <- coverages_TZA/100
coverages_TZA2 <- ifelse(is.na(coverages_TZA), NA, ifelse(coverages_TZA < 0.65, 0.25, 0.65))

condition <- with(dfAPOC_included3, ADMIN0ISO3 == "TZA" & trimws(Pre_ESPEN_MDA_history) == "Include" & Year > 1998 & Year < 2013)
indices <- which(condition)
# Use indexing to assign values based on the condition
dfAPOC_included3$MDA_CDTI_raw[indices] <- rep(coverages_TZA, length.out = length(indices))

# Use indexing to assign values based on the condition
dfAPOC_included3$cov_source[indices] <- rep("national-level coverage data", length.out = length(indices))

# Use indexing to assign values based on the condition
dfAPOC_included3$cov_specific_source[indices] <- rep("APOC report, 2015", length.out = length(indices))

# Use indexing to assign values based on the condition
dfAPOC_included3$MDA_CDTI[indices] <- rep(coverages_TZA2, length.out = length(indices))

check_df <- subset(dfAPOC_included3, Year == 2022)
nrow(check_df) # 2107 IUs 
length(unique(check_df$IU_ID_MAPPING)) # 2107 IUs

# ================  #
# Uganda (UGA)      #
# ================  #

#dfAPOC_included3_UGA <- subset(dfAPOC_included3, ADMIN0 == "Uganda")
#UGA_admin2 <- sort(unique(dfAPOC_included3_UGA$ADMIN2))

# # old = based only on APOC, 2015 report data #
# coverages_UGA <- split_vectors$Uganda
# start_year_UGA <- 0
# coverages_UGA <- coverages_UGA[start_year_UGA:length(coverages_UGA)]
# coverages_UGA <- coverages_UGA/100
# coverages_UGA2 <- ifelse(is.na(coverages_UGA), NA, ifelse(coverages_UGA < 0.65, 0.25, 0.65))
#
# condition <- with(dfAPOC_included3, ADMIN0ISO3 == "UGA" & trimws(Pre_ESPEN_MDA_history) == "Include" & Year > 1998 & Year < 2013)
# indices <- which(condition)
# dfAPOC_included3$MDA_CDTI[indices] <- rep(coverages_UGA2, length.out = length(indices))


UGA_strt_stp <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/UGA_Strt_Stp.csv")
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

# updated vector of coverage values 
coverages_UGA <- split_vectors$Uganda
coverages_UGA 
start_year_UGA <- 0
coverages_UGA <- coverages_UGA[start_year_UGA:length(coverages_UGA)]
coverages_UGA <- coverages_UGA/100
coverages_UGA_all <- ifelse(is.na(coverages_UGA), NA, ifelse(coverages_UGA < 0.65, 0.25, 0.65))

coverages_UGA1 <- c(rep(0.25, 9), coverages_UGA_all) # add 9 x 0.25 values at front of vector for 1990 - 1998 years (Katabarwa et al. data)
                                                     # assume pre-1999 non-CDTI therefore coverages low
# find conditions for each combination of RB_start_yr/RB_end_yr by district/ADMIN2
# Extract the element at location [1, 3]

# ======================== #
# first group of districts #
coverages_UGA1 <- c(rep(0.25, 9), coverages_UGA_all) # add 9 x 0.25 values at front of vector for 1990 - 1998 years (Katabarwa et al. data)
                                                     # assume pre-1999 non-CDTI therefore coverages low
districts1 <- unlist(strsplit(as.character(UGA_grouped_district[1, 3]), ",\\s*")) # Split the string into a character vector
condition1 <- with(dfAPOC_included3, ADMIN0ISO3 == "UGA" & Year > 1989 & Year < 2013 & ADMIN2 %in% districts1)
indices <- which(condition1)
dfAPOC_included3$MDA_CDTI[indices] <- rep(coverages_UGA1, length.out = length(indices))# Use indexing to assign values based on the condition

# for years covered by APOC, 2015 report
condition2 <- with(dfAPOC_included3, ADMIN0ISO3 == "UGA" & Year > 1998 & Year < 2013 & ADMIN2 %in% districts1) # valuyes just for 1999 - 2012 from APOC report
indices <- which(condition2)
dfAPOC_included3$MDA_CDTI_raw[indices] <- rep(coverages_UGA, length.out = length(indices))# Use indexing to assign values based on the condition
dfAPOC_included3$cov_source[indices] <- rep("national-level coverage data", length.out = length(indices))# Use indexing to assign values based on the condition
dfAPOC_included3$cov_specific_source[indices] <- rep("APOC report, 2015", length.out = length(indices))# Use indexing to assign values based on the condition

# for years pre-APOC, 2015 report; Katabarwa indicates cvoerage (assumed non-CDTI)
condition3 <- with(dfAPOC_included3, ADMIN0ISO3 == "UGA" & Year > 1989 & Year < 1999 & ADMIN2 %in% districts1) # valuyes just for 1999 - 2012 from APOC report
indices <- which(condition3)
dfAPOC_included3$cov_source[indices] <- rep("assumed", length.out = length(indices))# Use indexing to assign values based on the condition
dfAPOC_included3$cov_specific_source[indices] <- rep("Katabarwa et al. 2018 indicates MDA (no coverage; assumed non-CDTI)", length.out = length(indices))# Use indexing to assign values based on the condition

# ======================== #
# second group of districts #
coverages_UGA2 <- coverages_UGA1[-c(1, length(coverages_UGA1))] #remove first and last element
districts2 <- unlist(strsplit(as.character(UGA_grouped_district[2, 3]), ",\\s*")) # Split the string into a character vector
condition2 <- with(dfAPOC_included3, ADMIN0ISO3 == "UGA" & Year > 1990 & Year < 2012 & ADMIN2 %in% districts2)
indices <- which(condition2)
dfAPOC_included3$MDA_CDTI[indices] <- rep(coverages_UGA2, length.out = length(indices))

# for years covered by APOC, 2015 report
condition2 <- with(dfAPOC_included3, ADMIN0ISO3 == "UGA" & Year > 1998 & Year < 2012 & ADMIN2 %in% districts2) # valuyes just for 1999 - 2012 from APOC report
indices <- which(condition2)
dfAPOC_included3$MDA_CDTI_raw[indices] <- rep(coverages_UGA, length.out = length(indices))# Use indexing to assign values based on the condition
dfAPOC_included3$cov_source[indices] <- rep("national-level coverage data", length.out = length(indices))# Use indexing to assign values based on the condition
dfAPOC_included3$cov_specific_source[indices] <- rep("APOC report, 2015", length.out = length(indices))# Use indexing to assign values based on the condition

# for years pre-APOC, 2015 report; Katabarwa indicates cvoerage (assumed non-CDTI)
condition3 <- with(dfAPOC_included3, ADMIN0ISO3 == "UGA" & Year > 1990 & Year < 1999 & ADMIN2 %in% districts2) # valuyes just for 1999 - 2012 from APOC report
indices <- which(condition3)
dfAPOC_included3$cov_source[indices] <- rep("assumed", length.out = length(indices))# Use indexing to assign values based on the condition
dfAPOC_included3$cov_specific_source[indices] <- rep("Katabarwa et al. 2018 indicates MDA (no coverage; assumed non-CDTI)", length.out = length(indices))# Use indexing to assign values based on the condition

# ======================== #
# third group of districts #
coverages_UGA3 <- coverages_UGA1[-1] # remove first element
districts3a <- unlist(strsplit(as.character(UGA_grouped_district[3, 3]), ",\\s*")) # Split the string into a character vector
districts3b <- unlist(strsplit(as.character(UGA_grouped_district[4, 3]), ",\\s*")) # Split the string into a character vector
districts3 <- c(districts3a,districts3b)
condition3 <- with(dfAPOC_included3, ADMIN0ISO3 == "UGA" & Year > 1990 & Year < 2013 & ADMIN2 %in% districts3)
indices <- which(condition3)
dfAPOC_included3$MDA_CDTI[indices] <- rep(coverages_UGA3, length.out = length(indices))

# for years covered by APOC, 2015 report
condition2 <- with(dfAPOC_included3, ADMIN0ISO3 == "UGA" & Year > 1998 & Year < 2013 & ADMIN2 %in% districts3) # valuyes just for 1999 - 2012 from APOC report
indices <- which(condition2)
dfAPOC_included3$MDA_CDTI_raw[indices] <- rep(coverages_UGA, length.out = length(indices))# Use indexing to assign values based on the condition
dfAPOC_included3$cov_source[indices] <- rep("national-level coverage data", length.out = length(indices))# Use indexing to assign values based on the condition
dfAPOC_included3$cov_specific_source[indices] <- rep("APOC report, 2015", length.out = length(indices))# Use indexing to assign values based on the condition

# for years pre-APOC, 2015 report; Katabarwa indicates cvoerage (assumed non-CDTI)
condition3 <- with(dfAPOC_included3, ADMIN0ISO3 == "UGA" & Year > 1990 & Year < 1999 & ADMIN2 %in% districts3) # valuyes just for 1999 - 2012 from APOC report
indices <- which(condition3)
dfAPOC_included3$cov_source[indices] <- rep("assumed", length.out = length(indices))# Use indexing to assign values based on the condition
dfAPOC_included3$cov_specific_source[indices] <- rep("Katabarwa et al. 2018 indicates MDA (no coverage; assumed non-CDTI)", length.out = length(indices))# Use indexing to assign values based on the condition

# ======================== #
# fourth group of districts #
coverages_UGA4 <- coverages_UGA1[-c(1:3, (length(coverages_UGA1) - 1):length(coverages_UGA1))] # remove first 3 and last 2 elements from cov vector
districts4 <- unlist(strsplit(as.character(UGA_grouped_district[5, 3]), ",\\s*")) # Split the string into a character vector
condition4 <- with(dfAPOC_included3, ADMIN0ISO3 == "UGA" & Year > 1992 & Year < 2011 & ADMIN2 %in% districts4)
indices <- which(condition4)
dfAPOC_included3$MDA_CDTI[indices] <- rep(coverages_UGA4, length.out = length(indices))

# for years covered by APOC, 2015 report
condition2 <- with(dfAPOC_included3, ADMIN0ISO3 == "UGA" & Year > 1998 & Year < 2011 & ADMIN2 %in% districts4) # valuyes just for 1999 - 2012 from APOC report
indices <- which(condition2)
dfAPOC_included3$MDA_CDTI_raw[indices] <- rep(coverages_UGA, length.out = length(indices))# Use indexing to assign values based on the condition
dfAPOC_included3$cov_source[indices] <- rep("national-level coverage data", length.out = length(indices))# Use indexing to assign values based on the condition
dfAPOC_included3$cov_specific_source[indices] <- rep("APOC report, 2015", length.out = length(indices))# Use indexing to assign values based on the condition

# for years pre-APOC, 2015 report; Katabarwa indicates cvoerage (assumed non-CDTI)
condition3 <- with(dfAPOC_included3, ADMIN0ISO3 == "UGA" & Year > 1992 & Year < 1999 & ADMIN2 %in% districts4) # valuyes just for 1999 - 2012 from APOC report
indices <- which(condition3)
dfAPOC_included3$cov_source[indices] <- rep("assumed", length.out = length(indices))# Use indexing to assign values based on the condition
dfAPOC_included3$cov_specific_source[indices] <- rep("Katabarwa et al. 2018 indicates MDA (no coverage; assumed non-CDTI)", length.out = length(indices))# Use indexing to assign values based on the condition

# ======================== #
# fifth group of districts #
coverages_UGA5 <- coverages_UGA1[-c(1:3)] # remove first 3
districts5a <- unlist(strsplit(as.character(UGA_grouped_district[6, 3]), ",\\s*")) # Split the string into a character vector
districts5b <- unlist(strsplit(as.character(UGA_grouped_district[7, 3]), ",\\s*")) # Split the string into a character vector
districts5 <- c(districts5a,districts5b)
condition5 <- with(dfAPOC_included3, ADMIN0ISO3 == "UGA" & Year > 1992 & Year < 2013 & ADMIN2 %in% districts5)
indices <- which(condition5)
dfAPOC_included3$MDA_CDTI[indices] <- rep(coverages_UGA5, length.out = length(indices))

# for years covered by APOC, 2015 report
condition2 <- with(dfAPOC_included3, ADMIN0ISO3 == "UGA" & Year > 1998 & Year < 2013 & ADMIN2 %in% districts5) # valuyes just for 1999 - 2012 from APOC report
indices <- which(condition2)
dfAPOC_included3$MDA_CDTI_raw[indices] <- rep(coverages_UGA, length.out = length(indices))# Use indexing to assign values based on the condition
dfAPOC_included3$cov_source[indices] <- rep("national-level coverage data", length.out = length(indices))# Use indexing to assign values based on the condition
dfAPOC_included3$cov_specific_source[indices] <- rep("APOC report, 2015", length.out = length(indices))# Use indexing to assign values based on the condition

# for years pre-APOC, 2015 report; Katabarwa indicates cvoerage (assumed non-CDTI)
condition3 <- with(dfAPOC_included3, ADMIN0ISO3 == "UGA" & Year > 1992 & Year < 1999 & ADMIN2 %in% districts5) # valuyes just for 1999 - 2012 from APOC report
indices <- which(condition3)
dfAPOC_included3$cov_source[indices] <- rep("assumed", length.out = length(indices))# Use indexing to assign values based on the condition
dfAPOC_included3$cov_specific_source[indices] <- rep("Katabarwa et al. 2018 indicates MDA (no coverage; assumed non-CDTI)", length.out = length(indices))# Use indexing to assign values based on the condition

# ======================== #
# sixth group of districts #
coverages_UGA6 <- coverages_UGA1[-c(1:4)] # remove first 4
districts6a <- unlist(strsplit(as.character(UGA_grouped_district[8, 3]), ",\\s*")) # Split the string into a character vector
districts6b <- unlist(strsplit(as.character(UGA_grouped_district[9, 3]), ",\\s*")) # Split the string into a character vector
districts6 <- c(districts6a, districts6b)
condition6 <- with(dfAPOC_included3, ADMIN0ISO3 == "UGA" & Year > 1993 & Year < 2013 & ADMIN2 %in% districts6)
indices <- which(condition6)
dfAPOC_included3$MDA_CDTI[indices] <- rep(coverages_UGA6, length.out = length(indices))

# for years covered by APOC, 2015 report
condition2 <- with(dfAPOC_included3, ADMIN0ISO3 == "UGA" & Year > 1998 & Year < 2013 & ADMIN2 %in% districts6) # valuyes just for 1999 - 2012 from APOC report
indices <- which(condition2)
dfAPOC_included3$MDA_CDTI_raw[indices] <- rep(coverages_UGA, length.out = length(indices))# Use indexing to assign values based on the condition
dfAPOC_included3$cov_source[indices] <- rep("national-level coverage data", length.out = length(indices))# Use indexing to assign values based on the condition
dfAPOC_included3$cov_specific_source[indices] <- rep("APOC report, 2015", length.out = length(indices))# Use indexing to assign values based on the condition

# for years pre-APOC, 2015 report; Katabarwa indicates cvoerage (assumed non-CDTI)
condition3 <- with(dfAPOC_included3, ADMIN0ISO3 == "UGA" & Year > 1993 & Year < 1999 & ADMIN2 %in% districts6) # valuyes just for 1999 - 2012 from APOC report
indices <- which(condition3)
dfAPOC_included3$cov_source[indices] <- rep("assumed", length.out = length(indices))# Use indexing to assign values based on the condition
dfAPOC_included3$cov_specific_source[indices] <- rep("Katabarwa et al. 2018 indicates MDA (no coverage; assumed non-CDTI)", length.out = length(indices))# Use indexing to assign values based on the condition

# ======================== #
# seventh group of districts #
coverages_UGA7 <- coverages_UGA1[-c(1:7)] # remove first 7
districts7 <- unlist(strsplit(as.character(UGA_grouped_district[10, 3]), ",\\s*")) # Split the string into a character vector
condition7 <- with(dfAPOC_included3, ADMIN0ISO3 == "UGA" & Year > 1996 & Year < 2013 & ADMIN2 %in% districts7)
indices <- which(condition7)
dfAPOC_included3$MDA_CDTI[indices] <- rep(coverages_UGA7, length.out = length(indices))

# for years covered by APOC, 2015 report
condition2 <- with(dfAPOC_included3, ADMIN0ISO3 == "UGA" & Year > 1998 & Year < 2013 & ADMIN2 %in% districts7) # valuyes just for 1999 - 2012 from APOC report
indices <- which(condition2)
dfAPOC_included3$MDA_CDTI_raw[indices] <- rep(coverages_UGA, length.out = length(indices))# Use indexing to assign values based on the condition
dfAPOC_included3$cov_source[indices] <- rep("national-level coverage data", length.out = length(indices))# Use indexing to assign values based on the condition
dfAPOC_included3$cov_specific_source[indices] <- rep("APOC report, 2015", length.out = length(indices))# Use indexing to assign values based on the condition

# for years pre-APOC, 2015 report; Katabarwa indicates cvoerage (assumed non-CDTI)
condition3 <- with(dfAPOC_included3, ADMIN0ISO3 == "UGA" & Year > 1996 & Year < 1999 & ADMIN2 %in% districts7) # valuyes just for 1999 - 2012 from APOC report
indices <- which(condition3)
dfAPOC_included3$cov_source[indices] <- rep("assumed", length.out = length(indices))# Use indexing to assign values based on the condition
dfAPOC_included3$cov_specific_source[indices] <- rep("Katabarwa et al. 2018 indicates MDA (no coverage; assumed non-CDTI)", length.out = length(indices))# Use indexing to assign values based on the condition

# ======================== #
# eight group of districts #
# manually include Obongi focus #
coverages_UGA8 <- coverages_UGA1[-c(1:3)]  # remove first 7
condition8 <- with(dfAPOC_included3, ADMIN0ISO3 == "UGA" & Year > 1992 & Year < 2013 & ADMIN2 == "Obongi")
indices <- which(condition8)
dfAPOC_included3$MDA_CDTI[indices] <- rep(coverages_UGA8, length.out = length(indices))

# for years covered by APOC, 2015 report
condition2 <- with(dfAPOC_included3, ADMIN0ISO3 == "UGA" & Year > 1998 & Year < 2013 & ADMIN2 == "Obongi") # valuyes just for 1999 - 2012 from APOC report
indices <- which(condition2)
dfAPOC_included3$MDA_CDTI_raw[indices] <- rep(coverages_UGA, length.out = length(indices))# Use indexing to assign values based on the condition
dfAPOC_included3$cov_source[indices] <- rep("national-level coverage data", length.out = length(indices))# Use indexing to assign values based on the condition
dfAPOC_included3$cov_specific_source[indices] <- rep("APOC report, 2015", length.out = length(indices))# Use indexing to assign values based on the condition

# for years pre-APOC, 2015 report; Katabarwa indicates cvoerage (assumed non-CDTI)
condition3 <- with(dfAPOC_included3, ADMIN0ISO3 == "UGA" & Year > 1992 & Year < 1999 & ADMIN2 == "Obongi") # valuyes just for 1999 - 2012 from APOC report
indices <- which(condition3)
dfAPOC_included3$cov_source[indices] <- rep("assumed", length.out = length(indices))# Use indexing to assign values based on the condition
dfAPOC_included3$cov_specific_source[indices] <- rep("Katabarwa et al. 2018 indicates MDA (no coverage; assumed non-CDTI)", length.out = length(indices))# Use indexing to assign values based on the condition

check_df <- subset(dfAPOC_included3, Year == 2022)
nrow(check_df) # 2107 IUs 
length(unique(check_df$IU_ID_MAPPING)) # 2107 IUs

# ================  #
# Sudan (SDN)      #

# =========== #
# first foci #
#coverages_SDN_foci1 <- c(0.25, 0.25, 0.25, 0.65, 0.25, 0.25, 0.25, 0.65, 0.25, 0.65, 0.65, 0.25, 0.65, 0.65, 0.65, 0.65) # this is from the APOC report (starting at 2012)
coverages_SDN_foci1 <-c(0.25, 0.25, 0.25, 0.25, 0.65, 0.25, 0.25, 0.25, 0.65, 0.25, 0.65, 0.65, 0.25, 0.65, 0.65, 0.65, 0.65)
condition <- dfAPOC_included3$IUs_NAME_MAPPING %in% c("Abu Hamed", "Marawai", "Berber", "El Quresha") &
  dfAPOC_included3$ADMIN0ISO3 == "SDN" &
  dfAPOC_included3$Year > 1995 & dfAPOC_included3$Year < 2013
indices <- which(condition)
dfAPOC_included3$MDA_CDTI[indices] <- rep(coverages_SDN_foci1, length.out = length(indices))# Use indexing to assign values based on the condition

# for years covered by APOC, 2015 report
condition2 <- dfAPOC_included3$IUs_NAME_MAPPING %in% c("Abu Hamed", "Marawai", "Berber", "El Quresha") &
  dfAPOC_included3$ADMIN0ISO3 == "SDN" &
  dfAPOC_included3$Year > 1998 & dfAPOC_included3$Year < 2013
indices <- which(condition2)
dfAPOC_included3$cov_source[indices] <- rep("national-level coverage data", length.out = length(indices))# Use indexing to assign values based on the condition
dfAPOC_included3$cov_specific_source[indices] <- rep("APOC report, 2015", length.out = length(indices))# Use indexing to assign values based on the condition

# for years covered by APOC, 2015 report
condition3 <- dfAPOC_included3$IUs_NAME_MAPPING %in% c("Abu Hamed", "Marawai", "Berber", "El Quresha") &
  dfAPOC_included3$ADMIN0ISO3 == "SDN" &
  dfAPOC_included3$Year > 1995 & dfAPOC_included3$Year < 1999
indices <- which(condition3)
dfAPOC_included3$cov_source[indices] <- rep("assumed", length.out = length(indices))# Use indexing to assign values based on the condition
dfAPOC_included3$cov_specific_source[indices] <- rep("The Carter Centre info indicates MDA: no coverage, assume non-CDTI", length.out = length(indices))# Use indexing to assign values based on the condition

# =========== #
# second foci #
coverages_SDN_foci2 <- c(0.65, 0.65, 0.25, 0.65, 0.65, 0.65, 0.65, 0.65, 0.65, 0.65, 0.65) # assuming beyond 2012 cov is 65% because 65% before
condition <- dfAPOC_included3$IUs_NAME_MAPPING %in% c("Eastern El Qalabat") &
#condition <- dfAPOC_included3$IUs_NAME_MAPPING %in% c("Eastern El Qalabat","El Kurmuk") &
  dfAPOC_included3$ADMIN0ISO3 == "SDN" &
  dfAPOC_included3$Year > 2006 & dfAPOC_included3$Year < 2018
indices <- which(condition)
dfAPOC_included3$MDA_CDTI[indices] <- rep(coverages_SDN_foci2, length.out = length(indices))

# for years covered by APOC, 2015 report
condition2 <- dfAPOC_included3$IUs_NAME_MAPPING %in% c("Eastern El Qalabat") &
  dfAPOC_included3$ADMIN0ISO3 == "SDN" &
  dfAPOC_included3$Year > 2006 & dfAPOC_included3$Year < 2013
indices <- which(condition2)
dfAPOC_included3$cov_source[indices] <- rep("national-level coverage data", length.out = length(indices))# Use indexing to assign values based on the condition
dfAPOC_included3$cov_specific_source[indices] <- rep("APOC report, 2015", length.out = length(indices))# Use indexing to assign values based on the condition

# for yearsafter APOC report (2013 - 2017)
condition3 <- dfAPOC_included3$IUs_NAME_MAPPING %in% c("Eastern El Qalabat") &
  dfAPOC_included3$ADMIN0ISO3 == "SDN" &
  dfAPOC_included3$Year > 2012 & dfAPOC_included3$Year < 2018
indices <- which(condition3)
dfAPOC_included3$cov_source[indices] <- rep("assumed", length.out = length(indices))# Use indexing to assign values based on the condition
dfAPOC_included3$cov_specific_source[indices] <- rep("The Carter Centre info indicates MDA: no coverage in ESPEN, assume CDTI with 65% total population coverage", length.out = length(indices))# Use indexing to assign values based on the condition

# =========== #
# third foci #
coverages_SDN_foci3 <- c(0.65, 0.65, 0.65, 0.65, 0.65, 0.65, 0.65, 0.65, 0.65, 0.65, 0.65) # assuming beyond 2012 cov is 65% because 65% before
condition <- dfAPOC_included3$IUs_NAME_MAPPING %in% c("El Radoom") &
  dfAPOC_included3$ADMIN0ISO3 == "SDN" &
  dfAPOC_included3$Year > 2011
indices <- which(condition)
dfAPOC_included3$MDA_CDTI[indices] <- rep(coverages_SDN_foci3, length.out = length(indices))

# for years covered by APOC, 2015 report
condition2 <- dfAPOC_included3$IUs_NAME_MAPPING %in% c("El Radoom") &
  dfAPOC_included3$ADMIN0ISO3 == "SDN" &
  dfAPOC_included3$Year > 2011 & dfAPOC_included3$Year < 2013
indices <- which(condition2)
dfAPOC_included3$cov_source[indices] <- rep("national-level coverage data", length.out = length(indices))# Use indexing to assign values based on the condition
dfAPOC_included3$cov_specific_source[indices] <- rep("APOC report, 2015", length.out = length(indices))# Use indexing to assign values based on the condition

# for yearsafter APOC report (2013 - 2017)
condition3 <- dfAPOC_included3$IUs_NAME_MAPPING %in% c("El Radoom") &
  dfAPOC_included3$ADMIN0ISO3 == "SDN" &
  dfAPOC_included3$Year > 2012
indices <- which(condition3)
dfAPOC_included3$cov_source[indices] <- rep("assumed", length.out = length(indices))# Use indexing to assign values based on the condition
dfAPOC_included3$cov_specific_source[indices] <- rep("The Carter Centre info indicates MDA: no coverage in ESPEN, assume CDTI with 65% total population coverage", length.out = length(indices))# Use indexing to assign values based on the condition

check_df <- subset(dfAPOC_included3, Year == 2022)
nrow(check_df) # 2107 IUs 
length(unique(check_df$IU_ID_MAPPING)) # 2107 IUs

# =================================== #
# add in post-2012 histories (ESPEN)  #

dfAPOC_included4 <- dfAPOC_included3 # RESET HERE

nrow(subset(dfAPOC_included4, Year == 2022)) # check n = 2346 IUs (old); 
                                             # check n = 2107 IUs (April 2025)

# ================================================================================= #
#  UPDATE - MAY 2024: new 2022 updated MDA data from ESPEN (uploaded in Q1 of 2024) #

# =============== #
#     DRC         #

DRC_2022_EPSEN <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/ESPEN updates to check/ESPEN updated data for 2022/data-COD-Oncho-iu-2022.csv")
DRC_MDA_2022 <- subset(DRC_2022_EPSEN, EpiCov > 0) #find those IUs with epicov > 0

dfAPOC_DRC_2022 <- subset(dfAPOC_included4, ADMIN0ISO3 == "COD" & Year == 2022 & EpiCov > 0)

set1 <- unique(DRC_MDA_2022$IU_ID)
set2 <- unique(dfAPOC_DRC_2022$IU_ID_MAPPING)

# Check if sets are equal
if(all(sort(set1) == sort(set2))) {
  print("The character vectors have the same characters.")
} else {
  print("The character vectors do not have the same characters.")
}

# ============================= #
#              ETH              #
# now moved to start of script! #

# ETH_2022_EPSEN <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/ESPEN updates to check/ESPEN updated data for 2022/data-ETH-Oncho-iu-2022.csv")
# ETH_MDA_2022 <- subset(ETH_2022_EPSEN, EpiCov > 0) #find those IUs with epicov > 0
#
# dfAPOC_ETH_2022 <- subset(dfAPOC_included4, ADMIN0ISO3 == "ETH" & Year == 2022 & EpiCov > 0)
#
# # 1) first, update exisiting IUs with 2022 EpiCov from new ESPEN
# dfAPOC_included4_subset <- dfAPOC_included4[dfAPOC_included4$Year == 2022, ] # Subset dfAPOC_included4 to get only the rows where Year is equal to 2022
# matching_indices <- match(dfAPOC_included4_subset$IU_ID_MAPPING, ETH_MDA_2022$IU_ID)
# #any(!is.na(matching_indices))
#
# # Update EpiCov column in dfAPOC_included4_subset with values from EpiCov column in ETH_MDA_2022
# matching_indices_not_na <- !is.na(matching_indices)
# dfAPOC_included4_subset$EpiCov[matching_indices_not_na] <- ETH_MDA_2022$EpiCov[matching_indices[matching_indices_not_na]]
#
# dfAPOC_included4[dfAPOC_included4$Year == 2022, ] <- dfAPOC_included4_subset # Update the original dfAPOC_included4 with the updated values
#
# # 2) now need to do this for the endemicity, CUM_MDA and cov columns (replace the new ESPEN claissification in dfAPOC_included4)
# dfAPOC_included4_subset$Endemicity[matching_indices_not_na] <- ETH_MDA_2022$Endemicity[matching_indices[matching_indices_not_na]]
# dfAPOC_included4[dfAPOC_included4$Year == 2022, ] <- dfAPOC_included4_subset
#
# dfAPOC_included4_subset$Cum_MDA[matching_indices_not_na] <- ETH_MDA_2022$Cum_MDA[matching_indices[matching_indices_not_na]]
# dfAPOC_included4[dfAPOC_included4$Year == 2022, ] <- dfAPOC_included4_subset
#
# dfAPOC_included4_subset$Cov[matching_indices_not_na] <- ETH_MDA_2022$Cov[matching_indices[matching_indices_not_na]]
# dfAPOC_included4[dfAPOC_included4$Year == 2022, ] <- dfAPOC_included4_subset

# 3) need to check if any existing IUs starting MDA for first time in 2022 & whether can be included (baseline fitted mf prev?)
new_MDA_ETH_2022 <- subset(ETH_MDA_2022, Cum_MDA == 1)
new_IUs_ETH <- new_MDA_ETH_2022$IU_ID
new_IUs_ETH_name <- new_MDA_ETH_2022$ADMIN3

dfAPOC_included4_subset_ETH2022 <- dfAPOC_included4[dfAPOC_included4$Year == 2022 & dfAPOC_included4$ADMIN0ISO3 == "ETH", ] # which Ius already included in 2022 in ETH
dfAPOC_included4_subset_ETH2022_IUs <- dfAPOC_included4_subset_ETH2022$IU_ID_MAPPING # currently in histories

# are they in the histories currently?
new_IUs_ETH # all IUs with MDA for first time in ETH
IU_ID_notinhistories <- setdiff(new_IUs_ETH, dfAPOC_included4_subset_ETH2022_IUs) # find Ius that are not in the histories currently for 2022
IU_ID_notinhistories

# those not found after update (but should be included?) : c(Hidabu Abote, Gerar Jarso, Degeluna Tijo, Quarit, East Esite, Ensaro, West Esite)
# those not found after update (but should be included?) : c(18905, 18871, 18833, 18651, 18592, 18591, 18537)

# no baseline for: c(18755, 18502)

# make a vector of new IUs (minus those without baseline mf prev)
IU_ID_new_toinclude_ETH <- setdiff(new_IUs_ETH, IU_ID_notinhistories)
IU_ID_new_toinclude_ETH
# # do they have fitted baseline mf prev values?
# new_IUs_ETH_code <- new_MDA_ETH_2022$IU_CODE
# new_IUs_ETH_code
# ius_fitted <- summaries_apoc[,1]
# notinfitting <- setdiff(new_IUs_ETH_code, ius_fitted)
# notinfitting
#
# IU_ID_nobaseline <- substr(notinfitting, nchar(notinfitting) - 4, nchar(notinfitting))
# IU_ID_nobaseline
#
# # now remove these from IU_ID to include as new Ius in 2022 (because no baseline mf prev)
# IU_ID_new_toinclude <- setdiff(IU_ID_notinhistories, IU_ID_nobaseline)
# IU_ID_new_toinclude

#setdiff(new_IUs_ETH, dfAPOC_included4_subset_ETH2022_IUs) # find Ius that are not in the histories currently for 2022

# dfAPOC_endemic # check if in first
# summaries_apoc # check if IUs in baseline fitting
# final_df_oncho_Parent_Child # check if mapped to a parent IU

# =============================== #
#     COG (republic of congo)     #

COG_2022_EPSEN <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/ESPEN updates to check/ESPEN updated data for 2022/data-COG-Oncho-iu-2022.csv")
COG_MDA_2022 <- subset(COG_2022_EPSEN, EpiCov > 0) #find those IUs with epicov > 0
nrow(COG_MDA_2022) # 19 Ius with MDA in 2022 in ESPEN
IUs_2022_COG_withMDA_ESPEN <- COG_MDA_2022$IU_ID

dfAPOC_COG_2022 <- subset(dfAPOC_included4, ADMIN0ISO3 == "COG" & Year == 2022 & EpiCov > 0)
nrow(dfAPOC_COG_2022)
IUs_2022_COG_withMDA_histories <- dfAPOC_COG_2022$IU_ID # 19 IUs with MDA in histories so matches!

check_sameIUs_COG <- setdiff(IUs_2022_COG_withMDA_ESPEN, IUs_2022_COG_withMDA_histories)
check_sameIUs_COG

# checked and 19 Ius (the same) in both

# =============================== #
#         Uganda (UGA)            #
# now moved to start of script! #

UGA_2022_EPSEN <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/ESPEN updates to check/ESPEN updated data for 2022/data-UG-Oncho-iu-2022.csv")
UGA_MDA_2022 <- subset(UGA_2022_EPSEN, EpiCov > 0) #find those IUs with epicov > 0
nrow(UGA_MDA_2022) # 19 Ius with MDA in 2022 in ESPEN
IUs_2022_UGA_withMDA_ESPEN <- UGA_MDA_2022$IU_ID
IUs_2022_UGA_withMDA_ESPEN

IUs_2022_UGA_withMDA_ESPEN_IUname <- UGA_MDA_2022$ADMIN2
IUs_2022_UGA_withMDA_ESPEN_IUname

# any IUs in current histories with MDA in 2022 in UGA?
dfAPOC_UGA_2022 <- subset(dfAPOC_included4, ADMIN0ISO3 == "UGA" & Year == 2022 & EpiCov > 0)
nrow(dfAPOC_UGA_2022)
IUs_2022_UGA_withMDA_histories <- dfAPOC_UGA_2022$IU_ID # 0 IUs with MDA in histories -

# check if 12 IUs with MDA in ESPEN are in our histories currently
dfAPOC_UGA_2022 <- subset(dfAPOC_included4, ADMIN0ISO3 == "UGA" & Year == 2022)
nrow(dfAPOC_UGA_2022)
IUs_2022_UGA_histories <- dfAPOC_UGA_2022$IU_ID # 0 IUs with MDA in histories -

check_diffIUs_UGA <- setdiff(IUs_2022_UGA_withMDA_ESPEN, IUs_2022_UGA_histories)
check_diffIUs_UGA # 0 so no new IUs with MDA i.e., all present in 2022 in histories just without MDA at the moment

# checked and 12 Ius (the same) in both
check_sameIUs_UGA <- setdiff(IUs_2022_UGA_withMDA_ESPEN, IUs_2022_UGA_withMDA_histories)
check_sameIUs_UGA

# ========================================================================= #
# make new col to indicate new IUs for 2022 with MDA (NOT NEWLY CREATED IUs)

# for ETH
dfAPOC_included4$new_MDA_IUs_2022 <- ifelse(dfAPOC_included4$IU_ID %in% IU_ID_new_toinclude_ETH, "ESPEN update: first MDA round in 2022", "no update")

# for NGA
dfAPOC_included4$new_MDA_IUs_2022 <- ifelse(dfAPOC_included4$IU_ID %in% NGA_MDA_2022_withMDA_ESPEN, "ESPEN update: MDA present in 2022 with prior MDA", dfAPOC_included4$new_MDA_IUs_2022)

# for UGA
dfAPOC_included4$new_MDA_IUs_2022 <- ifelse(dfAPOC_included4$IU_ID %in% IUs_2022_UGA_withMDA_ESPEN, "ESPEN update: MDA present in 2022 with prior MDA", dfAPOC_included4$new_MDA_IUs_2022)

nrow(subset(dfAPOC_included4, Year == 2022)) # check n IUs = 2346; 
                                             # n = 2107 IUs (April 2025)

# ====================================== #
#     END OF MAY 2024 (ESPEN update)     #

# ======================= #
# Now fill in 2013 - 2022 #

# epi cov > 65%
condition <- dfAPOC_included4$EpiCov >= 65 &
  dfAPOC_included4$Year %in% 2013:2022 &
  dfAPOC_included4$ADMIN0ISO3 != "SDN"
indices <- which(condition)
dfAPOC_included4$MDA_CDTI[indices] <- rep(0.65, length.out = length(indices))

# add columns for raw coverage, and source of info
dfAPOC_included4$MDA_CDTI_raw[indices] <- dfAPOC_included4$EpiCov[indices]/100
dfAPOC_included4$cov_source[indices] <- rep("IU-level coverage", length.out = length(indices))# Use indexing to assign values based on the condition
dfAPOC_included4$cov_specific_source[indices] <- rep("ESPEN", length.out = length(indices))# Use indexing to assign values based on the condition

# epi cov < 25%
condition <- dfAPOC_included4$EpiCov > 0 &
  dfAPOC_included4$EpiCov < 65 &
  dfAPOC_included4$Year %in% 2013:2022 &
  dfAPOC_included4$ADMIN0ISO3 != "SDN"
indices <- which(condition)
dfAPOC_included4$MDA_CDTI[indices] <- rep(0.25, length.out = length(indices))

# add columns for raw coverage, and source of info
dfAPOC_included4$MDA_CDTI_raw[indices] <- dfAPOC_included4$EpiCov[indices]/100
dfAPOC_included4$cov_source[indices] <- rep("IU-level coverage", length.out = length(indices))# Use indexing to assign values based on the condition
dfAPOC_included4$cov_specific_source[indices] <- rep("ESPEN", length.out = length(indices))# Use indexing to assign values based on the condition

check_df <- subset(dfAPOC_included4, Year == 2022)
nrow(check_df) # 2107 IUs 
length(unique(check_df$IU_ID_MAPPING)) # 2107 IUs

# ======================================================================= #
# if 2013 or 2014 and "not reported" and "included" in ESPEN_MDA_history  #

# check if any IUs with Cum_MDA > 0 but not EpiCov > 0 during 2013 - 2022
result <- dfAPOC_included4 %>%
  filter(Year >= 2013 & Year <= 2022) %>%
  group_by(IU_ID_MAPPING) %>%
  summarise(has_positive_Cum_MDA = any(Cum_MDA > 0),
            no_positive_EpiCov = all(EpiCov == 0))

# Filter rows where Cum_MDA > 0 but no positive EpiCov
result_filtered <- result %>%
  filter(has_positive_Cum_MDA & no_positive_EpiCov)

unique_check_vec <- result_filtered$IU_ID_MAPPING
length(unique_check_vec)
#subset_check <- subset(dfAPOC_included4, IU_ID_MAPPING %in% unique_check_vec)

# check IUs where no EpiCoV values from 2013
result_noEpiCovs <- result %>%
  filter(no_positive_EpiCov)
unique_noEpiCov_vec <- result_noEpiCovs$IU_ID_MAPPING

# # # All countries that need 65% in 2013 or 2014 according to APOC Table 2015 #
# condition <- dfAPOC_included4$ESPEN_MDA_history == "Include" &
#   (dfAPOC_included4$Year == 2013 | dfAPOC_included4$Year == 2014) &
#   (dfAPOC_included4$Endemicity == "Not reported" | dfAPOC_included4$Endemicity == "Augmented") &
#   (dfAPOC_included4$ADMIN0ISO3 == "BDI" | dfAPOC_included4$ADMIN0ISO3 == "CMR" | dfAPOC_included4$ADMIN0ISO3 == "TCD" |
#      dfAPOC_included4$ADMIN0ISO3 == "COG" | dfAPOC_included4$ADMIN0ISO3 == "COD" | dfAPOC_included4$ADMIN0ISO3 == "ETH" |
#      dfAPOC_included4$ADMIN0ISO3 == "LBR" | dfAPOC_included4$ADMIN0ISO3 == "MWI" | dfAPOC_included4$ADMIN0ISO3 == "NGA" |
#      dfAPOC_included4$ADMIN0ISO3 == "TZA" | dfAPOC_included4$ADMIN0ISO3 == "UGA")
# indices <- which(condition)
# dfAPOC_included4$MDA_CDTI[indices] <- rep(0.65, length.out = length(indices))
# #check <- dfAPOC_included4[75831,]
#
# # # All countries that need 25% in 2013 or 2014 according to APOC Table 2015 #
# condition <- dfAPOC_included4$ESPEN_MDA_history == "Include" &
#   (dfAPOC_included4$Year == 2013 | dfAPOC_included4$Year == 2014) &
#   (dfAPOC_included4$Endemicity == "Not reported" | dfAPOC_included4$Endemicity == "Augmented") &
#   (dfAPOC_included4$ADMIN0ISO3 == "GNQ" | dfAPOC_included4$ADMIN0ISO3 == "SSD")
# indices <- which(condition)
# dfAPOC_included4$MDA_CDTI[indices] <- rep(0.25, length.out = length(indices))

# same as above but excluding IUs where no Epi_cov values > 0 in 2013 onwards #
condition <- dfAPOC_included4$ESPEN_MDA_history == "Include" &
  (dfAPOC_included4$Year == 2013 | dfAPOC_included4$Year == 2014) &
  (dfAPOC_included4$Endemicity == "Not reported" | dfAPOC_included4$Endemicity == "Augmented") &
  (dfAPOC_included4$ADMIN0ISO3 %in% c("BDI", "CMR", "TCD", "COG", "COD", "ETH", "LBR", "MWI", "NGA", "TZA", "UGA")) &
  !(dfAPOC_included4$IU_ID_MAPPING %in% unique_noEpiCov_vec)
indices <- which(condition)
dfAPOC_included4$MDA_CDTI[indices] <- rep(0.65, length.out = length(indices))
#check <- dfAPOC_included4[75831,]

# add columns for raw coverage, and source of info
#dfAPOC_included4$MDA_CDTI_raw[indices] <- dfAPOC_included4$EpiCov[indices]/100
dfAPOC_included4$cov_source[indices] <- rep("assumed", length.out = length(indices))# Use indexing to assign values based on the condition
dfAPOC_included4$cov_specific_source[indices] <- rep("assumed (continuous MDA in 2013 and/or 2014)", length.out = length(indices))# Use indexing to assign values based on the condition

# # # All countries that need 25% in 2013 or 2014 according to APOC Table 2015 #
condition <- dfAPOC_included4$ESPEN_MDA_history == "Include" &
  (dfAPOC_included4$Year == 2013 | dfAPOC_included4$Year == 2014) &
  (dfAPOC_included4$Endemicity == "Not reported" | dfAPOC_included4$Endemicity == "Augmented") &
  (dfAPOC_included4$ADMIN0ISO3 == "GNQ" | dfAPOC_included4$ADMIN0ISO3 == "SSD") &
  !(dfAPOC_included4$IU_ID_MAPPING %in% unique_noEpiCov_vec)
indices <- which(condition)
dfAPOC_included4$MDA_CDTI[indices] <- rep(0.25, length.out = length(indices))

# add columns for raw coverage, and source of info
#dfAPOC_included4$MDA_CDTI_raw[indices] <- dfAPOC_included4$EpiCov[indices]/100
dfAPOC_included4$cov_source[indices] <- rep("national-level coverage data", length.out = length(indices))# Use indexing to assign values based on the condition
dfAPOC_included4$cov_specific_source[indices] <- rep("APOC report, 2015", length.out = length(indices))# Use indexing to assign values based on the condition

check_df <- subset(dfAPOC_included4, Year == 2022)
nrow(check_df) # 2107 IUs  (April 25')
length(unique(check_df$IU_ID_MAPPING)) # 2107 IUs

# ================ #
# Nigeria in ESPEN #

# NOTE: IDEALLY NATIONAL ONCHO SNAPSHOT UPDATE FOR 2022 WOULD SUPERCEDE USING SS AND TCT DATA BELOW WHEN AVAILABLE!
# CURRENTLY TCT and SS data supercedes using espen 2022 where available.

# ESPEN 2022 update (May 2024) #
#  check whether same IUs with MDA in ESPEN in old histories for 2022 #
unique(NGA_MDA_2022$ADMIN2)
unique(NGA_MDA_2022$ADMIN1) # are Benue, Kogi, Kwara included?

#check individual rows for these 3:
dfAPOC_NGA_2022_3states <- subset(dfAPOC_included4, ADMIN0ISO3 == "NGA" & ADMIN1 %in% c("Benue", "Kogi", "Kwara") & Year == 2022)

dfAPOC_NGA_2022_Benue <- subset(dfAPOC_NGA_2022_3states, ADMIN1 == "Benue")
IUs_Benue <- unique(dfAPOC_NGA_2022_Benue$ADMIN2)

# IUs1_tocheck <- as.vector(NGA_grouped_LGA[3, 3])
# any(IUs_Benue %in% IUs1_tocheck)
#
# IUs2_tocheck <- as.vector(NGA_grouped_LGA[6, 3])
# any(IUs_Benue %in% IUs2_tocheck)
#
# IUs2_tocheck <- as.vector(NGA_grouped_LGA[6, 3])


# update NGA for 2022 & 2023 based on CC data (as most/all coded "MDA not delivered" according to ESPEN) - assume 65% cov until CC return with covs
# 2022
LGAs1_22 <- unlist(strsplit(as.character(NGA_grouped_LGA[3, 3]), ",\\s*"))
LGAs2_22 <- unlist(strsplit(as.character(NGA_grouped_LGA[6, 3]), ",\\s*"))
LGAs3_22 <- unlist(strsplit(as.character(NGA_grouped_LGA[8, 3]), ",\\s*"))
LGAs4_22 <- unlist(strsplit(as.character(NGA_grouped_LGA[10, 3]), ",\\s*"))
LGAs_22_MDA <- c(LGAs1_22, LGAs2_22, LGAs3_22, LGAs4_22)

# check if any IUs in the ESPEN 2022 update with MDA (with cov < 65%) and in our LGA 2022 data to update below (w/ 65% cov)
NGA_2022_ESPEN_covless65 <- subset(NGA_2022_EPSEN, EpiCov > 0 & EpiCov < 65)
IUs_covless65_NGA_2022 <- unique(NGA_2022_ESPEN_covless65$ADMIN2)
any(IUs_covless65_NGA_2022 %in% LGAs_22_MDA) # are there any IUs in ESPEN with <65% (non-zero) that are in our LGA_22 vector from SS?
to_remove <- intersect(IUs_covless65_NGA_2022, LGAs_22_MDA) # which IUs match? = Esan West - remove from LGAs_22_MDA (as already have an ESPEN which is <65%)
to_remove

length(LGAs_22_MDA)
LGAs_22_MDA <- LGAs_22_MDA[!LGAs_22_MDA %in% to_remove]
length(LGAs_22_MDA) #check length (should be minus 1)
intersect(IUs_covless65_NGA_2022, LGAs_22_MDA) # check now none present = now can use LGAs_22_MDA

# now update with CC data
condition_MDA22_NGA <- with(dfAPOC_included4, ADMIN0ISO3 == "NGA" & Year == 2022 & ADMIN2 %in% LGAs_22_MDA)
indices <- which(condition_MDA22_NGA)
dfAPOC_included4$MDA_CDTI[indices] <- rep(0.65, length.out = length(indices))

# add columns for raw coverage (if applicable), and source of info
dfAPOC_included4$cov_source[indices] <- rep("assumed", length.out = length(indices))# Use indexing to assign values based on the condition
dfAPOC_included4$cov_specific_source[indices] <- rep("The Carter Centre info indicates MDA: no coverage in ESPEN, assume CDTI with 65% total population coverage", length.out = length(indices))# Use indexing to assign values based on the condition

# dfAPOC_NGA_2022_3states <- subset(dfAPOC_included4, ADMIN0ISO3 == "NGA" & ADMIN1 %in% c("Benue", "Kogi", "Kwara") & Year == 2022) # check

# Sightsavers information for MDA 3 regions/ 60 LGAs in Nigeria in 2022
# dfAPOC_included4_sightsavers_NGA <- subset(dfAPOC_included4, ADMIN1 %in% c("Benue", "Kogi", "Kwara"))
# unique(dfAPOC_included4_sightsavers_NGA$ADMIN2)
NGA_sightsavers_2022 <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/data from CC/Sightsavers_2022_LGA_covs.csv")
LGAs_2022_SS <- NGA_sightsavers_2022$LGA_mappable

# check if any of SS IUs above already included in ESPEN (and covs matching?) - May 2024 update #
IUs_allcovs_NGA_2022 <- NGA_MDA_2022$ADMIN2
IUs_allcovs_NGA_2022
any(IUs_allcovs_NGA_2022 %in% LGAs_2022_SS)
to_check <- intersect(IUs_allcovs_NGA_2022, LGAs_2022_SS) # which IUs match? = Esan West - remove from LGAs_22_MDA (as already have an ESPEN which is <65%)
to_check

# NGA_sightsavers_2022_check <- subset(NGA_sightsavers_2022, LGA_mappable %in% to_check) # no Ius with 0 epicov
# ESPEN_2022_NGA_tocheck <- subset(NGA_2022_EPSEN, ADMIN2 %in% to_check) # 3 IUs with 0 epicov - use SS data to update 2022 for these IUs
# dfAPOC_included4_NGA_tocheck <- subset(dfAPOC_included4, ADMIN2 %in% to_check & Year == 2022) # confirmed: need SS data to update

# continue updating with SS 2022 data
condition <- (dfAPOC_included4$Year == 2022) &
   (dfAPOC_included4$ADMIN0ISO3 == "NGA") &
   (dfAPOC_included4$ADMIN1 %in% c("Benue", "Kogi", "Kwara"))
   (dfAPOC_included4$IUs_NAME_MAPPING %in% LGAs_2022_SS)
indices <- which(condition)
dfAPOC_included4$MDA_CDTI[indices] <- rep(0.65, length.out = length(indices)) # all epi.covs are > 0.6499 for 2022 from sightsavers data

# dfAPOC_included4_NGA_tocheck <- subset(dfAPOC_included4, ADMIN2 %in% to_check & Year == 2022) # confirmed: need SS data to update

# add columns for raw coverage (if applicable), and source of info
#dfAPOC_included4$MDA_CDTI_raw[indices] <- dfAPOC_included4$EpiCov[indices]/100
dfAPOC_included4$cov_source[indices] <- rep("IU-level coverage", length.out = length(indices))# Use indexing to assign values based on the condition
dfAPOC_included4$cov_specific_source[indices] <- rep("Sightsavers info", length.out = length(indices))# Use indexing to assign values based on the condition

check_df <- subset(dfAPOC_included4, Year == 2022)
nrow(check_df) # 2107 IUs 
length(unique(check_df$IU_ID_MAPPING)) # 2107 IUs (April 25')

# # 2023 # no 2023 here
# LGAs_23_MDA <- unlist(strsplit(as.character(NGA_grouped_LGA[9, 3]), ",\\s*"))
# condition_MDA23_NGA <- with(dfAPOC_included3, ADMIN0ISO3 == "NGA" & Year == 2023 & ADMIN2 %in% LGAs_23_MDA)
# indices <- which(condition_MDA23_NGA)
# dfAPOC_included3$MDA_CDTI[indices] <- rep(0.65, length.out = length(indices))

#check <- dfAPOC_included4[101559,]

# new col for Cov.in2
dfAPOC_included4$Cov.in2 <- dfAPOC_included4$MDA_CDTI

# need to remove MDA in 2013 or 2014 where dfAPOC_included4$new_MDA_IUs_2022 == "ESPEN update: first MDA round in 2022" (only an issue in ETH)

#dfAPOC_included4_ETH_tocheck <- subset(dfAPOC_included4, ADMIN0ISO3 =="ETH" & Year %in% c(2013,2014) & new_MDA_IUs_2022 == "ESPEN update: first MDA round in 2022") # confirmed: need SS data to update
condition <- (dfAPOC_included4$Year %in% c(2013)) &
  (dfAPOC_included4$ADMIN0ISO3 == "ETH") &
 (dfAPOC_included4$new_MDA_IUs_2022 == "ESPEN update: first MDA round in 2022")
indices <- which(condition)
dfAPOC_included4$Cov.in2[indices] <- rep(NA, length.out = length(indices)) # all epi.covs are > 0.6499 for 2022 from sightsavers data
#dfAPOC_included4_ETH_tocheck <- subset(dfAPOC_included4, ADMIN0ISO3 =="ETH" & Year %in% c(2013,2014) & new_MDA_IUs_2022 == "ESPEN update: first MDA round in 2022") # confirmed: need SS data to update
#dfAPOC_included4_ETH_tocheck <- subset(dfAPOC_included4, ADMIN0ISO3 =="ETH" & new_MDA_IUs_2022 == "ESPEN update: first MDA round in 2022") # confirmed: need SS data to update

dfAPOC_included4$cov_source[indices] <- rep(NA, length.out = length(indices))# Use indexing to assign values based on the condition
dfAPOC_included4$cov_specific_source[indices] <- rep(NA, length.out = length(indices))# Use indexing to assign values based on the condition

check_df <- subset(dfAPOC_included4, Year == 2022)
nrow(check_df) # 2107 IUs 
length(unique(check_df$IU_ID_MAPPING)) # 2107 IUs (April 25')

# =========================================#
# Next steps:                              #
# =========================================#

# ===================================================== #
# 1) code variable to state if annual CDTI in each year #
dfAPOC_included4$MDA_CDTI <- NA

condition <- dfAPOC_included4$Cov.in2 > 0
indices <- which(condition)
dfAPOC_included4$MDA_CDTI[indices] <- rep(1, length.out = length(indices))

# ============================================================================ #
# 2) code variable to state if biannual CDTI in each year: ETH, NGA, SSD, TZA  #

Biannual_APOC_IUs <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Biannual in APOC/Biannual_APOC_IUs.csv")
Biannual_APOC_IUs2 <- Biannual_APOC_IUs[!is.na(Biannual_APOC_IUs$Biannual), ]

#=======#
# Uganda #
unique_IUs_UGA <- Biannual_APOC_IUs2 %>%
  filter(ADMIN0ISO3 == "UGA")

unique_IUs_UGA_vec <- as.vector(unique_IUs_UGA$IUs_NAME)

#unique_IUs_UGA <- subset(unique_IUs_UGA, IUs_NAME2 != "Obongi")

grouped_IUs_biannual_UGA <- unique_IUs_UGA %>%
  group_by(Biannual) %>%
  summarise(unique_IUs = list(unique(IUs_NAME))) # find grouping of unique IUs with same biannual years

grouped_IUs_biannual_UGA_vec <- grouped_IUs_biannual_UGA$unique_IUs

dfAPOC_included4$MDA_CDTI_Biannual <- NA # only need this once to create empty col

condition <- dfAPOC_included4$Cov.in2 > 0 &
  dfAPOC_included4$Year > 2005 & dfAPOC_included4$Year < 2011 &
  dfAPOC_included4$ADMIN0ISO3 == "UGA" &
  dfAPOC_included4$IUs_NAME_MAPPING %in% grouped_IUs_biannual_UGA_vec[[1]]
indices <- which(condition)
dfAPOC_included4$MDA_CDTI_Biannual[indices] <- rep(1, length.out = length(indices))

condition <- dfAPOC_included4$Cov.in2 > 0 &
  dfAPOC_included4$Year > 2006 & dfAPOC_included4$Year < 2013 &
  dfAPOC_included4$ADMIN0ISO3 == "UGA" &
  dfAPOC_included4$IUs_NAME_MAPPING %in% grouped_IUs_biannual_UGA_vec[[2]]
indices <- which(condition)
dfAPOC_included4$MDA_CDTI_Biannual[indices] <- rep(1, length.out = length(indices))
# no treatment indicated for KIBAALE in ESPEN

condition <- dfAPOC_included4$Cov.in2 > 0 &
  dfAPOC_included4$Year > 2006 & dfAPOC_included4$Year < 2014 &
  dfAPOC_included4$ADMIN0ISO3 == "UGA" &
  dfAPOC_included4$IUs_NAME_MAPPING %in% grouped_IUs_biannual_UGA_vec[[3]]
indices <- which(condition)
dfAPOC_included4$MDA_CDTI_Biannual[indices] <- rep(1, length.out = length(indices))

condition <- dfAPOC_included4$Cov.in2 > 0 &
  dfAPOC_included4$Year > 2006 & dfAPOC_included4$Year < 2023 &
  dfAPOC_included4$ADMIN0ISO3 == "UGA" &
  dfAPOC_included4$IUs_NAME_MAPPING %in% grouped_IUs_biannual_UGA_vec[[4]]
indices <- which(condition)
dfAPOC_included4$MDA_CDTI_Biannual[indices] <- rep(1, length.out = length(indices))

condition <- dfAPOC_included4$Cov.in2 > 0 &
  dfAPOC_included4$Year > 2011 & dfAPOC_included4$Year < 2023 &
  dfAPOC_included4$ADMIN0ISO3 == "UGA" &
  dfAPOC_included4$IUs_NAME_MAPPING %in% grouped_IUs_biannual_UGA_vec[[5]]
indices <- which(condition)
dfAPOC_included4$MDA_CDTI_Biannual[indices] <- rep(1, length.out = length(indices))

# condition <- dfAPOC_included4$Year > 2011 & dfAPOC_included4$Year < 2023 &
#   dfAPOC_included4$ADMIN0ISO3 == "UGA" &
#   dfAPOC_included4$IU_CODE_MAPPING == "UGA0512349836"
# indices <- which(condition)
# dfAPOC_included4$MDA_CDTI_Biannual[indices] <- rep(1, length.out = length(indices))

condition <- dfAPOC_included4$Cov.in2 > 0 &
  dfAPOC_included4$Year > 2012 & dfAPOC_included4$Year < 2023 &
  dfAPOC_included4$ADMIN0ISO3 == "UGA" &
  dfAPOC_included4$IUs_NAME_MAPPING %in% grouped_IUs_biannual_UGA_vec[[6]]
indices <- which(condition)
dfAPOC_included4$MDA_CDTI_Biannual[indices] <- rep(1, length.out = length(indices))

condition <- dfAPOC_included4$Cov.in2 > 0 &
  dfAPOC_included4$Year > 2014 & dfAPOC_included4$Year < 2023 &
  dfAPOC_included4$ADMIN0ISO3 == "UGA" &
  dfAPOC_included4$IUs_NAME_MAPPING %in% grouped_IUs_biannual_UGA_vec[[7]]
indices <- which(condition)
dfAPOC_included4$MDA_CDTI_Biannual[indices] <- rep(1, length.out = length(indices))

# to check:
# dfAPOC_included4_UGA_tocheck <- subset(dfAPOC_included4, ADMIN2 %in% unique_IUs_UGA_vec & Year == 2022) # confirmed: need SS data to update
# IUs_2022_UGA_withMDA_ESPEN_IUname
# 11 of 12 IUs with 2022 MDA have biannual aswell in UGA now

#=======#
# ETH   #
unique_IUs_ETH <- Biannual_APOC_IUs2 %>%
  filter(ADMIN0ISO3 == "ETH")

grouped_IUs_biannual_ETH <- unique_IUs_ETH %>%
  group_by(Biannual) %>%
  summarise(unique_IUs = list(unique(IUs_NAME))) # find grouping of unique IUs with same biannual years

grouped_IUs_biannual_ETH_vec <- grouped_IUs_biannual_ETH$unique_IUs

condition <- dfAPOC_included4$Cov.in2 > 0 &
  dfAPOC_included4$Year > 2013 & dfAPOC_included4$Year < 2023 &
  dfAPOC_included4$ADMIN0ISO3 == "ETH" &
  dfAPOC_included4$IUs_NAME_MAPPING %in% grouped_IUs_biannual_ETH_vec[[1]]
indices <- which(condition)
dfAPOC_included4$MDA_CDTI_Biannual[indices] <- rep(1, length.out = length(indices))

condition <- dfAPOC_included4$Cov.in2 > 0 &
  dfAPOC_included4$Year > 2014 & dfAPOC_included4$Year < 2023 &
  dfAPOC_included4$ADMIN0ISO3 == "ETH" &
  dfAPOC_included4$IUs_NAME_MAPPING %in% grouped_IUs_biannual_ETH_vec[[2]]
indices <- which(condition)
dfAPOC_included4$MDA_CDTI_Biannual[indices] <- rep(1, length.out = length(indices))

# to check (now with 2022 update):
# unique_IUs_ETH_vec <- as.vector(unique_IUs_ETH$IUs_NAME)
# dfAPOC_included4_ETH_tocheck <- subset(dfAPOC_included4, ADMIN3 %in% unique_IUs_ETH_vec & Year == 2022)

# IUs_2022_ETH_withMDA_ESPEN_IUname <- ETH_MDA_2022$ADMIN3
# IUs_2022_ETH_withMDA_ESPEN_IUname
# dfAPOC_included4_ETH_tocheck2 <- subset(dfAPOC_included4_ETH_tocheck, ADMIN3 %in% IUs_2022_ETH_withMDA_ESPEN_IUname)
# dfAPOC_included4_ETH_tocheck3 <- subset(dfAPOC_included4, ADMIN0ISO3 == "ETH" & MDA_CDTI_Biannual == 1 & Year == 2022)
# dfAPOC_included4_ETH_tocheck3 <- subset(dfAPOC_included4, ADMIN0ISO3 == "ETH" & MDA_CDTI_Biannual == 1)
# dfAPOC_included4_ETH_tocheck4 <- subset(dfAPOC_included4, ADMIN0ISO3 == "ETH" & MDA_CDTI_Biannual == 1 & ADMIN3 %in% IUs_2022_ETH_withMDA_ESPEN_IUname)
# no baseline for: c(18755, 18502)

#=======#
# NGA  #
# no specific data: the CC operates in 9 states (Plateau, Nasarawa, Abia, Anambra, Enugu, Imo, Delta, Edo, and Ebonyi)
# likely biannual in these states. Nigeria 2017 oncho elim plan states annual to 2014 - therefore assume biannual
# from 2015 in the 9 Carter Centre states above

# NGA_CC_states <- c("Plateau", "Nasarawa", "Abia", "Anambra", "Enugu", "Imo", "Delta", "Edo", "Ebonyi")
#
# condition <- dfAPOC_included4$Cov.in2 > 0 &
#   dfAPOC_included4$Year > 2013 & dfAPOC_included4$Year < 2023 &
#   dfAPOC_included4$ADMIN0ISO3 == "NGA" &
#   dfAPOC_included4$ADMIN1 %in% NGA_CC_states
# indices <- which(condition)
# dfAPOC_included4$MDA_CDTI_Biannual[indices] <- rep(1, length.out = length(indices))

# based on CC data #
NGA_post2017 <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/data from CC/NGA_post2017.csv")
columns_to_update <- names(NGA_post2017)[6:21]
new_column_names <- sub("^.", "", columns_to_update)
names(NGA_post2017)[6:21] <- new_column_names # Rename the columns

NGA_grouped_LGA_biannual <- NGA_post2017 %>%
  group_by(biannual_years) %>%
  summarise(UniqueLGAs = toString(unique(LGA_updated))) # group LGAs

# first group of LGAs
LGAs1 <- unlist(strsplit(as.character(NGA_grouped_LGA_biannual[1, 2]), ",\\s*")) # Split the string into a character vector
condition1 <- with(dfAPOC_included4, ADMIN0ISO3 == "NGA" & Year %in% c(2016, 2017, 2018, 2021, 2022) & ADMIN2 %in% LGAs1)
indices <- which(condition1)
dfAPOC_included4$MDA_CDTI_Biannual[indices] <- rep(1, length.out = length(indices))

# 2nd group of LGAs
LGAs2 <- unlist(strsplit(as.character(NGA_grouped_LGA_biannual[2, 2]), ",\\s*")) # Split the string into a character vector
condition2 <- with(dfAPOC_included4, ADMIN0ISO3 == "NGA" & Year %in% c(2017) & ADMIN2 %in% LGAs2)
indices <- which(condition2)
dfAPOC_included4$MDA_CDTI_Biannual[indices] <- rep(1, length.out = length(indices))

# 3rd group of LGAs
LGAs3 <- unlist(strsplit(as.character(NGA_grouped_LGA_biannual[3, 2]), ",\\s*")) # Split the string into a character vector
condition3 <- with(dfAPOC_included4, ADMIN0ISO3 == "NGA" & Year %in% c(2017, 2018) & ADMIN2 %in% LGAs3)
indices <- which(condition3)
dfAPOC_included4$MDA_CDTI_Biannual[indices] <- rep(1, length.out = length(indices))

# 4th group of LGAs
LGAs4 <- unlist(strsplit(as.character(NGA_grouped_LGA_biannual[4, 2]), ",\\s*")) # Split the string into a character vector
condition4 <- with(dfAPOC_included4, ADMIN0ISO3 == "NGA" & Year %in% c(2017, 2018, 2021, 2022) & ADMIN2 %in% LGAs4)
indices <- which(condition4)
dfAPOC_included4$MDA_CDTI_Biannual[indices] <- rep(1, length.out = length(indices))

# 5th group of LGAs
LGAs5 <- unlist(strsplit(as.character(NGA_grouped_LGA_biannual[5, 2]), ",\\s*")) # Split the string into a character vector
condition5 <- with(dfAPOC_included4, ADMIN0ISO3 == "NGA" & Year %in% c(2018) & ADMIN2 %in% LGAs5)
indices <- which(condition5)
dfAPOC_included4$MDA_CDTI_Biannual[indices] <- rep(1, length.out = length(indices))

# look at NGA :
# dfAPOC_included4_NGA <- subset(dfAPOC_included4, ADMIN0 == "Nigeria")
# dfAPOC_included4_NGA$year_tmp <- dfAPOC_included4_NGA$Year

# check:
# dfAPOC_included4_NGA_tocheck <- subset(dfAPOC_included4, ADMIN0ISO3 == "NGA" & MDA_CDTI_Biannual == 1 & Year == 2022)

#======#
#  SDN #
unique_IUs_SDN <- Biannual_APOC_IUs2 %>%
  filter(ADMIN0ISO3 == "SDN")

grouped_IUs_biannual_SDN <- unique_IUs_SDN %>%
  group_by(Biannual) %>%
  summarise(unique_IUs = list(unique(IUs_NAME))) # find grouping of unique IUs with same biannual years

grouped_IUs_biannual_SDN_vec <- grouped_IUs_biannual_SDN$unique_IUs

condition <- dfAPOC_included4$Cov.in2 > 0 &
  dfAPOC_included4$Year > 2006 & dfAPOC_included4$Year < 2013 &
  dfAPOC_included4$ADMIN0ISO3 == "SDN" &
  dfAPOC_included4$IUs_NAME_MAPPING %in% grouped_IUs_biannual_SDN_vec[[1]]
indices <- which(condition)
dfAPOC_included4$MDA_CDTI_Biannual[indices] <- rep(1, length.out = length(indices))

condition <- dfAPOC_included4$Cov.in2 > 0 &
  dfAPOC_included4$Year > 2009 & dfAPOC_included4$Year < 2018 &
  dfAPOC_included4$ADMIN0ISO3 == "SDN" &
  dfAPOC_included4$IUs_NAME_MAPPING %in% grouped_IUs_biannual_SDN_vec[[2]]
indices <- which(condition)
dfAPOC_included4$MDA_CDTI_Biannual[indices] <- rep(1, length.out = length(indices))

condition <- dfAPOC_included4$Cov.in2 > 0 &
  dfAPOC_included4$Year > 2021 &
  dfAPOC_included4$ADMIN0ISO3 == "SDN" &
  dfAPOC_included4$IUs_NAME_MAPPING %in% grouped_IUs_biannual_SDN_vec[[3]]
indices <- which(condition)
dfAPOC_included4$MDA_CDTI_Biannual[indices] <- rep(1, length.out = length(indices))

#==== #
# TZN #
unique_IUs_TZA <- Biannual_APOC_IUs2 %>%
  filter(ADMIN0ISO3 == "TZA")

grouped_IUs_biannual_TZA <- unique_IUs_TZA %>%
  group_by(Biannual) %>%
  summarise(unique_IUs = list(unique(IUs_NAME))) # find grouping of unique IUs with same biannual years

grouped_IUs_biannual_TZA_vec <- grouped_IUs_biannual_TZA$unique_IUs

condition <- dfAPOC_included4$Cov.in2 > 0 &
  dfAPOC_included4$Year > 2018 &
  dfAPOC_included4$ADMIN0ISO3 == "TZA" &
  dfAPOC_included4$IUs_NAME_MAPPING %in% grouped_IUs_biannual_TZA_vec[[1]]

indices <- which(condition)
dfAPOC_included4$MDA_CDTI_Biannual[indices] <- rep(1, length.out = length(indices))

condition <- dfAPOC_included4$Year > 2018 &
  dfAPOC_included4$ADMIN0ISO3 == "TZA" &
  dfAPOC_included4$IUs_NAME == "Malinyi"
indices <- which(condition)
dfAPOC_included4$MDA_CDTI_Biannual[indices] <- rep(1, length.out = length(indices))

#==== #
# SSD #
unique_IUs_SSD <- Biannual_APOC_IUs2 %>%
  filter(ADMIN0ISO3 == "SSD")

grouped_IUs_biannual_SSD <- unique_IUs_SSD %>%
  group_by(Biannual) %>%
  summarise(unique_IUs = list(unique(IUs_NAME))) # find grouping of unique IUs with same biannual years

grouped_IUs_biannual_SSD_vec <- grouped_IUs_biannual_SSD$unique_IUs

condition <- dfAPOC_included4$Cov.in2 > 0 &
  dfAPOC_included4$Year > 2020 &
  dfAPOC_included4$ADMIN0ISO3 == "SSD" &
  dfAPOC_included4$IUs_NAME_MAPPING %in% grouped_IUs_biannual_SSD_vec[[1]]
indices <- which(condition)
dfAPOC_included4$MDA_CDTI_Biannual[indices] <- rep(1, length.out = length(indices))

check_df <- subset(dfAPOC_included4, Year == 2022)
nrow(check_df) # 2107 IUs 
length(unique(check_df$IU_ID_MAPPING)) # 2107 IUs

#===================================================#
# 3) code variable to state if vector control : UGA #

# TO DO: find foci in UGA where "Vector eliminated" and reduce ABR to 0 after VC in these IUs

unique_IUs_UGA <- Biannual_APOC_IUs2 %>%
  filter(ADMIN0ISO3 == "UGA")

grouped_IUs_vectorcntrl_UGA <- unique_IUs_UGA %>%
  group_by(vector_control) %>%
  summarise(unique_IUs = list(unique(IUs_NAME))) # find grouping of unique IUs with same biannual years

grouped_IUs_vectorcntrl_UGA_vec <- grouped_IUs_vectorcntrl_UGA$unique_IUs

dfAPOC_included4$vector_control <- NA # only need this once to create empty col

# UGA foci 1 with VC (Mpamba-Nkusi); 2003-2007 VC and vector elimination after 2007
condition <- dfAPOC_included4$Year > 2002 & dfAPOC_included4$Year < 2008 &
  dfAPOC_included4$ADMIN0ISO3 == "UGA" &
  dfAPOC_included4$IUs_NAME_MAPPING %in% grouped_IUs_vectorcntrl_UGA_vec[[2]] # Mpamba-Nkusi foci (Kibaale) VC
indices <- which(condition)
dfAPOC_included4$vector_control[indices] <- rep(1, length.out = length(indices))

condition <- dfAPOC_included4$Year > 2007 & dfAPOC_included4$Year < 2023 &
  dfAPOC_included4$ADMIN0ISO3 == "UGA" &
  dfAPOC_included4$IUs_NAME_MAPPING %in% grouped_IUs_vectorcntrl_UGA_vec[[2]] # Mpamba-Nkusi foci (Kibaale) VE
indices <- which(condition)
dfAPOC_included4$vector_control[indices] <- rep(2, length.out = length(indices))

# UGA foci 2 with VC (# Mt Elgon foci); 2006-2009 VC and vector elimination after 2009
condition <- dfAPOC_included4$Year > 2006 & dfAPOC_included4$Year < 2010 &
  dfAPOC_included4$ADMIN0ISO3 == "UGA" &
  dfAPOC_included4$IUs_NAME_MAPPING %in% grouped_IUs_vectorcntrl_UGA_vec[[3]] # Mt Elgon foci (Mbale etc) VC
indices <- which(condition)
dfAPOC_included4$vector_control[indices] <- rep(1, length.out = length(indices))

condition <- dfAPOC_included4$Year > 2009 & dfAPOC_included4$Year < 2023 &
  dfAPOC_included4$ADMIN0ISO3 == "UGA" &
  dfAPOC_included4$IUs_NAME_MAPPING %in% grouped_IUs_vectorcntrl_UGA_vec[[3]] # Mt Elgon foci (Mbale etc) VE
indices <- which(condition)
dfAPOC_included4$vector_control[indices] <- rep(2, length.out = length(indices))

# UGA foci 3 with VC (# Kashoya-Kitomi & Wambabya-Rwamarongo foci); 2008-2010 VC and vector elimination after 2010
condition <- dfAPOC_included4$Year > 2007 & dfAPOC_included4$Year < 2011 &
  dfAPOC_included4$ADMIN0ISO3 == "UGA" &
  dfAPOC_included4$IUs_NAME_MAPPING %in% grouped_IUs_vectorcntrl_UGA_vec[[4]] # Kashoya-Kitomi foci (Ibanda etc) & Wambabya-Rwamarongo (Hoima)
indices <- which(condition)
dfAPOC_included4$vector_control[indices] <- rep(1, length.out = length(indices))

condition <- dfAPOC_included4$Year > 2010 & dfAPOC_included4$Year < 2023 &
  dfAPOC_included4$ADMIN0ISO3 == "UGA" &
  dfAPOC_included4$IUs_NAME_MAPPING %in% grouped_IUs_vectorcntrl_UGA_vec[[4]] # Kashoya-Kitomi foci (Ibanda etc) & Wambabya-Rwamarongo (Hoima)
indices <- which(condition)
dfAPOC_included4$vector_control[indices] <- rep(2, length.out = length(indices))

# UGA foci 4 with VC (# Nyagak-Bondo foci); 2012-2013 VC and vector elimination after 2013
condition <- dfAPOC_included4$Year > 2011 & dfAPOC_included4$Year < 2014 &
  dfAPOC_included4$ADMIN0ISO3 == "UGA" &
  dfAPOC_included4$IUs_NAME_MAPPING %in% grouped_IUs_vectorcntrl_UGA_vec[[5]] # Nyagak-Bondo foci (Zombo etc)
indices <- which(condition)
dfAPOC_included4$vector_control[indices] <- rep(1, length.out = length(indices))

condition <- dfAPOC_included4$Year > 2013 & dfAPOC_included4$Year < 2023 &
  dfAPOC_included4$ADMIN0ISO3 == "UGA" &
  dfAPOC_included4$IUs_NAME_MAPPING %in% grouped_IUs_vectorcntrl_UGA_vec[[5]] # Nyagak-Bondo foci (Zombo etc)
indices <- which(condition)
dfAPOC_included4$vector_control[indices] <- rep(2, length.out = length(indices))

# UGA foci 5 with VC (# Budongo foci); 2012-2014 VC and vector elimination after 2014
condition <- dfAPOC_included4$Year > 2011 & dfAPOC_included4$Year < 2015 &
  dfAPOC_included4$ADMIN0ISO3 == "UGA" &
  dfAPOC_included4$IUs_NAME_MAPPING %in% grouped_IUs_vectorcntrl_UGA_vec[[6]] # Budongo foci (Masindi etc)
indices <- which(condition)
dfAPOC_included4$vector_control[indices] <- rep(1, length.out = length(indices))

condition <- dfAPOC_included4$Year > 2014 & dfAPOC_included4$Year < 2023 &
  dfAPOC_included4$ADMIN0ISO3 == "UGA" &
  dfAPOC_included4$IUs_NAME_MAPPING %in% grouped_IUs_vectorcntrl_UGA_vec[[6]] # Budongo foci (Masindi etc)
indices <- which(condition)
dfAPOC_included4$vector_control[indices] <- rep(2, length.out = length(indices))

# UGA foci 6 with VC (# Madi Mid North foci); 2012-current VC and NO vector elimination
condition <- dfAPOC_included4$Year > 2011 & dfAPOC_included4$Year < 2023 &
  dfAPOC_included4$ADMIN0ISO3 == "UGA" &
  dfAPOC_included4$IUs_NAME_MAPPING %in% grouped_IUs_vectorcntrl_UGA_vec[[7]] # Madi Mid North foci (Gulu etc)
indices <- which(condition)
dfAPOC_included4$vector_control[indices] <- rep(1, length.out = length(indices))

# UGA foci 7 with VC (# Lhubirha foci); 2015-current VC and NO vector elimination
condition <- dfAPOC_included4$Year > 2014 & dfAPOC_included4$Year < 2023 &
  dfAPOC_included4$ADMIN0ISO3 == "UGA" &
  dfAPOC_included4$IUs_NAME_MAPPING %in% grouped_IUs_vectorcntrl_UGA_vec[[8]] # Lhubirha foci (Kasese; not also Nyamugasani foci here)
indices <- which(condition)
dfAPOC_included4$vector_control[indices] <- rep(1, length.out = length(indices))

# UGA foci 8 with VC (# Itwara foci); 1993 - 2003 VC and vector elimination after 2003
condition <- dfAPOC_included4$Year > 1992 & dfAPOC_included4$Year < 2004 &
  dfAPOC_included4$ADMIN0ISO3 == "UGA" &
  dfAPOC_included4$IUs_NAME_MAPPING %in% c("Bunyangabu", "KABAROLE", "KYENJOJO") # Itwara foci (Kabarole etc)
indices <- which(condition)
dfAPOC_included4$vector_control[indices] <- rep(1, length.out = length(indices))

condition <- dfAPOC_included4$Year > 2003 & dfAPOC_included4$Year < 2023 &
  dfAPOC_included4$ADMIN0ISO3 == "UGA" &
  dfAPOC_included4$IUs_NAME_MAPPING %in% c("Bunyangabu", "KABAROLE", "KYENJOJO") # Itwara foci (Kabarole etc)
indices <- which(condition)
dfAPOC_included4$vector_control[indices] <- rep(2, length.out = length(indices))

#====================================================================================================#
# 3) code variable to state if vector control : Equatorial Guinea (Bioke Island; vector elimination) #

# info from Herrador et al. 2018
# more specifics from Traore et al. 2009 - VC attempts in 2001, 2003 and 2005, elimination in 2006 ("last biting S. damnosum s.l. was caught in March 2005")
# https://www.sciencedirect.com/science/article/pii/S0001706X09000618?via%3Dihub

# 2001 - 2005 - vector control (ABR 80% proportional reduction = 1 ) #
condition <- dfAPOC_included4$Year > 2000 & dfAPOC_included4$Year < 2006 &
  dfAPOC_included4$ADMIN0ISO3 == "GNQ" &
  dfAPOC_included4$ADMIN1 %in% c("Bioko Norte", "Bioko Sur") #
indices <- which(condition)
dfAPOC_included4$vector_control[indices] <- rep(1, length.out = length(indices))

# 2006 - 2022 - vector elimination (ABR to 0 = 2) #
condition <- dfAPOC_included4$Year > 2005 & dfAPOC_included4$Year < 2023 &
  dfAPOC_included4$ADMIN0ISO3 == "GNQ" &
  dfAPOC_included4$ADMIN1 %in% c("Bioko Norte", "Bioko Sur") #
indices <- which(condition)
dfAPOC_included4$vector_control[indices] <- rep(2, length.out = length(indices))

# ================================================================== #
#    Also need to update Bioko MDA info according to Herrador et al. #

# should all start from 1989 on Bioko to 1998 according to ernández González et al. 2016 : https://parasitesandvectors.biomedcentral.com/articles/10.1186/s13071-016-1779-8
# assume low pre-APOC voerage of 25% total pop cov
condition <- dfAPOC_included4$Year > 1988 & dfAPOC_included4$Year < 1999 &
  dfAPOC_included4$ADMIN0ISO3 == "GNQ" &
  dfAPOC_included4$IUs_NAME_MAPPING %in% c("Malabo", "Baney", "Luba", "Riaba") #
indices <- which(condition)
dfAPOC_included4$MDA_CDTI[indices] <- rep(1, length.out = length(indices))
dfAPOC_included4$Cov.in2[indices] <- rep(0.25, length.out = length(indices)) # Hernández González et al. 2016 indicate total eligible coverage < 80%
# (< 65% total pop coverage on average) = 25% coverage used for these years

# add columns for raw coverage (if applicable), and source of info
#dfAPOC_included4$MDA_CDTI_raw[indices] <- dfAPOC_included4$EpiCov[indices]/100
dfAPOC_included4$cov_source[indices] <- rep("Subnational coverage data (literature)", length.out = length(indices))# Use indexing to assign values based on the condition
dfAPOC_included4$cov_specific_source[indices] <- rep("Hernández González et al. 2016", length.out = length(indices))# Use indexing to assign values based on the condition


# extend MDA (annual ) from 2011 to 2016 for 3 IUs #
condition <- dfAPOC_included4$Year > 2010 & dfAPOC_included4$Year < 2017 &
  dfAPOC_included4$ADMIN0ISO3 == "GNQ" &
  dfAPOC_included4$IUs_NAME_MAPPING %in% c("Baney", "Luba", "Riaba") #
indices <- which(condition)
dfAPOC_included4$MDA_CDTI[indices] <- rep(1, length.out = length(indices))
dfAPOC_included4$Cov.in2[indices] <- rep(0.25, length.out = length(indices)) # Hernández González et al. 2016 indicate total eligible coverage < 80%
                                                                            # (< 65% total pop coverage on average) = 25% coverage used for these years
# add columns for raw coverage (if applicable), and source of info
#dfAPOC_included4$MDA_CDTI_raw[indices] <- dfAPOC_included4$EpiCov[indices]/100
dfAPOC_included4$cov_source[indices] <- rep("Subnational coverage data (literature)", length.out = length(indices))# Use indexing to assign values based on the condition
dfAPOC_included4$cov_specific_source[indices] <- rep("Hernández González et al. 2016", length.out = length(indices))# Use indexing to assign values based on the condition


condition <- dfAPOC_included4$Year > 2010 & dfAPOC_included4$Year < 2013 &
  dfAPOC_included4$ADMIN0ISO3 == "GNQ" &
  dfAPOC_included4$IUs_NAME_MAPPING %in% c("Malabo") #
indices <- which(condition)
dfAPOC_included4$MDA_CDTI[indices] <- rep(1, length.out = length(indices))
dfAPOC_included4$Cov.in2[indices] <- rep(0.25, length.out = length(indices)) # Hernández González et al. 2016 indicate total eligible coverage < 80%
                                                                             # (< 65% total pop coverage on average) = 25% coverage used for these years
#dfAPOC_included4$Year_tmp <- dfAPOC_included4$Year
# add columns for raw coverage (if applicable), and source of info
#dfAPOC_included4$MDA_CDTI_raw[indices] <- dfAPOC_included4$EpiCov[indices]/100
dfAPOC_included4$cov_source[indices] <- rep("Subnational coverage data (literature)", length.out = length(indices))# Use indexing to assign values based on the condition
dfAPOC_included4$cov_specific_source[indices] <- rep("Hernández González et al. 2016", length.out = length(indices))# Use indexing to assign values based on the condition

check_df <- subset(dfAPOC_included4, Year == 2022)
nrow(check_df) # 2107 IUs 
length(unique(check_df$IU_ID_MAPPING)) # 2107 IUs

# map VC and biannual again?

dfAPOC_included4 <- dfAPOC_included4 %>%
  group_by(IU_ID_MAPPING) %>%
  mutate(biannual_included = any(MDA_CDTI_Biannual == 1))

dfAPOC_included4 <- dfAPOC_included4 %>%
  group_by(IU_ID_MAPPING) %>%
  mutate(VC_included = any(vector_control == 1))

dfAPOC_included4 <- dfAPOC_included4 %>%
  group_by(IU_ID_MAPPING) %>%
  mutate(annual_only_included = all(is.na(biannual_included)) & any(MDA_CDTI == 1))

dfAPOC_included4 <- dfAPOC_included4 %>%
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

biannualVC_df_lastyr <- subset(dfAPOC_included4, Year == 2021)

all_MDA_VC_trtnaive_IUs <- ESPEN_IUs_ALL %>%
  left_join(biannualVC_df_lastyr, by = c("IU_ID" = "IU_ID_MAPPING"))

#all_MDA_VC_trtnaive_IUs$biannual_VC_mapping <- ifelse(is.na(all_MDA_VC_trtnaive_IUs$biannual_VC_mapping), "non-endemic", all_MDA_VC_trtnaive_IUs$biannual_VC_mapping)

cbPalette_MDAVC <- c("#CC79A7","#E69F00","#009E73","#F0E442","#0072B2")

# check
ggplot() +
  geom_sf(data = all_MDA_VC_trtnaive_IUs, aes(fill = biannual_VC_mapping), colour = NA, alpha = 0.7) +
  geom_sf(data = ESPEN_IUs_APOC, aes(), colour = NA, size = 1, fill = NA, alpha = 0.1) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
  coord_sf(xlim = c(-20, 50), ylim = c(38, -35)) +
  theme_bw() +
  scale_fill_manual(values = cbPalette_MDAVC, na.value = "gray") +
  scale_colour_manual(na.value="gray")+
  labs(fill='') +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal")

# check Bioko island (Eq Guinea) - now with VC
ggplot() +
  geom_sf(data = all_MDA_VC_trtnaive_IUs, aes(fill = biannual_VC_mapping), colour = NA, alpha = 0.7) +
  geom_sf(data = ESPEN_IUs_APOC, aes(), colour = NA, size = 1, fill = NA, alpha = 0.1) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
  coord_sf(xlim = c(8, 10), ylim = c(3, 5)) +
  theme_bw() +
  scale_fill_manual(values = cbPalette_MDAVC, na.value = "gray") +
  scale_colour_manual(na.value="gray")+
  labs(fill='') +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal")

# plot for south sudan (Maridi focus)
ggplot() +
  geom_sf(data = all_MDA_VC_trtnaive_IUs, aes(fill = biannual_VC_mapping), colour = NA, alpha = 0.7) +
  geom_sf(data = ESPEN_IUs_APOC, aes(), colour = NA, size = 1, fill = NA, alpha = 0.1) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", linewidth = 1.2) +
  coord_sf(xlim = c(24.5, 35), ylim = c(4, 12)) +
  theme_bw() +
  scale_fill_manual(values = cbPalette_MDAVC, na.value = "gray") +
  scale_colour_manual(na.value="gray")+
  labs(fill='') +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal")



# # just eq.guinea
# ggplot() +
#   geom_sf(data = all_MDA_VC_trtnaive_IUs, aes(fill = biannual_VC_mapping), colour = NA, alpha = 0.7) +
#   geom_sf(data = ESPEN_IUs_APOC, aes(), colour = "grey", size = 1, fill = NA, alpha = 0.1) +
#   geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
#   coord_sf(xlim = c(9.5, 12), ylim = c(0.9, 2.5)) +
#   theme_bw() +
#   scale_fill_manual(values = cbPalette_endemicity, na.value = "gray") +
#   scale_colour_manual(na.value="gray")+
#   labs(fill='') +
#   theme(
#     legend.position = "bottom",  # Place the legend at the bottom
#     legend.direction = "horizontal")


# check if any IUs where only MDA_CDTI == 1 found for 2013 (erroneously coded)

problem_IUS_2013 <- dfAPOC_included4 %>%
  filter(MDA_CDTI == 1) %>%
  group_by(IU_ID_MAPPING) %>%
  filter(n_distinct(Year) == 1 & Year == 2013) %>%
  ungroup() %>%
  distinct(IU_ID_MAPPING)

problem_IUS_2013

nrow(subset(dfAPOC_included4, Year == 2022)) # check n IUs = 2346 (old) # 2460 now (March 25 - all mapped samples added)
                                             # 2107 IUs (April 2025)

# ============================================================================ #
#    filter out non-endemics (from baseline mapping) as cannot fit at baseline #

no_baseline_IUs <- load("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/no_map_samples.Rdata")

non_endemic_IUs <- dfAPOC_included4 %>%
  filter(endemicity_baseline == "non-endemic") %>%
  select(IUs_NAME_MAPPING, Endemicity, MAX_Endemicity, MAX_Endemicity_Parent, endemicity_baseline, mean) %>%
  distinct()

non_endemic_IUs

non_endemic_IUs_baseline_vec <-  non_endemic_IUs$IUs_NAME_MAPPING

length(unique(non_endemic_IUs$IUs_NAME_MAPPING)) # IT ACTUALLY 62 IUs (April 25') - unique names/codes

test_df <- subset(dfAPOC_included4, !(IUs_NAME_MAPPING %in% non_endemic_IUs_baseline_vec))
test_df_lastyr <- subset(test_df, Year == 2021)

length(unique(test_df_lastyr$IU_ID_MAPPING)) # 2045 IUs (April 25')

all_endemic_with_baseline_IUs <- ESPEN_IUs_ALL %>%
  left_join(test_df_lastyr, by = c("IU_ID" = "IU_ID_MAPPING"))

cbPalette_endemicity <- c("#CC79A7","#E69F00","#009E73")


# check
ggplot() +
  geom_sf(data = all_endemic_with_baseline_IUs, aes(fill = endemicity_reclassified), colour = NA, alpha = 0.7) +
  geom_sf(data = ESPEN_IUs_APOC, aes(), colour = NA, size = 1, fill = NA, alpha = 0.1) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
  coord_sf(xlim = c(-20, 50), ylim = c(38, -35)) +
  theme_bw() +
  scale_fill_manual(values = cbPalette_endemicity, na.value = "gray") +
  scale_colour_manual(na.value="gray")+
  labs(fill='') +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal")

# take these out of the main "endemic" dataframe

dfAPOC_included4 <- subset(dfAPOC_included4, !(IUs_NAME_MAPPING %in% non_endemic_IUs_baseline_vec))
nrow(subset(dfAPOC_included4, Year == 2022)) # check n IUs = 2280 (reduced from 2346 IUs)
                                             # April 25' = 2045 IUs reduced from 2107

# NOTE : in the flowchart, this is the same as removing these "non-endemic" in Zoure from those that are 
# ever "Endemic" in ESPEN (creating the 1463 --> 1437 IUs - 62 = 1375 IUs)
# Old total was : 1437 + 151 + 301 + 475 = 2364 IUs
# New total is  : 1375 + 151 + 301 + 475  = 2302 IUs

biannualVC_df_lastyr_upd <- subset(dfAPOC_included4, Year == 2021)

length(unique(biannualVC_df_lastyr_upd$IU_ID_MAPPING))

all_MDA_VC_trtnaive_IUs2 <- ESPEN_IUs_ALL %>%
  left_join(biannualVC_df_lastyr_upd, by = c("IU_ID" = "IU_ID_MAPPING"))

ggplot() +
  geom_sf(data = all_MDA_VC_trtnaive_IUs2, aes(fill = biannual_VC_mapping), colour = NA, alpha = 0.7) +
  geom_sf(data = ESPEN_IUs_APOC, aes(), colour = NA, size = 1, fill = NA, alpha = 0.1) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
  coord_sf(xlim = c(-20, 50), ylim = c(38, -35)) +
  theme_bw() +
  scale_fill_manual(values = cbPalette_MDAVC, na.value = "gray") +
  scale_colour_manual(na.value="gray")+
  labs(fill='') +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal")

# ======================= #
# check new IUs included - March 25

# Find common elements between the two columns
common_IU_codes <- intersect(APOC_baseline_additional$IU_CODE, dfAPOC_included4$IU_CODE)

common_IU_codes # Print the common IU_CODE values

# additional IUs found for NGA
missing_IU_codes_NGA <- setdiff(APOC_baseline_additional$IU_CODE, APOC_baseline$IU_CODE)

missing_IU_codes_NGA


nrow(subset(dfAPOC_included4, Year == 2022)) # check n IUs = 2280 (reduced from 2346 IUs)
                                             # April 25' : 2045 IUs reduced from 2107

# ========================================================================= #
#    too many NGA IUs (non-endemic in snapshot) - need to filter these out) #
#                          March 2025                                       #

NGA_subset <- subset(dfAPOC_included4, IU_CODE %in% missing_IU_codes_NGA)
NGA_subset_IUsnames <- unique(NGA_subset$IUs_NAME)

length(NGA_subset_IUsnames)
length(common_IU_codes)
length(missing_IU_codes_NGA)

# check in oncho snapshot #
oncho_snapshot_subset <- subset(oncho_snapshot_long_raw, ADMIN2 %in% NGA_subset_IUsnames)
length(unique(oncho_snapshot_subset$LGA))

missing_IUs_check <- setdiff(NGA_subset_IUsnames, oncho_snapshot_long_raw$ADMIN2)
missing_IUs_check # 14 IU names in espen but not found in oncho snapshot so ommitted

# find those that are endemic #
unique(oncho_snapshot_subset$Endemic_classification)

oncho_snapshot_subset_endemic <- subset(oncho_snapshot_subset, Endemic_classification == "Endemic ")

endemic_NGA_extraIUs <- unique(oncho_snapshot_subset_endemic$ADMIN2)
endemic_NGA_extraIUs # 3 extra IUs found for Nigeria (because of missing in original APOC_Baseline values) - Surulere in Lagos to be removed next!

# remove  and rows with both"Surulere" in ADMIN2 & "Lagos" ADMIN1
dfAPOC_included4 <- dfAPOC_included4 %>% filter(!(ADMIN2 == "Surulere" & ADMIN1 == "Lagos"))

nrow(subset(dfAPOC_included4, Year == 2022)) # 2044 (1 removed from NGA) = now a total of 251 NGA IUs removed because non-endemic

# # just eq.guinea
# ggplot() +
#   geom_sf(data = all_MDA_VC_trtnaive_IUs2, aes(fill = biannual_VC_mapping), colour = NA, alpha = 0.7) +
#   geom_sf(data = ESPEN_IUs_APOC, aes(), colour = "grey", size = 1, fill = NA, alpha = 0.1) +
#   geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
#   coord_sf(xlim = c(8.5, 11.5), ylim = c(0.9, 5)) +
#   theme_bw() +
#   scale_fill_manual(values = cbPalette_endemicity, na.value = "gray") +
#   scale_colour_manual(na.value="gray")+
#   labs(fill='') +
#   theme(
#     legend.position = "bottom",  # Place the legend at the bottom
#     legend.direction = "horizontal")

# =========================================== #
# 4) extend each IU by 3 years (2023 - 2025)  #

dfAPOC_included5 <- dfAPOC_included4 # RESET HERE

nrow(subset(dfAPOC_included5, Year == 2022)) # check n IUs = 2346; 2108 IUs March 2025
                                             # 2044 IUs (April 2025)

#FinalESPENyr <- 2013
FinalESPENyr <- 2022

dfls <- split(dfAPOC_included5, dfAPOC_included5$IU_ID_MAPPING)

## new list of dfs
newdf <- vector("list", length(dfls))

lsid <- split(dfAPOC_included5$IU_ID_MAPPING, dfAPOC_included5$IU_ID_MAPPING)
lsid <- unique(unlist(lsid))

# loop to fill in missing years and columns

#loop_func <- function(dfls, dfAPOC_included5, FinalESPENyr, newdf, lsid) {
for (i in 1:length(dfls)) {

  newdf[[i]] <- subset(dfls[[i]], select=colnames(dfAPOC_included5))

  #IUStartMDA <- max(min(newdf[[i]]$Year) - min(newdf[[i]]$Cum_MDA) - start)

  IUend <- 2025

  newdf[[i]]$IUend <- IUend
  Futureyrs <- IUend - FinalESPENyr ## prior years to include

  #PriorMDA <- FinalESPENyr - IUStartMDA ## prior MDA rounds

  newdf[[i]]$lsID <- i

  i

  if (is.na(IUend)!=T) {
    if(IUend>FinalESPENyr) {
      # if cumulative rounds greater than 1, augment data frame
      # by repeating the first row

      tmp <- newdf[[i]][rep(1, each = (min(IUend - newdf[[i]]$Year))), ]

      # fill in the years from start date of IU to first year on ESPEN
      #tmp$Year <- seq(IUstart, min(newdf[[i]]$Year)-1)
      tmp$Year <- seq(FinalESPENyr+1, IUend)

      # fill in with years that had MDA (where any cum_MDA > 1 in 2013 onwards)

      # if(any(head(newdf[[i]]$Cum_MDA)> 0)) {
      #   # tmp$MDA[1:PriorMDA] <- 1 # this should be from last row (2012) to n row (i.e. (nrow(tmp) - priorMDA) : (nrow(tmp)) )
      #
      #   val_totest <- head(newdf[[i]]$Cum_MDA,1)
      #
      #   # where top cum_MDA value is greater than 1
      #   if(val_totest > 1){
      #
      #     # Determine the number of leading zeros based on your condition
      #     leading_zeros <- rep(0, 37 - head(newdf[[i]]$Cum_MDA,1) + 1) # back to 2000 (or 1999)
      #     #leading_zeros <- rep(0, 27 - head(newdf[[1]]$Cum_MDA,1) + 1)
      #
      #
      #     # Create the sequence that goes in reverse from 0 to 27
      #     tmp$Cum_MDA <- c(leading_zeros, seq(from = 0, to = head(newdf[[i]]$Cum_MDA,1) - 1, by = 1))
      #
      #   }

      }

      # #enedmicity category given "Augmented Status"
      tmp$Endemicity <- "Augmented"
      tmp$MDA_scheme <- "Augmented"
      # #tmp$Endemicity_ID <- 6

      newdf[[i]] <- rbind(newdf[[i]], tmp) # want tmp at end
    }
  }

#}


#test <- loop_func(dfls = dfls, dfAPOC_included = dfAPOC_included5, FinalESPENyr = FinalESPENyr, newdf = newdf, lsid = lsid)

newdf <- do.call(rbind, newdf)

dfAPOC_included5 <- newdf

dfAPOC_included5$Year_tmp <- dfAPOC_included5$Year

# ================================================================================================ #
# 5) label each IU on forward status: MDA stopped (post intervention surveillance in 2022 or 2021
#    or all EpiCov == 0 for "unknowns" / MDA continue/ treatment naive

final_endemic_PIS_yr_check <- dfAPOC_included5 %>%
  filter(Endemicity == "Endemic (under post-intervention surveillance)") %>%
  group_by(IU_ID_MAPPING) %>%
  summarise(LastYear = max(Year))

# =================================================== #
dfAPOC_included6 <- dfAPOC_included5 # reset here
length(unique(dfAPOC_included6$IU_ID_MAPPING)) # 2044 IUs (April 2025)

# one IU needs treatment introduced in UGA based on "Endemic (under post intervention surveillance")
condition <- with(dfAPOC_included6, IU_ID_MAPPING == 49752 & trimws(Pre_ESPEN_MDA_history) == "Include" & Year > 1998 & Year < 2013)
indices <- which(condition)
replace_MDA_CDTI <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1)
dfAPOC_included6$MDA_CDTI[indices] <- rep(replace_MDA_CDTI, length.out = length(indices))
dfAPOC_included6$Cov.in2[indices] <- rep(coverages_UGA2, length.out = length(indices))

# for years covered by APOC, 2015 report
dfAPOC_included6$MDA_CDTI_raw[indices] <- rep(coverages_UGA, length.out = length(indices))# Use indexing to assign values based on the condition
dfAPOC_included6$cov_source[indices] <- rep("national-level coverage data", length.out = length(indices))# Use indexing to assign values based on the condition
dfAPOC_included6$cov_specific_source[indices] <- rep("APOC report, 2015", length.out = length(indices))# Use indexing to assign values based on the condition

# make treatment status in 2022 label

dfAPOC_included6 <- dfAPOC_included6 %>%
  group_by(IU_ID_MAPPING) %>%
  mutate(
    trt_status_2022 = case_when(
      all(
        any(Endemicity %in% c("Endemic (MDA not delivered)", "Endemic (under MDA)") & Year %in% c(2021, 2022) & !all(is.na(Cov.in2)))
        #& !any(Endemicity == "Endemic (under post-intervention surveillance)" & Year %in% c(2021, 2022) & !all(is.na(Cov.in2)))
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

dfAPOC_included6$Endemicity_tmp <- dfAPOC_included6$Endemicity
length(unique(dfAPOC_included6$IU_ID_MAPPING)) # 2044 IUs (April 25')

# testing/ checking labelling over trt_status_2022
dfAPOC_included6_check_MDAstopped <- subset(dfAPOC_included6, trt_status_2022 == "MDA stopped")
length(unique(dfAPOC_included6_check_MDAstopped$IU_ID_MAPPING))

dfAPOC_included6_check_MDAcont <- subset(dfAPOC_included6, trt_status_2022 == "MDA continues")
length(unique(dfAPOC_included6_check_MDAcont$IU_ID_MAPPING))

dfAPOC_included6_check_trtnaive <- subset(dfAPOC_included6, trt_status_2022 == "Treatment naive")
length(unique(dfAPOC_included6_check_trtnaive$IU_ID_MAPPING))


dfAPOC_included6_check_IU <- subset(dfAPOC_included6, IU_ID_MAPPING == 36548)
dfAPOC_included6_check_IU <- dfAPOC_included6_check_IU %>%
  group_by(IU_ID_MAPPING) %>%
  mutate(
    trt_status_2022 = case_when(
      all(Endemicity == "Unknown (under LF MDA)" & Year %in% c(2021, 2022) & !all(is.na(Cov.in2))) ~ "MDA continues",
      TRUE ~ NA_character_
    )
  )

dfAPOC_included6_check_IU <- dfAPOC_included6_check_IU %>%
  group_by(IU_ID_MAPPING) %>%
  mutate(
    trt_status_2022 = case_when(
      any(Endemicity == "Unknown (under LF MDA)" & Year %in% c(2021:2022)) & any(Cov.in2 > 0 & Year %in% c(2013:2022)) ~ "MDA continues",
      TRUE ~ NA_character_
    )
  )

# check any violations where treatment_naive doesnt match columns
violated_combinations <- dfAPOC_included6 %>%
  group_by(biannual_VC_mapping, trt_status_2022, IU_ID_MAPPING) %>%
  filter((biannual_VC_mapping == "treatment naive" & trt_status_2022 != "Treatment naive") |
           (biannual_VC_mapping != "treatment naive" & trt_status_2022 == "Treatment naive")) %>%
  summarise(count = n())
# check: IU: 49752

violated_combinations2 <- dfAPOC_included6 %>%
  group_by(biannual_VC_mapping, trt_status_2022) %>%
  filter((biannual_VC_mapping == "treatment naive" & trt_status_2022 != "Treatment naive") |
           (biannual_VC_mapping != "treatment naive" & trt_status_2022 == "Treatment naive")) %>%
  summarise(count = n())

# check any remaining NAs in trt_status_2022

IUs_with_NA <- dfAPOC_included6 %>%
  filter(is.na(trt_status_2022)) %>%
  select(IU_ID_MAPPING) %>%
  distinct()

# how many IUs become non-endemic when reconfigure?

# non_endemic_IUs <- dfAPOC_included6 %>%
#   filter(Endemicity == "Non-endemic" & Year %in% c(2021, 2022)) %>%
#   distinct(IU_ID_MAPPING)
#
# filtered_df <- dfAPOC_included6 %>%
#   semi_join(non_endemic_IUs, by = "IU_ID_MAPPING")
#
# filtered_IUs <- filtered_df %>%
#   filter(Year >= 2013 & Year <= 2022) %>%
#   group_by(IU_ID_MAPPING) %>%
#   filter(all(Endemicity %in% c("Non-endemic", "Not reported")) &
#            length(unique(Endemicity)) == 2) %>%
#   distinct(IU_ID_MAPPING)
#
# filtered_df2 <- filtered_df %>%
#   semi_join(filtered_IUs, by = "IU_ID_MAPPING")
#
# non_endemic_IUs <- dfAPOC_included6 %>%
#   filter(
#     Endemicity == "non-endemic" &
#       Year %in% c(2021, 2022) &
#       !is.na(Parent1_IU_ID)
#   ) %>%
#   distinct(IU_ID_MAPPING)

# added July 2024 #

dfAPOC_included6 <- dfAPOC_included6 %>%
  group_by(IU_ID_MAPPING) %>%
  mutate(
    trt_status_2022_v2 = case_when(
      all(
        any(Endemicity %in% c("Endemic (MDA not delivered)", "Endemic (under MDA)") & Year %in% c(2021, 2022) & !all(is.na(Cov.in2)))
        #& !any(Endemicity == "Endemic (under post-intervention surveillance)" & Year %in% c(2021, 2022) & !all(is.na(Cov.in2)))
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

unique(dfAPOC_included6$trt_status_2022_v2)
length(unique(dfAPOC_included6$IU_ID_MAPPING)) # 2280 IUs
                                               # 2044 IUs (April 2025)

# ===========================================================================================#
# 6) continue MDA (65% coverage) in those IUs with "MDA continuing"                          #

dfAPOC_included7 <- dfAPOC_included6

dfAPOC_included7 <- dfAPOC_included7 %>%
  mutate(
    MDA_CDTI = if_else(
      trt_status_2022 == "MDA continues" & Year %in% c(2023:2025),
      1,
      MDA_CDTI
    )
  )

dfAPOC_included7 <- dfAPOC_included7 %>%
  mutate(
    Cov.in2 = if_else(
      trt_status_2022 == "MDA continues" & Year %in% c(2023:2025),
      0.65,
      Cov.in2
    )
  )

dfAPOC_included7 <- dfAPOC_included7 %>%
  mutate(
    cov_source = if_else(
      trt_status_2022 == "MDA continues" & Year %in% c(2023:2025),
      "assumed (projected)",
      cov_source
    )
  )


# check
dfAPOC_included7_check_MDAstopped <- subset(dfAPOC_included7, trt_status_2022 == "MDA stopped")
length(unique(dfAPOC_included7_check_MDAstopped$IU_ID_MAPPING))
dfAPOC_included7_check_MDAstopped <- subset(dfAPOC_included7_check_MDAstopped, Year %in% c(2023:2025))

dfAPOC_included7_check_MDAcont <- subset(dfAPOC_included7, trt_status_2022 == "MDA continues")
length(unique(dfAPOC_included7_check_MDAcont$IU_ID_MAPPING))
dfAPOC_included7_check_MDAcont <- subset(dfAPOC_included7_check_MDAcont, Year %in% c(2023:2025))

dfAPOC_included7_check_trtnaive <- subset(dfAPOC_included7, trt_status_2022 == "Treatment naive")
length(unique(dfAPOC_included7_check_trtnaive$IU_ID_MAPPING))

# 272 + 1224 + 784 = 2280 IUs
# 259 (MDA Stopped) + 1230 (MDA continues) + 555 (treatment naive) = 2044 IUs (April 2025)


# ============================================================================================== #
# need to continue biannual where any biannual in years 2020 - 2022 (NGA and UGA) for 2023-2025

# first find IUs where any biannual in 2020 - 2022 (and MDA continues classification with MDA_CDTI in 2023-25)

filtered_ids <- dfAPOC_included7 %>%
  group_by(IU_ID_MAPPING) %>%
  filter(
    any(Year %in% c(2020, 2021, 2022) & MDA_CDTI_Biannual == 1) &
      any(Year %in% c(2023, 2024, 2025) & MDA_CDTI == 1) &
      trt_status_2022 == "MDA continues"
  ) %>%
  distinct(IU_ID_MAPPING) %>%
  pull(IU_ID_MAPPING)

dfAPOC_included7$MDA_CDTI_Biannual <- ifelse(dfAPOC_included7$IU_ID_MAPPING %in% filtered_ids & dfAPOC_included7$Year %in% c(2023,2024,2025), 1, dfAPOC_included7$MDA_CDTI_Biannual)

# check Nigeria

# ============================================================================================== #
# need to continue VC status (either 2 = vector eliminated or 1 = under VC) in UGA for 2023-2025
# where vector control is 1 in 2022 make 1 in 2023 - 2025
condition <- dfAPOC_included7$vector_control == 1 & dfAPOC_included7$Year == 2022
indices <- which(condition)
selected_rows <- dfAPOC_included7[indices, ]
unique_IDIU <- unique(selected_rows$IU_ID_MAPPING)
dfAPOC_included7$vector_control <- ifelse(dfAPOC_included7$IU_ID_MAPPING %in% unique_IDIU & dfAPOC_included7$Year %in% c(2023,2024,2025), 1, dfAPOC_included7$vector_control)

# where vector control is 2 in 2022 make 2 in 2023-2025
condition <- dfAPOC_included7$vector_control == 2 & dfAPOC_included7$Year == 2022
indices <- which(condition)
selected_rows <- dfAPOC_included7[indices, ]
unique_IDIU <- unique(selected_rows$IU_ID_MAPPING)
dfAPOC_included7$vector_control <- ifelse(dfAPOC_included7$IU_ID_MAPPING %in% unique_IDIU & dfAPOC_included7$Year %in% c(2023,2024,2025), 2, dfAPOC_included7$vector_control)

# ========================================================== #
#     modify certain countries based on expected non-endemic - MARCH UPDATE # #

length(unique(dfAPOC_included7$IU_ID_MAPPING)) # 2280 IUs  - UP TO HERE SHOULD INCLUDE FOR THE HISTORIES PAPER
                                               # 2044 IUs (April 25')

# Uganda - most of country (any considered treatment naive should be non-endemic)
condition <- dfAPOC_included7$ADMIN0ISO3 == "UGA" &
  dfAPOC_included7$trt_status_2022 == "Treatment naive"
indices <- which(condition)
dfAPOC_included7$trt_status_2022[indices] <- "non-endemic"

# filter these out so no longer included as IUs
dfAPOC_included7 <- subset(dfAPOC_included7, !(dfAPOC_included7$trt_status_2022 == "non-endemic"))

length(unique(dfAPOC_included7$IU_ID_MAPPING)) # 2194 IUs
                                               # 1956 IUs (April 2025) - 88 IUs removed from UGA

# ================================================================= #
#   remove treatment naive IUs (with not-reported etc) - MARCH UPDATE #

# condition <- dfAPOC_included7$trt_status_2022 == "Treatment naive" &
#   dfAPOC_included7$MAX_Endemicity == "Not reported"
condition <- dfAPOC_included7$ADMIN1 != "Blue Nile" & dfAPOC_included7$trt_status_2022 == "Treatment naive" &
  dfAPOC_included7$MAX_Endemicity %in% c("Not reported","Unknown (under LF MDA)","Unknown (consider Oncho Elimination Mapping)")
# condition <- dfAPOC_included7$trt_status_2022 == "Treatment naive" &
#   dfAPOC_included7$MAX_Endemicity %in% c("Not reported","Unknown (under LF MDA)","Unknown (consider Oncho Elimination Mapping)")
indices <- which(condition)
dfAPOC_included7$exclude[indices] <- "exclude"

# check #
dfAPOC_included7_exclude <- subset(dfAPOC_included7, exclude == "exclude")
dfAPOC_included7_exclude_2022 <- subset(dfAPOC_included7_exclude, Year == 2022)
nrow(dfAPOC_included7_exclude_2022) # 56 IUs
                                    # 430 IUs (April 2025)
trt_naive_IUs_APOC_exclude <- unique(dfAPOC_included7_exclude_2022$IU_ID_MAPPING)
trt_naive_IUs_APOC_exclude
#writeLines(as.character(trt_naive_IUs_APOC_exclude), "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/trt_naive_IUs_APOC_exclude_n659.txt")

unique(dfAPOC_included7_exclude_2022$MAX_Endemicity)

# filter these out so no longer included as IUs
dfAPOC_included7 <- subset(dfAPOC_included7, is.na(dfAPOC_included7$exclude))
length(unique(dfAPOC_included7$IU_ID_MAPPING)) # 1535 IUs (now 1524 IUs: March 2025)
                                               # 1526 IUs (April 2025) (1956 IUs - 430 IUs = 1526 IUs in April 25')

# ====================================================== #
# 7) create rho parameter column = start all with 0.3    #
#
# dfAPOC_included7 <- dfAPOC_included7 %>%
#   mutate(
#     adherence_par = ifelse(MDA_CDTI == 1, 0.5, NA_real_)
#   ) # for business case

dfAPOC_included7 <- dfAPOC_included7 %>%
  mutate(
    adherence_par = ifelse(MDA_CDTI == 1, 0.3, NA_real_)
  ) # for Endgame


# ===================================================== #
#  8) create num_rnds and modelled_CUM_MDA cols         #

dfAPOC_included7$number_rnds <- rowSums(dfAPOC_included7[c("MDA_CDTI", "MDA_CDTI_Biannual")], na.rm = TRUE)

dfAPOC_included7$any_MDA <- ifelse(dfAPOC_included7$number_rnds > 0, 1, 0)

dfAPOC_included7 <- dfAPOC_included7 %>%
  group_by(IU_ID_MAPPING) %>%
  mutate(CUM_MDA_modelled = cumsum(number_rnds))

dfAPOC_included7$Cum_MDA_ESPEN <- dfAPOC_included7$Cum_MDA

length(unique(dfAPOC_included7$IU_ID_MAPPING))
check_df <- subset(dfAPOC_included7, Year == 2022)
nrow(check_df) # 1526 IUs left

# =====================================#
# 9) label co-endemic IUS with loa     #

co_endemic_IUs <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Co-endemicity/co_endemic_IUs.csv")

unique(dfAPOC_included7$ADMIN0ISO3)

co_endemic_IUs_oncho_LF_loa <- subset(co_endemic_IUs, co_endemicity == "oncho,LF,loa")
co_endemic_IUs_oncho_LF_loa_vec <- unique(co_endemic_IUs_oncho_LF_loa$IU_ID_MAPPING)

co_endemic_IUs_oncho_LF <- subset(co_endemic_IUs, co_endemicity == "oncho,LF")
co_endemic_IUs_oncho_LF_vec <- unique(co_endemic_IUs_oncho_LF$IU_ID_MAPPING)

co_endemic_IUs_oncho_loa <- subset(co_endemic_IUs, co_endemicity == "oncho,loa")
co_endemic_IUs_oncho_loa_vec <- unique(co_endemic_IUs_oncho_loa$IU_ID_MAPPING)

co_endemic_IUs_oncho <- subset(co_endemic_IUs, co_endemicity == "oncho")
co_endemic_IUs_oncho_vec <- unique(co_endemic_IUs_oncho$IU_ID_MAPPING)

dfAPOC_included7$co_endemicity <- ifelse(dfAPOC_included7$IU_ID_MAPPING %in% co_endemic_IUs_oncho_LF_loa_vec, "oncho,LF,loa",
                                         ifelse(dfAPOC_included7$IU_ID_MAPPING %in% co_endemic_IUs_oncho_LF_vec, "oncho,LF",
                                                ifelse(dfAPOC_included7$IU_ID_MAPPING %in% co_endemic_IUs_oncho_loa_vec, "oncho,loa",
                                                       ifelse(dfAPOC_included7$IU_ID_MAPPING %in% co_endemic_IUs_oncho_vec, "oncho", "assumed oncho only"))))

length(unique(dfAPOC_included7$IU_ID_MAPPING))
check_df <- subset(dfAPOC_included7, Year == 2022)
nrow(check_df) # 1526 IUs left
# ==========================================================================
# 9) in co-endemic IUs with loa, increase rho parameter value to 0.5 or 0.6

# dfAPOC_included7 <- dfAPOC_included7 %>%
#   mutate(
#     adherence_par = case_when(
#       co_endemicity %in% c("oncho,LF,loa", "oncho,loa") & adherence_par == 0.5 ~ 0.6,
#       TRUE ~ adherence_par
#     )
#   ) # only for business case (for Endgame keep all rho as 0.3?)

# ======================================================================================
# 10) make extra empty columns to match OCP dataframe when ready & make FINAL dataframe #

dfAPOC_included7$PHASE <- NA
dfAPOC_included7$SIZ_label <- NA
dfAPOC_included7$MDA_nonCDTI <- NA

dfAPOC_included7$IUID <- dfAPOC_included7$IU_ID_MAPPING
dfAPOC_included7$IUID[nchar(dfAPOC_included7$IU_ID_MAPPING)==4] <-  paste(0, dfAPOC_included7$IUID[nchar(dfAPOC_included7$IU_ID_MAPPING)==4], sep="")
dfAPOC_included7$IUID <- paste(dfAPOC_included7$ADMIN0ISO3, dfAPOC_included7$IUID, sep="")

dfAPOC_included7$Control_prog <- "APOC"

dfAPOC_included7$Cov_raw <- dfAPOC_included7$MDA_CDTI_raw

length(unique(dfAPOC_included7$IU_ID_MAPPING))
check_df <- subset(dfAPOC_included7, Year == 2022)
nrow(check_df) # 1526 IUs left

# =============================================================== #
#    NOTE THIS STEP IS PERFORMED IN OCP SCRIPT - END OF APRIL 25' #

# Full_histories_df_popinfo <- Full_histories_df_popinfo %>%
#   filter(!IU_ID_MAPPING %in% c(19408, 49752, 49821)) # removed 2 CMR IUs (now these are included in histories) - March 25'

# note 49752 no longer in dataset so only removes the two above in UGA (49821) and ETH (19408)

# 49821 = UGA49821 
# 19408 = ETH 
# both endemic - so need to remove from 1437 - country info (62 + 2 = 64) = 1373


# =================================================================== #
#  NOTE THIS STEP IS PERFORMED IN PLOTTING HISTORIES APRIL 25' SCRIPT #

# ================================================================ #
#         Those in Endgame, only keep Treatment naive IUs in Gabon #

# this removes the 5 ETH treatment naive IUs , just keeping the GAB (27) and SDN (2) ones

# Full_histories_df_minimal_lastyr_2022_check_trtnaive <- subset(Full_histories_df_minimal_lastyr_2022_check, trt_status_2022 == "Treatment naive")
# 
# nrow(Full_histories_df_minimal_lastyr_2022_check_trtnaive)
# 
# table(Full_histories_df_minimal_lastyr_2022_check_trtnaive$ADMIN0ISO3) # 5 in ETH, 27 in GAB and 2 in SDN (from CC)
# 
# # # only keep SDN and GAB treatment naive IUs
# # Full_histories_df_minimal_lastyr_2022_check <- Full_histories_df_minimal_lastyr_2022_check %>%
# #   filter(
# #     # Keep "treatment naive" only if ADMI0ISO3 is "GAB"
# #     (trt_status_2022 == "Treatment naive" & ADMIN0ISO3 %in% c("GAB", "SDN")) |
# #       # Keep any row where trt_status_2022 is not "treatment naive"
# #       trt_status_2022 != "Treatment naive"
# #   )
# 
Full_histories_df_minimal_lastyr_2022_check <- Full_histories_df_minimal_lastyr_2022_check %>%
  filter(
    # Keep "Treatment naive" only if ADMIN0ISO3 is "GAB" or "SDN"
    (trt_status_2022 == "Treatment naive" & ADMIN0ISO3 %in% c("GAB", "SDN")) |
      # Keep any row where trt_status_2022 is not "Treatment naive"
      trt_status_2022 != "Treatment naive",
    # Exclude a specific IU_ID
    !IUID %in% c("ETH19529", "NGA37017") # these two are both revised_cum_MDA == 0 so treatment naive 
  )

# Full_histories_df_minimal_lastyr_2022_check_trtnaive <- subset(Full_histories_df_minimal_lastyr_2022_check, trt_status_2022 == "Treatment naive")
# 
# table(Full_histories_df_minimal_lastyr_2022_check_trtnaive$ADMIN0ISO3) # 5 in ETH, 27 in GAB and 2 in SDN (from CC)
# 
# Full_histories_df_minimal_lastyr_2022 <- Full_histories_df_minimal_lastyr_2022_check # RESET HERE

# =========================================================================
# 11) final minimal dataframe to share

# Full_APOC_histories_df_popinfo <- dfAPOC_included7[, c("IU_ID_MAPPING","IUs_NAME_MAPPING","IU_CODE_MAPPING", "IUID", "ADMIN0ISO3","Endemicity", "MAX_Endemicity",
#                                                        "PHASE", "SIZ_label", "endemicity_baseline","Year","PopTot","PopPreSAC","PopSAC","PopAdult","PopReq",
#                                                        "PopTrg","PopTreat", "MDA_scheme","Cum_MDA_ESPEN","Cov","EpiCov",
#                                                        "vector_control","biannual_VC_mapping","MDA_nonCDTI", "MDA_CDTI", "MDA_CDTI_Biannual","number_rnds",
#                                                        "Cov.in2","CUM_MDA_modelled","trt_status_2022","trt_status_2022_v2","adherence_par","co_endemicity", "Control_prog",
#                                                        "new_MDA_IUs_2022")]

# adding in raw coverages and source of data (Feb 25)
Full_APOC_histories_df_popinfo <- dfAPOC_included7[, c("IU_ID_MAPPING","IUs_NAME_MAPPING","IU_CODE_MAPPING", "IUID", "ADMIN0ISO3","Endemicity", "MAX_Endemicity",
                                                       "PHASE", "SIZ_label", "endemicity_baseline","Year","PopTot","PopPreSAC","PopSAC","PopAdult","PopReq",
                                                       "PopTrg","PopTreat", "MDA_scheme","Cum_MDA_ESPEN","Cov","EpiCov",
                                                       "vector_control","biannual_VC_mapping","MDA_nonCDTI", "MDA_CDTI", "MDA_CDTI_Biannual","number_rnds",
                                                       "Cov.in2","Cov_raw","cov_source","cov_specific_source","CUM_MDA_modelled","trt_status_2022","trt_status_2022_v2","adherence_par","co_endemicity", "Control_prog",
                                                       "new_MDA_IUs_2022")]

#Full_APOC_histories_df_popinfo$Control_prog <- "APOC"

# write.csv(Full_APOC_histories_df_popinfo, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_popinfo_200324.csv")
# write.csv(Full_APOC_histories_df_popinfo, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_popinfo_200324.csv")

# with ESPEN 2022 update (May 2024)
# write.csv(Full_APOC_histories_df_popinfo, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_popinfo_170524.csv")

# with TCC update (South Sudan including Blue Nile IUs - Nov 2024)
# write.csv(Full_APOC_histories_df_popinfo, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_popinfo_111124.csv")


# # just 4 countries with chnages from May 2024 and new trt_status_2022_v2 from July 2024:
# Full_APOC_histories_df_popinfo_4countries <- subset(Full_APOC_histories_df_popinfo, ADMIN0ISO3 %in% c("NGA", "CMR", "ETH", "SSD"))
# write.csv(Full_APOC_histories_df_popinfo_4countries, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_popinfo_4countries_220724.csv")


Full_APOC_histories_df_minimal <- dfAPOC_included7[, c("IU_ID_MAPPING","IUs_NAME_MAPPING","IU_CODE_MAPPING", "IUID", "ADMIN0ISO3","Endemicity", "MAX_Endemicity",
                                                       "PHASE", "SIZ_label", "endemicity_baseline","Year","MDA_scheme","Cum_MDA_ESPEN","Cov","EpiCov",
                                                       "vector_control","biannual_VC_mapping","MDA_nonCDTI", "MDA_CDTI", "MDA_CDTI_Biannual","number_rnds",
                                                       "Cov.in2","Cov_raw","cov_source","cov_specific_source","CUM_MDA_modelled","trt_status_2022","trt_status_2022_v2","adherence_par","co_endemicity", "Control_prog",
                                                       "new_MDA_IUs_2022")]

Full_APOC_histories_df_minimal_lastyr_2022 <- subset(Full_APOC_histories_df_popinfo, Year == 2022)
Full_APOC_histories_df_minimal_lastyr_2025 <- subset(Full_APOC_histories_df_popinfo, Year == 2025)

nrow(Full_APOC_histories_df_minimal_lastyr_2022)

# maps & frequency distributions ~ country #

co_endemicity_maps_final <- ESPEN_IUs_ALL %>%
  left_join(Full_APOC_histories_df_minimal_lastyr_2022, by = c("IU_ID" = "IU_ID_MAPPING"))

cbPalette <- c("#CC79A7","#E69F00","#009E73","#F0E442","#0072B2")

# Co-endemicity #

# large SSA map
ggplot() +
  geom_sf(data = co_endemicity_maps_final, aes(fill = co_endemicity), colour = NA, alpha = 0.7) +
  geom_sf(data = ESPEN_IUs_APOC, aes(), colour = NA, size = 1, fill = NA, alpha = 0.1) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
  coord_sf(xlim = c(-20, 50), ylim = c(38, -35)) +
  theme_bw() +
  scale_fill_manual(values = cbPalette, na.value = "gray") +
  scale_colour_manual(na.value="gray")+
  labs(fill='') +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal")

# just Bioko
ggplot() +
  geom_sf(data = co_endemicity_maps_final, aes(fill = co_endemicity), colour = NA, alpha = 0.7) +
  geom_sf(data = ESPEN_IUs_APOC, aes(), colour = NA, size = 1, fill = NA, alpha = 0.1) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
  coord_sf(xlim = c(8, 10), ylim = c(3, 5)) +
  theme_bw() +
  scale_fill_manual(values = cbPalette, na.value = "gray") +
  scale_colour_manual(na.value="gray")+
  labs(fill='') +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal")

# frequency dist plot
freq_table_coendemicity <- table(Full_APOC_histories_df_minimal_lastyr_2022$co_endemicity, Full_APOC_histories_df_minimal_lastyr_2022$ADMIN0ISO3) # Create a frequency table

freq_df_coendemicity <- as.data.frame(as.table(freq_table_coendemicity)) # Convert the frequency table to a data frame
colnames(freq_df_coendemicity) <- c("co_endemicity", "ADMIN0ISO3", "Frequency") # Rename the columns for better readability

# Create the frequency distribution plot
ggplot(freq_df_coendemicity, aes(x = co_endemicity, y = Frequency, fill = co_endemicity)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Frequency Distribution of co_endemicity by ADMIN0ISO3",
       x = "co_endemicity",
       y = "Frequency") +
  scale_fill_manual(values = cbPalette) +
  theme_minimal()+
  theme(axis.text.x = element_blank(),  # Remove x-axis labels
        axis.ticks.x = element_blank(),  # Remove x-axis ticks if needed
        #axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.line = element_line(colour = "black"))+
  facet_wrap(~ADMIN0ISO3)

# Intervention distribution #

# full Africa map
ggplot() +
  geom_sf(data = co_endemicity_maps_final, aes(fill = biannual_VC_mapping), colour = NA, alpha = 0.7) +
  geom_sf(data = ESPEN_IUs_APOC, aes(), colour = NA, size = 1, fill = NA, alpha = 0.1) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
  coord_sf(xlim = c(-20, 50), ylim = c(38, -35)) +
  theme_bw() +
  scale_fill_manual(values = cbPalette, na.value = "gray") +
  scale_colour_manual(na.value="gray")+
  labs(fill='') +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal")

country_boundary <- subset(african_countries, NAME_0 == "Equatorial Guinea")

# just Bioko
ggplot() +
  geom_sf(data = co_endemicity_maps_final, aes(fill = biannual_VC_mapping), colour = NA, alpha = 0.7) +
  geom_sf(data = ESPEN_IUs_APOC, aes(), colour = NA, size = 1, fill = NA, alpha = 0.1) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
  geom_sf(data = country_boundary, aes(), fill = NA, colour = "black", size = 1.25, linewidth = 1.1) +
  coord_sf(xlim = c(8, 10), ylim = c(3, 5)) +
  theme_bw() +
  scale_fill_manual(values = cbPalette, na.value = "gray") +
  scale_colour_manual(na.value="gray")+
  labs(fill='') +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal")

# just Uganda
ESPEN_IUs_UGA <- ESPEN_IUs_APOC %>% filter(ADMIN0ISO3 == "UGA")
co_endemicity_maps_final$biannual_VC_mapping2 <- ifelse(co_endemicity_maps_final$IUs_NAME %in% c("KIBAALE", "MANAFWA",
                                                                                                  "Namisindwa","BUDUDA","MBALE",
                                                                                                  "SIRONKO", "Buhweju", "IBANDA",
                                                                                                  "KAMWENGE","Kitagwenda","Rubirizi",
                                                                                                  "HOIMA", "Kikuube", "BULIISA",
                                                                                                 "MASINDI", "ARUA", "MadI-Okollo",
                                                                                                 "Zombo"),
                                                        "biannual & vector elimination",
                                                        co_endemicity_maps_final$biannual_VC_mapping)

co_endemicity_maps_final$biannual_VC_mapping2 <- ifelse(co_endemicity_maps_final$IUs_NAME %in% c("KYENJOJO", "KABAROLE", "Bunyangabu"),
                                                        "annual & vector elimination",
                                                        co_endemicity_maps_final$biannual_VC_mapping2)

merged_UGA <- co_endemicity_maps_final[which((co_endemicity_maps_final$ADMIN0ISO3.x == "UGA")),]
merged_UGA2 <- merged_UGA[which((merged_UGA$trt_status_2022 %in% c("MDA continues", "MDA stopped", "Treatment naive"))),]

cbPalette_UGA <- c("#CC79A7","#CC0099","#E69F00","#009E73","#F0E442","#FFFF99","#0072B2")

country_boundary <- subset(african_countries, NAME_0 == "Uganda")

ggplot() +
  geom_sf(data = co_endemicity_maps_final, aes(fill = biannual_VC_mapping2), colour = NA, alpha = 0.7) +
  geom_sf(data = ESPEN_IUs_UGA, aes(), colour = "black", size = 1, fill = NA, alpha = 0.1) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
  #geom_sf_text(data = merged_UGA2, aes(label = IUs_NAME_MAPPING), size = 2, color = "black", check_overlap = TRUE) +
  coord_sf(xlim = c(29.7, 35), ylim = c(-1.2, 4.3)) +
  theme_bw() +
  scale_fill_manual(values = cbPalette_UGA, na.value = "gray") +
  scale_colour_manual(na.value="gray")+
  labs(fill='') +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal")

ggplot() +
  geom_sf(data = co_endemicity_maps_final, aes(fill = biannual_VC_mapping2), colour = NA, alpha = 0.7) +
  geom_sf(data = ESPEN_IUs_UGA, aes(), colour = "black", size = 1, fill = NA, alpha = 0.1) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.25) +
  geom_sf(data = country_boundary, aes(), fill = NA, colour = "black", size = 1.25, linewidth = 1.1) +
  #geom_sf_text(data = merged_UGA2, aes(label = IUs_NAME_MAPPING), size = 2, color = "black", check_overlap = TRUE) +
  coord_sf(xlim = c(29.7, 35), ylim = c(-1.2, 4.3)) +
  theme_bw() +
  scale_fill_manual(values = cbPalette_UGA, na.value = "gray") +
  scale_colour_manual(na.value="gray")+
  labs(fill='') +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal")

# just Ethiopia
ESPEN_IUs_ETH <- ESPEN_IUs_APOC %>% filter(ADMIN0ISO3 == "ETH")

merged_ETH <- co_endemicity_maps_final[which((co_endemicity_maps_final$ADMIN0ISO3.x == "ETH")),]
merged_ETH2 <- merged_ETH[which((merged_ETH$trt_status_2022 %in% c("MDA continues", "MDA stopped", "Treatment naive"))),]

country_boundary <- subset(african_countries, NAME_0 == "Ethiopia")


cbPalette_ETH <- c("#CC79A7","#CC0099","#E69F00","#009E73","#F0E442","#FFFF99","#0072B2")

ggplot() +
  geom_sf(data = co_endemicity_maps_final, aes(fill = biannual_VC_mapping), colour = NA, alpha = 0.7) +
  geom_sf(data = ESPEN_IUs_ETH, aes(), colour = "black", size = 1, fill = NA, alpha = 0.1) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
  #geom_sf_text(data = merged_ETH2, aes(label = IUs_NAME_MAPPING), size = 2, color = "black", check_overlap = TRUE) +
  coord_sf(xlim = c(33, 47), ylim = c(4, 14.7)) +
  theme_bw() +
  scale_fill_manual(values = cbPalette, na.value = "gray") +
  scale_colour_manual(na.value="gray")+
  labs(fill='') +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal")

ggplot() +
  geom_sf(data = co_endemicity_maps_final, aes(fill = biannual_VC_mapping), colour = NA, alpha = 0.7) +
  geom_sf(data = ESPEN_IUs_ETH, aes(), colour = "black", size = 1, fill = NA, alpha = 0.1) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
  geom_sf_text(data = merged_ETH2, aes(label = IUs_NAME_MAPPING), size = 2.5, color = "black", check_overlap = TRUE) +
  coord_sf(xlim = c(33.5, 40), ylim = c(4.7, 14.1)) +
  theme_bw() +
  scale_fill_manual(values = cbPalette, na.value = "gray") +
  scale_colour_manual(na.value="gray")+
  labs(fill='') +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal")

# for NTD MC tech meeting ppt
ggplot() +
  geom_sf(data = co_endemicity_maps_final, aes(fill = biannual_VC_mapping), colour = NA, alpha = 0.7) +
  geom_sf(data = ESPEN_IUs_ETH, aes(), colour = "black", size = 1, fill = NA, alpha = 0.1) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
  geom_sf(data = country_boundary, aes(), fill = NA, colour = "black", linewidth = 1.1) +
 coord_sf(xlim = c(33.5, 47), ylim = c(4.1, 14.3)) +
  theme_bw() +
  scale_fill_manual(values = cbPalette, na.value = "gray") +
  scale_colour_manual(na.value="gray")+
  labs(fill='') +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal")

# isolate IUs not with biannual in ETH #
co_endemicity_maps_final_ETHannualMDA <- subset(co_endemicity_maps_final, ADMIN0 == "Ethiopia" &  biannual_VC_mapping == "annual only")
merged_ETH_annualMDA <- subset(merged_ETH, biannual_VC_mapping == "annual only")

ggplot() +
  geom_sf(data = co_endemicity_maps_final_ETHannualMDA, aes(fill = biannual_VC_mapping), colour = NA, alpha = 0.7) +
  geom_sf(data = ESPEN_IUs_ETH, aes(), colour = "black", size = 1, fill = NA, alpha = 0.1) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
  geom_sf(data = country_boundary, aes(), fill = NA, colour = "black", linewidth = 1.1) +
  geom_sf_text(data = merged_ETH_annualMDA, aes(label = IUs_NAME_MAPPING), size = 2.5, color = "black", check_overlap = TRUE) +
  coord_sf(xlim = c(33.5, 47), ylim = c(4.1, 14.3)) +
  theme_bw() +
  scale_fill_manual(values = cbPalette, na.value = "gray") +
  scale_colour_manual(na.value="gray")+
  labs(fill='') +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal")

unique_IUs_ETHannualMDA <- unique(co_endemicity_maps_final_ETHannualMDA$IU_CODE_MAPPING)
length(unique_IUs_ETHannualMDA)
writeLines(unique_IUs_ETHannualMDA, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Biannual in APOC/ETH_IUs_switchto_biannual.txt")

library(sf)
library(ggplot2)

# Extract centroids of multipolygon geometries
centroids <- st_centroid(merged_ETH2$geometry)

# Create a new dataframe with the centroids and labels
labels_df <- data.frame(
  label = merged_ETH2$IUs_NAME_MAPPING,
  x = st_coordinates(centroids)[, 1],
  y = st_coordinates(centroids)[, 2]
)

library(ggrepel)

ggplot() +
  geom_sf(data = co_endemicity_maps_final, aes(fill = biannual_VC_mapping), colour = NA, alpha = 0.7) +
  geom_sf(data = ESPEN_IUs_ETH, aes(), colour = "black", size = 1, fill = NA, alpha = 0.1) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
  geom_label_repel(data = labels_df, aes(label = label, x = x, y = y), size = 2, color = "black",
                   nudge_x = 1, nudge_y = 1, box.padding = unit(0, "lines"),  # Set box.padding to zero
                   label.padding = unit(0, "lines"),  # Set label.padding to zero
                   fill = NA,  # Make label background transparent
                   max.overlaps = Inf) +  # Increase max.overlaps to allow more overlapping labels
  geom_segment(data = labels_df, aes(x = x, y = y, xend = x + 0.5, yend = y + 0.5),
               color = "black", size = 0.5, alpha = 0.1) +  # Adding lines connecting labels to centroids
  coord_sf(xlim = c(33, 47), ylim = c(4, 14.7)) +
  theme_bw() +
  scale_fill_manual(values = cbPalette, na.value = "gray") +
  scale_colour_manual(na.value = "gray") +
  labs(fill = '') +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal"
  )


#  focus on Nigeria #
ESPEN_IUs_Nigeria <- ESPEN_IUs[which((ESPEN_IUs$ADMIN0ISO3 == "NGA")),]
st_geometry_type(ESPEN_IUs_Nigeria )
st_crs(ESPEN_IUs_Nigeria )

merged_Nigeria <- co_endemicity_maps_final[which((co_endemicity_maps_final$ADMIN0ISO3.x == "NGA")),]
merged_Nigeria2 <- merged_Nigeria[which((merged_Nigeria$trt_status_2022 %in% c("MDA continues", "MDA stopped", "Treatment naive"))),]

# river layer #
rivers <- st_read('C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Biannual in APOC/africa_rivers_1.shp')
st_geometry_type(rivers)
st_crs(rivers)

country_boundary <- subset(african_countries, NAME_0 == "Nigeria")

cbPalette2 <- c("#E69F00", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#cbPalette2 <- c("#009E73","#E69F00", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# with labels for IU names in Nigeria
ggplot() +
  geom_sf(data = co_endemicity_maps_final, aes(fill = biannual_VC_mapping), colour = NA, alpha = 0.7) +
  geom_sf(data = merged_Nigeria2, aes(), colour = "black", size = 1, fill = NA, alpha = 0.1) +
  #geom_sf(data = rivers, aes(), colour = "blue", size = 1, fill = "blue") +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", linewidth = 1) +
  geom_sf(data = country_boundary, aes(), fill = NA, colour = "black", linewidth = 1.1) +
  #geom_sf_text(data = merged_Nigeria2, aes(label = IUs_NAME_MAPPING), size = 2, color = "black", check_overlap = TRUE) +
  coord_sf(xlim = c(3, 14.5), ylim = c(4, 13.4)) +
  theme_bw() +
  scale_fill_manual(values = cbPalette, na.value = "gray") +
  scale_colour_manual(na.value="gray") +
  labs(fill = '') +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal"
  )

# frequency dist plot
freq_table_intervention <- table(Full_APOC_histories_df_minimal_lastyr_2022$biannual_VC_mapping, Full_APOC_histories_df_minimal_lastyr_2022$ADMIN0ISO3) # Create a frequency table

freq_df_intervention <- as.data.frame(as.table(freq_table_intervention)) # Convert the frequency table to a data frame
colnames(freq_df_intervention) <- c("intervention", "ADMIN0ISO3", "Frequency") # Rename the columns for better readability

ggplot(freq_df_intervention, aes(x = intervention, y = Frequency, fill = intervention)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Frequency Distribution of co_endemicity by ADMIN0ISO3",
       x = "intervention history",
       y = "Frequency") +
  scale_fill_manual(values = cbPalette) +
  theme_minimal()+
  theme(axis.text.x = element_blank(),  # Remove x-axis labels
        axis.ticks.x = element_blank(),  # Remove x-axis ticks if needed
        #axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.line = element_line(colour = "black"))+
  facet_wrap(~ADMIN0ISO3)

# ===== #
# Sudan #
ESPEN_IUs_Sudan <- ESPEN_IUs[which((ESPEN_IUs$ADMIN0ISO3 == "SDN")),]
st_geometry_type(ESPEN_IUs_Sudan )
st_crs(ESPEN_IUs_Sudan )

merged_Sudan <- co_endemicity_maps_final[which((co_endemicity_maps_final$ADMIN0ISO3.x == "Sudan")),]
merged_Sudan2 <- merged_Nigeria[which((merged_Sudan$trt_status_2022 %in% c("MDA continues", "MDA stopped", "Treatment naive"))),]

country_boundary <- subset(african_countries, NAME_0 == "Sudan")

cbPalette2 <- c("#E69F00", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#cbPalette2 <- c("#009E73","#E69F00", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# with labels for IU names in Sudan
ggplot() +
  geom_sf(data = co_endemicity_maps_final, aes(fill = biannual_VC_mapping), colour = NA, alpha = 0.7) +
  geom_sf(data = merged_Sudan2, aes(), colour = "black", size = 1, fill = NA, alpha = 0.1) +
  #geom_sf(data = rivers, aes(), colour = "blue", size = 1, fill = "blue") +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", linewidth = 1) +
  geom_sf(data = country_boundary, aes(), fill = NA, colour = "black", linewidth = 1.1) +
  #geom_sf_text(data = merged_Nigeria2, aes(label = IUs_NAME_MAPPING), size = 2, color = "black", check_overlap = TRUE) +
  coord_sf(xlim = c(22, 38), ylim = c(9, 23)) +
  theme_bw() +
  scale_fill_manual(values = cbPalette, na.value = "gray") +
  scale_colour_manual(na.value="gray") +
  labs(fill = '') +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal"
  )

country_boundary <- subset(african_countries, NAME_0 %in% c("Sudan", "South Sudan"))

merged_Sudan <- co_endemicity_maps_final[which((co_endemicity_maps_final$ADMIN0ISO3.x %in% c("SSD","SDN"))),]
merged_Sudan2 <- merged_Sudan[which((merged_Sudan$trt_status_2022 %in% c("MDA continues", "MDA stopped", "Treatment naive"))),]

# with labels for IU names in Sudan & south Sudan
ggplot() +
  geom_sf(data = co_endemicity_maps_final, aes(fill = biannual_VC_mapping), colour = NA, alpha = 0.7) +
  geom_sf(data = merged_Sudan2, aes(), colour = "black", size = 1, fill = NA, alpha = 0.1) +
  #geom_sf(data = rivers, aes(), colour = "blue", size = 1, fill = "blue") +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", linewidth = 1) +
  geom_sf(data = country_boundary, aes(), fill = NA, colour = "black", linewidth = 1.1) +
  #geom_sf_text(data = merged_Nigeria2, aes(label = IUs_NAME_MAPPING), size = 2, color = "black", check_overlap = TRUE) +
  coord_sf(xlim = c(22, 38), ylim = c(4, 23)) +
  theme_bw() +
  scale_fill_manual(values = cbPalette, na.value = "gray") +
  scale_colour_manual(na.value="gray") +
  labs(fill = '') +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal"
  )

# ===== #
# South Sudan #
ESPEN_IUs_SSudan <- ESPEN_IUs[which((ESPEN_IUs$ADMIN0ISO3 == "SSD")),]
st_geometry_type(ESPEN_IUs_SSudan )
st_crs(ESPEN_IUs_SSudan )

merged_SSudan <- co_endemicity_maps_final[which((co_endemicity_maps_final$ADMIN0ISO3.x == "SSD")),]
merged_SSudan2 <- merged_Nigeria[which((merged_SSudan$trt_status_2022 %in% c("MDA continues", "MDA stopped", "Treatment naive"))),]

country_boundary <- subset(african_countries, NAME_0 == "South Sudan")

cbPalette2 <- c("#E69F00", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#cbPalette2 <- c("#009E73","#E69F00", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# with labels for IU names in Sudan
ggplot() +
  geom_sf(data = co_endemicity_maps_final, aes(fill = biannual_VC_mapping), colour = NA, alpha = 0.7) +
  geom_sf(data = merged_SSudan2, aes(), colour = "black", size = 1, fill = NA, alpha = 0.1) +
  #geom_sf(data = rivers, aes(), colour = "blue", size = 1, fill = "blue") +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", linewidth = 1) +
  geom_sf(data = country_boundary, aes(), fill = NA, colour = "black", linewidth = 1.1) +
  #geom_sf_text(data = merged_Nigeria2, aes(label = IUs_NAME_MAPPING), size = 2, color = "black", check_overlap = TRUE) +
  coord_sf(xlim = c(24.5, 35), ylim = c(4, 12)) +
  theme_bw() +
  scale_fill_manual(values = cbPalette, na.value = "gray") +
  scale_colour_manual(na.value="gray") +
  labs(fill = '') +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal"
  )


# CUM_MDA modelled distribution #
# all of Africa
ggplot() +
  geom_sf(data = co_endemicity_maps_final, aes(fill = CUM_MDA_modelled), colour = NA, alpha = 0.7) +
  geom_sf(data = ESPEN_IUs_APOC, aes(), colour = NA, size = 1, fill = NA, alpha = 0.1) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
  coord_sf(xlim = c(-20, 50), ylim = c(38, -35)) +
  theme_bw() +
  scale_fill_gradient(low = "yellow", high = "darkblue") +
  scale_colour_manual(na.value="lightgray") +
  labs(fill='') +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal",
    legend.box = "horizontal"  # Control the position of the legend box
  )

# just Bioko
ggplot() +
  geom_sf(data = co_endemicity_maps_final, aes(fill = CUM_MDA_modelled), colour = NA, alpha = 0.7) +
  geom_sf(data = ESPEN_IUs_APOC, aes(), colour = NA, size = 1, fill = NA, alpha = 0.1) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
  coord_sf(xlim = c(8, 10), ylim = c(3, 5)) +
  theme_bw() +
  scale_fill_gradient(low = "yellow", high = "darkblue") +
  scale_colour_manual(na.value="lightgray") +
  labs(fill='') +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal",
    legend.box = "horizontal"  # Control the position of the legend box
  )

# treatment status in 2022 #
# all of Africa
ggplot() +
  geom_sf(data = co_endemicity_maps_final, aes(fill = trt_status_2022), colour = NA, alpha = 0.7) +
  geom_sf(data = ESPEN_IUs_APOC, aes(), colour = NA, size = 1, fill = NA, alpha = 0.1) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
  coord_sf(xlim = c(-20, 50), ylim = c(38, -35)) +
  theme_bw() +
  scale_fill_manual(values = cbPalette, na.value = "gray") +
  scale_colour_manual(na.value="gray")+
  labs(fill='') +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal")
# just Bioko
ggplot() +
  geom_sf(data = co_endemicity_maps_final, aes(fill = trt_status_2022), colour = NA, alpha = 0.7) +
  geom_sf(data = ESPEN_IUs_APOC, aes(), colour = NA, size = 1, fill = NA, alpha = 0.1) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
  coord_sf(xlim = c(8, 10), ylim = c(3, 5)) +
  theme_bw() +
  scale_fill_manual(values = cbPalette, na.value = "gray") +
  scale_colour_manual(na.value="gray")+
  labs(fill='') +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal")


# frequency dist plot
freq_table_status <- table(Full_APOC_histories_df_minimal_lastyr_2022$trt_status_2022, Full_APOC_histories_df_minimal_lastyr_2022$ADMIN0ISO3) # Create a frequency table

freq_df_status <- as.data.frame(as.table(freq_table_status)) # Convert the frequency table to a data frame
colnames(freq_df_status) <- c("status", "ADMIN0ISO3", "Frequency") # Rename the columns for better readability

ggplot(freq_df_status, aes(x = status, y = Frequency, fill = status)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Frequency Distribution of IU status at the end of 2022 by ADMIN0ISO3",
       x = "status",
       y = "Frequency") +
  scale_fill_manual(values = cbPalette) +
  theme_minimal()+
  theme(axis.text.x = element_blank(),  # Remove x-axis labels
        axis.ticks.x = element_blank(),  # Remove x-axis ticks if needed
        #axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.line = element_line(colour = "black"))+
  facet_wrap(~ADMIN0ISO3)

# Save Business Case hsitories- Oct/Nov 2023 #
# write.csv(Full_APOC_histories_df_minimal, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_minimal_231123.csv")
# write.csv(Full_APOC_histories_df_minimal_lastyr_2022, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_minimal_lastyr_2022_231123.csv")
# write.csv(Full_APOC_histories_df_minimal_lastyr_2025, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_minimal_lastyr_2025_231123.csv")

# Save - endgame runs in Feb 2024 #
# write.csv(Full_APOC_histories_df_minimal, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_minimal_160224.csv")
# write.csv(Full_APOC_histories_df_minimal_lastyr_2022, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_minimal_lastyr_2022_160224.csv")
# write.csv(Full_APOC_histories_df_minimal_lastyr_2025, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_minimal_lastyr_2025_160224.csv")

# Save - endgame runs in March 2024 #
# write.csv(Full_APOC_histories_df_minimal, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_minimal_200324.csv")
# write.csv(Full_APOC_histories_df_minimal_lastyr_2022, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_minimal_lastyr_2022_200324.csv")
# write.csv(Full_APOC_histories_df_minimal_lastyr_2025, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_minimal_lastyr_2025_200324.csv")

# Save - endgame runs in May 2024 - ESPEN 2022 update#
# write.csv(Full_APOC_histories_df_minimal, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_minimal_170524.csv")
# write.csv(Full_APOC_histories_df_minimal_lastyr_2022, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_minimal_lastyr_2022_170524.csv")
# write.csv(Full_APOC_histories_df_minimal_lastyr_2025, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_minimal_lastyr_2025_170524.csv")

# Save Business Case hsitories- May 2024 #
# write.csv(Full_APOC_histories_df_minimal, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_minimal_BC_230524.csv")
# write.csv(Full_APOC_histories_df_minimal_lastyr_2022, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_minimal_lastyr_2022_BC_230524.csv")
# write.csv(Full_APOC_histories_df_minimal_lastyr_2025, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_minimal_lastyr_2025_BC_230524.csv")

# Save - endgame runs in May 2024 - ESPEN 2022 update & additional variables for reporting histories (July 2024)#
# write.csv(Full_APOC_histories_df_popinfo, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_popinfo_300724.csv")
# write.csv(Full_APOC_histories_df_minimal, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_minimal_300724.csv")
# write.csv(Full_APOC_histories_df_minimal_lastyr_2022, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_minimal_lastyr_2022_300724.csv")
# write.csv(Full_APOC_histories_df_minimal_lastyr_2025, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_minimal_lastyr_2025_300724.csv")

# Save - adding in Blue Nile IUs in SDN (TCC update for Nov 2024)
# write.csv(Full_APOC_histories_df_popinfo, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_popinfo_111124.csv")
# write.csv(Full_APOC_histories_df_minimal, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_minimal_111124.csv")
# write.csv(Full_APOC_histories_df_minimal_lastyr_2022, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_minimal_lastyr_2022_111124.csv")
# write.csv(Full_APOC_histories_df_minimal_lastyr_2025, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_minimal_lastyr_2025_111124.csv")

# # Save - adding in raw coverage and source of cov cols (Feb 2025)
# write.csv(Full_APOC_histories_df_popinfo, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_popinfo_070225.csv")
# write.csv(Full_APOC_histories_df_minimal, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_minimal_070225.csv")
# write.csv(Full_APOC_histories_df_minimal_lastyr_2022, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_minimal_lastyr_2022_070225.csv")
# write.csv(Full_APOC_histories_df_minimal_lastyr_2025, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_minimal_lastyr_2025_070225.csv")


# # # Save - adding some ETH, CMR, NGA IUs to endemic/with MDA and removing some TZN IUs (March - April 2025)
# write.csv(Full_APOC_histories_df_popinfo, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_popinfo_260325.csv")
# write.csv(Full_APOC_histories_df_minimal, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_minimal_260325.csv")
# write.csv(Full_APOC_histories_df_minimal_lastyr_2022, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_minimal_lastyr_2022_260325.csv")
# write.csv(Full_APOC_histories_df_minimal_lastyr_2025, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_minimal_lastyr_2025_260325.csv")

nrow(Full_APOC_histories_df_minimal_lastyr_2022)

# # # Save - adding some ETH, CMR, NGA IUs to endemic/with MDA and removing some TZN IUs (April 2025): adding back in two NGA IUs
# write.csv(Full_APOC_histories_df_popinfo, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_popinfo_070425.csv")
# write.csv(Full_APOC_histories_df_minimal, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_minimal_070425.csv")
# write.csv(Full_APOC_histories_df_minimal_lastyr_2022, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_minimal_lastyr_2022_070425.csv")
# write.csv(Full_APOC_histories_df_minimal_lastyr_2025, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_minimal_lastyr_2025_070425.csv")

# # Save - adding some ETH, CMR, NGA IUs to endemic/with MDA and removing some TZN IUs (END OF April 2025): adding back in two NGA IUs
write.csv(Full_APOC_histories_df_popinfo, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_popinfo_290425.csv")
write.csv(Full_APOC_histories_df_minimal, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_minimal_290425.csv")
write.csv(Full_APOC_histories_df_minimal_lastyr_2022, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_minimal_lastyr_2022_290425.csv")
write.csv(Full_APOC_histories_df_minimal_lastyr_2025, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_minimal_lastyr_2025_290425.csv")


any(Full_APOC_histories_df_minimal$MDA_CDTI == 0 & Full_APOC_histories_df_minimal$MDA_CDTI_Biannual == 1,  na.rm = TRUE) # check

check_2022_25 <- subset(Full_APOC_histories_df_minimal, Year %in% c(2023:2025))

# ============================================================================== #
# 11) column for whether 2013-2022 has 10 round of effective cov MDA (>= 0.65)?


# # ========================================================================================================================================= #
# #                                                           Repeat for OCP                                                                  #
# # ========================================================================================================================================= #
#
# # LOAD IN OCP #
#
# #OCP_DF_lastyr <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Cleaning IUs (Paul B)/OCP_DF.csv")
#
# load("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Cleaning IUs (Paul B)/OCP_DF_ALL.RData")
#
# dfOCP_included <- oncho_df_phase_complete
# dfOCP_included_lastyr <- subset(dfOCP_included, Year == 2022)
#
# vec_to_include <- c("Included Endemic", "Intervention (OCP)")
# dfOCP_included1 <- dfOCP_included[dfOCP_included$Included %in% vec_to_include,]
#
# matches <- dfOCP_included1$Cum_MDA == dfOCP_included1$Cum_MDA_Final
# isFALSE(matches)
# matches_df <- as.data.frame(matches)
#
# length(unique(dfOCP_included1$IU_ID_MAPPING))
#
# # =========================================================================== #
# # Find Ius that have "not-reported" in 2013 and "non-endemic" all other years #
#
# filtered_ius <- dfOCP_included1 %>%
#   group_by(IU_ID_MAPPING) %>%
#   filter(
#     any(Year == 2013 & Endemicity == "Not reported") &
#       any(Year %in% 2014:2022 & Endemicity == "Non-endemic") &
#       !any(Year %in% 2014:2022 & Endemicity %in% c("Endemic (under MDA)", "Endemic (MDA not delivered)",
#                                                    "Unknown (consider Oncho Elimination Mapping)"))
#   )
#
# # Get the unique IU_names that satisfy the conditions
# unique_ius <- unique(filtered_ius$IU_ID_MAPPING)
#
# # relabel these IUs as not included in overall dataframe (new included column) #
# dfOCP_included$Included_updated <- ifelse(dfOCP_included$IU_ID_MAPPING %in% unique_ius, "Not included (under OCP area but not reported/non-endemic)", dfOCP_included$Included)
# dfOCP_included1$Included_updated <- ifelse(dfOCP_included1$IU_ID_MAPPING %in% unique_ius, "Not included (under OCP area but not reported/non-endemic)", dfOCP_included1$Included)
# dfOCP_included_lastyr$Included_updated <- ifelse(dfOCP_included_lastyr$IU_ID_MAPPING %in% unique_ius, "Not included (under OCP area but not reported/non-endemic)", dfOCP_included_lastyr$Included)
#
# dfOCP_included1_lastyr_notincluded <- subset(dfOCP_included_lastyr, Included_updated == "Not included") # want to exclude these
# length(unique(dfOCP_included1_lastyr_notincluded$IU_ID_MAPPING))
# dfOCP_included1_lastyr_noncontrol <- subset(dfOCP_included_lastyr, PHASE == "NON-CONTROL") # theres 20 IUs extra here want to keep!
# length(unique(dfOCP_included1_lastyr_noncontrol$IU_ID_MAPPING))
#
# # Assuming df1 and df2 are your two dataframes
# difference_iu <- setdiff(unique(dfOCP_included1_lastyr_noncontrol$IU_ID_MAPPING), unique(dfOCP_included1_lastyr_notincluded$IU_ID_MAPPING)) # these are the 20 extra want to keep!
#
# dfOCP_included1_check <- subset(dfOCP_included1, IU_ID_MAPPING %in% difference_iu)
# #dfOCP_included1_check2 <- subset(dfOCP_included1, IU_ID_MAPPING %in% difference_iu)
#
# # want to filter on not included rather than Non-control in Phase
# # above dataframe still has all 873 IUs, but labels some as not to be included (non-endemic in the )
#
# # map to check #
#
# # set-up objects #
# ESPEN_IUs <- st_read('C:/Users/mad206/OneDrive/Endgame/Endgame IUs/ESPEN_IU_2021.shp')
#
# #OCP_histories_updated_lastyr_270923 <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/New Endgame Sept 2023 -/OCP_histories_updated_lastyr_270923.csv")
# #df_OCP_endemic_minimal <- OCP_histories_updated_lastyr_270923[, c("ADMIN0", "ADMIN0ISO3", "ADMIN1", "IU_ID_MAPPING", "IUs_NAME_MAPPING","IU_CODE_MAPPING", "endemicity")]
# OCP_countries <- unique(dfOCP_included1$ADMIN0ISO3)
# ESPEN_IUs_OCP <- ESPEN_IUs[which((ESPEN_IUs$ADMIN0ISO3 %in% OCP_countries)),]
# st_geometry_type(ESPEN_IUs_ALL)
# st_crs(ESPEN_IUs_ALL)
#
# shapefile_path <- "C:/Users/mad206/OneDrive/Endgame/OCP mapping/African countries/Africa_Boundaries.shp"
# african_countries <- st_read(dsn = shapefile_path)
# summary(african_countries)
#
# dfOCP_included_lastyr1 <- subset(dfOCP_included1, Year == 2022)
#
# OCP_includedIUs_676 <- ESPEN_IUs_OCP %>%
#   left_join(dfOCP_included_lastyr1, by = c("IU_ID" = "IU_ID_MAPPING"))
#
# #cbPalette <- c("#CC79A7","#E69F00","#009E73","#F0E442","#0072B2")
#
# ggplot() +
#   geom_sf(data = OCP_includedIUs_676, aes(fill = PHASE), colour = NA, alpha = 0.7) +
#   geom_sf(data = ESPEN_IUs_OCP, aes(), colour = NA, size = 1, fill = NA, alpha = 0.1) +
#   geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
#   coord_sf(xlim = c(-20, 7), ylim = c(4, 20)) +
#   theme_bw() +
#   #scale_fill_manual(na.value = "gray") +
#   scale_colour_manual(na.value="gray")+
#   labs(fill='') +
#   theme(
#     legend.position = "bottom",  # Place the legend at the bottom
#     legend.direction = "horizontal")
#
# # ================================================================================================== #
# # 1) identify treatment naive (map these) vs. just applying treatment history to all in OCP shape    #
#
# # 2) modify those identified as treatment naive - remove pre-2012 history (if not too many IUs - check with MG)
#
# # 3) update biannual in Ghana (meso- hyper- from 2009)
#
# # 4) extend by 3 years (2023- 2025)
#
# # 5) label each IU on forward status: MDA stopped (post intervention surveillance in 2022 or 2021
# #    or all EpiCov == 0 for "unknowns" / MDA continue/ treatment naive
#
# # 6) create rho parameter column = start all with 0.3
#
# # 7) label co-endemic IUS with loa
#
# # 8) in co-endemic IUs with loa, increase rho parameter value to 0.5 or 0.6
#
#
#
#
#
#
#
#
#
#
#
#
#
#
