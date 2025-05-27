# ==================================================================================================================== #
#                 Functions to compile and run the OCP IU onchocerciasis histories                                     #
# ==================================================================================================================== #


# =============================================================================================== #
#     Function to load and filter IUs in OCP (based on being endemic in ESPEN or in shapefile)    #
# =============================================================================================== #

load_and_filter_ocp_data <- function(file_path_input) {
  
  OCP_data_file <- file.path(base_path, file_path_input)
  
  # Load the OCP data
  load(OCP_data_file)
  
  # Assign to dfOCP_included
  dfOCP_included <- oncho_df_phase_complete
  
  # Filter the data for Year 2022
  dfOCP_included_lastyr <- subset(dfOCP_included, Year == 2022)
  
  # Number of endemic IUs (443)
  endemic_IUs <- dfOCP_included_lastyr[dfOCP_included_lastyr$MAX_Endemicity %in% c("Endemic (under MDA)",
                                                                                   "Endemic (MDA not delivered)",
                                                                                   "Endemic (under post-intervention surveillance)"), ]
  cat("Number of endemic IUs:", nrow(endemic_IUs), "\n")
  
  # Number of non-endemic IUs (430)
  non_endemic_IUs <- dfOCP_included_lastyr[dfOCP_included_lastyr$MAX_Endemicity %in% c("Unknown (consider Oncho Elimination Mapping)",
                                                                                       "Unknown (under LF MDA)",
                                                                                       "Not reported",
                                                                                       "Non-endemic"), ]
  cat("Number of non-endemic IUs:", nrow(non_endemic_IUs), "\n")
  
  # Total IUs (443 + 430 = 873)
  total_IUs <- nrow(dfOCP_included_lastyr)
  cat("Total number of IUs:", total_IUs, "\n")
  
  # Check 'Included' status
  included_status <- table(dfOCP_included_lastyr$Included)
  cat("Frequency of 'Included' status in 2022:\n")
  print(included_status)
  
  # Endemic IUs (Included as 'Endemic')
  dfOCP_included_lastyr_incl_endemic <- subset(dfOCP_included_lastyr, Included == "Included Endemic")
  cat("Number of IUs with 'Included Endemic':", nrow(dfOCP_included_lastyr_incl_endemic), "\n")
  cat("Unique MAX_Endemicity in 'Included Endemic':\n")
  print(unique(dfOCP_included_lastyr_incl_endemic$MAX_Endemicity))
  cat("Frequency of MAX_Endemicity in 'Included Endemic':\n")
  print(table(dfOCP_included_lastyr_incl_endemic$MAX_Endemicity))
  
  # Subset 'Not reported' or 'Non-endemic' IUs from the 'Included Endemic'
  dfOCP_included_lastyr_incl_endemic_subset <- subset(dfOCP_included_lastyr_incl_endemic, MAX_Endemicity %in% c("Not reported", "Non-endemic"))
  cat("Number of IUs with 'Not reported' or 'Non-endemic' in 'Included Endemic':", nrow(dfOCP_included_lastyr_incl_endemic_subset), "\n")
  
  # Intervention (OCP) IUs
  dfOCP_included_lastyr_incl_OCPshape <- subset(dfOCP_included_lastyr, Included == "Intervention (OCP)")
  cat("Number of IUs with 'Intervention (OCP)':", nrow(dfOCP_included_lastyr_incl_OCPshape), "\n")
  cat("Frequency of MAX_Endemicity in 'Intervention (OCP)':\n")
  print(table(dfOCP_included_lastyr_incl_OCPshape$MAX_Endemicity))
  
  # Filter based on 'Included' status
  vec_to_include <- c("Included Endemic", "Intervention (OCP)")
  dfOCP_included1 <- dfOCP_included[dfOCP_included$Included %in% vec_to_include,]
  
  # # Check if Cum_MDA matches Cum_MDA_Final
  # matches <- dfOCP_included1$Cum_MDA == dfOCP_included1$Cum_MDA_Final
  # cat("Matches between Cum_MDA and Cum_MDA_Final (FALSE indicates mismatch):\n")
  # print(isFALSE(matches))
  
  # Display number of unique IUs
  cat("Number of unique IU_ID_MAPPING in filtered data:", length(unique(dfOCP_included1$IU_ID_MAPPING)), "\n")
  
  return(dfOCP_included1)
}

# ========================================================================== #
#         Function to perform further checks on IUs in OCP dataframe         #
# ========================================================================== #

further_IU_checks_update_df_OCP <- function(df) {
  
  # =========================================================================== #
  # Find IUs that have "not-reported" in 2013 and "non-endemic" all other years #
  
  filtered_ius <- df %>%
    group_by(IU_ID_MAPPING) %>%
    filter(
      any(Year == 2013 & Endemicity == "Not reported") &
        any(Year %in% 2014:2022 & Endemicity == "Non-endemic") &
        !any(Year %in% 2014:2022 & Endemicity %in% c("Endemic (under MDA)", "Endemic (MDA not delivered)",
                                                     "Unknown (consider Oncho Elimination Mapping)"))
    )
  
  # Get the unique IU_names that satisfy the conditions
  unique_ius <- unique(filtered_ius$IU_ID_MAPPING)
  cat("Number of unique IUs that satisfy the conditions:", length(unique_ius), "\n")
  
  # Relabel these IUs as not included in overall dataframe (new included column)
  df$Included_updated <- ifelse(df$IU_ID_MAPPING %in% unique_ius, 
                                "Not included (under OCP area but not reported/non-endemic)", 
                                df$Included)
  
  # Check the frequency of Included_updated status
  cat("Frequency of 'Included_updated' status:\n")
  print(table(df$Included_updated))
  
  # IUs marked as "Not included"
  df_not_included <- subset(df, Included_updated == "Not included")
  cat("Number of IUs marked as 'Not included':", length(unique(df_not_included$IU_ID_MAPPING)), "\n")
  
  # IUs with 'NON-CONTROL' phase
  df_noncontrol <- subset(df, PHASE == "NON-CONTROL")
  cat("Number of IUs in 'NON-CONTROL' phase:", length(unique(df_noncontrol$IU_ID_MAPPING)), "\n")
  
  # Find the difference (the 20 extra IUs that should be kept)
  difference_iu <- setdiff(unique(df_noncontrol$IU_ID_MAPPING), 
                           unique(df_not_included$IU_ID_MAPPING))
  cat("Number of extra IUs to keep:", length(difference_iu), "\n")
  
  # Create the final subset with these IUs
  df_check <- subset(df, IU_ID_MAPPING %in% difference_iu)
  
  # Check the final counts for 2022
  df_lastyr <- subset(df, Year == 2022)
  cat("Number of IUs in 2022 after processing:", nrow(df_lastyr), "\n")
  
  # Check the frequency of 'Included' and 'Included_updated'
  cat("Frequency of 'Included' status:\n")
  print(table(df_lastyr$Included))
  
  cat("Frequency of 'Included_updated' status:\n")
  print(table(df_lastyr$Included_updated))
  
  # Return the final updated data
  return(df)
}

# ================================================================================ #
#           Function to fill years back to 1975 for each Iu & perform checks       #
# ================================================================================ #

backfill_histories <- function(df) {
  # Split data by IU_ID_MAPPING
  dfls <- split(df, df$IU_ID_MAPPING)
  
  # Initialize a list to hold the updated data frames
  newdf <- vector("list", length(dfls))
  
  # Start processing each IU
  for (i in 1:length(dfls)) {
    # Extract individual IU data
    newdf[[i]] <- subset(dfls[[i]], select = colnames(df))
    
    # Get the final ESPEN year
    FinalESPENyr <- head(newdf[[i]]$Year, 1)
    
    # Check if the IU starts before ESPEN and calculate prior years
    IUstart <- 1975
    newdf[[i]]$IUstart <- IUstart
    Prioryrs <- FinalESPENyr - IUstart
    
    IUStartMDA <- max(min(newdf[[i]]$Year) - min(newdf[[i]]$Cum_MDA_Final) - (min(newdf[[i]]$Year) - FinalESPENyr))
    newdf[[i]]$lsID <- i
    
    # Log current IU's history
    cat("Processing IU:", i, "with start year:", IUstart, "\n")
    
    if (!is.na(IUstart)) {
      if (IUstart < FinalESPENyr) {
        # Augment data for years before the first year on ESPEN
        tmp <- newdf[[i]][rep(1, each = (min(newdf[[i]]$Year) - IUstart)), ]
        
        # Fill in missing years and set MDA to 0 for those years
        tmp$Year <- seq(IUstart, min(newdf[[i]]$Year) - 1)
        
        # If there is any MDA before the FinalESPENyr, fill the cumulative MDA
        if (any(head(newdf[[i]]$Cum_MDA_Final) > 0)) {
          val_totest <- head(newdf[[i]]$Cum_MDA_Final, 1)
          
          if (val_totest > 1) {
            leading_zeros <- rep(0, 37 - head(newdf[[i]]$Cum_MDA_Final, 1) + 1)
            tmp$Cum_MDA_Final <- c(leading_zeros, seq(from = 0, to = head(newdf[[i]]$Cum_MDA_Final, 1) - 1, by = 1))
          }
        }
        
        # Assign augmented values for the "Endemicity" and "MDA_scheme" columns
        tmp$Endemicity <- "Augmented"
        tmp$MDA_scheme <- "Augmented"
        
        # Bind the augmented data to the original data for this IU
        newdf[[i]] <- rbind(tmp, newdf[[i]])
      }
    }
  }
  
  # Combine all the individual IU data frames into a single data frame
  newdf <- do.call(rbind, newdf)
  df_updated <- newdf
  
  # Check if all IUs have 1975-2022 data (48 rows)
  check_result <- df_updated %>%
    group_by(IU_ID_MAPPING) %>%
    filter(n() != 48) %>%
    ungroup()
  
  # Log the number of IUs with incomplete history
  cat("Number of IUs with missing history data:", nrow(check_result), "\n")
  
  # Log the total number of unique IUs after processing
  cat("Total number of unique IUs after processing:", length(unique(df_updated$IU_ID_MAPPING)), "\n")
  
  # Return the updated dataframe
  return(df_updated)
}

# =================================================================================================== #
#  Function to load, process and integrate special intervention zone labels to IUs where SIZ occurred #
# =================================================================================================== #

add_SIZ_labels <- function(df, file_path_input) {
  
  SIZ_data_file <- file.path(base_path, file_path_input)
  
  # Load the SIZ data from the given path
  SIZs_IUs <- read_excel(SIZ_data_file)
  
  # Create a vector of IUs from the SIZ file
  SIZ_IU_vec <- as.character(SIZs_IUs$IU)
  cat("Number of IUs in SIZs list:", length(SIZ_IU_vec), "\n")
  cat("IUs in SIZs list:", SIZ_IU_vec, "\n")
  
  # Define a function to find the closest match
  find_closest_match <- function(target, candidates) {
    distances <- stringdist::stringdistmatrix(target, candidates, method = "jw")
    closest_match <- candidates[which.min(distances)]
    return(closest_match)
  }
  
  # Apply the function to match IUs in the main dataframe
  matched_names <- sapply(SIZ_IU_vec, function(name) find_closest_match(name, df$IUs_NAME_MAPPING))
  
  # Subset the dataframe based on the matched names
  df_SIZs <- df[df$IUs_NAME_MAPPING %in% matched_names, ]
  cat("Number of IUs matched in SIZs:", length(unique(df_SIZs$IUs_NAME)), "\n")
  
  # Create a vector of logical values indicating whether the conditions are met
  conditions_met <- df$IUs_NAME_MAPPING %in% matched_names &
    !is.na(df$Year) &  # Check for non-NA values in Year
    df$Year %in% 2003:2012
  
  # Use ifelse to assign "SIZ" where conditions are met and NA otherwise
  df$SIZ_label <- ifelse(conditions_met, "SIZ", NA)
  
  # Final check on the number of unique IUs
  cat("Number of unique IUs after adding SIZ labels:", length(unique(df$IU_ID_MAPPING)), "\n")
  
  # Return the updated dataframe
  return(df)
}

# =============================================================================== #
# Function to check how many are not endemic in ESPEN but included in OCP shape?  #
# =============================================================================== #

# Function to perform checks and print cat() messages
perform_endemicity_checks <- function(df) {
  
  # Subset for ESPEN endemic IUs
  dfOCP_included2_ESPENendemic <- subset(df, MAX_Endemicity %in% c("Endemic (under MDA)",
                                                                   "Endemic (MDA not delivered)",
                                                                   "Endemic (under post-intervention surveillance)"))
  dfOCP_included2_ESPENendemic_lastyr <- subset(dfOCP_included2_ESPENendemic, Year == 2022)
  cat("Number of ESPEN endemic IUs in 2022:", nrow(dfOCP_included2_ESPENendemic_lastyr), "\n")
  
  # Subset for ESPEN endemic IUs that are not in the OCP shape file
  dfOCP_included2_ESPENendemic_notinOCPshape <- subset(dfOCP_included2_ESPENendemic_lastyr, PHASE == "NON-CONTROL")
  cat("Number of ESPEN endemic IUs not in the OCP shape file:", nrow(dfOCP_included2_ESPENendemic_notinOCPshape), "\n")
  
  # Subset for only OCP shape IUs
  dfOCP_included2_onlyOCPshape <- subset(df, MAX_Endemicity %in% c("Unknown (under LF MDA)",
                                                                   "Not reported",
                                                                   "Non-endemic",
                                                                   "Unknown (consider Oncho Elimination Mapping)"))
  dfOCP_included2_onlyOCPshape_lastyr <- subset(dfOCP_included2_onlyOCPshape, Year == 2022)
  cat("Number of only OCP shape IUs in 2022:", nrow(dfOCP_included2_onlyOCPshape_lastyr), "\n")
  
}

# =============================================================================================================== #
#     Function to load, process and integrate baseline endemicity, then remove IUs without baseline information   #
# =============================================================================================================== #

integrate_baseline_endemicity <- function(df, file_path_input) {
  
  # Load the baseline endemicity data from the specified path
  baseline_data_file <- file.path(base_path, file_path_input)
  load(baseline_data_file)
  
  # Convert the matrix to a dataframe
  OCP_baseline <- as.data.frame(summaries_ocp) 
  cat("OCP baseline data loaded. Total number of IUs in baseline data:", nrow(OCP_baseline), "\n")
  
  # Add ADMIN0 and IU_ID_MAPPING to the baseline data
  OCP_baseline$ADMIN0 <- substr(OCP_baseline$IU_CODE, 1, 3)
  OCP_baseline$IU_ID_MAPPING <- substr(OCP_baseline$IU_CODE, nchar(OCP_baseline$IU_CODE) - 4, nchar(OCP_baseline$IU_CODE))
  OCP_baseline$IU_ID_MAPPING <- as.numeric(as.character(OCP_baseline$IU_ID_MAPPING)) # Remove leading 0 from IU_ID_MAPPING
  
  # Merge the baseline data with the input dataframe based on 'IU_ID_MAPPING'
  df <- merge(df, OCP_baseline, by.x = "IU_ID_MAPPING", by.y = "IU_ID_MAPPING", all.x = TRUE)
  #cat("Data merged. Number of rows in merged dataframe:", nrow(df), "\n")
  
  # Remove extra columns from the merged dataframe (keeping only relevant columns)
  df <- df[, 1:(ncol(df) - 8)]  # Remove last 8 columns (optional)
  
  # Add the baseline endemicity column
  df <- df %>%
    mutate(endemicity_baseline = case_when(
      is.na(mean) ~ "non-endemic",  # No baseline data
      mean < 0.40 ~ "hypoendemic",  # Hypoendemic
      mean < 0.60 ~ "mesoendemic",  # Mesoendemic
      mean < 0.80 ~ "hyperendemic", # Hyperendemic
      TRUE ~ "holoendemic"          # Holoendemic
    ))
  
  cat("Baseline endemicity column added.\n")
  
  # Remove IUs with non-endemic baseline from the dataframe
  non_endemic_baseline <- subset(df, endemicity_baseline == "non-endemic")
  cat("Number of IUs with non-endemic baseline:", nrow(non_endemic_baseline), "\n")
  
  # Identify IUs to exclude (those without baseline endemicity information)
  non_endemic_baseline_vec <- unique(non_endemic_baseline$IU_ID_MAPPING)
  df <- df[!df$IU_ID_MAPPING %in% non_endemic_baseline_vec, ]
  cat("Number of IUs remaining after removing non-endemic baseline:", length(unique(df$IU_ID_MAPPING)), "\n")
  
  # Check number of IUs in the last year (2022) after the removal
  check_df <- subset(df, Year == 2022)
  cat("Number of IUs remaining in 2022:", nrow(check_df), "\n")
  cat("Frequency of 'Included' status in 2022:\n")
  print(table(check_df$Included))
  
  return(df)
}


# -======================================================================================================== #
# Function to produce columns to define whether an IU is inclued/excluded for Pre-ESPEN and ESPEN histories #
# ========================================================================================================= #

add_pre_post_ESPEN_history <- function(df) {
  
  # ============================== #
  # Pre-ESPEN history to include  #
  cat("Processing Pre-ESPEN MDA history...\n")
  
  df <- df %>%
    group_by(IU_ID_MAPPING) %>%
    mutate(
      Pre_ESPEN_MDA_history = ifelse(ADMIN0ISO3 == "NER" |
                                       (any(MAX_Endemicity %in% c("Endemic (MDA not delivered)", "Endemic (under MDA)", "Endemic (under post-intervention surveillance)")) &
                                          any(Cum_MDA > 0 & Year == 2013)) |
                                       (any(MAX_Endemicity %in% c("Unknown (consider Oncho Elimination Mapping)", "Unknown (under LF MDA)", "Not reported", "Non-endemic")) &
                                          any(endemicity_baseline %in% c("hyperendemic", "mesoendemic", "hypoendemic")) &
                                          any(Cum_MDA > 0 & Year == 2013)),
                                     "Include", "Exclude")
    ) %>%
    ungroup()  # Remove grouping information
  
  cat("Pre-ESPEN MDA history processed.\n")
  
  # need to arrange by year # 
  df <- df %>% arrange(IU_ID_MAPPING, Year) # order by ascending year within each IU_ID_MAPPING
  
  
  # ============================== #
  # Post-ESPEN history to include  #
  cat("Processing Post-ESPEN MDA history...\n")
  
  df <- df %>%
    group_by(IU_ID_MAPPING) %>%
    mutate(
      ESPEN_MDA_history = ifelse(
        (any(Endemicity %in% c("Endemic (MDA not delivered)", "Endemic (under MDA)", "Endemic (under post-intervention surveillance)") &
               Year > 2012) &
           any(EpiCov > 0 & Year > 2013)) |
          (any(MAX_Endemicity %in% c("Unknown (consider Oncho Elimination Mapping)", "Unknown (under LF MDA)", "Not reported", "Non-endemic")) &
             any(endemicity_baseline %in% c("hyperendemic", "mesoendemic", "hypoendemic")) &
             any(EpiCov > 0 & Year > 2013)),
        "Include", "Exclude")
    ) %>%
    ungroup()  # Remove grouping information
  
  cat("Post-ESPEN MDA history processed.\n")
  
  # Check Treatment Naive IUs
  cat("Checking for treatment naive IUs...\n")
  check_histories_trtnaive <- df %>%
    filter(Pre_ESPEN_MDA_history == "Exclude" & ESPEN_MDA_history == "Exclude")
  cat("Number of treatment naive IUs:", length(unique(check_histories_trtnaive$IU_ID_MAPPING)), "\n")
  
  # Process MDA status for 2022
  df_lastyr <- subset(df, Year == 2022)
  df_lastyr$MDA_status <- ifelse(df_lastyr$Pre_ESPEN_MDA_history == "Exclude" & 
                                   df_lastyr$ESPEN_MDA_history == "Exclude", 
                                 "Treatment naive", "MDA history")
  
  
  
  return(df)
}

# ========================================================================================= #
#                Function to apply vector control and MDA interventions up to 2002          #
# ========================================================================================= #

apply_OCP_interventions_up_to_2002 <- function(df) {
  
  # create a MDA_status col and modify ADMINO col name
  df$MDA_status <- ifelse(df$Pre_ESPEN_MDA_history == "Exclude" &
                            df$ESPEN_MDA_history == "Exclude", "Treatment naive", "MDA history")
  
  df$ADMIN0 <- df$ADMIN0.x
  
  
  # ============================== #
  #  Vector Control (up to 2002)   #
  #cat("Applying vector control interventions (up to 2002)...\n")
  
  df$vector_control <- ifelse(df$ADMIN0 %in% "Benin" & df$PHASE %in% "PHASE III EAST - JUL 1977" & df$Year %in% c(1977:2002), 1,
                              ifelse(df$ADMIN0 %in% "Benin" & df$PHASE %in% "SOUTHERN EXTENSION - FEB 1988" & df$Year %in% c(1988:2002), 1,
                                     ifelse(df$ADMIN0 %in% "Burkina Faso" & df$PHASE %in% "PHASE 1 - FEB 1975" & df$Year %in% c(1975:1989), 1,
                                            ifelse(df$ADMIN0 %in% "Burkina Faso" & df$PHASE %in% "PHASE II - JAN 1976" & df$Year %in% c(1976:1989), 1,
                                                   ifelse(df$ADMIN0 %in% "Burkina Faso" & df$PHASE %in% "PHASE III EAST - JUL 1977" & df$Year %in% c(1977:1989), 1,
                                                          ifelse(df$ADMIN0 %in% "Cote d'Ivoire" & df$PHASE %in% "PHASE 1 - FEB 1975" & df$Year %in% c(1975:1991), 1,
                                                                 ifelse(df$ADMIN0 %in% "Cote d'Ivoire" & df$PHASE %in% "PHASE III WEST - MAR 1977" & df$Year %in% c(1977:1991), 1,
                                                                        ifelse(df$ADMIN0 %in% "Cote d'Ivoire" & df$PHASE %in% "PHASE IV - MAR 1979" & df$Year %in% c(1979:1991), 1,
                                                                               ifelse(df$ADMIN0 %in% "Ghana" & df$PHASE %in% "PHASE 1 - FEB 1975" & df$Year %in% c(1975:1989), 1,
                                                                                      ifelse(df$ADMIN0 %in% "Ghana" & df$PHASE %in% "PHASE II - JAN 1976" & df$Year %in% c(1976:1989), 1,
                                                                                             ifelse(df$ADMIN0 %in% "Ghana" & df$PHASE %in% "PHASE III EAST - JUL 1977" & df$Year %in% c(1977:1989), 1,
                                                                                                    ifelse(df$ADMIN0 %in% "Ghana" & df$PHASE %in% "SOUTHERN EXTENSION - FEB 1988" & df$Year %in% c(1988:1998), 1,
                                                                                                           ifelse(df$ADMIN0 %in% "Togo" & df$ADMIN1 %in% c("Kara","Centrale") & df$PHASE %in% "PHASE II - JAN 1976" & df$Year %in% c(1976:2002), 1,
                                                                                                                  ifelse(df$ADMIN0 %in% "Togo" & df$ADMIN1 %in% "Savanes" & df$PHASE %in% "PHASE II - JAN 1976" & df$Year %in% c(1976:1993), 1,
                                                                                                                         ifelse(df$ADMIN0 %in% "Togo" & df$ADMIN1 %in% c("Kara","Centrale") & df$PHASE %in% "PHASE III EAST - JUL 1977" & df$Year %in% c(1977:2002), 1,
                                                                                                                                ifelse(df$ADMIN0 %in% "Togo" & df$ADMIN1 %in% "Savanes" & df$PHASE %in% "PHASE III EAST - JUL 1977" & df$Year %in% c(1977:1993), 1,
                                                                                                                                       ifelse(df$ADMIN0 %in% "Togo" & df$PHASE %in% "SOUTHERN EXTENSION - FEB 1988" & df$Year %in% c(1989:2002), 1,
                                                                                                                                              ifelse(df$ADMIN0 %in% "Guinea" & df$PHASE %in% "WESTERN EXTENSION - MAR 1989" & df$Year %in% c(1989:2002), 1,
                                                                                                                                                     ifelse(df$ADMIN0 %in% "Mali" & df$PHASE %in% "PHASE 1 - FEB 1975" & df$Year %in% c(1975:1987), 1,
                                                                                                                                                            ifelse(df$ADMIN0 %in% "Mali" & df$PHASE %in% "PHASE III EAST - JUL 1977" & df$Year %in% c(1977:1987), 1,
                                                                                                                                                                   ifelse(df$ADMIN0 %in% "Mali" & df$PHASE %in% "WESTERN EXTENSION - MAR 1989" & df$Year %in% c(1989:2002), 1,
                                                                                                                                                                          ifelse(df$ADMIN0 %in% "Mali" & df$PHASE %in% "PHASE III WEST - MAR 1977" & df$Year %in% c(1977:1991), 1,
                                                                                                                                                                                 ifelse(df$ADMIN0 %in% "Niger" & df$PHASE %in% c("PHASE III EAST - JUL 1977", "NON-CONTROL") & df$Year %in% c(1977:1989), 1,
                                                                                                                                                                                        NA)))))))))))))))))))))))
  
  cat("vector control to 2002 applied.\n")
  
  # Ensure Treatment Naive Areas are set to 0 for MDA_nonCDTI
  df$vector_control <- ifelse(df$vector_control == "Treatment naive", 0, df$vector_control)
  #cat("vector control to 2002 applied updated for Treatment naive areas.\n")
  
  # non-CDTI MDA (up to 1996/1997) #
  
  #cat("Applying non-CDTI MDA (up to 1996)...\n")
  df$MDA_nonCDTI <- ifelse(df$ADMIN0 %in% "Benin" & df$PHASE %in% c("PHASE III EAST - JUL 1977", "SOUTHERN EXTENSION - FEB 1988") & df$Year %in% c(1988:1996), 1,
                           ifelse(df$ADMIN0 %in% "Burkina Faso" & df$PHASE %in% c("PHASE 1 - FEB 1975", "PHASE II - JAN 1976", "PHASE III EAST - JUL 1977") & df$Year %in% c(1990:1996), 1,
                                  ifelse(df$ADMIN0 %in% "Cote d'Ivoire" & df$PHASE %in% c("PHASE 1 - FEB 1975", "PHASE III WEST - MAR 1977", "PHASE IV - MAR 1979") & df$Year %in% c(1988:1996), 1,
                                         ifelse(df$ADMIN0 %in% "Cote d'Ivoire" & df$PHASE %in% c("SOUTHERN FOREST EXTENSION - JAN 1990") & df$Year %in% c(1990:1996), 1,
                                                ifelse(df$ADMIN0 %in% "Ghana" & df$PHASE %in% c("PHASE 1 - FEB 1975", "PHASE II - JAN 1976", "PHASE III EAST - JUL 1977") & df$Year %in% c(1990:1996), 1,
                                                       ifelse(df$ADMIN0 %in% "Ghana" & df$PHASE %in% "SOUTHERN EXTENSION - FEB 1988" & df$Year %in% c(1988:1996), 1,
                                                              ifelse(df$ADMIN0 %in% "Ghana" & df$PHASE %in% "SOUTHERN FOREST EXTENSION - JAN 1990" & df$Year %in% c(1990:1996), 1,
                                                                     ifelse(df$ADMIN0 %in% "Togo" & df$ADMIN1 %in% c("Kara","Centrale", "Savanes") & df$PHASE %in% c("PHASE II - JAN 1976", "PHASE III EAST - JUL 1977") & df$Year %in% c(1991:1996), 1,
                                                                            ifelse(df$ADMIN0 %in% "Togo" & df$PHASE %in% "SOUTHERN EXTENSION - FEB 1988" & df$Year %in% c(1991:1996), 1,
                                                                                   ifelse(df$ADMIN0 %in% "Guinea" & df$PHASE %in% "WESTERN EXTENSION - MAR 1989" & df$Year %in% c(1989:1996), 1,
                                                                                          ifelse(df$ADMIN0 %in% "Guinea" & df$PHASE %in% c("WESTERN EXTENSION - 1990", "SOUTHERN FOREST EXTENSION - JAN 1990") & df$Year %in% c(1990:1996), 1,
                                                                                                 ifelse(df$ADMIN0 %in% "Guinea-Bissau" & df$PHASE %in% "WESTERN EXTENSION - 1990" & df$Year %in% c(1990:1996), 1,
                                                                                                        ifelse(df$ADMIN0 %in% "Mali" & df$PHASE %in% c("PHASE 1 - FEB 1975", "PHASE III EAST - JUL 1977", "PHASE III WEST - MAR 1977") & df$Year %in% c(1988:1996), 1,
                                                                                                               ifelse(df$ADMIN0 %in% "Mali" & df$PHASE %in% "WESTERN EXTENSION - MAR 1989" & df$Year %in% c(1989:1996), 1,
                                                                                                                      ifelse(df$ADMIN0 %in% "Mali" & df$PHASE %in% "WESTERN EXTENSION - 1990" & df$Year %in% c(1990:1996), 1,
                                                                                                                             ifelse(df$ADMIN0 %in% "Niger" & df$PHASE %in% c("PHASE III EAST - JUL 1977", "NON-CONTROL") & df$Year %in% c(1990:1996), 1,
                                                                                                                                    ifelse(df$ADMIN0 %in% "Senegal" & df$PHASE %in% "WESTERN EXTENSION - 1990" & df$Year %in% c(1990:1996), 1,
                                                                                                                                           NA)))))))))))))))))
  
  cat("Non-CDTI MDA applied (to 1996).\n")
  
  # Ensure Treatment Naive Areas are set to 0 for MDA_nonCDTI
  df$MDA_nonCDTI <- ifelse(df$MDA_status == "Treatment naive", 0, df$MDA_nonCDTI)
  #cat("MDA_nonCDTI status updated for Treatment naive areas.\n")
  
  
  # ============================== #
  #  CDTI MDA (up to 2002)        #
  #cat("Applying MDA interventions (up to 2002)...\n")
  
  df$MDA_CDTI <- ifelse(df$ADMIN0 %in% "Benin" & df$PHASE %in% c("PHASE III EAST - JUL 1977","SOUTHERN EXTENSION - FEB 1988") & df$Year %in% c(1997:2002), 1,
                        ifelse(df$ADMIN0 %in% "Burkina Faso" & df$PHASE %in% c("PHASE 1 - FEB 1975", "PHASE II - JAN 1976", "PHASE III EAST - JUL 1977") & df$Year %in% c(1997:2002), 1,
                               ifelse(df$ADMIN0 %in% "Cote d'Ivoire" & df$PHASE %in% c("PHASE 1 - FEB 1975", "PHASE III WEST - MAR 1977", "PHASE IV - MAR 1979") & df$Year %in% c(1997:2000), 1,
                                      ifelse(df$ADMIN0 %in% "Cote d'Ivoire" & df$PHASE %in% c("SOUTHERN FOREST EXTENSION - JAN 1990") & df$Year %in% c(1997:2002), 1,
                                             ifelse(df$ADMIN0 %in% "Ghana" & df$PHASE %in% c("PHASE 1 - FEB 1975", "PHASE II - JAN 1976", "PHASE III EAST - JUL 1977") & df$Year %in% c(1997:2002), 1,
                                                    ifelse(df$ADMIN0 %in% "Ghana" & df$PHASE %in% "SOUTHERN EXTENSION - FEB 1988" & df$Year %in% c(1997:2002), 1,
                                                           ifelse(df$ADMIN0 %in% "Ghana" & df$PHASE %in% "SOUTHERN FOREST EXTENSION - JAN 1990" & df$Year %in% c(1997:2002), 1,
                                                                  ifelse(df$ADMIN0 %in% "Togo" & df$ADMIN1 %in% c("Kara","Centrale", "Savanes") & df$PHASE %in% c("PHASE II - JAN 1976", "PHASE III EAST - JUL 1977") & df$Year %in% c(1997:2002), 1,
                                                                         ifelse(df$ADMIN0 %in% "Togo" & df$PHASE %in% "SOUTHERN EXTENSION - FEB 1988" & df$Year %in% c(1997:2002), 1,
                                                                                ifelse(df$ADMIN0 %in% "Guinea" & df$PHASE %in% "WESTERN EXTENSION - MAR 1989" & df$Year %in% c(1997:2002), 1,
                                                                                       ifelse(df$ADMIN0 %in% "Guinea" & df$PHASE %in% c("WESTERN EXTENSION - 1990", "SOUTHERN FOREST EXTENSION - JAN 1990") & df$Year %in% c(1997:2002), 1,
                                                                                              ifelse(df$ADMIN0 %in% "Guinea-Bissau" & df$PHASE %in% "WESTERN EXTENSION - 1990" & df$Year %in% c(1997:2002), 1,
                                                                                                     ifelse(df$ADMIN0 %in% "Mali" & df$PHASE %in% c("PHASE 1 - FEB 1975", "PHASE III EAST - JUL 1977", "PHASE III WEST - MAR 1977") & df$Year %in% c(1997:2002), 1,
                                                                                                            ifelse(df$ADMIN0 %in% "Mali" & df$PHASE %in% "WESTERN EXTENSION - MAR 1989" & df$Year %in% c(1997:2002), 1,
                                                                                                                   ifelse(df$ADMIN0 %in% "Mali" & df$PHASE %in% "WESTERN EXTENSION - 1990" & df$Year %in% c(1997:2002), 1,
                                                                                                                          ifelse(df$ADMIN0 %in% "Niger" & df$PHASE %in% c("PHASE III EAST - JUL 1977", "NON-CONTROL") & df$Year %in% c(1997:2002), 1,
                                                                                                                                 NA))))))))))))))))
  
  cat("CDTI MDA to 2002 applied.\n")
  
  # Ensure Treatment Naive Areas are set to 0 for MDA_nonCDTI
  df$MDA_CDTI <- ifelse(df$MDA_CDTI == "Treatment naive", 0, df$MDA_CDTI)
  #cat("CDTI MDA to 2002 applied updated for Treatment naive areas.\n")
  
  # check number of IUs at this stage:
  cat("Rows in 2022 after processing (updating interventions 1975 - 2002):", nrow(subset(df, Year == 2022)), "\n")
  cat("Unique IU_ID_MAPPING count after processing (updating interventions 1975 - 2002):", length(unique(df$IU_ID_MAPPING)), "\n")
  
  
  return(df)
  
}

# ============================================================================= #
#          Function to apply vector control and MDA interventions               #
#        up 2003-2012 (including SIZ vector control) & biannual to 2022         #
# ============================================================================= #

apply_OCP_interventions_SIZ_2003_2012_biannual <- function(df) {
  
  # ============================== #
  # Vector Control (2003-2012)   #
  cat("Applying vector control interventions (2003-2012)...\n")
  
  df$vector_control <- ifelse(df$ADMIN0 %in% "Benin" & df$SIZ_label == "SIZ" & df$PHASE %in% c("PHASE III EAST - JUL 1977","SOUTHERN EXTENSION - FEB 1988") & df$Year %in% c(2003:2007), 1,
                              ifelse(df$ADMIN0 %in% "Ghana" & df$SIZ_label == "SIZ" & df$PHASE %in% "PHASE III EAST - JUL 1977" & df$Year %in% c(2003:2007), 1,
                                     ifelse(df$ADMIN0 %in% "Togo" & df$SIZ_label == "SIZ" & df$ADMIN1 %in% c("Kara", "Centrale") & df$Year %in% c(2003:2007), 1,
                                            df$vector_control))) # only Oti in Ghana
  
  # Update vector control for treatment naive areas
  df$vector_control <- ifelse(df$MDA_status == "Treatment naive", 0, df$vector_control)
  #cat("Vector control for 2003-2012 applied and updated for Treatment naive areas.\n")
  
  # ============================== #
  # MDA (2003-2012)   #
  cat("Applying MDA interventions (2003-2012)...\n")
  
  df$MDA_CDTI <- ifelse(df$ADMIN0 %in% "Benin" & df$PHASE %in% c("PHASE III EAST - JUL 1977", "SOUTHERN EXTENSION - FEB 1988") & df$Year %in% c(2003:2013), 1,
                        ifelse(df$ADMIN0 %in% "Burkina Faso" & df$PHASE %in% c("PHASE 1 - FEB 1975", "PHASE II - JAN 1976", "PHASE III EAST - JUL 1977") & df$Year %in% c(2003:2013), 1,
                               ifelse(df$ADMIN0 %in% "Cote d'Ivoire" & df$PHASE %in% c("PHASE 1 - FEB 1975", "PHASE III WEST - MAR 1977", "PHASE IV - MAR 1979", "SOUTHERN FOREST EXTENSION - JAN 1990") & df$Year %in% c(2008:2013), 1,
                                      ifelse(df$ADMIN0 %in% "Ghana" & df$PHASE %in% c("PHASE 1 - FEB 1975", "PHASE II - JAN 1976", "PHASE III EAST - JUL 1977", "SOUTHERN EXTENSION - FEB 1988", "SOUTHERN FOREST EXTENSION - JAN 1990") & df$Year %in% c(2003:2013), 1,
                                             ifelse(df$ADMIN0 %in% "Togo" & df$ADMIN1 %in% c("Kara", "Centrale", "Savanes") & df$PHASE %in% c("PHASE II - JAN 1976", "PHASE III EAST - JUL 1977") & df$Year %in% c(2003:2013), 1,
                                                    ifelse(df$ADMIN0 %in% "Togo" & df$PHASE %in% "SOUTHERN EXTENSION - FEB 1988" & df$Year %in% c(2003:2013), 1,
                                                           ifelse(df$ADMIN0 %in% "Guinea" & df$PHASE %in% c("WESTERN EXTENSION - MAR 1989", "WESTERN EXTENSION - 1990", "SOUTHERN FOREST EXTENSION - JAN 1990") & df$Year %in% c(2003:2013), 1,
                                                                  ifelse(df$ADMIN0 %in% "Guinea-Bissau" & df$PHASE %in% "WESTERN EXTENSION - 1990" & df$Year %in% c(2003:2013), 1,
                                                                         ifelse(df$ADMIN0 %in% "Mali" & df$PHASE %in% c("PHASE 1 - FEB 1975", "PHASE III EAST - JUL 1977", "WESTERN EXTENSION - MAR 1989", "WESTERN EXTENSION - 1990", "PHASE III WEST - MAR 1977") & df$Year %in% c(2003:2013), 1,
                                                                                ifelse(df$ADMIN0 %in% "Niger" & df$PHASE %in% c("PHASE III EAST - JUL 1977", "NON-CONTROL") & df$Year %in% c(2003:2013), 1,
                                                                                       ifelse(df$ADMIN0 %in% "Senegal" & df$PHASE %in% "WESTERN EXTENSION - 1990" & df$Year %in% c(2003:2013), 1,
                                                                                              df$MDA_CDTI)))))))))))
  
  # Update MDA for treatment naive areas
  df$MDA_CDTI <- ifelse(df$MDA_status == "Treatment naive", 0, df$MDA_CDTI)
  #cat("MDA for 2003-2012 applied and updated for Treatment naive areas.\n")
  
  # ============================== #
  # Biannual CDTI (2003-2012)   #
  cat("Applying biannual MDA interventions (2003 onwards)...\n")
  
  df$MDA_CDTI_Biannual <- ifelse(df$ADMIN0 %in% "Benin" & df$SIZ_label == "SIZ" & df$PHASE %in% c("PHASE III EAST - JUL 1977", "SOUTHERN EXTENSION - FEB 1988") & df$Year %in% c(2003:2013), 1,
                                 ifelse(df$ADMIN0 %in% "Ghana" & df$SIZ_label == "SIZ" & df$PHASE %in% "PHASE III EAST - JUL 1977" & df$Year %in% c(2003:2013), 1,
                                        ifelse(df$ADMIN0 %in% "Ghana" & df$endemicity_baseline %in% c("mesoendemic", "hyperendemic") & df$Year %in% c(2009:2022), 1,
                                               ifelse(df$ADMIN0 %in% "Guinea" & df$SIZ_label == "SIZ" & df$PHASE %in% c("WESTERN EXTENSION - MAR 1989", "WESTERN EXTENSION - 1990") & df$Year %in% c(2003:2013), 1,
                                                      ifelse(df$ADMIN0 %in% "Togo" & df$IUs_NAME_MAPPING %in% c("Anie", "OGOU", "AMOU", "HAHO") & df$Year %in% c(2014:2022), 1,
                                                             ifelse(df$ADMIN0 %in% "Togo" & df$IUs_NAME_MAPPING %in% c("Kpendjal", "Kpendjal-Ouest", "OTI", "OtI-Sud",
                                                                                                                       "ASSOLI", "BASSAR", "BINAH", "DANKPEN", "DOUFELGOU", "KERAN", "KOZAH",
                                                                                                                       "TCHAOUDJO", "MO", "SOTOUBOUA") & df$Year %in% c(2003:2022), 1, NA))))))
  
  # Update biannual MDA for treatment naive areas
  df$MDA_CDTI_Biannual <- ifelse(df$MDA_status == "Treatment naive", 0, df$MDA_CDTI_Biannual)
  #cat("Biannual MDA for 2003-2012 applied and updated for Treatment naive areas.\n")
  
  # check number of IUs at this stage:
  cat("Rows in 2022 after processing (updating interventions after 2002):", nrow(subset(df, Year == 2022)), "\n")
  cat("Unique IU_ID_MAPPING count after processing (updating interventions after 2002):", length(unique(df$IU_ID_MAPPING)), "\n")
  
  
  return(df)
}

# ============================================================================= #
#   Function to update MDA and fill in MDA during ESPEN years                   #
# ============================================================================= #

update_MDA_including_ESPENyrs <- function(df) {
  # Remove vector control, annual non-CDTI, and CDTI for pre-ESPEN EXCLUDE IUs
  
  # Update MDA_nonCDTI for pre-ESPEN "Exclude" IUs
  df$MDA_nonCDTI <- ifelse(df$Pre_ESPEN_MDA_history == "Exclude" & df$Year < 2014 & df$ADMIN0 != "Ghana", 0, df$MDA_nonCDTI)
  df$MDA_nonCDTI <- ifelse(df$Pre_ESPEN_MDA_history == "Exclude" & df$Year < 2014 & df$ADMIN0 == "Ghana" & df$endemicity_baseline == "hypoendemic", 0, df$MDA_nonCDTI)
  
  # Update MDA_CDTI for pre-ESPEN "Exclude" IUs
  df$MDA_CDTI <- ifelse(df$Pre_ESPEN_MDA_history == "Exclude" & df$Year < 2014 & df$ADMIN0 != "Ghana", 0, df$MDA_CDTI)
  df$MDA_CDTI <- ifelse(df$Pre_ESPEN_MDA_history == "Exclude" & df$Year < 2014 & df$ADMIN0 == "Ghana" & df$endemicity_baseline == "hypoendemic", 0, df$MDA_CDTI)
  
  # Update vector_control for pre-ESPEN "Exclude" IUs
  df$vector_control <- ifelse(df$Pre_ESPEN_MDA_history == "Exclude" & df$Year < 2014 & df$ADMIN0 != "Ghana", 0, df$vector_control)
  df$vector_control <- ifelse(df$Pre_ESPEN_MDA_history == "Exclude" & df$Year < 2014 & df$ADMIN0 == "Ghana" & df$endemicity_baseline == "hypoendemic", 0, df$vector_control)
  
  cat("MDA for pre-ESPEN Exclude IUs has been updated.\n")
  
  # Filling in MDA presence based on ESPEN data (2013 - 2022)
  df$MDA_CDTI <- ifelse(df$EpiCov > 0 & df$Year %in% c(2014:2022), 1, df$MDA_CDTI)
  
  cat("MDA_CDTI updated for ESPEN data from 2014 to 2022.\n")
  
  # Remove 1 from biannual where no annual CDTI between 2014-2022
  df$MDA_CDTI_Biannual <- ifelse(df$MDA_CDTI == 0 | is.na(df$MDA_CDTI) & df$Year %in% c(2014:2022), 0, df$MDA_CDTI_Biannual)
  
  cat("Biannual MDA_CDTI has been updated for 2014-2022.\n")
  
  # check number of IUs at this stage:
  cat("Rows in 2022 after processing (updating MDA & including MDA during ESPEN years):", nrow(subset(df, Year == 2022)), "\n")
  cat("Unique IU_ID_MAPPING count after processing (updating MDA & including MDA during ESPEN years):", length(unique(df$IU_ID_MAPPING)), "\n")
  
  
  # Return the updated dataframe
  return(df)
}

# ============================================================================================ #
#                           Function to extend years from 2023-2025                            #
# ============================================================================================ #

extend_IU_years_2023_2025 <- function(df, FinalESPENyr = 2022) {
  
  # Subset the dataframe for the last year (2022)
  df_lastyr <- subset(df, Year == FinalESPENyr)
  
  # Split the dataframe by IU_ID_MAPPING
  dfls <- split(df, df$IU_ID_MAPPING)
  
  # New list of dataframes to store the extended rows
  newdf <- vector("list", length(dfls))
  
  # Loop through each IU and add rows for 2023-2025
  for (i in 1:length(dfls)) {
    
    newdf[[i]] <- subset(dfls[[i]], select = colnames(df))  # Copy the columns structure of original df
    
    # Define the end year for extension (2025)
    IUend <- 2025
    
    newdf[[i]]$IUend <- IUend
    Futureyrs <- IUend - FinalESPENyr  # Number of years to add
    
    newdf[[i]]$lsID <- i  # Store the list ID as a reference
    
    # Check if the IU should have rows extended for 2023-2025
    if (!is.na(IUend)) {
      if (IUend > FinalESPENyr) {
        # Create temporary rows for the years 2023-2025
        tmp <- newdf[[i]][rep(1, each = Futureyrs), ]
        tmp$Year <- seq(FinalESPENyr + 1, IUend)
        
        # Set the Endemicity and MDA_scheme for the new rows
        tmp$Endemicity <- "Augmented"
        tmp$MDA_scheme <- "Augmented"
        
        # Add the new rows to the existing data
        newdf[[i]] <- rbind(newdf[[i]], tmp)
      }
    }
  }
  
  # Combine the list of dataframes back into a single dataframe
  newdf_combined <- do.call(rbind, newdf)
  
  # check number of IUs at this stage:
  cat("Rows in 2022 after processing (creating years 2023-2025):", nrow(subset(newdf_combined, Year == 2022)), "\n")
  cat("Unique IU_ID_MAPPING count after processing (creating years 2023-2025):", length(unique(newdf_combined$IU_ID_MAPPING)), "\n")
  
  # Return the updated dataframe
  return(newdf_combined)
}

# ============================================================================================ #
#     Function to create new intervention cols (including source info) & modify existing       #
# ============================================================================================ #

create_edit_intervention_cols <- function(df) {
  # Create a new column for raw MDA CDTI
  df$MDA_CDTI_raw <- NA
  
  # Apply interventions for non-CDTI MDA (1988 - 2013)
  df$Cov.in2 <- ifelse(df$MDA_nonCDTI == 1 & df$Year %in% c(1988:2013), 0.52, NA)
  df$cov_source <- ifelse(df$MDA_nonCDTI == 1 & df$Year %in% c(1988:2013), "assumed", NA)
  df$cov_specific_source <- ifelse(df$MDA_nonCDTI == 1 & df$Year %in% c(1988:2013), "assumed (non-CDTI coverage)", NA)
  
  # Apply interventions for CDTI MDA (1997 - 2013)
  df$Cov.in2 <- ifelse(df$MDA_CDTI == 1 & df$Year %in% c(1997:2013), 0.65, df$Cov.in2)
  df$cov_source <- ifelse(df$MDA_CDTI == 1 & df$Year %in% c(1997:2013), "assumed", df$cov_source)
  df$cov_specific_source <- ifelse(df$MDA_CDTI == 1 & df$Year %in% c(1997:2013), "assumed (CDTI coverage)", df$cov_specific_source)
  
  # Apply interventions for ESPEN MDA (2014 - 2022)
  df$Cov.in2 <- ifelse(df$EpiCov >= 65 & df$Year %in% c(2014:2022), 0.65, df$Cov.in2)
  df$cov_source <- ifelse(df$EpiCov >= 65 & df$Year %in% c(2014:2022), "IU-level coverage", df$cov_source)
  df$cov_specific_source <- ifelse(df$EpiCov >= 65 & df$Year %in% c(2014:2022), "ESPEN", df$cov_specific_source)
  
  # For lower coverage values (<65%) between 2014-2022
  df$Cov.in2 <- ifelse(df$EpiCov < 65 & df$Year %in% c(2014:2022), 0.25, df$Cov.in2)
  df$cov_source <- ifelse(df$EpiCov < 65 & df$Year %in% c(2014:2022), "IU-level coverage", df$cov_source)
  df$cov_specific_source <- ifelse(df$EpiCov < 65 & df$Year %in% c(2014:2022), "ESPEN", df$cov_specific_source)
  
  # For 0 coverage values, set Cov.in2 to NA
  df$Cov.in2 <- ifelse(df$EpiCov == 0 & df$Year %in% c(2014:2022), NA, df$Cov.in2)
  
  # Calculate the number of rounds (non-CDTI + CDTI + Biannual)
  df$number_rnds <- rowSums(df[c("MDA_nonCDTI", "MDA_CDTI", "MDA_CDTI_Biannual")], na.rm = TRUE)
  
  # Adjust vector control for 2023-2025, excluding Uganda
  df$vector_control <- ifelse(df$Year %in% c(2023:2025) & df$ADMIN0 != "Uganda", 0, df$vector_control)
  
  # check number of IUs at this stage:
  cat("Rows in 2022 after processing (creating further & editing existing intervention cols):", nrow(subset(df, Year == 2022)), "\n")
  cat("Unique IU_ID_MAPPING count after processing (creating further & editing existing intervention cols):", length(unique(df$IU_ID_MAPPING)), "\n")
  
  # Return the updated dataframe
  return(df)
}

# ================================================================================================== #
#          Function to create trt_status_2022 and trt_status_2022_v2 columns/variables               #
# ================================================================================================== #

create_trt_status_variable <- function(df) {
  
  # create trt_status_2022 variable:
  df <- df %>%
    group_by(IU_ID_MAPPING) %>%
    mutate(
      trt_status_2022 = case_when(
        all(
          any(Endemicity %in% c("Endemic (MDA not delivered)", "Endemic (under MDA)") & Year %in% c(2021, 2022) & !all(is.na(Cov.in2)))
          #& !any(Endemicity == "Endemic (under post-intervention surveillance)" & Year %in% c(2021, 2022) & !all(is.na(Cov.in2)))
        ) ~ "MDA continues",
        all(
          any(MAX_Endemicity %in% c("Unknown (under LF MDA)", "Unknown (consider Oncho Elimination Mapping)", "Not reported", "Non-endemic")) &
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
        #all(ADMIN0ISO3 == "SDN" & !all(is.na(Cov.in2))) ~ "MDA stopped",
        all(is.na(Cov.in2)) ~ "Treatment naive",
        TRUE ~ NA
      )
    )
  
  # Perform checks for different `trt_status_2022` categories
  df_check_MDAstopped <- subset(df, trt_status_2022 == "MDA stopped")
  cat("Number of IUs consdiered 'MDA stopped':", length(unique(df_check_MDAstopped$IU_ID_MAPPING)), "\n")
  
  df_check_MDAcont <- subset(df, trt_status_2022 == "MDA continues")
  cat("Number of IUs considered 'MDA continues':", length(unique(df_check_MDAcont$IU_ID_MAPPING)), "\n")
  
  df_check_trtnaive <- subset(df, trt_status_2022 == "Treatment naive")
  cat("Number of IUs considered 'Treatment naive':", length(unique(df_check_trtnaive$IU_ID_MAPPING)), "\n")
  
  # Check where Cov.in2 is 0 for all years pre-2014
  filtered_df <- df %>% filter(Year < 2014) # Filter rows where Year is pre-2014
  unique_subset <- filtered_df %>%
    group_by(IU_ID_MAPPING) %>%
    summarise(Cov.in2_sum = sum(Cov.in2, na.rm = TRUE)) %>%
    filter(Cov.in2_sum == 0) %>%
    select(IU_ID_MAPPING) # Group by IU_ID_Mapping and check if the sum of Cov.in2 for each group is 0
  
  result <- unique_subset$IU_ID_MAPPING # Get the unique IU_ID_Mapping where Cov.in2 is 0 for all years pre-2014
  df_check_pre2014_Cov0 <- subset(df, IU_ID_MAPPING %in% result)
  cat("Number of IUs with Cov.in2 = 0 for all years pre-2014:", length(unique(df_check_pre2014_Cov0$IU_ID_MAPPING)), "\n")
  
  # Create or update the trt_status_2022_v2 column
  df <- df %>%
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
    ) %>%
    ungroup()  # Remove grouping information
  
  df_check <- subset(df, Year == 2022)
  
  # Check unique values of trt_status_2022_v2
  cat("Frequency of values in 'trt_status_2022_v2':\n")
  print(table(df_check$trt_status_2022_v2))
  
  # check breakdwon of those IUs included:
  cat("Breakdown of 'included' IUs (should be the same as earlier):\n")
  print(table(df_check$Included))
  
  # check number of IUs at this stage:
  cat("Rows in 2022 after processing (creating trt_status_2022 variable):", nrow(subset(df, Year == 2022)), "\n")
  cat("Unique IU_ID_MAPPING count after processing (creating trt_status_2022 variable):", length(unique(df$IU_ID_MAPPING)), "\n")
  
  # Return the updated dataframe
  return(df)
}

# ======================================================================== #
#        Function to update MDA in 2023-2025 including biannual            #
# ======================================================================== #

apply_MDA_interventions_2023_2025 <- function(df) {
  
  # Apply MDA for "MDA continues" in 2023-2025
  df <- df %>%
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
      )
    )
  
  # Check the number of IUs in each treatment status for 2023-2025
  df_check_MDAstopped <- subset(df, trt_status_2022 == "MDA stopped")
  cat("Number of IUs with 'MDA stopped' in 2023-2025:", length(unique(df_check_MDAstopped$IU_ID_MAPPING)), "\n")
  
  df_check_MDAcont <- subset(df, trt_status_2022 == "MDA continues")
  cat("Number of IUs with 'MDA continues' in 2023-2025:", length(unique(df_check_MDAcont$IU_ID_MAPPING)), "\n")
  
  df_check_trtnaive <- subset(df, trt_status_2022 == "Treatment naive")
  cat("Number of IUs with 'Treatment naive' in 2023-2025:", length(unique(df_check_trtnaive$IU_ID_MAPPING)), "\n")
  
  # Identify IUs where biannual coverage occurred in 2020-2022 and MDA continues
  filtered_ids <- df %>%
    group_by(IU_ID_MAPPING) %>%
    filter(
      any(Year %in% c(2020, 2021, 2022) & MDA_CDTI_Biannual == 1) &
        any(Year %in% c(2023, 2024, 2025) & MDA_CDTI == 1) &
        trt_status_2022 == "MDA continues"
    ) %>%
    distinct(IU_ID_MAPPING) %>%
    pull(IU_ID_MAPPING)
  
  # Continue biannual MDA in 2023-2025 where conditions are met
  df$MDA_CDTI_Biannual <- ifelse(df$IU_ID_MAPPING %in% filtered_ids & df$Year %in% c(2023, 2024, 2025), 1, df$MDA_CDTI_Biannual)
  
  # biannual checks (2023 - 2025)
  cat("Biannual MDA applied for 2023-2025.\n")
  
  check_biannual <- subset(df, MDA_CDTI_Biannual == 1 & Year == 2025)
  cat("check number of IUs with biannual MDA applied for 2023-2025:", nrow(check_biannual), "\n")
  
  # update vector control - shouldnt be any in 2023-2025 but check
  condition <- df$vector_control == 1 & df$Year == 2022
  indices <- which(condition)
  selected_rows <- df[indices, ]
  unique_IDIU <- unique(selected_rows$IU_ID_MAPPING)
  df$vector_control <- ifelse(df$IU_ID_MAPPING %in% unique_IDIU & df$Year %in% c(2023,2024,2025), 1, df$vector_control)
  
  check_vc_2025 <- subset(df, vector_control == 1 & Year == 2025)
  cat("Number of IUs with vector control in 2023-2025:", nrow(check_vc_2025), "\n")
  
  # Check the number of IUs in 2023-2025 for verification
  check_2023_2025 <- subset(df, Year %in% c(2025))
  cat("Number of IUs in 2023-2025:", nrow(check_2023_2025), "\n")
  
  # Return the updated dataframe
  return(df)
}

# ============================================================================ #
#           Function to create co-endemicity column (oncho-LF-loa)             #
# ============================================================================ #

create_co_endemicity_column_OCP <- function(df, file_path_input) {
  
  # Load the co-endemicity data
  co_endemic_data_file <- file.path(base_path, file_path_input)
  co_endemic_IUs <- read.csv(co_endemic_data_file)
  
  # Identify unique IUs for different co-endemicity groups
  co_endemic_IUs_oncho_LF_loa <- subset(co_endemic_IUs, co_endemicity == "oncho,LF,loa")
  co_endemic_IUs_oncho_LF_loa_vec <- unique(co_endemic_IUs_oncho_LF_loa$IU_ID_MAPPING)
  
  co_endemic_IUs_oncho_LF <- subset(co_endemic_IUs, co_endemicity == "oncho,LF")
  co_endemic_IUs_oncho_LF_vec <- unique(co_endemic_IUs_oncho_LF$IU_ID_MAPPING)
  
  co_endemic_IUs_oncho_loa <- subset(co_endemic_IUs, co_endemicity == "oncho,loa")
  co_endemic_IUs_oncho_loa_vec <- unique(co_endemic_IUs_oncho_loa$IU_ID_MAPPING)
  
  co_endemic_IUs_oncho <- subset(co_endemic_IUs, co_endemicity == "oncho")
  co_endemic_IUs_oncho_vec <- unique(co_endemic_IUs_oncho$IU_ID_MAPPING)
  
  # Create the co_endemicity column based on the conditions
  df$co_endemicity <- ifelse(df$IU_ID_MAPPING %in% co_endemic_IUs_oncho_LF_loa_vec, "oncho,LF,loa",
                             ifelse(df$IU_ID_MAPPING %in% co_endemic_IUs_oncho_LF_vec, "oncho,LF",
                                    ifelse(df$IU_ID_MAPPING %in% co_endemic_IUs_oncho_loa_vec, "oncho,loa",
                                           ifelse(df$IU_ID_MAPPING %in% co_endemic_IUs_oncho_vec, "oncho", "assumed oncho only"))))
  
  df_check <- subset(df, Year == 2022)
  
  # Check unique values of trt_status_2022_v2
  cat("Frequency of values in 'trt_status_2022_v2':\n")
  print(table(df_check$co_endemicity))
  
  # check number of IUs at this stage:
  cat("Rows in 2022 after processing (creating biannual_vc_mapping variable):", nrow(subset(df, Year == 2022)), "\n")
  cat("Unique IU_ID_MAPPING count after processing (creating biannual_vc_mapping variable):", length(unique(df$IU_ID_MAPPING)), "\n")
  
  # Return the updated dataframe
  return(df)
}

# ============================================================================ #
#         Function to create extra intervention cols and                       #
#        checks of IUs where only MDA in 2013 or 2014 (incorrect)              #
# ============================================================================ #

create_extra_intervention_cols  <- function(df) {
  
  # Add adherence_par column
  df <- df %>%
    mutate(
      adherence_par = ifelse(MDA_CDTI == 1 | MDA_nonCDTI == 1, 0.3, NA_real_)
    )  # for Endgame
  
  # Create num_rnds and modelled_CUM_MDA cols
  df$number_rnds <- rowSums(df[c("MDA_nonCDTI", "MDA_CDTI", "MDA_CDTI_Biannual")], na.rm = TRUE)
  df$any_MDA <- ifelse(df$number_rnds > 0, 1, 0)
  
  # Group by IU_ID_MAPPING to calculate CUM_MDA_modelled
  df <- df %>%
    group_by(IU_ID_MAPPING) %>%
    mutate(CUM_MDA_modelled = cumsum(number_rnds)) %>%
    ungroup()  # remove grouping
  
  df$Cum_MDA_ESPEN <- df$Cum_MDA
  
  # Create IUID column
  df$IUID <- df$IU_ID_MAPPING
  df$IUID[nchar(df$IUID) == 4] <- paste(0, df$IUID[nchar(df$IUID) == 4], sep = "")
  df$IUID <- paste(df$ADMIN0ISO3, df$IUID, sep = "")
  
  # make control programme col
  df$Control_prog <- "OCP"
  
  # update col
  df$Cov_raw <- df$MDA_CDTI_raw
  
  # Check for any IUs where only MDA_CDTI == 1 in 2013 (erroneously coded)
  problem_IUS_2013 <- df %>%
    filter(MDA_CDTI == 1) %>%
    group_by(IU_ID_MAPPING) %>%
    filter(n_distinct(Year) == 1 & Year == 2013) %>%
    ungroup() %>%
    distinct(IU_ID_MAPPING)
  
  # Check for any IUs where only MDA_CDTI == 1 in 2014 (erroneously coded)
  problem_IUS_2014 <- df %>%
    filter(MDA_CDTI == 1) %>%
    group_by(IU_ID_MAPPING) %>%
    filter(n_distinct(Year) == 1 & Year == 2014) %>%
    ungroup() %>%
    distinct(IU_ID_MAPPING)
  
  # Output results of checks (no return value, just printed results)
  cat("Number of IUs with MDA_CDTI in 2013 (problematic): ", nrow(problem_IUS_2013), "\n")
  cat("Problematic IUs in 2013 (erroneously coded MDA_CDTI == 1):\n")
  print(problem_IUS_2013)
  
  cat("Number of IUs with MDA_CDTI in 2014 (problematic): ", nrow(problem_IUS_2014), "\n")
  cat("Problematic IUs in 2014 (erroneously coded MDA_CDTI == 1):\n")
  print(problem_IUS_2014)
  
  # check number of IUs at this stage:
  cat("Rows in 2022 after processing (creating extra intervention cols):", nrow(subset(df, Year == 2022)), "\n")
  cat("Unique IU_ID_MAPPING count after processing (creating extra intervention cols):", length(unique(df$IU_ID_MAPPING)), "\n")
  
  # Return the updated dataframe
  return(df)
}

# ==============================================================================#
#   Function to create biannual_vc_mapping (intervention distribution) variable #
# ==============================================================================#

create_biannual_vc_mapping <- function(df) {
  
  # Group by IU_ID_MAPPING and create biannual_included column
  df <- df %>%
    group_by(IU_ID_MAPPING) %>%
    mutate(biannual_included = any(MDA_CDTI_Biannual == 1))
  
  # Group by IU_ID_MAPPING and create VC_included column
  df <- df %>%
    group_by(IU_ID_MAPPING) %>%
    mutate(VC_included = any(vector_control == 1))
  
  # Group by IU_ID_MAPPING and create annual_only_included column
  df <- df %>%
    group_by(IU_ID_MAPPING) %>%
    mutate(annual_only_included = all(is.na(biannual_included)) & any(MDA_CDTI == 1))
  
  # Group by IU_ID_MAPPING and create biannual_VC_mapping column
  df <- df %>%
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
  
  df_check <- subset(df, Year == 2022)
  
  # Check unique values of trt_status_2022_v2
  cat("Frequency of values in 'trt_status_2022_v2':\n")
  print(table(df_check$biannual_VC_mapping))
  
  # check number of IUs at this stage:
  cat("Rows in 2022 after processing (creating biannual_vc_mapping variable):", nrow(subset(df, Year == 2022)), "\n")
  cat("Unique IU_ID_MAPPING count after processing (creating biannual_vc_mapping variable):", length(unique(df$IU_ID_MAPPING)), "\n")
  
  # Return the updated dataframe
  return(df)
}

# ============================================================================= #
#     Function to remove treatment naive IUs and checks                         #
# ============================================================================= #

remove_treatment_naive_IUs_and_check <- function(df) {
  # Identify treatment naive IUs
  condition <- df$trt_status_2022 == "Treatment naive"
  indices <- which(condition)
  
  # Mark IUs to exclude
  df$exclude <- NA
  df$exclude[indices] <- "exclude"
  
  # Check IUs labelled as "exclude"
  df_exclude_subset <- subset(df, exclude == "exclude")
  df_exclude_subset_lastyr <- subset(df_exclude_subset, Year == 2022)
  cat("Number of treatment naive IUs in 2022:", nrow(df_exclude_subset_lastyr), "\n")
  cat("Unique Endemicity values in treatment naive IUs:", unique(df_exclude_subset_lastyr$MAX_Endemicity), "\n")
  cat("Frequency of Endemicity in treatment naive IUs:\n")
  print(table(df_exclude_subset_lastyr$MAX_Endemicity))
  
  nIUs_pre <- length(unique(df$IU_ID_MAPPING)) # store
  
  # Filter out treatment naive IUs
  df <- subset(df, is.na(df$exclude))
  
  # Check the number of IUs after exclusion
  df_lastyr <- subset(df, Year == 2022)
  #cat("Number of IUs after exclusion in 2022:", nrow(df_lastyr), "\n")
  
  # Check for "NON-CONTROL" phase IUs
  df_lastyr_subset <- subset(df_lastyr, PHASE == "NON-CONTROL")
  cat("Number of NON-CONTROL phase IUs in 2022:", nrow(df_lastyr_subset), "\n")
  
  # Check for Endemic IUs in 2022
  df_lastyr_subset2 <- subset(df_lastyr, MAX_Endemicity %in% c("Endemic (under MDA)", "Endemic (MDA not delivered)", "Endemic (under post-intervention surveillance)"))
  cat("Number of endemic IUs in 2022:", nrow(df_lastyr_subset2), "\n")
  
  # check number of IUs at this stage:
  cat("Unique IU_ID_MAPPING count BFORE removing treatment-naive IUs:", nIUs_pre, "\n")
  cat("Unique IU_ID_MAPPING count After removing treatment-naive IUs:", length(unique(df$IU_ID_MAPPING)), "\n")
  cat("Unique IU_ID_MAPPING removed because treatment-naive IUs:", nIUs_pre - length(unique(df$IU_ID_MAPPING)), "\n")
  cat("Rows in 2022 after removing treatment-naive IUs (double-check):", nrow(subset(df, Year == 2022)), "\n")
  
  
  # Return the updated dataframe
  return(df)
}

# ========================================================================================== #
#                 Function to create final dataframes for OCP IUs                            #
# ========================================================================================== #

create_OCP_histories_dataframes <- function(df) {
  
  # Create Full_OCP_histories_df_popinfo dataframe
  Full_df_popinfo <- df[, c("IU_ID_MAPPING","IUs_NAME_MAPPING","IU_CODE_MAPPING", "IUID", "ADMIN0ISO3","Endemicity","MAX_Endemicity",
                            "PHASE", "SIZ_label", "endemicity_baseline","Year","PopTot","PopPreSAC","PopSAC","PopAdult","PopReq",
                            "PopTrg","PopTreat", "MDA_scheme","Cum_MDA_ESPEN","Cov","EpiCov",
                            "vector_control","biannual_VC_mapping","MDA_nonCDTI", "MDA_CDTI", "MDA_CDTI_Biannual","number_rnds",
                            "Cov.in2","Cov_raw","cov_source","cov_specific_source","CUM_MDA_modelled","trt_status_2022","trt_status_2022_v2","adherence_par","co_endemicity", "Control_prog")]
  
  # Create Full_OCP_histories_df_minimal dataframe
  Full_df_minimal <- df[, c("IU_ID_MAPPING","IUs_NAME_MAPPING","IU_CODE_MAPPING", "IUID", "ADMIN0ISO3","Endemicity",
                            "PHASE", "SIZ_label", "endemicity_baseline","Year","MDA_scheme","Cum_MDA_ESPEN","Cov","EpiCov",
                            "vector_control","biannual_VC_mapping","MDA_nonCDTI", "MDA_CDTI", "MDA_CDTI_Biannual","number_rnds",
                            "Cov.in2","Cov_raw","cov_source","cov_specific_source","CUM_MDA_modelled","trt_status_2022","trt_status_2022_v2","adherence_par","co_endemicity", "Control_prog")]
  
  # Create Full_OCP_histories_df_minimal_lastyr_2022 dataframe (subset for Year == 2022)
  Full_df_minimal_lastyr_2022 <- subset(Full_df_popinfo, Year == 2022)
  
  # Create Full_OCP_histories_df_minimal_lastyr_2025 dataframe (subset for Year == 2025)
  Full_df_minimal_lastyr_2025 <- subset(Full_df_popinfo, Year == 2025)
  
  # Return the dataframes as a list
  return(list(
    Full_df_popinfo,
    Full_df_minimal,
    Full_df_minimal_lastyr_2022,
    Full_df_minimal_lastyr_2025
  ))
}
