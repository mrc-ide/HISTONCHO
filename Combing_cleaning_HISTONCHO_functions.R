# ====================================================================================================================== #
#                        CLEANING FINAL HISTONCHO DATABASE & PLOTTING FOR PAPER FUNCTION                                 #
# ====================================================================================================================== #

# ============================================================================================== #
# FUNCTION : First update the trt_status_2022 col based on those with some IVM requiring mapping #
# ============================================================================================== #

update_trt_status <- function(df, 
                              additional_IUs_MDA_to_trtnaive_input, 
                              additional_IUs_MDA_to_trtnaive_OCP_input) {
  
  # Load the additional IUs from CSV files
  additional_IUs_MDA_to_trtnaive_path <- file.path(base_path, additional_IUs_MDA_to_trtnaive_input)
  Additional_IUs_MDA_to_trtnaive <- read.csv(additional_IUs_MDA_to_trtnaive_path)
  
  additional_IUs_MDA_to_trtnaive_OCP_path <- file.path(base_path, additional_IUs_MDA_to_trtnaive_OCP_input)
  Additional_IUs_MDA_to_trtnaive_OCP <- read.csv(additional_IUs_MDA_to_trtnaive_OCP_path)
  
  # Extract the IU_IDs for the relevant groups
  IUs_MDA_to_trtnaive <- Additional_IUs_MDA_to_trtnaive$IU_ID
  IUs_MDA_to_trtnaive_OCP <- Additional_IUs_MDA_to_trtnaive_OCP$IU_ID
  
  # Update the trt_status_2022 for APOC IUs
  df$trt_status_2022 <- ifelse(df$IU_ID_MAPPING %in% IUs_MDA_to_trtnaive, 
                               "some previous IVM MDA; potentially requiring OEM and/or SM", 
                               df$trt_status_2022)
  
  # Update the trt_status_2022 for OCP IUs
  df$trt_status_2022 <- ifelse(df$IU_ID_MAPPING %in% IUs_MDA_to_trtnaive_OCP, 
                               "some previous IVM MDA; potentially requiring OEM and/or SM*", 
                               df$trt_status_2022)
  
  # Update the trt_status_2022 for treatment naive IUs
  df <- df %>%
    mutate(trt_status_2022 = ifelse(trt_status_2022 == "Treatment naive", 
                                    "Treatment-naïve; potentially requiring OEM and/or SM", 
                                    trt_status_2022))
  
  # Drop the IUID column if needed
  df <- df %>%
    select(-IUID)
  
  # print categories in new variable
  df_subset <- subset(df, Year == 2022)
  cat("Categories in updated trt_status_2022 variable:")
  print(table(df_subset$trt_status_2022))
  cat("Number of IUs in combined OCP and APOC dataframe:", length(unique(df_subset$IU_ID_MAPPING)), "\n")
  
  # Return the updated dataframe
  return(df)
}


# =============================================================================== #
#            FUNCTION to update Variable values to match paper                    #
# =============================================================================== #

update_histories_and_labels <- function(df, co_endemic_IUs_input) {
  
  # ============================== #
  # Rename columns to match paper  #
  # ============================== #
  df <- df %>%
    rename(
      OCP_PHASE = PHASE,
      Endemicity_baseline = endemicity_baseline,
      Cum_MDA = Cum_MDA_ESPEN,
      Vector_control = vector_control,
      Non_CDTI_MDA = MDA_nonCDTI,
      CDTI_MDA = MDA_CDTI,
      Biannual_CDTI_MDA = MDA_CDTI_Biannual,
      Number_Rounds = number_rnds,
      Cov_Raw = Cov_raw,
      Cov_Source = cov_source,
      Cov_Cat = Cov.in2,
      MDA_VC_Mapping = biannual_VC_mapping,
      Cum_MDA_Rev = CUM_MDA_modelled,
      Co_endemicity = co_endemicity,
      Trt_Status_2022 = trt_status_2022
    )
  
  # ============================== #
  # Update OCP Phase labeling      #
  # ============================== #
  phase_replacements <- c(
    "PHASE 1 - FEB 1975" = "Former OCP Phase I - 1975",
    "PHASE II - JAN 1976" = "Former OCP Phase II - 1976",
    "PHASE III EAST - JUL 1977" = "Former OCP Phase III East - 1977",
    "PHASE III WEST - MAR 1977" = "Former OCP Phase III West - 1977",
    "PHASE IV - MAR 1979" = "Former OCP Phase IV - 1979",
    "SOUTHERN EXTENSION - FEB 1988" = "Former OCP Southern Extension - 1988",
    "SOUTHERN FOREST EXTENSION - JAN 1990" = "Former OCP Southern Forest Extension- 1990",
    "WESTERN EXTENSION - MAR 1989" = "Former OCP Western Extension - 1989",
    "WESTERN EXTENSION - 1990" = "Former OCP Western Extension - 1990",
    "WESTERN EXTENSION - MAR 1990" = "Former OCP Western Extension - 1990 (March)",
    "NON-CONTROL" = "Former OCP: Non-Control",
    "APOC" = "Former APOC"
  )
  
  df <- df %>%
    mutate(OCP_PHASE = recode(OCP_PHASE, !!!phase_replacements))
  
  # print check
  df_subset <- subset(df, Year == 2022)
  cat("CHECK 1: categories in updated OCP_PHASE variable:")
  print(table(df_subset$OCP_PHASE))
  
  # ============================== #
  # Update Endemicity_baseline     #
  # ============================== #
  endemicity_replacements <- c(
    "hypoendemic" = "Hypoendemic",
    "mesoendemic" = "Mesoendemic",
    "hyperendemic" = "Hyperendemic"
  )
  
  df <- df %>%
    mutate(Endemicity_baseline = recode(Endemicity_baseline, !!!endemicity_replacements))
  
  # print check
  df_subset <- subset(df, Year == 2022)
  cat("CHECK 2: categories in updated Endemicity_baseline variable:")
  print(table(df_subset$Endemicity_baseline))
  
  # ============================== #
  # Update Vector_control          #
  # ============================== #
  df <- df %>%
    mutate(Vector_control = ifelse(is.na(Vector_control), 0, Vector_control))
  
  # print check
  df_subset <- subset(df, Year == 2022)
  cat("CHECK 3: categories in updated Vector_control variable:")
  print(table(df_subset$Vector_control))
  
  # ============================== #
  # Update MDA columns             #
  # ============================== #
  
  # Non_CDTI_MDA
  df <- df %>%
    mutate(Non_CDTI_MDA = ifelse(is.na(Non_CDTI_MDA), 0, Non_CDTI_MDA))
  
  # print check
  df_subset <- subset(df, Year == 2022)
  cat("CHECK 4: categories in updated Non_CDTI_MDA variable:")
  print(table(df_subset$Non_CDTI_MDA))
  
  # CDTI_MDA
  df <- df %>%
    mutate(CDTI_MDA = ifelse(is.na(CDTI_MDA), 0, CDTI_MDA))
  
  # print check
  df_subset <- subset(df, Year == 2022)
  cat("CHECK 5: categories in updated CDTI_MDA variable:")
  print(table(df_subset$CDTI_MDA))
  
  # Biannual_CDTI_MDA
  df <- df %>%
    mutate(Biannual_CDTI_MDA = ifelse(is.na(Biannual_CDTI_MDA), 0, Biannual_CDTI_MDA))
  
  # print check
  df_subset <- subset(df, Year == 2022)
  cat("CHECK 6: categories in updated Biannual_CDTI_MDA variable:")
  print(table(df_subset$Biannual_CDTI_MDA))
  
  # MDA_VC_Mapping
  MDA_VC_Mapping_replacements <- c(
    "annual only" = "Annual MDA",
    "annual & vector control" = "Annual MDA and vector control",
    "biannual" = "Biannual MDA",
    "biannual & vector control" = "Biannual MDA and vector control",
    "treatment naive" = "Treatment-naïve"
  )
  
  df <- df %>%
    mutate(MDA_VC_Mapping = recode(MDA_VC_Mapping, !!!MDA_VC_Mapping_replacements))
  
  # print check
  df_subset <- subset(df, Year == 2022)
  cat("CHECK 7: categories in updated MDA_VC_Mapping variable:")
  print(table(df_subset$MDA_VC_Mapping))
  
  # ==================================================== #
  # Update Co_endemicity with endemicity level for loa   #
  # ==================================================== #
  
  co_endemic_IUs_path <- file.path(base_path, co_endemic_IUs_input)
  co_endemic_IUs <- read.csv(co_endemic_IUs_path)
  
  #unique(dfOCP_included7$ADMIN0ISO3)
  
  co_endemic_IUs_oncho_LF_loahyper <- subset(co_endemic_IUs, co_endemicity == "oncho,LF,loa hyper")
  co_endemic_IUs_oncho_LF_loahyper_vec <- unique(co_endemic_IUs_oncho_LF_loahyper$IU_ID_MAPPING)
  
  co_endemic_IUs_oncho_LF_loameso <- subset(co_endemic_IUs, co_endemicity == "oncho,LF,loa meso")
  co_endemic_IUs_oncho_LF_loameso_vec <- unique(co_endemic_IUs_oncho_LF_loameso$IU_ID_MAPPING)
  
  co_endemic_IUs_oncho_LF_loahypo <- subset(co_endemic_IUs, co_endemicity == "oncho,LF,loa hypo")
  co_endemic_IUs_oncho_LF_loahypo_vec <- unique(co_endemic_IUs_oncho_LF_loahypo$IU_ID_MAPPING)
  
  co_endemic_IUs_oncho_LF <- subset(co_endemic_IUs, co_endemicity == "oncho,LF")
  co_endemic_IUs_oncho_LF_vec <- unique(co_endemic_IUs_oncho_LF$IU_ID_MAPPING)
  
  co_endemic_IUs_oncho_loahyper <- subset(co_endemic_IUs, co_endemicity == "oncho,loa hyper")
  co_endemic_IUs_oncho_loahyper_vec <- unique(co_endemic_IUs_oncho_loahyper$IU_ID_MAPPING)
  
  co_endemic_IUs_oncho_loameso <- subset(co_endemic_IUs, co_endemicity == "oncho,loa meso")
  co_endemic_IUs_oncho_loameso_vec <- unique(co_endemic_IUs_oncho_loameso$IU_ID_MAPPING)
  
  co_endemic_IUs_oncho_loahypo <- subset(co_endemic_IUs, co_endemicity == "oncho,loa hypo")
  co_endemic_IUs_oncho_loahypo_vec <- unique(co_endemic_IUs_oncho_loahypo$IU_ID_MAPPING)
  
  co_endemic_IUs_oncho <- subset(co_endemic_IUs, co_endemicity == "oncho")
  co_endemic_IUs_oncho_vec <- unique(co_endemic_IUs_oncho$IU_ID_MAPPING)
  
  df$Co_endemicity<- ifelse(df$IU_ID_MAPPING %in% co_endemic_IUs_oncho_LF_loahyper_vec, "oncho,LF,loa hyper",
                                                    ifelse(df$IU_ID_MAPPING %in% co_endemic_IUs_oncho_LF_loameso_vec, "oncho,LF,loa meso",
                                                           ifelse(df$IU_ID_MAPPING %in% co_endemic_IUs_oncho_LF_loahypo_vec, "oncho,LF,loa hypo",
                                                                  ifelse(df$IU_ID_MAPPING %in% co_endemic_IUs_oncho_LF_vec, "oncho,LF",
                                                                         ifelse(df$IU_ID_MAPPING %in% co_endemic_IUs_oncho_loahyper_vec, "oncho,loa hyper",
                                                                                ifelse(df$IU_ID_MAPPING %in% co_endemic_IUs_oncho_loameso_vec, "oncho,loa meso",
                                                                                       ifelse(df$IU_ID_MAPPING %in% co_endemic_IUs_oncho_loahypo_vec, "oncho,loa hypo",
                                                                                              ifelse(df$IU_ID_MAPPING %in% co_endemic_IUs_oncho_vec, "oncho", "assumed oncho only"))))))))
  
  co_endemicity_replacements <- c(
    "oncho" = "Only Oncho",
    "assumed oncho only" = "Only Oncho",
    "oncho,LF" = "Oncho and LF",
    "oncho,loa hypo" = "Oncho and Loa hypo",
    "oncho,loa meso" = "Oncho and Loa meso",
    "oncho,loa hyper" = "Oncho and Loa hyper",
    "oncho,LF,loa hypo" = "Oncho and LF and Loa hypo",
    "oncho,LF,loa meso" = "Oncho and LF and Loa meso",
    "oncho,LF,loa hyper" = "Oncho and LF and Loa hyper"
  )
  
  df <- df %>%
    mutate(Co_endemicity = recode(Co_endemicity, !!!co_endemicity_replacements))
  
  # Define the desired order
  desired_order <- c(
    "Only Oncho","Oncho and Loa hypo","Oncho and Loa meso","Oncho and Loa hyper",
    "Oncho and LF","Oncho and LF and Loa hypo","Oncho and LF and Loa meso","Oncho and LF and Loa hyper")
  
  # Reorder the co_endemicity column in your dataframe
  df$Co_endemicity <- factor(
    df$Co_endemicity, 
    levels = desired_order
  )
  
  
  # print check
  df_subset <- subset(df, Year == 2022)
  cat("CHECK 8: categories in updated Co_endemicity variable:")
  print(table(df_subset$Co_endemicity))
  
  # Return the updated dataframe
  return(df)
}

