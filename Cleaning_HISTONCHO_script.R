# ====================================================================================================================== #
#                        CLEANING FINAL HISTONCHO DATABASE                                                               #
# ====================================================================================================================== #

library(dplyr)

# May 2025 #

# ======================== #
#   Load in final database #
# ======================== #

Full_histories_df_popinfo_290425 <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_popinfo_290425.csv")
# View(Full_histories_df_popinfo_290425)

df <- Full_histories_df_popinfo_290425

# ======================== #
#   Select columns         #
# ======================== #

HISTONCHO_raw_cols_df <- df[c("IU_ID_MAPPING", "IUs_NAME_MAPPING",
                                       "IU_CODE_MAPPING","IUID", "ADMIN0ISO3",
                                       "Endemicity", "MAX_Endemicity",
                                       "Control_prog", "PHASE", "SIZ_label",
                                       "endemicity_baseline", "Year",
                                       "PopTot", "PopPreSAC", "PopSAC", "PopAdult",
                                       "PopReq", "PopTrg", "PopTreat", "MDA_scheme",
                                       "Cum_MDA_ESPEN", "Cov", "EpiCov",
                                       "vector_control", "MDA_nonCDTI", "MDA_CDTI",
                                       "MDA_CDTI_Biannual", "number_rnds", "Cov_raw",
                                       "cov_source","Cov.in2","biannual_VC_mapping",
                                       "CUM_MDA_modelled", "co_endemicity", 
                                       "trt_status_2022")]  # Replace with your specific column names

ncol(HISTONCHO_raw_cols_df)

# =================================================================================== #
# First update the trt_status_2022 col based on those with some IVM requiring mapping #
# =================================================================================== #

# APOC IUS:
Additional_IUs_MDA_to_trtnaive <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Planning final Endgame sims (March-May 25)/Additional_IUs_MDA_to_trtnaive.csv")

IUs_MDA_to_trtnaive <- Additional_IUs_MDA_to_trtnaive$IU_ID

HISTONCHO_raw_cols_df$trt_status_2022 <- ifelse(HISTONCHO_raw_cols_df$IU_ID %in% IUs_MDA_to_trtnaive, "some previous IVM MDA; 
potentially requiring OEM and/or SM", HISTONCHO_raw_cols_df$trt_status_2022)

unique(HISTONCHO_raw_cols_df$trt_status_2022)

# OCP IUs:
Additional_IUs_MDA_to_trtnaive_OCP <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Planning final Endgame sims (March-May 25)/Additional_IUs_MDA_to_trtnaive_OCP.csv")

IUs_MDA_to_trtnaive_OCP <- Additional_IUs_MDA_to_trtnaive_OCP$IU_ID

HISTONCHO_raw_cols_df$trt_status_2022 <- ifelse(HISTONCHO_raw_cols_df$IU_ID %in% IUs_MDA_to_trtnaive_OCP, "some previous IVM MDA; 
potentially requiring OEM and/or SM*", HISTONCHO_raw_cols_df$trt_status_2022)

unique(HISTONCHO_raw_cols_df$trt_status_2022)

# treatment naive IUs re-labeling:

HISTONCHO_raw_cols_df <- HISTONCHO_raw_cols_df %>%
  mutate(trt_status_2022 = ifelse(trt_status_2022 == "Treatment naive", "Treatment-naïve;
potentially requiring OEM and/or SM", trt_status_2022))

unique(HISTONCHO_raw_cols_df$trt_status_2022)

# can now drop IUID col # 
HISTONCHO_raw_cols_df <- HISTONCHO_raw_cols_df %>%
  select(-IUID)

#  ================================================================================= #
#                   Rename col names to macth paper                                  #
# =============================================================================== #

names(HISTONCHO_raw_cols_df)

# Rename columns
HISTONCHO_raw_cols_df <- HISTONCHO_raw_cols_df %>%
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
names(HISTONCHO_raw_cols_df)


# =============================================================================== #
#                   Update Variable values to match paper                         #
# =============================================================================== #

# ===================== #
# 1) OCP Phase labeling #

unique(HISTONCHO_raw_cols_df$OCP_PHASE)

# Define a named vector for phase replacements
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

# Apply the recode using dplyr::mutate and dplyr::recode
HISTONCHO_raw_cols_df <- HISTONCHO_raw_cols_df %>%
  mutate(OCP_PHASE = recode(OCP_PHASE, !!!phase_replacements))

unique(HISTONCHO_raw_cols_df$OCP_PHASE)

# ======================= #
# 2) Endemicity_baseline  #

unique(HISTONCHO_raw_cols_df$Endemicity_baseline)

endemicity_replacements <- c(
  "hypoendemic" = "Hypoendemic",
  "mesoendemic" = "Mesoendemic",
  "hyperendemic" = "Hyperendemic"
)

# Apply the recode using dplyr::mutate and dplyr::recode
HISTONCHO_raw_cols_df <- HISTONCHO_raw_cols_df %>%
  mutate(Endemicity_baseline = recode(Endemicity_baseline, !!!endemicity_replacements))

unique(HISTONCHO_raw_cols_df$Endemicity_baseline)

# ======================= #
# 3) vector control       #

unique(HISTONCHO_raw_cols_df$Vector_control)

HISTONCHO_raw_cols_df <- HISTONCHO_raw_cols_df %>%
  mutate(Vector_control = ifelse(is.na(Vector_control), 0, Vector_control))

# ======================= #
# 4) MDA cols            #

# Non_CDTI_MDA
unique(HISTONCHO_raw_cols_df$Non_CDTI_MDA)

HISTONCHO_raw_cols_df <- HISTONCHO_raw_cols_df %>%
  mutate(Non_CDTI_MDA = ifelse(is.na(Non_CDTI_MDA), 0, Non_CDTI_MDA))

# CDTI_MDA
unique(HISTONCHO_raw_cols_df$CDTI_MDA)

HISTONCHO_raw_cols_df <- HISTONCHO_raw_cols_df %>%
  mutate(CDTI_MDA = ifelse(is.na(CDTI_MDA), 0, CDTI_MDA))

# Biannaul_CDTI_MDA
unique(HISTONCHO_raw_cols_df$Biannaul_CDTI_MDA)

HISTONCHO_raw_cols_df <- HISTONCHO_raw_cols_df %>%
  mutate(CDTI_MDA = ifelse(is.na(Biannaul_CDTI_MDA), 0, Biannaul_CDTI_MDA))

# MDA_VC_Mapping
MDA_VC_Mapping_replacements <- c(
  "annual only" = "Annual MDA",
  "annual & vector control" = "Annual MDA and vector control",
  "biannual" = "Biannual MDA",
  "biannual & vector control" = "Biannual MDA and vector control",
  "treatment naive" = "Treatment-naïve"
)

HISTONCHO_raw_cols_df <- HISTONCHO_raw_cols_df %>%
  mutate(MDA_VC_Mapping = recode(MDA_VC_Mapping, !!!MDA_VC_Mapping_replacements))

unique(HISTONCHO_raw_cols_df$MDA_VC_Mapping)

# ============== #
# Co_endemicity  #

# Define a named vector for co_endemicity replacements
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

# Apply the recode using dplyr::mutate and dplyr::recode
HISTONCHO_raw_cols_df <- HISTONCHO_raw_cols_df %>%
  mutate(Co_endemicity = recode(Co_endemicity, !!!co_endemicity_replacements))

unique(HISTONCHO_raw_cols_df$Co_endemicity)

# ===================================== #
# Trt_status_2022 already recoded above #

unique(HISTONCHO_raw_cols_df$Trt_Status_2022)

HISTONCHO_updated_cols_df <- HISTONCHO_raw_cols_df

# ==== #
# save #
saveRDS(HISTONCHO_updated_cols_df, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Publications/Intervention histories/HISTONCHO_database_120525.RDS")
write.csv(HISTONCHO_updated_cols_df, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Publications/Intervention histories//HISTONCHO_database_120525.csv")
