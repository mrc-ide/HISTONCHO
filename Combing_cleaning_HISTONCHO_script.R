# ====================================================================================================================== #
#                        CLEANING FINAL HISTONCHO DATABASE                                                               #
# ====================================================================================================================== #

# ================================================= #
# Define the base path for calling all data sources #

base_path <- "C:/Users/mad206/OneDrive - Imperial College London/Documents/HISTONCHO/input data" # set base path specific to your system

# ======================== #
#   Load in final database #
# ======================== #

# Full_histories_df_popinfo_290425 <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_popinfo_290425.csv")
# View(Full_histories_df_popinfo_290425)

#df <- Full_histories_df_popinfo_290425

# ================================ #
#  Combine APOC and OCP dataframes #
# ================================ #

ncol(Full_APOC_histories_df_popinfo)
ncol(Full_OCP_histories_df_popinfo)

# combine
df <- rbind(Full_APOC_histories_df_popinfo, Full_OCP_histories_df_popinfo)


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

# call function:
HISTONCHO_raw_cols_df <- update_trt_status(
  HISTONCHO_raw_cols_df, 
  "/Additional_IUs_MDA_to_trtnaive.csv", 
  "/Additional_IUs_MDA_to_trtnaive_OCP.csv"
)

# =============================================================================== #
#                   Update Variable values to match paper                         #
# =============================================================================== #

# call function:
HISTONCHO_final_cols_df <- update_histories_and_labels(HISTONCHO_raw_cols_df,
                                                       "/co_endemic_IUs_ALL.csv")

# ==== #
# save #

#saveRDS(HISTONCHO_final_cols_df, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Publications/Intervention histories/HISTONCHO_database_120525.RDS")
#write.csv(HISTONCHO_final_cols_df, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Publications/Intervention histories//HISTONCHO_database_120525.csv")


