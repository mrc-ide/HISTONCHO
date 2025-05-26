# ================================================================================================================================== #
#                                           New OCP histories (NOV 2023)                                                            #
# ================================================================================================================================== #

# ================================================= #
# Define the base path for calling all data sources #
base_path <- "C:/Users/mad206/OneDrive - Imperial College London/Documents/HISTONCHO/input data" # set base path specific to your system


# =============================================================================================== #
#       Load and filter IUs in OCP (based on being endemic in ESPEN or in shapefile)              #
# =============================================================================================== #

# call function :
dfOCP_included1 <- load_and_filter_ocp_data("/OCP_DF_ALL.RData")


# ========================================================================== #
#           Perform further checks on IUs in OCP dataframe                   #
# ========================================================================== #

# Assuming dfOCP_included1 is already loaded in the environment
dfOCP_included1 <- further_IU_checks_update_df_OCP(dfOCP_included1)

# ================================================================================ #
#             Fill years back to 1975 for each Iu & perform checks                 #
# ================================================================================ #

# call function (note this will take some time)
dfOCP_included2 <- backfill_histories(dfOCP_included1)

# =================================================================================================== #
#      Load, process and integrate special intervention zone labels to IUs where SIZ occurred         #
# =================================================================================================== #

# call function
dfOCP_included2 <- add_SIZ_labels(dfOCP_included2, "/SIZs_IUs.xlsx")

# =============================================================================== #
#       Check how many are not endemic in ESPEN but included in OCP shape?        #
# =============================================================================== #

# call function:
perform_endemicity_checks(dfOCP_included2)


# ================================================================================================= #
#   load, process and integrate baseline endemicity, then remove IUs without baseline information   #
# ================================================================================================= #

# Call function:
dfOCP_included2 <- integrate_baseline_endemicity(dfOCP_included2, "/summaries_ocp.Rdata")

# -======================================================================================================== #
#     produce columns to define whether an IU is inclued/excluded for Pre-ESPEN and ESPEN histories         #
# ========================================================================================================= #

# call function:
dfOCP_included3 <- add_pre_post_ESPEN_history(dfOCP_included2)

# ========================================================================================= #
#                Apply vector control and MDA interventions up to 2002                      #
# ========================================================================================= #

# call function:
dfOCP_included4 <- apply_OCP_interventions_up_to_2002(dfOCP_included3)

# ============================================================================= #
#               Apply vector control and MDA interventions                      #
#        up 2003-2012 (including SIZ vector control) & biannual to 2022         #
# ============================================================================= #

# call function :
dfOCP_included4 <- apply_OCP_interventions_SIZ_2003_2012_biannual(dfOCP_included4)

# ============================================================================= #
#            Update MDA and fill in MDA during ESPEN years                      #
# ============================================================================= #

# Call function:
dfOCP_included4 <- update_MDA_including_ESPENyrs(dfOCP_included4)

# ============================================================================================ #
#                           Extend years from 2023-2025                            #
# ============================================================================================ #

# call function (this will take some time):
dfOCP_included5 <- extend_IU_years_2023_2025(dfOCP_included4, FinalESPENyr = 2022)

# ============================================================================================ #
#          Create new intervention cols (including source info) & modify existing              #
# ============================================================================================ #

# call function:
dfOCP_included5 <- create_edit_intervention_cols(dfOCP_included5)

# ================================================================================================== #
#              Create trt_status_2022 and trt_status_2022_v2 columns/variables                       #
# ================================================================================================== #

# call function:
dfOCP_included6 <- create_trt_status_variable(dfOCP_included5)

# ======================================================================== #
#              Update MDA in 2023-2025 including biannual                  #
# ======================================================================== #

# call function:
dfOCP_included7 <- apply_MDA_interventions_2023_2025(dfOCP_included6)

# ============================================================================ #
#                Create co-endemicity column (oncho-LF-loa)                    #
# ============================================================================ #

# call function:
dfOCP_included7 <- create_co_endemicity_column_OCP(dfOCP_included7, "/co_endemic_IUs_OCP.csv")

# ============================================================================ #
#                Create extra intervention cols and                            #
#        checks of IUs where only MDA in 2013 or 2014 (incorrect)              #
# ============================================================================ #

# call function:
dfOCP_included7 <- create_extra_intervention_cols(dfOCP_included7)

# ==============================================================================#
#       Create biannual_vc_mapping (intervention distribution) variable         #
# ==============================================================================#

# call function:
dfOCP_included7 <- create_biannual_vc_mapping(dfOCP_included7)

# ============================================================================= #
#              Remove treatment naive IUs and checks                            #
# ============================================================================= #

# call function:
dfOCP_included7 <- remove_treatment_naive_IUs_and_check(dfOCP_included7)

# ========================================================================================== #
#                        Create final dataframes for OCP IUs                                 #
# ========================================================================================== #

# call function:
OCP_histories_data <- create_OCP_histories_dataframes(dfOCP_included7)

# Access individual dataframes:
Full_OCP_histories_df_popinfo <- OCP_histories_data[[1]]
Full_OCP_histories_df_minimal <- OCP_histories_data[[2]]
Full_OCP_histories_df_minimal_lastyr_2022 <- OCP_histories_data[[3]]
Full_OCP_histories_df_minimal_lastyr_2025 <- OCP_histories_data[[4]]