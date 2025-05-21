# ================================================================================================================================== #
#                                           MASTER SCRIPT: COMPILING APOC INTERVENTION HISTORIES                                     #
# ================================================================================================================================== #

# ================================================= #
# Define the base path for calling all data sources #
base_path <- "C:/Users/mad206/OneDrive - Imperial College London/Documents/HISTONCHO/input data" # set base path specific to your system

# ========================================================================================================= #
#                  Load Cleaned IU Dataframe from Paul - based on preliminary ESPEN data from J Cano (2023) #
# ========================================================================================================= #

final_df_oncho_Parent_Child_file <- file.path(base_path, "/final_df_oncho_Parent_Child.csv")

final_df_oncho_Parent_Child <- read.csv(final_df_oncho_Parent_Child_file)

# ============================================================================ #
#            Update ESPEN 2022 Data for Specific Countries (MAY 2024)         #
# ============================================================================ #

# -------------------- #
#   ETHIOPIA (ETH)     #
# -------------------- #

ESPEN_ETH_file <- file.path(base_path, "/data-ETH-Oncho-iu-2022.csv")

final_df_oncho_Parent_Child <- update_espen_data(
  final_df_oncho_Parent_Child,
  "ETH",
  ESPEN_ETH_file
)

# -------------------- #
#    NIGERIA (NGA)     #
# -------------------- #

ESPEN_NGA_file <- file.path(base_path, "/data-NGA-Oncho-iu-2022.csv")

final_df_oncho_Parent_Child <- update_espen_data(
  final_df_oncho_Parent_Child,
  "NGA",
  ESPEN_NGA_file
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

# call function:
extracted_data <- extract_baseline_geostat_mfp_func(
  "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/IU pre-control fitted prevalence/summaries_apoc.Rdata",
  "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/IU pre-control fitted prevalence/summaries_liberia.Rdata",
  "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/IU pre-control fitted prevalence/summaries_ocp.Rdata"
)

dfAPOC <- extracted_data[[1]]
APOC_liberia_baseline <- extracted_data[[2]] # rename here

# =========================================================================== #
#      Determine whether IU classified as Endemic at any point                #
# =========================================================================== #

# call function :
result <- clean_EndemicIUs_alignment_func(dfAPOC)

dfAPOC_endemic <- result[[1]]
IU_endemic_codes <- result[[2]]

# ============================================================== #
#    Function to merge the baseline mfp and classify endemicity  #
# ============================================================== #

# call function
result <- merge_and_process_data(dfAPOC_endemic, APOC_liberia_baseline, endemicity = "endemic")

# The processed dataframe
dfAPOC_endemic <- result$df_merged

# ===================================================================================================#
#   Process IUs classified as Non-endemic IUs: classified as "unknown / not-reported " at any point  #
# ===================================================================================================#

# call function and extract outputs:
result <- process_unknown_nonendemic_IUs_func(dfAPOC, IU_endemic_codes)
dfAPOC_unknowns <- result$df_unknowns
dfAPOC_nonendemic <- result$df_nonendemic
IU_nonendemic_codes <- result$IU_nonendemic_codes


# ========================================= #
# merge baseline mfp with unknown dataframe #
# ========================================= #

# call function
result <- merge_and_process_data(dfAPOC_unknowns, APOC_liberia_baseline, endemicity = "unknown")

# The processed dataframe
dfAPOC_unknowns <- result$df_merged

# ================================================================================== #
#   Process Unknown IUs using baseline endemicity inputs converted from Zoure et al. #
# ================================================================================== #

# call function:
result <- process_baseline_conditions(dfAPOC_unknowns, endemicity = "unknown")
dfAPOC_unknowns_mesoendemic <- result$df_mesoendemic
dfAPOC_unknowns_hypoendemic_with_mda <- result$df_hypoendemic_with_mda
dfAPOC_unknowns_hypoendemic_no_mda <- result$df_hypoendemic_no_mda
#dfAPOC_unknowns_nonendemic <- result$df_NOT_hypo_meso_endemic
dfAPOC_unknowns_nonendemic <- result$df_nonendemic

# ============================================================== #
#      Combine daaframes on endemicity status and process        #
# ============================================================== #

# call function:
dfAPOC_complete <- combine_apoc_sub_data_with_checks(dfAPOC_endemic, dfAPOC_unknowns_mesoendemic,
                                                 dfAPOC_unknowns_hypoendemic_with_mda, dfAPOC_unknowns_hypoendemic_no_mda,
                                                 dfAPOC_unknowns_nonendemic, dfAPOC_nonendemic)

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

# =================================================================================== #
#     Isolate final endemic IUs in APOC (incl. excluding IUs in specific countries)   #
# =================================================================================== #

# call function:
result <- Exclude_select_endemic_IUs(dfAPOC_complete, ETH_exclude, ETH_exclude2, ETH_exclude3, ETH_include, SDN_include, TZN_include)

# Output the results
final_APOC_IUs_vec <- result$final_APOC_IUs_vec
df_APOC_endemic_minimal <- result$df_APOC_endemic_minimal

# ========================================================== #
#  Perform final endemic IUs section and do further checks   #
# ========================================================== #

# call function:
result <- filter_select_ALL_endemicIUs_and_check_func(dfAPOC_complete, final_APOC_IUs_vec)

# Output the results
dfAPOC_included <- result$dfAPOC_included


# ============================================ #
#   Create rows going back to 1975 for each IU #
# ============================================ #

# call function:
dfAPOC_included2 <- process_apoc_IUs_tobaseline_func(dfAPOC_included, final_APOC_IUs_vec)

# ==================================================================================== #
#            Defining inclusion of histories (ESPEN yrs 2013 - 2022)                   #
# ==================================================================================== #

# call function:
dfAPOC_included2 <- define_pre_post_espen_inclusion_func(dfAPOC_included2)

# ==================================================================================== #
#         Extract and process information on IUs with biannual MDA                     #
# ==================================================================================== #

# call function:
result <- get_biannual_ius("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Biannual in APOC/Biannual_APOC_IUs.csv")

# Extract the results
Biannual_IUs_vec <- result$Biannual_IUs_vec
Biannual_Sudan_IUs_vec <- result$Biannual_Sudan_IUs_vec


# ===================================================== #
#   Load in MDA start years based on APOC report (2015) #
# ===================================================== #

dfAPOC_report2 <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/APOC/APOC_data_97_13_v2.csv")
colnames(dfAPOC_report2) <- c("ADMIN0", "EpiCov", "Year")

split_vectors <- setNames(split(dfAPOC_report2$EpiCov, dfAPOC_report2$ADMIN0), unique(dfAPOC_report2$ADMIN0))

# ====================================================================================== #
#     Iterate over country_info list to update the dataframe with MDA data (APOC report) #
# ====================================================================================== #

# List of countries and their respective coverage data (country_code, coverage data, and start year for each country)
country_info <- list(
  list(country_code = "AGO", coverage_data = split_vectors$Angola, start_year = 7, condition_year_start = 2004, condition_year_end = 2013),
  list(country_code = "BDI", coverage_data = split_vectors$Burundi, start_year = 7, condition_year_start = 2004, condition_year_end = 2013),
  list(country_code = "CMR", coverage_data = split_vectors$Cameroon, start_year = 0, condition_year_start = 1998, condition_year_end = 2013),
  list(country_code = "CAF", coverage_data = split_vectors$`Central African Republic`, start_year = 0, condition_year_start = 1998, condition_year_end = 2013),
  list(country_code = "TCD", coverage_data = split_vectors$Chad, start_year = 0, condition_year_start = 1998, condition_year_end = 2013),
  list(country_code = "COG", coverage_data = split_vectors$Congo, start_year = 3, condition_year_start = 2000, condition_year_end = 2013),
  list(country_code = "COD", coverage_data = split_vectors$`Democratic Republic of the Congo`, start_year = 3, condition_year_start = 2000, condition_year_end = 2013),
  list(country_code = "GNQ", coverage_data = split_vectors$`Equatorial Guinea`, start_year = 0, condition_year_start = 1998, condition_year_end = 2013),
  list(country_code = "ETH", coverage_data = split_vectors$Ethiopia, start_year = 3, condition_year_start = 2000, condition_year_end = 2013),
  list(country_code = "GAB", coverage_data = split_vectors$Gabon, start_year = 0, condition_year_start = 1998, condition_year_end = 2013),
  list(country_code = "LBR", coverage_data = split_vectors$Liberia, start_year = 0, condition_year_start = 1998, condition_year_end = 2013),
  list(country_code = "MWI", coverage_data = split_vectors$Malawi, start_year = 0, condition_year_start = 1998, condition_year_end = 2013),
  list(country_code = "SSD", coverage_data = split_vectors$`South Sudan`, start_year = 0, condition_year_start = 1998, condition_year_end = 2013),
  list(country_code = "TZA", coverage_data = split_vectors$`Tanzania (Mainland)`, start_year = 0, condition_year_start = 1998, condition_year_end = 2013)
)


dfAPOC_included3 <- process_all_countries(dfAPOC_included2, split_vectors, country_info)

# ======================================================================================== #
#         Extract, process and integrate oncho snapshot data for Nigeria                   #
# ======================================================================================== #

dfAPOC_included3 <- process_nigeria_data(dfAPOC_included3, split_vectors, 
                                         "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/data from CC/oncho_snapshot_NGA.csv")


# ====================================================================================================== #
#      Extract, process and integrate MDA data for Uganda based on Katabarwa for Uganda                  #
# ====================================================================================================== #


# call function:
result <- process_uganda_data(dfAPOC_included3, 
                                        "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/UGA_Strt_Stp.csv", 
                                        split_vectors)

dfAPOC_included3 <- result[[1]]
coverages_UGA2 <- result[[2]] # need this for later

# ==================================================================================== #
#                   Integrate Sudan MDA using The Carter Center data                   #
# ==================================================================================== #

# call function
dfAPOC_included3 <- process_sudan_data(dfAPOC_included3, split_vectors)

# ================================================================================ #
#               Update MDA cov in ESPEN years (2013-2022)                          #
# ================================================================================ #

# call function
dfAPOC_included4 <- update_espen_mda_years(dfAPOC_included3)

# ================================================================================ #
#                        Update MDA in years 2013/14                               #
# ================================================================================ #

# call function:
dfAPOC_included4 <- update_mda_coverage_2013_2014(dfAPOC_included4)


# ================================================================================ #
#       Update IUs in Nigeria for 2022 with TCC & Sightsavers info                 #
# ================================================================================ #

# call function #
dfAPOC_included4 <- update_nigeria_2022_data(dfAPOC_included4, 
                                       "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/data from CC/NGA_strt_stp_LGA.csv", 
                                       "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/data from CC/LGAs_names_missing_tocheck.csv", 
                                       "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/data from CC/Sightsavers_2022_LGA_covs.csv",
                                       "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/ESPEN updates to check/ESPEN updated data for 2022/data-NGA-Oncho-iu-2022.csv")

# ============================================================================================== #
#          Create new coverage (Cov.in2) col and update CDTI cols & ETH (MDA in 2022)            #
# ============================================================================================== #

# call function
dfAPOC_included4 <- update_mda_coverage_cols_ETH_update(dfAPOC_included4, 
                                        "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/ESPEN updates to check/ESPEN updated data for 2022/data-ETH-Oncho-iu-2022.csv")


# ================================================================ #
#              Update biannual MDA in specific countries           #
# ================================================================ #

# ===================== #
# Uganda- call function #

dfAPOC_included4 <- update_biannual_uganda_mda(dfAPOC_included4, 
                                               "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Biannual in APOC/Biannual_APOC_IUs.csv")

# ======================= #
# Ethiopia- call function #

dfAPOC_included4 <- update_biannual_ethiopia_mda(dfAPOC_included4, 
                                                 "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Biannual in APOC/Biannual_APOC_IUs.csv")

# ======================= #
# Nigeria - call function #
dfAPOC_included4 <- update_biannual_nigeria_mda(dfAPOC_included4, 
                                          "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/data from CC/NGA_post2017.csv")
# ======================= #
# Sudan - call function   #
dfAPOC_included4 <- update_biannual_mda_sudan(dfAPOC_included4, 
                                              "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Biannual in APOC/Biannual_APOC_IUs.csv")
# ========================== #
# Tanzania - call function   #
dfAPOC_included4 <- update_biannual_mda_tanzania(dfAPOC_included4, 
                                                 "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Biannual in APOC/Biannual_APOC_IUs.csv")

# ========================== #
# S Sudan - call function    #
dfAPOC_included4 <- update_biannual_mda_southsudan(dfAPOC_included4, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Biannual in APOC/Biannual_APOC_IUs.csv")


# ================================================================ #
#         Update vector control in specific countries              #
# ================================================================ #

# ===================== #
# Uganda- call function #

dfAPOC_included4 <- update_vector_control_UGA(dfAPOC_included4, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Biannual in APOC/Biannual_APOC_IUs.csv")

# ========================== #
# Eq. Guinea - call function #

dfAPOC_included4 <- update_vector_control_mda_GNQ(dfAPOC_included4, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Biannual in APOC/Biannual_APOC_IUs.csv")

# ===================================================================== #
#    Create intervention label column (biannual_VC_mapping)             #
# ===================================================================== #

dfAPOC_included4 <- create_intervention_mapping_variable(dfAPOC_included4)

# ===================================================================== #
#            Remove IUs without baseline mapped samples                 #
# ===================================================================== #

# call function
dfAPOC_included4 <- remove_non_endemic_IUs(dfAPOC_included4, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/no_map_samples.Rdata")

# ======================================================================= #
#            Augment (extend) each Iu from 2022 - 2025                    #
# ======================================================================= #

# call function:
dfAPOC_included5 <- augment_mda_to2025_func(dfAPOC_included4, FinalESPENyr = 2022, IUend = 2025)


# ============================================= #
#          Update single IU (IU 49752)          # 
# ============================================= #

# call function
dfAPOC_included6 <- update_iu_mda_data(dfAPOC_included5, IU_ID = 49752, coverage_input = coverages_UGA2)


# ============================================================================= #
#     Create trt_status_2022 variable and print frequency of categories         #
# ============================================================================= #

# call function 
dfAPOC_included6 <- create_trt_status_col(dfAPOC_included6)

# ==================================================================== #
#               Update MDA in 2023 - 2025 period                       #
# ==================================================================== #

# call function:
dfAPOC_included7 <- update_mda_status_to2025(dfAPOC_included6)

# ========================================================================== #
#           Update biannual MDA and vector control in 2023 - 2025            #
# ========================================================================== #

# Call function:
dfAPOC_included7 <- update_biannual_and_vector_control_to2025(dfAPOC_included7)

# ============================================================== #
#       Remove a) non-endemic IUs in Uganda and                  #
#       b) treatment naive IUs which are not-reported/ unknown   #
#       except in SDN Blue Nile Admin1                           #
# ============================================================== #

# call function:
dfAPOC_included7 <- remove_non_endemic_and_trtnaive_IUs(dfAPOC_included7)

# ============================================================== #
#                 Make final columns & tidying                   #
# ============================================================== #

# Call function :
dfAPOC_included7 <- create_final_columns(dfAPOC_included7)


# ============================================================================== #
#                   Create a co-endemic column for oncho-LF-loa                  #
# ============================================================================== #

# Call function:
dfAPOC_included7 <- create_co_endemicity_column(dfAPOC_included7, 
                                                "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Co-endemicity/co_endemic_IUs.csv")

# ====================================================================== #
#              Make final, minimal & single year dataframes              #
# ====================================================================== #

# call function:
result <- create_apoc_histories_dataframes(dfAPOC_included7)
Full_APOC_histories_df_popinfo <- result[[1]]
Full_APOC_histories_df_minimal <- result[[2]]
Full_APOC_histories_df_minimal_lastyr_2022 <- result[[3]]
Full_APOC_histories_df_minimal_lastyr_2025 <- result[[4]]


