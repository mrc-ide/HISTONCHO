# ========================================================================
#          Combine with APOC dataframe                                    #

# load in OCP if needed:
# Full_OCP_histories_df_popinfo_150324_v2 <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_OCP_histories_df_popinfo_150324_v2.csv")
# Full_APOC_histories_df_popinfo_200324_v2 <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_popinfo_200324_v2.csv")

# Full_APOC_histories_df_popinfo_231123 <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_popinfo_231123.csv")
# Full_APOC_histories_df_popinfo_200324 <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_minimal_200324.csv")
# Full_APOC_histories_df_popinfo_200324 <- Full_APOC_histories_df_popinfo_200324[, -1]

# upload the updated May 2024 APOC data (including ESPEN updates)
#Full_APOC_histories_df_popinfo_170524 <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_popinfo_170524.csv")
#Full_APOC_histories_df_popinfo_170524 <- Full_APOC_histories_df_popinfo_170524[, -1]

# upload the updated May 2024 APOC data ((including ESPEN updates)) for business case !
# Full_APOC_histories_df_popinfo_230524 <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_minimal_BC_230524.csv")
# Full_APOC_histories_df_popinfo_230524 <- Full_APOC_histories_df_popinfo_170524[, -1]

# # upload the updated JULY 2024 APOC data ((including ESPEN updates)) with new columsn for reporting !
# Full_APOC_histories_df_popinfo_300724 <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_popinfo_300724.csv")
# Full_APOC_histories_df_popinfo_300724 <- Full_APOC_histories_df_popinfo_300724[, -1]

# # upload the updated NOV 2024 APOC data ((including SDN)) with new columsn for reporting !
# Full_APOC_histories_df_popinfo_111124 <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_popinfo_111124.csv")
# #Full_APOC_histories_df_popinfo_111124 <- Full_APOC_histories_df_popinfo_111124[, -1]

# # upload the updated FEB 2025 APOC data ((including SDN)) with new columsn for reporting !
# Full_APOC_histories_df_popinfo_070225 <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_popinfo_070225.csv")
# #Full_APOC_histories_df_popinfo_070225 <- Full_APOC_histories_df_popinfo_070225[, -1]

# # upload the updated MARCH-APRIL 2025 APOC data ((including SDN)) with new columsn for reporting !
# Full_APOC_histories_df_popinfo_260325 <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_popinfo_260325.csv") # actually updated on 030425
# #Full_APOC_histories_df_popinfo_260325 <- Full_APOC_histories_df_popinfo_260325[, -1]

# April 2025 APOC updates
#Full_APOC_histories_df_popinfo_070425 <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_popinfo_070425.csv") # actually updated on 030425
Full_APOC_histories_df_popinfo_290425 <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_popinfo_290425.csv") # actually updated on 030425
#Full_APOC_histories_df_popinfo_260325 <- Full_APOC_histories_df_popinfo_290425[, -1]
length(unique(Full_APOC_histories_df_popinfo_290425$IU_ID_MAPPING)) # 1526


#Full_OCP_histories_df_popinfo_260325 <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_OCP_histories_df_popinfo_260225.csv")
Full_OCP_histories_df_popinfo_290425 <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_OCP_histories_df_popinfo_290425.csv")
length(unique(Full_OCP_histories_df_popinfo_290425$IU_ID_MAPPING)) # 613

# 1526 + 613 = 2139

#Full_OCP_histories_df_popinfo_260325 <- Full_OCP_histories_df_popinfo_260325[, -1]

#Full_OCP_histories_df_popinfo <- Full_OCP_histories_df_popinfo_150324_v2
#Full_OCP_histories_df_popinfo$new_MDA_IUs_2022 <- "no update" # need to add this now to OCP dataframe
Full_OCP_histories_df_popinfo_290425$new_MDA_IUs_2022 <- "no update" # need to add this now to OCP dataframe

# # Find column names in Full_OCP_histories_df_popinfo_260225 that are not in Full_APOC_histories_df_popinfo_260325
# missing_columns_OCP <- setdiff(colnames(Full_OCP_histories_df_popinfo_260225), colnames(Full_APOC_histories_df_popinfo_260325))
# 
# # Find column names in Full_APOC_histories_df_popinfo_260325 that are not in Full_OCP_histories_df_popinfo_260225
# missing_columns_APOC <- setdiff(colnames(Full_APOC_histories_df_popinfo_260325), colnames(Full_OCP_histories_df_popinfo_260225))
# 
# # Print the missing columns
# missing_columns_OCP
# missing_columns_APOC

# remove some columns from the OCP

# Full_histories_df_popinfo <- rbind(Full_OCP_histories_df_popinfo_150324_v2, Full_APOC_histories_df_popinfo_200324_v2)
# Full_histories_df_popinfo <- rbind(Full_OCP_histories_df_popinfo, Full_APOC_histories_df_popinfo_170524)
# Full_histories_df_popinfo <- rbind(Full_OCP_histories_df_popinfo, Full_APOC_histories_df_popinfo_300724) # latest - JULY 24
# Full_histories_df_popinfo <- rbind(Full_OCP_histories_df_popinfo, Full_APOC_histories_df_popinfo_111124) # latest - NOV 24
#Full_histories_df_popinfo <- rbind(Full_OCP_histories_df_popinfo, Full_APOC_histories_df_popinfo_070225) # latest - FEB 25
#Full_histories_df_popinfo <- rbind(Full_OCP_histories_df_popinfo, Full_APOC_histories_df_popinfo_260325) # latest - FEB 25
# Full_histories_df_popinfo <- rbind(Full_OCP_histories_df_popinfo_260325, Full_APOC_histories_df_popinfo_260325) # latest - FEB 25
#Full_histories_df_popinfo <- rbind(Full_OCP_histories_df_popinfo_260325, Full_APOC_histories_df_popinfo_070425) # latest - APRIL 25
Full_histories_df_popinfo <- rbind(Full_OCP_histories_df_popinfo_290425, Full_APOC_histories_df_popinfo_290425) # latest - APRIL 25 v2

Full_histories_df_popinfo_lastyr_2022 <- subset(Full_histories_df_popinfo, Year == 2022)

Full_histories_df_popinfo_lastyr_2022_OCP <- subset(Full_histories_df_popinfo_lastyr_2022, Control_prog == "OCP")
nrow(Full_histories_df_popinfo_lastyr_2022_OCP) # 609 IUs - 614 IUs (APril 2025) - 613 IUs (end of APril 25')
Full_histories_df_popinfo_lastyr_2022_APOC <- subset(Full_histories_df_popinfo_lastyr_2022, Control_prog == "APOC")
nrow(Full_histories_df_popinfo_lastyr_2022_APOC) # 1529 IUs - 1526 IUs (April 2025) - 1526 IUs (end of April 25')

# # remove El Kurmuk from SSD (blue nile foci: no MDA yet) - 200324 - DO NOT REMOVE (NOV 2024 -TCC)
# Full_histories_df_popinfo <- Full_histories_df_popinfo %>%
#   filter(IUs_NAME_MAPPING != "El Kurmuk")

# update columns for MDA treatment status in 2022 etc for both OCP and APOC now combined
# added July 2024 #

# correct coding error for Senegal status in 2022
Full_histories_df_popinfo <- Full_histories_df_popinfo %>%
  group_by(IU_ID_MAPPING) %>%
  mutate(
    trt_status_2022_v2 = case_when(
      all(ADMIN0ISO3 == "SDN" & !all(is.na(Cov.in2)) & IUs_NAME_MAPPING != "El Radoom") ~ "MDA stopped: under PIS (other sources) or eliminated",
      all(ADMIN0ISO3 == "SDN" & IUs_NAME_MAPPING == "El Radoom") ~ "MDA continues: remove SDN",
      all(ADMIN0ISO3 == "SEN") ~ "MDA stopped: under PIS (other sources) or eliminated",
      TRUE ~ trt_status_2022_v2
    )
  )

# correct coding error for Senegal status in 2022
Full_histories_df_popinfo <- Full_histories_df_popinfo %>%
  group_by(IU_ID_MAPPING) %>%
  mutate(
    trt_status_2022 = case_when(
      all(ADMIN0ISO3 == "SDN" & !all(is.na(Cov.in2)) & IUs_NAME_MAPPING != "El Radoom") ~ "MDA stopped",
      all(ADMIN0ISO3 == "SDN" & IUs_NAME_MAPPING == "El Radoom") ~ "MDA stopped",
      all(ADMIN0ISO3 == "SEN") ~ "MDA stopped",
      TRUE ~ trt_status_2022
    )
  )

# 11 IUs in Nigeria need to be include (i.e., change from MDA stopped) based on SS and CC data for 2022:
Full_histories_df_popinfo <- Full_histories_df_popinfo %>%
  group_by(IU_ID_MAPPING) %>%
  mutate(
    trt_status_2022 = case_when(
      IU_ID_MAPPING %in% c(36543, 36548, 36549, 36805, 36806, 36807, 36808, 36543, 36548, 36549, 36805, 36806, 36807, 36808, 36529, 36530, 36533, 36536) ~ "MDA continues",
      TRUE ~ trt_status_2022
    )
  ) %>%
  ungroup()  # Ungroup to avoid unintended group-wise operations later

Full_histories_df_popinfo <- Full_histories_df_popinfo %>%
  group_by(IU_ID_MAPPING) %>%
  mutate(
    trt_status_2022_v2 = case_when(
      IU_ID_MAPPING %in% c(36543, 36548, 36549, 36805, 36806, 36807, 36808, 36543, 36548, 36549, 36805, 36806, 36807, 36808, 36529, 36530, 36533, 36536) ~ "MDA continues: SS/TCT data for NGA in 2022",
      TRUE ~ trt_status_2022_v2
    )
  ) %>%
  ungroup()  # Ungroup to avoid unintended group-wise operations later

# Full_histories_df_popinfo <- Full_histories_df_popinfo %>%
#   group_by(IU_ID_MAPPING) %>%
#   mutate(
#     treatment_2026 = case_when(
#       IU_ID_MAPPING %in% c(36543, 36548, 36549, 36805, 36806, 36807, 36808) ~ "include for treatment from 2026",
#       TRUE ~ treatment_2026
#     )
#   ) %>%
#   ungroup()  # Ungroup to avoid unintended group-wise operations later


Full_histories_df_popinfo$treatment_2026 <- ifelse(is.na(Full_histories_df_popinfo$trt_status_2022),
                                                   NA,
                                                   ifelse(Full_histories_df_popinfo$trt_status_2022 %in% c("MDA continues","Treatment naive"),
                                                          "include for treatment from 2026",
                                                          "exclude for treatment from 2026"))

# change those that should now be MDA stopped (so no MDA in 2023 - 25)
Full_histories_df_popinfo <- Full_histories_df_popinfo %>%
  mutate(
    MDA_CDTI = if_else(
      trt_status_2022 == "MDA stopped" & Year %in% c(2023:2025),
      0,
      MDA_CDTI
    )
  )

Full_histories_df_popinfo <- Full_histories_df_popinfo %>%
  mutate(
    Cov.in2 = if_else(
      trt_status_2022 == "MDA stopped" & Year %in% c(2023:2025),
      NA,
      Cov.in2
    )
  )

Full_histories_df_popinfo <- Full_histories_df_popinfo %>%
  mutate(
    number_rnds = if_else(
      trt_status_2022 == "MDA stopped" & Year %in% c(2023:2025),
      0,
      number_rnds
    )
  )

Full_histories_df_popinfo <- Full_histories_df_popinfo %>%
  mutate(
    adherence_par = if_else(
      trt_status_2022 == "MDA stopped" & Year %in% c(2023:2025),
      NA,
      adherence_par
    )
  )

# change those that should now be MDA continues (so no MDA in 2023 - 25)
Full_histories_df_popinfo <- Full_histories_df_popinfo %>%
  mutate(
    MDA_CDTI = if_else(
      trt_status_2022 == "MDA continues" & Year %in% c(2023:2025),
      1,
      MDA_CDTI
    )
  )

Full_histories_df_popinfo <- Full_histories_df_popinfo %>%
  mutate(
    Cov.in2 = if_else(
      trt_status_2022 == "MDA continues" & Year %in% c(2023:2025),
      0.65,
      Cov.in2
    )
  )

Full_histories_df_popinfo <- Full_histories_df_popinfo %>%
  mutate(
    number_rnds = if_else(
      trt_status_2022 == "MDA continues" & Year %in% c(2023:2025) & IU_ID_MAPPING %in% c(36543, 36548, 36549, 36805, 36806, 36807, 36808, 36543, 36548, 36549, 36805, 36806, 36807, 36808, 36529, 36530, 36533, 36536),
      1,
      number_rnds
    )
  )

Full_histories_df_popinfo <- Full_histories_df_popinfo %>%
  mutate(
    adherence_par = if_else(
      trt_status_2022 == "MDA continues" & Year %in% c(2023:2025),
      0.3,
      adherence_par
    )
  )

Full_histories_df_minimal_lastyr_2022 <- subset(Full_histories_df_popinfo, Year == 2022)
nrow(Full_histories_df_minimal_lastyr_2022) # 2139 IUs (April 25')

## Add Mamuela's column
# Full_histories_df_popinfo <- Full_histories_df_popinfo %>%
#   mutate(Cov.in2_binary = is.na(Cov.in2), 0, 1) %>%
#   group_by(IU_ID_MAPPING, grp = with(rle(Cov.in2_binary), rep(seq_along(lengths), lengths))) %>%
#   mutate(CUM_MDAcont_modelled = seq_along(grp)) %>%
#   ungroup() %>%
#   select(-grp) %>%
#   ungroup() %>%
#   dplyr::group_by(IU_ID_MAPPING) %>%
#   mutate(CUM_MDAcont_modelledMAX = max(CUM_MDAcont_modelled)) %>%
#   mutate(CUM_MDA_modelledMAX = max(CUM_MDA_modelled))

Full_histories_df_minimal_lastyr_2022 <- subset(Full_histories_df_popinfo, Year == 2022)
Full_histories_df_minimal_lastyr_2025 <- subset(Full_histories_df_popinfo, Year == 2025)

Full_histories_df_minimal_lastyr_2022_OCP <- subset(Full_histories_df_minimal_lastyr_2022, Control_prog == "OCP")
nrow(Full_histories_df_minimal_lastyr_2022_OCP) # 609 IUs - 614 IUs (APril 2025) - 613 IUs (end of April 25')
Full_histories_df_minimal_lastyr_2022_APOC <- subset(Full_histories_df_minimal_lastyr_2022, Control_prog == "APOC")
nrow(Full_histories_df_minimal_lastyr_2022_APOC) # 1529 IUs - 1526 IUs (April 2025) - 1526 IUs (end of April 25')

# SAVE BUSINESS CASE: APOC AND OCP COMBINED (NOV 23)
# write.csv(Full_histories_df_popinfo, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_popinfo_271123.csv")
# write.csv(Full_histories_df_minimal_lastyr_2022, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_minimal_lastyr_2022_271123.csv")
# write.csv(Full_histories_df_minimal_lastyr_2025, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_minimal_lastyr_2025_271123.csv")

# SAVE ENDGAME: APOC AND OCP COMBINED (MARCH 24)
# write.csv(Full_histories_df_popinfo, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_popinfo_150324.csv")
# write.csv(Full_histories_df_minimal_lastyr_2022, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_minimal_lastyr_2022_150324.csv")
# write.csv(Full_histories_df_minimal_lastyr_2025, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_minimal_lastyr_2025_150324.csv")

# SAVE ENDGAME: APOC AND OCP COMBINED (MAY 24 with ESPEN 2022 UPDATE)
# write.csv(Full_histories_df_popinfo, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_popinfo_170524.csv")
# write.csv(Full_histories_df_minimal_lastyr_2022, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_minimal_lastyr_2022_170524.csv")
# write.csv(Full_histories_df_minimal_lastyr_2025, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_minimal_lastyr_2025_170524.csv")

# ================ #
# load in final DF #
#Full_histories_df_popinfo_271123 <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_popinfo_271123.csv")
#Full_histories_df_popinfo <- Full_histories_df_popinfo_271123


# subset specific IUs
#Full_histories_df_popinfo_CIV0162715440 <- subset(Full_histories_df_popinfo_271123, IU_CODE_MAPPING == "CIV0162715440")
#write.csv(Full_histories_df_popinfo_CIV0162715440, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/CIV0162715440_rho0.5.csv")

# =====================================#
# 9) label co-endemic IUS with loa     #

co_endemic_IUs <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Co-endemicity/co_endemic_IUs_ALL.csv")

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

Full_histories_df_popinfo$co_endemicity <- ifelse(Full_histories_df_popinfo$IU_ID_MAPPING %in% co_endemic_IUs_oncho_LF_loahyper_vec, "oncho,LF,loa hyper",
                                                  ifelse(Full_histories_df_popinfo$IU_ID_MAPPING %in% co_endemic_IUs_oncho_LF_loameso_vec, "oncho,LF,loa meso",
                                                         ifelse(Full_histories_df_popinfo$IU_ID_MAPPING %in% co_endemic_IUs_oncho_LF_loahypo_vec, "oncho,LF,loa hypo",
                                                                ifelse(Full_histories_df_popinfo$IU_ID_MAPPING %in% co_endemic_IUs_oncho_LF_vec, "oncho,LF",
                                                                       ifelse(Full_histories_df_popinfo$IU_ID_MAPPING %in% co_endemic_IUs_oncho_loahyper_vec, "oncho,loa hyper",
                                                                              ifelse(Full_histories_df_popinfo$IU_ID_MAPPING %in% co_endemic_IUs_oncho_loameso_vec, "oncho,loa meso",
                                                                                     ifelse(Full_histories_df_popinfo$IU_ID_MAPPING %in% co_endemic_IUs_oncho_loahypo_vec, "oncho,loa hypo",
                                                                                            ifelse(Full_histories_df_popinfo$IU_ID_MAPPING %in% co_endemic_IUs_oncho_vec, "oncho", "assumed oncho only"))))))))

# ==========================================================================
# 9) in co-endemic IUs with loa, increase rho parameter value to 0.5 or 0.6

# # only for Buisness case!
#
# Full_histories_df_popinfo <- Full_histories_df_popinfo %>%
#   mutate(
#     adherence_par = ifelse(MDA_CDTI == 1 | MDA_nonCDTI == 1, 0.5, NA_real_)
#   )
#
#
# Full_histories_df_popinfo <- Full_histories_df_popinfo %>%
#   mutate(
#     adherence_par = case_when(
#       co_endemicity %in% c("oncho,LF,loa hyper", "oncho,loa hyper") & adherence_par == 0.5 ~ 0.6,
#       TRUE ~ adherence_par
#     )
#   )
# should be no change as no loa!

Full_histories_df_popinfo <- Full_histories_df_popinfo %>%
  mutate(vector_control = na_if(vector_control, 0))

Full_histories_df_minimal_lastyr_2022 <- subset(Full_histories_df_popinfo, Year == 2022)
nrow(Full_histories_df_minimal_lastyr_2022) # 2139 (end of Aoril 25')

# # Add Mamuela's column
# Full_histories_df_popinfo <- Full_histories_df_popinfo %>%
#   mutate(Cov.in2_binary = is.na(Cov.in2), 0, 1) %>%
#   group_by(IU_ID_MAPPING, grp = with(rle(Cov.in2_binary), rep(seq_along(lengths), lengths))) %>%
#   mutate(CUM_MDAcont_modelled = seq_along(grp)) %>%
#   ungroup() %>%
#   select(-grp) %>%
#   ungroup() %>%
#   dplyr::group_by(IU_ID_MAPPING) %>%
#   mutate(CUM_MDAcont_modelledMAX = max(CUM_MDAcont_modelled)) %>%
#   mutate(CUM_MDA_modelledMAX = max(CUM_MDA_modelled))


#Full_histories_df_minimal_lastyr_2022 <- subset(Full_histories_df_popinfo, Year == 2022)

# =================================================== #
# make a column with business as usual classification #

Full_histories_df_popinfo <- Full_histories_df_popinfo %>%
  mutate(
    MDA_CDTI = ifelse(MDA_CDTI == 0, NA, MDA_CDTI),
    MDA_CDTI_Biannual = ifelse(MDA_CDTI_Biannual == 0, NA, MDA_CDTI_Biannual),
    vector_control = ifelse(vector_control == 0, NA, vector_control),
  )

Full_histories_df_popinfo <- Full_histories_df_popinfo %>%
  group_by(IU_ID_MAPPING) %>%
  mutate(
    business_as_usual = case_when(
      any(is.na(MDA_CDTI) & is.na(MDA_CDTI_Biannual) & vector_control == 2 & Year == 2025) ~ -1,
      any(is.na(MDA_CDTI) & is.na(MDA_CDTI_Biannual) & is.na(vector_control) & Year == 2025) ~ 0,
      any(MDA_CDTI == 1 & is.na(MDA_CDTI_Biannual) & is.na(vector_control) & Year == 2025) ~ 1,
      any(MDA_CDTI == 1 & is.na(MDA_CDTI_Biannual) & vector_control == 1 & Year == 2025) ~ 2,
      any(MDA_CDTI == 1 & is.na(MDA_CDTI_Biannual) & vector_control == 2 & Year == 2025) ~ 3,
      any(MDA_CDTI == 1 & MDA_CDTI_Biannual == 1 & is.na(vector_control) & Year == 2025) ~ 4,
      any(MDA_CDTI == 1 & MDA_CDTI_Biannual == 1 & vector_control == 1 & Year == 2025) ~ 5,
      any(MDA_CDTI == 1 & MDA_CDTI_Biannual == 1 & vector_control == 2 & Year == 2025) ~ 6,
      TRUE ~ NA_real_  # Set to NA if none of the conditions are met
    )
  )


Full_histories_df_popinfo <- Full_histories_df_popinfo %>%
  group_by(IU_ID_MAPPING) %>%
  mutate(
    business_as_usual = case_when(
      any(trt_status_2022 == "Treatment naive") ~ 1,
      TRUE ~ business_as_usual  # Set to NA if none of the conditions are met
    )
  )

Full_histories_df_minimal_lastyr_2022 <- subset(Full_histories_df_popinfo, Year == 2022)
Full_histories_df_minimal_lastyr_2025 <- subset(Full_histories_df_popinfo, Year == 2025)


## CHECKs #
check_NAs <- subset(Full_histories_df_popinfo, is.na(business_as_usual))
check_NAs # some are present - see code below to deal with these

# check if any IUs do not have 51 entries #
count_per_mapping <- Full_histories_df_popinfo %>%
  group_by(IU_ID_MAPPING) %>%
  summarise(count = n()) # Count the number of rows for each unique IU_ID_MAPPING

unique_mappings_not_51 <- count_per_mapping %>%
  filter(count != 51) %>%
  pull(IU_ID_MAPPING) # Filter out the unique IU_ID_MAPPING values that do not have 51 rows

unique_mappings_not_51

# Full_histories_df_popinfo <- Full_histories_df_popinfo %>%
#   filter(!IU_ID_MAPPING %in% c(7424, 7441, 19408, 49752, 49821))

Full_histories_df_popinfo <- Full_histories_df_popinfo %>%
  #filter(!IU_ID_MAPPING %in% c(19408, 49752, 49821))
  filter(!IU_ID_MAPPING %in% c(19408, 49821)) # removed 2 CMR IUs (now these are included in histories) - March 25'
# note 49752 no longer in dataset so only removes the two above in UGA (49821) and ETH (19408)

check_df <- subset(Full_histories_df_popinfo, Year == 2022)
nrow(check_df) # 2137 (end of April 25')

check_NAs <- subset(Full_histories_df_popinfo, is.na(business_as_usual))

check_NAs # now no NAs

# need to correct some NA's in the check's below:

# 1) those that are not reported in 2021/2022
Full_histories_df_popinfo <- Full_histories_df_popinfo %>%
  group_by(IU_ID_MAPPING) %>%
  mutate(
    trt_status_2022 = case_when(
      !IUs_NAME_MAPPING %in% c("Qessan", "El Kurmuk") &
        all(Endemicity[Year == 2021] == "Not reported", Endemicity[Year == 2022] == "Not reported") ~ "MDA stopped",
      TRUE ~ trt_status_2022
    )
  ) %>%
  ungroup()  # Ungroup to avoid unintended group-wise operations later

Full_histories_df_popinfo <- Full_histories_df_popinfo %>%
  group_by(IU_ID_MAPPING) %>%
  mutate(
    trt_status_2022_v2 = case_when(
      !IUs_NAME_MAPPING %in% c("Qessan", "El Kurmuk") &
        all(Endemicity[Year == 2021] == "Not reported", Endemicity[Year == 2022] == "Not reported") ~ "MDA stopped: non-endemic & not reported in ESPEN (21/22)",
      TRUE ~ trt_status_2022_v2
    )
  ) %>%
  ungroup()  # Ungroup to avoid unintended group-wise operations later

Full_histories_df_popinfo <- Full_histories_df_popinfo %>%
  group_by(IU_ID_MAPPING) %>%
  mutate(
    treatment_2026 = case_when(
      !IUs_NAME_MAPPING %in% c("Qessan", "El Kurmuk") &
        all(Endemicity[Year == 2021] == "Not reported", Endemicity[Year == 2022] == "Not reported") ~ "exclude for treatment from 2026",
      TRUE ~ treatment_2026
    )
  ) %>%
  ungroup()  # Ungroup to avoid unintended group-wise operations later

# make full label for business as usual from 2026
Full_histories_df_popinfo <- Full_histories_df_popinfo %>%
  mutate(BAU_label = case_when(
    business_as_usual == -1 ~ "no intervention (and ABR = 0)",
    business_as_usual == 0 ~ "no intervention",
    business_as_usual == 1 ~ "annual MDA",
    business_as_usual == 2 ~ "annual MDA + vector control",
    business_as_usual == 3 ~ "annual MDA + vector elimination",
    business_as_usual == 4 ~ "biannual MDA",
    business_as_usual == 5 ~ "biannual MDA + vector control",
    business_as_usual == 6 ~ "biannual MDA + vector elimination",
    TRUE ~ NA_character_  # This handles any other values that might exist
  ))

check_df <- subset(Full_histories_df_popinfo, Year == 2022)
nrow(check_df) # 2137 (end of April 25')


# =============== #
# check - no intervention and 0 ABR (-1 BAU)
# ============== #
Full_histories_df_popinfo_BAU0a <- subset(Full_histories_df_popinfo, business_as_usual == -1) # no intervention (and ABR = 0)
unique(Full_histories_df_popinfo_BAU0a$trt_status_2022)
unique(Full_histories_df_popinfo_BAU0a$trt_status_2022_v2)
unique(Full_histories_df_popinfo_BAU0a$treatment_2026)

Full_histories_df_popinfo_BAU0a  <- Full_histories_df_popinfo_BAU0a  %>%
  select(
    IU_ID_MAPPING, Endemicity, Year, Cov, EpiCov, Cov.in2, vector_control,
    MDA_nonCDTI, MDA_CDTI, MDA_CDTI_Biannual, trt_status_2022,
    trt_status_2022_v2, adherence_par, treatment_2026, BAU_label
  ) %>%
  filter(Year >= 2021 & Year <= 2025)

# =============== #
# check - no intervention  (0 BAU)
# ============== #
Full_histories_df_popinfo_BAU0 <- subset(Full_histories_df_popinfo, business_as_usual == 0) # no intervention
Full_histories_df_popinfo_BAU0  <- Full_histories_df_popinfo_BAU0  %>%
  select(
    IU_ID_MAPPING, Endemicity, Year, Cov, EpiCov, Cov.in2, vector_control,
    MDA_nonCDTI, MDA_CDTI, MDA_CDTI_Biannual, trt_status_2022,
    trt_status_2022_v2, adherence_par, treatment_2026, BAU_label
  ) %>%
  filter(Year >= 2021 & Year <= 2025)
unique(Full_histories_df_popinfo_BAU0$trt_status_2022)
unique(Full_histories_df_popinfo_BAU0$trt_status_2022_v2)
unique(Full_histories_df_popinfo_BAU0$treatment_2026)

Full_histories_df_popinfo_BAU0_check <- subset(Full_histories_df_popinfo_BAU0, IU_ID_MAPPING %in% c(40179, 40180, 40184, 40206, 40207, 40210, 40212)) #ALL SENEGAL = under PIS (MG) but endemic (under MDA) in 21/22
Full_histories_df_popinfo_BAU0_check <- subset(Full_histories_df_popinfo_BAU0, IU_ID_MAPPING %in% c(36543, 36548, 36549, 36805, 36806, 36807, 36808, 36529, 36530, 36533, 36536)) #cov.in2 > 0 in 2021/22

Full_histories_df_popinfo_check_NGA <- subset(Full_histories_df_popinfo, IU_ID_MAPPING %in% c(36543, 36548, 36549, 36805, 36806, 36807, 36808, 36529, 36530, 36533, 36536)) #cov.in2 > 0 in 2021/22
Full_histories_df_popinfo_check_NGA  <- Full_histories_df_popinfo_check_NGA  %>%
  select(
    IU_ID_MAPPING, Endemicity, Year, Cov, EpiCov, Cov.in2, vector_control,
    MDA_nonCDTI, MDA_CDTI, MDA_CDTI_Biannual, trt_status_2022,
    trt_status_2022_v2, adherence_par, number_rnds, treatment_2026, BAU_label
  )

# =============== #
# check - annual MDA  (1 BAU)
# ============== #
Full_histories_df_popinfo_BAU1 <- subset(Full_histories_df_popinfo, business_as_usual == 1) # annual MDA

unique(Full_histories_df_popinfo_BAU1$trt_status_2022)
unique(Full_histories_df_popinfo_BAU1$trt_status_2022_v2)
unique(Full_histories_df_popinfo_BAU1$treatment_2026)

Full_histories_df_popinfo_BAU1  <- Full_histories_df_popinfo_BAU1  %>%
  select(
    IU_ID_MAPPING, Endemicity, Year, Cov, EpiCov, Cov.in2, vector_control,
    MDA_nonCDTI, MDA_CDTI, MDA_CDTI_Biannual, trt_status_2022,
    trt_status_2022_v2, adherence_par, number_rnds, treatment_2026, BAU_label
  ) %>%
  filter(Year >= 2021 & Year <= 2025)

Full_histories_df_popinfo_BAU1_trtnaive <- subset(Full_histories_df_popinfo_BAU1, trt_status_2022_v2 == "Treatment naive")
unique(Full_histories_df_popinfo_BAU1_trtnaive$IU_ID_MAPPING)

#Full_histories_df_popinfo_BAU1_check <- subset(Full_histories_df_popinfo_BAU1, trt_status_2022_v2 == "MDA stopped: non-endemic in 2021/2022 ESPEN")

# need to update
Full_histories_df_popinfo <- Full_histories_df_popinfo %>%
  # First, update rows where IU_ID_MAPPING is 53007
  mutate(
    trt_status_2022 = if_else(IU_ID_MAPPING == 53007, "MDA stopped", trt_status_2022),
    trt_status_2022_v2 = if_else(IU_ID_MAPPING == 53007, "MDA stopped: non-endemic in 2021/2022 ESPEN", trt_status_2022_v2),
    treatment_2026 = if_else(IU_ID_MAPPING == 53007, "exclude for treatment from 2026", treatment_2026),
    BAU_label = if_else(IU_ID_MAPPING == 53007, "no intervention", BAU_label),
    business_as_usual = if_else(IU_ID_MAPPING == 53007, 0, business_as_usual)
  ) %>%
  # Then, update rows for years 2023 to 2025
  mutate(
    Cov.in2 = if_else(IU_ID_MAPPING == 53007 & Year %in% 2023:2025, NA_real_, Cov.in2),  # Use NA_real_ for numeric columns
    MDA_CDTI = if_else(IU_ID_MAPPING == 53007 & Year %in% 2023:2025, NA_real_, MDA_CDTI),
    number_rnds = if_else(IU_ID_MAPPING == 53007 & Year %in% 2023:2025, 0, number_rnds),
    adherence_par = if_else(IU_ID_MAPPING == 53007 & Year %in% 2023:2025, NA_real_, adherence_par)
  )

# a bunch of IUs where "Endemic (MDA not delivered)" in both 2021/2022
unique_iu_ids <- Full_histories_df_popinfo_BAU1 %>%
  filter(Endemicity == "Endemic (MDA not delivered)", Year %in% c(2021, 2022)) %>%
  group_by(IU_ID_MAPPING) %>%
  filter(n() == 2) %>%  # Ensure there are 2 records (one for each year)
  pull(IU_ID_MAPPING) %>%
  unique()

# include for now but rename trt_status_2022_v2 to highlight these
Full_histories_df_popinfo <- Full_histories_df_popinfo %>%
  # First, update rows where IU_ID_MAPPING is 53007
  mutate(
    trt_status_2022_v2 = if_else(IU_ID_MAPPING %in% unique_iu_ids, "MDA continues: note both 21/22 MDA not delivered", trt_status_2022_v2)
  )

# some IUs where "Endemic (MDA not delivered)" in both 2021/2022 in endemicity but cov.in2 > 0 (in some NGA IU's?)
unique_iu_ids2 <- Full_histories_df_popinfo %>%
  filter(Year %in% c(2021, 2022)) %>%
  group_by(IU_ID_MAPPING) %>%
  filter(
    all(Endemicity == "Endemic (MDA not delivered)"),  # Check Endemicity for both years
    any(Cov.in2 > 0)  # Check if Cov.in2 is greater than 0 in either year
  ) %>%
  pull(IU_ID_MAPPING) %>%
  unique()

# include for now but rename trt_status_2022_v2 to highlight these
Full_histories_df_popinfo <- Full_histories_df_popinfo %>%
  # First, update rows where IU_ID_MAPPING is 53007
  mutate(
    trt_status_2022_v2 = if_else(IU_ID_MAPPING %in% unique_iu_ids2, "MDA continues: note all NGA IUs with both 21/22 MDA not delivered but TCT/SS indicate MDA", trt_status_2022_v2)
  )

# 2 IUs which are treatment naive should be under PIS:
Full_histories_df_popinfo <- Full_histories_df_popinfo %>%
  # First, update rows where IU_ID_MAPPING is 53007
  mutate(
    trt_status_2022 = if_else(IU_ID_MAPPING %in% c(19529, 37017), "MDA stopped", trt_status_2022),
    trt_status_2022_v2 = if_else(IU_ID_MAPPING %in% c(19529, 37017), "MDA stopped: under PIS (ESPEN)", trt_status_2022_v2),
    treatment_2026 = if_else(IU_ID_MAPPING %in% c(19529, 37017), "exclude for treatment from 2026", treatment_2026),
    BAU_label = if_else(IU_ID_MAPPING %in% c(19529, 37017), "no intervention", BAU_label),
    business_as_usual = if_else(IU_ID_MAPPING %in% c(19529, 37017), 0, business_as_usual)
  )


#Full_histories_df_popinfo_subsetIU <- subset(Full_histories_df_popinfo, IU_CODE_MAPPING == "GHA0216121392")
#write.csv(Full_histories_df_popinfo_subsetIU, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_popinfo_GHA0216121392.csv")

# =============== #
# check - annual MDA + VC  (2 BAU)
# ============== #
Full_histories_df_popinfo_BAU2 <- subset(Full_histories_df_popinfo, business_as_usual == 2) # annual MDA + VC

unique(Full_histories_df_popinfo_BAU2$trt_status_2022)
unique(Full_histories_df_popinfo_BAU2$trt_status_2022_v2)
unique(Full_histories_df_popinfo_BAU2$treatment_2026)

Full_histories_df_popinfo_BAU2  <- Full_histories_df_popinfo_BAU2  %>%
  select(
    IU_ID_MAPPING, Endemicity, Year, Cov, EpiCov, Cov.in2, vector_control,
    MDA_nonCDTI, MDA_CDTI, MDA_CDTI_Biannual, trt_status_2022,
    trt_status_2022_v2, adherence_par, number_rnds, treatment_2026, BAU_label
  ) %>%
  filter(Year >= 2021 & Year <= 2025)


# =============== #
# check - annual MDA + VE (3 BAU)
# ============== #
Full_histories_df_popinfo_BAU3 <- subset(Full_histories_df_popinfo, business_as_usual == 3) # annual MDA + Ve

unique(Full_histories_df_popinfo_BAU3$trt_status_2022)
unique(Full_histories_df_popinfo_BAU3$trt_status_2022_v2)
unique(Full_histories_df_popinfo_BAU3$treatment_2026)

Full_histories_df_popinfo_BAU3  <- Full_histories_df_popinfo_BAU3  %>%
  select(
    IU_ID_MAPPING, Endemicity, Year, Cov, EpiCov, Cov.in2, vector_control,
    MDA_nonCDTI, MDA_CDTI, MDA_CDTI_Biannual, trt_status_2022,
    trt_status_2022_v2, adherence_par, number_rnds, treatment_2026, BAU_label
  ) %>%
  filter(Year >= 2021 & Year <= 2025)

# =============== #
# check - biannual MDA (4 BAU)
# ============== #
Full_histories_df_popinfo_BAU4 <- subset(Full_histories_df_popinfo, business_as_usual == 4) # biannual MDA

unique(Full_histories_df_popinfo_BAU4$trt_status_2022)
unique(Full_histories_df_popinfo_BAU4$trt_status_2022_v2)
unique(Full_histories_df_popinfo_BAU4$treatment_2026)

Full_histories_df_popinfo_BAU4  <- Full_histories_df_popinfo_BAU4  %>%
  select(
    IU_ID_MAPPING, Endemicity, Year, Cov, EpiCov, Cov.in2, vector_control,
    MDA_nonCDTI, MDA_CDTI, MDA_CDTI_Biannual, trt_status_2022,
    trt_status_2022_v2, adherence_par, number_rnds, treatment_2026, BAU_label
  ) %>%
  filter(Year >= 2021 & Year <= 2025)

# some IUs where ESPEN cov = 0 but Cov.in2 > 0 in NGA (and biannual):
unique_iu_ids3 <- Full_histories_df_popinfo %>%
  filter(Year %in% c(2021, 2022)) %>%
  group_by(IU_ID_MAPPING) %>%
  filter(
    any(
      Endemicity == "Endemic (MDA not delivered)" &
        Cov.in2 > 0 &
        number_rnds == 2
    )
  ) %>%
  pull(IU_ID_MAPPING) %>%
  unique()

# update these 3 IUs trt_status_2022_v2 label:
Full_histories_df_popinfo <- Full_histories_df_popinfo %>%
  # First, update rows where IU_ID_MAPPING is 53007
  mutate(
    trt_status_2022_v2 = if_else(IU_ID_MAPPING %in% unique_iu_ids3, "MDA continues: note all NGA IUs with one of 21/22 yrs w/ MDA not delivered but TCT/SS indicate biannual MDA", trt_status_2022_v2)
  )


# =============== #
# check - biannual MDA + vc (5 BAU)
# ============== #
Full_histories_df_popinfo_BAU5 <- subset(Full_histories_df_popinfo, business_as_usual == 5) # biannual MDA + VC

unique(Full_histories_df_popinfo_BAU5$trt_status_2022)
unique(Full_histories_df_popinfo_BAU5$trt_status_2022_v2)
unique(Full_histories_df_popinfo_BAU5$treatment_2026)

Full_histories_df_popinfo_BAU5  <- Full_histories_df_popinfo_BAU5  %>%
  select(
    IU_ID_MAPPING, Endemicity, Year, Cov, EpiCov, Cov.in2, vector_control,
    MDA_nonCDTI, MDA_CDTI, MDA_CDTI_Biannual, trt_status_2022,
    trt_status_2022_v2, adherence_par, number_rnds, treatment_2026, BAU_label
  ) %>%
  filter(Year >= 2021 & Year <= 2025)

# =============== #
# check - biannual MDA + ve (6 BAU)
# ============== #
Full_histories_df_popinfo_BAU6 <- subset(Full_histories_df_popinfo, business_as_usual == 6) # biannual MDA + Ve

unique(Full_histories_df_popinfo_BAU6$trt_status_2022)
unique(Full_histories_df_popinfo_BAU6$trt_status_2022_v2)
unique(Full_histories_df_popinfo_BAU6$treatment_2026)

Full_histories_df_popinfo_BAU6  <- Full_histories_df_popinfo_BAU6  %>%
  select(
    IU_ID_MAPPING, Endemicity, Year, Cov, EpiCov, Cov.in2, vector_control,
    MDA_nonCDTI, MDA_CDTI, MDA_CDTI_Biannual, trt_status_2022,
    trt_status_2022_v2, adherence_par, number_rnds, treatment_2026, BAU_label
  ) %>%
  filter(Year >= 2021 & Year <= 2025)


check_df <- subset(Full_histories_df_popinfo, Year == 2022)
nrow(check_df) # 2137 (end of April 25')


# =============== #
# check - OVERALL
# ============== #
unique(Full_histories_df_popinfo$trt_status_2022)
unique(Full_histories_df_popinfo$trt_status_2022_v2)
unique(Full_histories_df_popinfo$treatment_2026)

## need to change some trt_status_2022_v2 from "MDA stopped: no MDA in ESPEN years" to ""MDA stopped: under PIS (ESPEN)"
# backup <- Full_histories_df_popinfo
# Full_histories_df_popinfo <- backup

Full_histories_df_popinfo <- Full_histories_df_popinfo %>%
  group_by(IU_ID_MAPPING) %>%
  mutate(
    trt_status_2022_v2 = case_when(
      (trt_status_2022_v2 %in% c("MDA stopped: no MDA in ESPEN years", "MDA stopped: no MDA in 2021/2022 ESPEN")) &
        any(Year %in% c(2021, 2022) & Endemicity == "Non-endemic") ~ "MDA stopped: non-endemic & not reported in ESPEN (21/22)",
      TRUE ~ trt_status_2022_v2
    )
  ) %>%
  ungroup()  # Ungroup to avoid unintended group-wise operations later

# need to update trt_status_2022_v2 to include "treatment naive" again:
Full_histories_df_popinfo <- Full_histories_df_popinfo %>%
  mutate(
    trt_status_2022_v2 = if_else(trt_status_2022 == "Treatment naive", "Treatment naive", trt_status_2022_v2)
  )


Full_histories_df_minimal_lastyr_2022 <- subset(Full_histories_df_popinfo, Year == 2022)

length(unique(Full_histories_df_minimal_lastyr_2022$IU_ID_MAPPING))

# Calculate the frequency of "MDA continues" in trt_status_2022_v2 column
trt_status_2022_counts <- Full_histories_df_minimal_lastyr_2022 %>%
  count(trt_status_2022)

trt_status_2022_counts

Full_histories_df_minimal_lastyr_2022_MDAstopped <- subset(Full_histories_df_minimal_lastyr_2022, trt_status_2022 == "MDA stopped")

trt_status_2022_MDAstopped_counts <- Full_histories_df_minimal_lastyr_2022_MDAstopped %>%
  count(trt_status_2022_v2)

trt_status_2022_MDAstopped_counts


# =================================================================== #
#  NOTE THIS STEP IS PERFORMED IN PLOTTING HISTORIES APRIL 25' SCRIPT #

# ================================================================ #
#         Those in Endgame, only keep Treatment naive IUs in Gabon #

Full_histories_df_minimal_lastyr_2022_check_trtnaive <- subset(Full_histories_df_minimal_lastyr_2022, trt_status_2022 == "Treatment naive")

nrow(Full_histories_df_minimal_lastyr_2022_check_trtnaive)

table(Full_histories_df_minimal_lastyr_2022_check_trtnaive$ADMIN0ISO3) # 5 in ETH, 27 in GAB and 2 in SDN (from CC)

# # only keep SDN and GAB treatment naive IUs
# Full_histories_df_minimal_lastyr_2022_check <- Full_histories_df_minimal_lastyr_2022_check %>%
#   filter(
#     # Keep "treatment naive" only if ADMI0ISO3 is "GAB"
#     (trt_status_2022 == "Treatment naive" & ADMIN0ISO3 %in% c("GAB", "SDN")) |
#       # Keep any row where trt_status_2022 is not "treatment naive"
#       trt_status_2022 != "Treatment naive"
#   )

Full_histories_df_minimal_lastyr_2022_check <- Full_histories_df_minimal_lastyr_2022_check %>%
  filter(
    # Keep "Treatment naive" only if ADMIN0ISO3 is "GAB" or "SDN"
    (trt_status_2022 == "Treatment naive" & ADMIN0ISO3 %in% c("GAB", "SDN")) |
      # Keep any row where trt_status_2022 is not "Treatment naive"
      trt_status_2022 != "Treatment naive",
    # Exclude a specific IU_ID
    IU_CODE_MAPPING != "ETH0194319529"
  )

Full_histories_df_minimal_lastyr_2022_check_trtnaive <- subset(Full_histories_df_minimal_lastyr_2022_check, trt_status_2022 == "Treatment naive")

table(Full_histories_df_minimal_lastyr_2022_check_trtnaive$ADMIN0ISO3) # 5 in ETH, 27 in GAB and 2 in SDN (from CC)

Full_histories_df_minimal_lastyr_2022 <- Full_histories_df_minimal_lastyr_2022_check # RESET HERE

# frequency of categories:

#cbPalette <- c("#99CCFF","#9999FF","#009E73","#CC0000","#FFFF99","#FF6600","#CC0000","#FFFF99","#FF6600")

ggplot(Full_histories_df_minimal_lastyr_2022, aes(x = trt_status_2022_v2)) +
  geom_bar(aes(fill = trt_status_2022_v2), colour = "black", alpha = 0.7) +
  facet_wrap(~ ADMIN0ISO3, scales = "free_y") +
  labs(x = "Treatment Status 2022", y = "Frequency") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  +
  theme(
    axis.text.x = element_blank(),    # Remove x-axis text
    axis.ticks.x = element_blank(),   # Remove x-axis ticks
    axis.title.x = element_blank()    # Remove x-axis title
  )

# calculate total in each country to include in plot
totals <- Full_histories_df_minimal_lastyr_2022 %>%
  as.data.frame() %>% # to ignore the geometry in the spatial object
  group_by(ADMIN0ISO3) %>%
  summarise(total_count = n())

# Create a named vector for the labeller
total_labels <- totals %>%
  mutate(label = paste(ADMIN0ISO3, "\nTotal:", total_count)) %>%
  select(ADMIN0ISO3, label) %>%
  deframe()

ggplot(Full_histories_df_minimal_lastyr_2022, aes(x = trt_status_2022_v2)) +
  geom_bar(aes(fill = trt_status_2022_v2), colour = "black", alpha = 0.7) +
  facet_wrap(~ ADMIN0ISO3, scales = "free_y", labeller = labeller(ADMIN0ISO3 = total_labels)) +
  labs(x = "Treatment Status 2022", y = "Frequency") +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),    # Remove x-axis text
    axis.ticks.x = element_blank(),   # Remove x-axis ticks
    axis.title.x = element_blank(),   # Remove x-axis title
    legend.position = "bottom",
    legend.text = element_text(size = 8.5)
  )+
  guides(fill = guide_legend(title = NULL))  # Remove legend title

ggplot(Full_histories_df_minimal_lastyr_2022, aes(x = trt_status_2022_v2)) +
  geom_bar(aes(fill = trt_status_2022_v2), colour = "black", alpha = 0.7) +
  facet_wrap(~ ADMIN0ISO3, scales = "free_y", labeller = labeller(ADMIN0ISO3 = total_labels), ncol = 5) +
  labs(x = "Treatment Status 2022", y = "Frequency") +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),    # Remove x-axis text
    axis.ticks.x = element_blank(),   # Remove x-axis ticks
    axis.title.x = element_blank(),   # Remove x-axis title
    legend.position = "bottom",
    legend.text = element_text(size = 8.5)
  )+
  guides(fill = guide_legend(title = NULL))  # Remove legend title


# maps & frequency distributions ~ country #
ESPEN_IUs <- st_read('C:/Users/mad206/OneDrive/Endgame/Endgame IUs/ESPEN_IU_2021.shp')
all_countries <- unique(Full_histories_df_minimal_lastyr_2022$ADMIN0ISO3)
ESPEN_IUs_ALL <- ESPEN_IUs[which((ESPEN_IUs$ADMIN0ISO3 %in% all_countries)),]
st_geometry_type(ESPEN_IUs_ALL)
st_crs(ESPEN_IUs_ALL)

co_endemicity_maps_final <- ESPEN_IUs_ALL %>%
  left_join(Full_histories_df_minimal_lastyr_2022, by = c("IU_ID" = "IU_ID_MAPPING"))

ggplot() +
  geom_sf(data = co_endemicity_maps_final, aes(fill = trt_status_2022_v2), colour = NA, alpha = 0.7) +
  geom_sf(data = ESPEN_IUs_ALL, aes(), colour = NA, size = 1, fill = NA, alpha = 0.1) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
  coord_sf(xlim = c(-20, 50), ylim = c(38, -35)) +
  theme_bw() +
  #scale_fill_manual(values = cbPalette, na.value = "gray") +
  scale_colour_manual(na.value="gray")+
  labs(fill='') +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal",
    legend.text = element_text(size = 6))

ggplot() +
  geom_sf(data = co_endemicity_maps_final, aes(fill = trt_status_2022_v2), colour = NA, alpha = 0.7) +
  geom_sf(data = ESPEN_IUs_ALL, aes(), colour = NA, size = 1, fill = NA, alpha = 0.1) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
  coord_sf(xlim = c(-20, 50), ylim = c(38, -35)) +
  theme_bw() +
  #scale_fill_manual(values = cbPalette, na.value = "gray") +
  scale_colour_manual(na.value="gray")+
  labs(fill='') +
  theme(
    legend.position = "right",  # Place the legend at the bottom
    legend.direction = "vertical")

Full_histories_df_popinfo_minimial_tocheck  <- Full_histories_df_popinfo  %>%
  select(
    IU_ID_MAPPING, ADMIN0ISO3, Endemicity, Year, Cov, EpiCov, Cov.in2, vector_control,
    MDA_nonCDTI, MDA_CDTI, MDA_CDTI_Biannual, trt_status_2022,
    trt_status_2022_v2, adherence_par, number_rnds, treatment_2026, BAU_label
  ) %>%
  filter(Year >= 2021 & Year <= 2025)

Full_histories_df_popinfo_minimial_tocheck_BFA <- subset(Full_histories_df_popinfo_minimial_tocheck, ADMIN0ISO3 == "BFA")

View(Full_histories_df_popinfo_minimial_tocheck_BFA)

check_df <- subset(Full_histories_df_popinfo, Year == 2022)
nrow(check_df) # 2137 (end of April 25')

# FURTHER CHECKS #
# # important for Uganda - check
# check_MDAstopped_noMDA <- subset(Full_histories_df_popinfo, trt_status_2022 == "MDA stopped" & Year == 2025)
# check_MDAstopped_noMDA <- subset(Full_histories_df_popinfo, trt_status_2022 == "MDA stopped" & MDA_CDTI == 1 & Year == 2025)
# check_MDAstopped_vectorcntrl <- subset(Full_histories_df_popinfo, trt_status_2022 == "MDA stopped" & vector_control == 1 & Year == 2025)
# check_MDAstopped_vectorelim <- subset(Full_histories_df_popinfo, trt_status_2022 == "MDA stopped" & vector_control == 2 & Year == 2025)
# check_MDAcont_vectorcntrl <- subset(Full_histories_df_popinfo, trt_status_2022 == "MDA continues" & vector_control == 1 & Year == 2025)
# # check for GHA, TGO, ETH, SSD, SDN, NGA and UGA
# check_MDAcont_biannualMDA <- subset(Full_histories_df_popinfo, trt_status_2022 == "MDA continues" & MDA_CDTI_Biannual == 1 & Year == 2025)
# length(unique(dfAPOC_included6_check_MDAstopped$IU_ID_MAPPING))


# SAVE BUSINESS CASE: APOC AND OCP COMBINED (NOV 23)
# write.csv(Full_histories_df_popinfo, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_popinfo_160224.csv")
# write.csv(Full_histories_df_minimal_lastyr_2022, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_minimal_lastyr_2022_160224.csv")
# write.csv(Full_histories_df_minimal_lastyr_2025, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_minimal_lastyr_2025_160224.csv")

# SAVE ENDGAME: APOC AND OCP COMBINED (MARCH 24)
# write.csv(Full_histories_df_popinfo, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_popinfo_210324.csv")
# write.csv(Full_histories_df_minimal_lastyr_2022, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_minimal_lastyr_2022_210324.csv")
# write.csv(Full_histories_df_minimal_lastyr_2025, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_minimal_lastyr_2025_210324.csv")

# SAVE ENDGAME: APOC AND OCP COMBINED (MAY 24 with ESPEN 2022 UPDATE)
# write.csv(Full_histories_df_popinfo, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_popinfo_170524.csv")
# write.csv(Full_histories_df_minimal_lastyr_2022, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_minimal_lastyr_2022_170524.csv")
# write.csv(Full_histories_df_minimal_lastyr_2025, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_minimal_lastyr_2025_170524.csv")



# subset and save 3 countries for testing Endgame pipeline (March 24)
# Full_histories_df_popinfo_test <- subset(Full_histories_df_popinfo, ADMIN0ISO3 %in% c("UGA", "GHA", "LBR"))
# unique(Full_histories_df_popinfo_test$ADMIN0ISO3)
# unique(Full_histories_df_popinfo_test$IU_CODE_MAPPING)
# write.csv(Full_histories_df_popinfo_test, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_popinfo_3countries_210324.csv")


iu_mapping_count <- Full_histories_df_popinfo %>%
  group_by(business_as_usual) %>%
  summarise(num_iu_mapping = n_distinct(IU_ID_MAPPING))
iu_mapping_count

# iu_mapping_count_3countries <- Full_histories_df_popinfo_test %>%
#   group_by(business_as_usual) %>%
#   summarise(num_iu_mapping = n_distinct(IU_ID_MAPPING))
# iu_mapping_count_3countries

# SAVE ENDGAME: APOC AND OCP COMBINED (MARCH 24 with new trt_status_2022 classification made in July 2024)
# write.csv(Full_histories_df_popinfo, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_popinfo_March24_v2.csv")
# write.csv(Full_histories_df_minimal_lastyr_2022, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_minimal_lastyr_2022_March24_v2.csv")
# write.csv(Full_histories_df_minimal_lastyr_2025, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_minimal_lastyr_2025_March24_v2.csv")

## Save all countries for full Endgame pipeline (July 24)
# write.csv(Full_histories_df_popinfo, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_popinfo_010824.csv")
#Full_histories_df_popinfo_010824 <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_popinfo_010824.csv")

# SAVE ENDGAME: APOC AND OCP COMBINED (NOV 24 with BLUE NILE SDN update from TCC)
# write.csv(Full_histories_df_popinfo, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_popinfo_111124.csv")
# write.csv(Full_histories_df_minimal_lastyr_2022, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_minimal_lastyr_2022_111124.csv")
# write.csv(Full_histories_df_minimal_lastyr_2025, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_minimal_lastyr_2025_111124.csv")

# SAVE ENDGAME: APOC AND OCP COMBINED (Feb 2025 - updated with raw coverages and info)
# write.csv(Full_histories_df_popinfo, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_popinfo_110225.csv")
# write.csv(Full_histories_df_minimal_lastyr_2022, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_minimal_lastyr_2022_110225.csv")
# write.csv(Full_histories_df_minimal_lastyr_2025, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_minimal_lastyr_2025_110225.csv")

# # SAVE ENDGAME: APOC AND OCP COMBINED (MARCH 2025)
# write.csv(Full_histories_df_popinfo, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_popinfo_260325.csv")
# write.csv(Full_histories_df_minimal_lastyr_2022, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_minimal_lastyr_2022_260325.csv")
# write.csv(Full_histories_df_minimal_lastyr_2025, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_minimal_lastyr_2025_260325.csv")

# # SAVE ENDGAME: APOC AND OCP COMBINED (APRIL 2025)
# write.csv(Full_histories_df_popinfo, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_popinfo_070425.csv")
# write.csv(Full_histories_df_minimal_lastyr_2022, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_minimal_lastyr_2022_070425.csv")
# write.csv(Full_histories_df_minimal_lastyr_2025, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_minimal_lastyr_2025_070425.csv")

# SAVE ENDGAME: APOC AND OCP COMBINED (end of APRIL 2025)
write.csv(Full_histories_df_popinfo, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_popinfo_290425.csv")
write.csv(Full_histories_df_minimal_lastyr_2022, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_minimal_lastyr_2022_290425.csv")
write.csv(Full_histories_df_minimal_lastyr_2025, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_minimal_lastyr_2025_290425.csv")


#Full_histories_df_popinfo <- Full_histories_df_popinfo_010824

Full_histories_df_minimal_lastyr_2022 <- subset(Full_histories_df_popinfo, Year == 2022)
Full_histories_df_minimal_lastyr_2025 <- subset(Full_histories_df_popinfo, Year == 2025)


# ====================================================================== #
#            EXTRACT SPECIFIC IUS FOR FINAL ENDGAME RUNS (MARCH 2025)    #

Specific_IUs_torun <- c("ETH0194818882", "ETH0194818895", "ETH0194819380",
                        "CMR0081807424", "CMR0081907441", "NGA0380236900",
                        "NGA0380436967", "NGA0380536989", "NER0366935348",
                        "NER0366935352","NER0366935353", "NER0367335378",
                        "NER0367335379")

Specific_IUs_torun <- c("18882", "18895", "19380",
                        "7424", "7441", "36900",
                        "36967", "36989", "35348",
                        "35352","35353", "35378","35379")

length(Specific_IUs_torun)

Full_histories_df_popinfo_extraIUs_March25 <- subset(Full_histories_df_popinfo, IU_ID_MAPPING %in% Specific_IUs_torun)

# check
Full_histories_df_popinfo_extraIUs_March25_2022 <- subset(Full_histories_df_popinfo_extraIUs_March25, Year == 2022)

#write.csv(Full_histories_df_popinfo_extraIUs_March25, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_popinfo_extraIUs_March25.csv")


# ====================================== #
#  check if all have 51 entries (1975 - 2022) #

# check if any IUs do not have 51 entries #
count_per_mapping <- Full_histories_df_popinfo %>%
  group_by(IU_ID_MAPPING) %>%
  summarise(count = n()) # Count the number of rows for each unique IU_ID_MAPPING

unique_mappings_not_51 <- count_per_mapping %>%
  filter(count != 51) %>%
  pull(IU_ID_MAPPING) # Filter out the unique IU_ID_MAPPING values that do not have 51 rows

unique_mappings_not_51

# Appear to be duplicated rows in ESPEN years for Nigerian Ius: - remove duplicates

Full_histories_df_popinfo_CLEAN <- Full_histories_df_popinfo

Full_histories_df_popinfo_CLEAN <- Full_histories_df_popinfo_CLEAN[!duplicated(Full_histories_df_popinfo_CLEAN), ]

# check if any IUs do not have 51 entries #
count_per_mapping <- Full_histories_df_popinfo_CLEAN %>%
  group_by(IU_ID_MAPPING) %>%
  summarise(count = n()) # Count the number of rows for each unique IU_ID_MAPPING

unique_mappings_not_51 <- count_per_mapping %>%
  filter(count != 51) %>%
  pull(IU_ID_MAPPING) # Filter out the unique IU_ID_MAPPING values that do not have 51 rows

unique_mappings_not_51


# Full_histories_df_popinfo <- Full_histories_df_popinfo %>%
#   filter(!IU_ID_MAPPING %in% c(7424, 7441, 19408, 49752, 49821))

# Full_histories_df_popinfo <- Full_histories_df_popinfo %>%
#   filter(!IU_ID_MAPPING %in% c(19408, 49752, 49821)) # removed 2 CMR IUs (now these are included in histories) - March 25'



# ==================================================== #
#   Subset specific countries for December deadline    #
# ==================================================== #

Full_histories_df_popinfo_4countries <- subset(Full_histories_df_popinfo, ADMIN0ISO3 %in% c("SSD","CMR","ETH","GHA"))

# Nov 2023 #
#write.csv(Full_histories_df_popinfo_4countries, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_popinfo_4countries_301123.csv")

# May 2024 #
#write.csv(Full_histories_df_popinfo_4countries, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_popinfo_4countries_230524.csv")


# make list of IUs for 0.5 and 0.6 rho inputs

Full_histories_df_popinfo_4countries <- Full_histories_df_popinfo_4countries_301123

Full_histories_df_popinfo_4countries_lastyr_2022 <- subset(Full_histories_df_popinfo_4countries, Year == 2022)
length(unique(Full_histories_df_popinfo_4countries_lastyr_2022$IU_ID_MAPPING))

# rho = 0.5 IUs
Subset_rho0.5 <- subset(Full_histories_df_popinfo_4countries_lastyr_2022, co_endemicity %in% c("oncho", "oncho,LF", "oncho,LF,loa meso",
                                                                                               "oncho,loa meso", "oncho,LF,loa hypo",
                                                                                               "oncho,loa hypo"))
Subset_rho0.5 <- Subset_rho0.5[, c("IU_ID_MAPPING","IUs_NAME_MAPPING","IU_CODE_MAPPING", "IUID", "co_endemicity")]

IUs_rho0.5_vec <- unique(Subset_rho0.5$IU_ID_MAPPING)
length(IUs_rho0.5_vec)

IUs_rho0.5_vec2 <- unique(Subset_rho0.5$IU_CODE_MAPPING)
length(IUs_rho0.5_vec2)

IUs_i_list <- data.frame(IU_ID = Subset_rho0.5$IU_CODE_MAPPING, IU_ID_MAPPING = Subset_rho0.5$IU_ID_MAPPING)

#write.csv(IUs_i_list, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/IUs_i_list.csv")

# rho = 0.6 IUs
Subset_rho0.6 <- subset(Full_histories_df_popinfo_4countries_lastyr_2022, co_endemicity %in% c("oncho,LF,loa hyper", "oncho,loa hyper"))
Subset_rho0.6 <- Subset_rho0.6[, c("IU_ID_MAPPING","IUs_NAME_MAPPING","IU_CODE_MAPPING", "IUID", "co_endemicity")]

IUs_rho0.6_vec <- unique(Subset_rho0.6$IU_ID_MAPPING)
length(IUs_rho0.6_vec)

IUs_rho0.6_vec2 <- unique(Subset_rho0.6$IU_CODE_MAPPING)
length(IUs_rho0.6_vec2)

IUs_j_list <- data.frame(IU_ID = Subset_rho0.6$IU_CODE_MAPPING, IU_ID_MAPPING = Subset_rho0.6$IU_ID_MAPPING)

#write.csv(Subset_rho0.5, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Subset_rho0.5.csv")
#write.csv(Subset_rho0.6, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Subset_rho0.6.csv")

#write.csv(IUs_j_list, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/IUs_j_list.csv")

# =========================================== #
#    Fine new IUs for ETH (from 2022 update)  #

unique(Full_histories_df_popinfo_4countries$new_MDA_IUs_2022)

subset_new_IUs_ETH <- subset(Full_histories_df_popinfo_4countries, new_MDA_IUs_2022 == "ESPEN update: first MDA round in 2022")

length(unique(subset_new_IUs_ETH$IU_ID_MAPPING))
unique(subset_new_IUs_ETH$IU_CODE_MAPPING)

# ==========================================================================
#                   Make plots                                             #

#Full_histories_df_popinfo_310724 <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_popinfo_310724.csv")
Full_histories_df_minimal_lastyr_2022 <- subset(Full_histories_df_popinfo_310724, Year == 2022)

Full_histories_df_minimal_lastyr_2022 <- subset(Full_histories_df_popinfo, Year == 2022)


# maps & frequency distributions ~ country #
ESPEN_IUs <- st_read('C:/Users/mad206/OneDrive/Endgame/Endgame IUs/ESPEN_IU_2021.shp')
all_countries <- unique(Full_histories_df_minimal_lastyr_2022$ADMIN0ISO3)
ESPEN_IUs_ALL <- ESPEN_IUs[which((ESPEN_IUs$ADMIN0ISO3 %in% all_countries)),]
st_geometry_type(ESPEN_IUs_ALL)
st_crs(ESPEN_IUs_ALL)

co_endemicity_maps_final <- ESPEN_IUs_ALL %>%
  left_join(Full_histories_df_minimal_lastyr_2022, by = c("IU_ID" = "IU_ID_MAPPING"))

cbPalette <- c("#99CCFF","#9999FF","#009E73","#CC0000","#FFFF99","#FF6600","#CC0000","#FFFF99","#FF6600")

# ============= #
# Co-endemicity #
# ============= #
ggplot() +
  geom_sf(data = co_endemicity_maps_final, aes(fill = co_endemicity), colour = NA, alpha = 0.7) +
  geom_sf(data = ESPEN_IUs_ALL, aes(), colour = NA, size = 1, fill = NA, alpha = 0.1) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
  coord_sf(xlim = c(-20, 50), ylim = c(38, -35)) +
  theme_bw() +
  scale_fill_manual(values = cbPalette, na.value = "gray") +
  scale_colour_manual(na.value="gray")+
  labs(fill='') +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal")

# just bioko #
ggplot() +
  geom_sf(data = co_endemicity_maps_final, aes(fill = co_endemicity), colour = NA, alpha = 0.7) +
  geom_sf(data = ESPEN_IUs_ALL, aes(), colour = NA, size = 1, fill = NA, alpha = 0.1) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
  coord_sf(xlim = c(8, 10), ylim = c(3, 5)) +
  theme_bw() +
  scale_fill_manual(values = cbPalette, na.value = "gray") +
  scale_colour_manual(na.value="gray")+
  labs(fill='') +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal")

# frequency dist plot by country
freq_table_coendemicity <- table(Full_histories_df_minimal_lastyr_2022$co_endemicity, Full_histories_df_minimal_lastyr_2022$ADMIN0ISO3) # Create a frequency table

freq_df_coendemicity <- as.data.frame(as.table(freq_table_coendemicity)) # Convert the frequency table to a data frame
colnames(freq_df_coendemicity) <- c("co_endemicity", "ADMIN0ISO3", "Frequency") # Rename the columns for better readability

# Create the frequency distribution plot
ggplot(freq_df_coendemicity, aes(x = co_endemicity, y = Frequency, fill = co_endemicity)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  labs(title = "Frequency Distribution of co_endemicity by ADMIN0ISO3",
       x = "Co-endemicity",
       y = "Frequency",
       fill = "Co-endemicity") +
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

# frequency dist plot
freq_table_coendemicity2 <- table(Full_histories_df_minimal_lastyr_2022$co_endemicity) # Create a frequency table

freq_df_coendemicity2 <- as.data.frame(as.table(freq_table_coendemicity2)) # Convert the frequency table to a data frame
colnames(freq_df_coendemicity2) <- c("co_endemicity", "Frequency") # Rename the columns for better readability

# Create the frequency distribution plot
ggplot(freq_df_coendemicity2, aes(x = co_endemicity, y = Frequency, fill = co_endemicity)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  labs(title = "Frequency Distribution of co_endemicity",
       x = "Co-endemicity",
       y = "Frequency",
       fill = "Co-endemicity") +
  scale_fill_manual(values = cbPalette) +
  theme_minimal()+
  theme(axis.text.x = element_blank(),  # Remove x-axis labels
        axis.ticks.x = element_blank(),  # Remove x-axis ticks if needed
        #axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.line = element_line(colour = "black"))

# ========================= #
# Intervention distribution #
# ========================= #
cbPalette<- cbPalette <- c("#CC79A7","#E69F00","#009E73","#F0E442","#0072B2")
ggplot() +
  geom_sf(data = co_endemicity_maps_final, aes(fill = biannual_VC_mapping), colour = NA, alpha = 0.7) +
  geom_sf(data = ESPEN_IUs_ALL, aes(), colour = NA, size = 1, fill = NA, alpha = 0.1) +
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
  geom_sf(data = co_endemicity_maps_final, aes(fill = biannual_VC_mapping), colour = NA, alpha = 0.7) +
  geom_sf(data = ESPEN_IUs_ALL, aes(), colour = NA, size = 1, fill = NA, alpha = 0.1) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
  coord_sf(xlim = c(8, 10), ylim = c(3, 5)) +
  theme_bw() +
  scale_fill_manual(values = cbPalette, na.value = "gray") +
  scale_colour_manual(na.value="gray")+
  labs(fill='') +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal")

# frequency dist plot by country
freq_table_histint <- table(Full_histories_df_minimal_lastyr_2022$biannual_VC_mapping, Full_histories_df_minimal_lastyr_2022$ADMIN0ISO3) # Create a frequency table

freq_df_histint <- as.data.frame(as.table(freq_table_histint)) # Convert the frequency table to a data frame
colnames(freq_df_histint) <- c("Historical_int", "ADMIN0ISO3", "Frequency") # Rename the columns for better readability

# Create the frequency distribution plot
ggplot(freq_df_histint, aes(x = Historical_int, y = Frequency, fill = Historical_int)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Frequency Distribution of historical interventions by ADMIN0ISO3",
       x = "Historical interventions",
       y = "Frequency",
       fill = "Historical interventions") +
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

# frequency dist plot
freq_table_histint2 <- table(Full_histories_df_minimal_lastyr_2022$biannual_VC_mapping) # Create a frequency table

freq_df_histint2 <- as.data.frame(as.table(freq_table_histint2)) # Convert the frequency table to a data frame
colnames(freq_df_histint2) <- c("Historical_int", "Frequency") # Rename the columns for better readability

# Create the frequency distribution plot
ggplot(freq_df_histint2, aes(x = Historical_int, y = Frequency, fill = Historical_int)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Frequency Distribution of Historical interventions",
       x = "Historical interventions",
       y = "Frequency",
       fill = "Historical interventions") +
  scale_fill_manual(values = cbPalette) +
  theme_minimal()+
  theme(axis.text.x = element_blank(),  # Remove x-axis labels
        axis.ticks.x = element_blank(),  # Remove x-axis ticks if needed
        #axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.line = element_line(colour = "black"))


# ============================= #
# CUM_MDA modelled distribution #
# ============================= #
ggplot() +
  geom_sf(data = co_endemicity_maps_final, aes(fill = CUM_MDA_modelled), colour = NA, alpha = 0.7) +
  geom_sf(data = ESPEN_IUs_ALL, aes(), colour = NA, size = 1, fill = NA, alpha = 0.1) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
  labs(fill = "Cumulative modelled MDA rounds")+
  coord_sf(xlim = c(-20, 50), ylim = c(38, -35)) +
  theme_bw() +
  scale_fill_gradient(low = "yellow", high = "darkblue", na.value = "gray") +
  scale_colour_manual(na.value="white") +
  labs(fill='') +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal",
    legend.box = "horizontal"  # Control the position of the legend box
  )

# just bioko #
ggplot() +
  geom_sf(data = co_endemicity_maps_final, aes(fill = CUM_MDA_modelled), colour = NA, alpha = 0.7) +
  geom_sf(data = ESPEN_IUs_ALL, aes(), colour = NA, size = 1, fill = NA, alpha = 0.1) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
  coord_sf(xlim = c(8, 10), ylim = c(3, 5)) +
  theme_bw() +
  scale_fill_gradient(low = "yellow", high = "darkblue", na.value = "gray") +
  scale_colour_manual(na.value="white") +
  labs(fill='') +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal",
    legend.box = "horizontal"  # Control the position of the legend box
  )

# frequency dist plot by country
library(ggplot2)

# Plot the histogram with a gradient fill
ggplot(Full_histories_df_minimal_lastyr_2022, aes(x = CUM_MDA_modelled, fill = CUM_MDA_modelled)) +
  geom_histogram(binwidth = 1, color = "black", position = "identity", alpha = 0.7) +
  scale_fill_gradient(low = "yellow", high = "darkblue", na.value = "gray") +
  labs(x = "Cumulative MDA Continuous Modelled",
       y = "Frequency",
       fill = "Cumulative MDA") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.line = element_line(colour = "black")) +
  facet_wrap(~ADMIN0ISO3, scales = "free_y")  # Separate histograms by ADMIN0ISO3 with free y-axis scaling


# Bin the data manually
binned_data <- Full_histories_df_minimal_lastyr_2022 %>%
  mutate(Cum_MDAcont_modelled_bin = cut(CUM_MDA_modelled, breaks = seq(0, 50, by = 1), include.lowest = TRUE)) %>%
  group_by(ADMIN0ISO3, Cum_MDAcont_modelled_bin) %>%
  summarise(Frequency = n(), .groups = 'drop') %>%
  mutate(Cum_MDAcont_modelled_mid = (as.numeric(sub("\\((.*),.*", "\\1", Cum_MDAcont_modelled_bin)) +
                                       as.numeric(sub(".*,\\s*(.*)\\]", "\\1", Cum_MDAcont_modelled_bin))) / 2)



# Plot the binned data with gradient fill
ggplot(binned_data, aes(x = Cum_MDAcont_modelled_mid, y = Frequency, fill = Cum_MDAcont_modelled_mid)) +
  #geom_bar(stat = "identity", color = "black", alpha = 0.7) +
  geom_bar(stat = "identity", color = NA, alpha = 0.9) +
  scale_fill_gradient(low = "yellow", high = "darkblue", na.value = "gray") +
  scale_x_continuous(breaks = seq(0, 50, by = 10)) +  # Set x-axis ticks every 10 values
  labs(title = "Distribution of Cum_MDAcont_modelled by ADMIN0ISO3",
       x = "Cumulative MDA Continuous Modelled (Midpoints)",
       y = "Frequency",
       fill = "Cumulative MDA rounds") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ADMIN0ISO3, scales = "free_y")


binned_data2 <- Full_histories_df_minimal_lastyr_2022 %>%
  mutate(Cum_MDAcont_modelled_bin = cut(CUM_MDA_modelled, breaks = seq(0, 50, by = 1), include.lowest = TRUE)) %>%
  group_by(Cum_MDAcont_modelled_bin) %>%
  summarise(Frequency = n(), .groups = 'drop') %>%
  mutate(Cum_MDAcont_modelled_mid = (as.numeric(sub("\\((.*),.*", "\\1", Cum_MDAcont_modelled_bin)) +
                                       as.numeric(sub(".*,\\s*(.*)\\]", "\\1", Cum_MDAcont_modelled_bin))) / 2)

# Plot the binned data with gradient fill
ggplot(binned_data2, aes(x = Cum_MDAcont_modelled_mid, y = Frequency, fill = Cum_MDAcont_modelled_mid)) +
  #geom_bar(stat = "identity", color = "black", alpha = 0.7) +
  geom_bar(stat = "identity", color = NA, alpha = 0.9) +
  scale_fill_gradient(low = "yellow", high = "darkblue", na.value = "gray") +
  scale_x_continuous(breaks = seq(0, 50, by = 10)) +  # Set x-axis ticks every 10 values
  labs(title = "Distribution of Cum_MDAcont_modelled",
       x = "Cumulative MDA Continuous Modelled (Midpoints)",
       y = "Frequency",
       fill = "Cumulative MDA rounds") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1))


# ======================== #
# treatment status in 2022 #
# ======================== #

cbPalette <- c("#CC79A7","#E69F00","#009E73","#F0E442","#0072B2")

ggplot() +
  geom_sf(data = co_endemicity_maps_final, aes(fill = trt_status_2022), colour = NA, alpha = 0.7) +
  geom_sf(data = ESPEN_IUs_ALL, aes(), colour = NA, size = 1, fill = NA, alpha = 0.1) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
  coord_sf(xlim = c(-20, 50), ylim = c(38, -35)) +
  theme_bw() +
  scale_fill_manual(values = cbPalette, na.value = "gray") +
  scale_colour_manual(na.value="gray")+
  labs(fill='') +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal")

# bioko #
ggplot() +
  geom_sf(data = co_endemicity_maps_final, aes(fill = trt_status_2022), colour = NA, alpha = 0.7) +
  geom_sf(data = ESPEN_IUs_ALL, aes(), colour = NA, size = 1, fill = NA, alpha = 0.1) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
  coord_sf(xlim = c(8, 10), ylim = c(3, 5)) +
  theme_bw() +
  scale_fill_manual(values = cbPalette, na.value = "gray") +
  scale_colour_manual(na.value="gray")+
  labs(fill='') +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal")

# frequency dist plot by country
freq_table_trtstat22 <- table(Full_histories_df_minimal_lastyr_2022$trt_status_2022, Full_histories_df_minimal_lastyr_2022$ADMIN0ISO3) # Create a frequency table

freq_df_trtstat22 <- as.data.frame(as.table(freq_table_trtstat22)) # Convert the frequency table to a data frame
colnames(freq_df_trtstat22) <- c("trt_status_2022", "ADMIN0ISO3", "Frequency") # Rename the columns for better readability

# Create the frequency distribution plot
ggplot(freq_df_trtstat22, aes(x = trt_status_2022, y = Frequency, fill = trt_status_2022)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Frequency Distribution of treatment status in 2022 by ADMIN0ISO3",
       x = "Treatment status in 2022",
       y = "Frequency",
       fill = "treatment status") +
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

# frequency dist plot
freq_table_trtstat22_2 <- table(Full_histories_df_minimal_lastyr_2022$trt_status_2022) # Create a frequency table

freq_df_trtstat22_2 <- as.data.frame(as.table(freq_table_trtstat22_2)) # Convert the frequency table to a data frame
colnames(freq_df_trtstat22_2) <- c("trt_status_2022", "Frequency") # Rename the columns for better readability

# Create the frequency distribution plot
ggplot(freq_df_trtstat22_2, aes(x = trt_status_2022, y = Frequency, fill = trt_status_2022)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Frequency Distribution of treatment status in 2022",
       x = "Treatment status in 2022",
       y = "Frequency",
       fill = "treatment status") +
  scale_fill_manual(values = cbPalette) +
  theme_minimal()+
  theme(axis.text.x = element_blank(),  # Remove x-axis labels
        axis.ticks.x = element_blank(),  # Remove x-axis ticks if needed
        #axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.line = element_line(colour = "black"))

# ===================================== #
#      Inclusion/Exclusion of countries #
# ===================================== #

histories_updated <- Full_histories_df_minimal_lastyr_2022

histories_updated_APOC <- subset(histories_updated, Control_prog == "APOC")
histories_updated_OCP <- subset(histories_updated, Control_prog == "OCP")

histories_updated_APOC_MDAcont <- subset(histories_updated_APOC, treatment_2026 == "include for treatment from 2026")
histories_updated_APOC_MDAstop <- subset(histories_updated_APOC, treatment_2026 == "exclude for treatment from 2026")

histories_updated_OCP_MDAcont <- subset(histories_updated_OCP, treatment_2026 == "include for treatment from 2026")
histories_updated_OCP_MDAstop <- subset(histories_updated_OCP, treatment_2026 == "exclude for treatment from 2026")

histories_updated_MDAstop <- subset(histories_updated, treatment_2026 == "exclude for treatment from 2026")
histories_updated_MDAstop_underPISnonend <- subset(histories_updated_MDAstop, Endemicity %in% c("Not reported", "Non-endemic", "Endemic (under post-intervention surveillance)"))

histories_updated_MDAcont <- subset(histories_updated, treatment_2026 == "include for treatment from 2026")
histories_updated_MDAcont_underPISnonend <- subset(histories_updated_MDAcont, Endemicity %in% c("Not reported", "Non-endemic", "Endemic (under post-intervention surveillance)"))

#write.csv(histories_updated, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/IUclassifications_230724.csv")

# make a new treatment_2026 col where break down "exclude for treatment from 2026" to also include "histories_updated$treatment_2026: under PIS or nonendemic"
IUs_to_change2 <- unique(histories_updated_MDAstop_underPISnonend$IU_CODE_MAPPING)

histories_updated$treatment_2026_v2 <- ifelse(histories_updated$IU_CODE_MAPPING %in% IUs_to_change2, "exclude for treatment from 2026: under PIS/non-endemic", histories_updated$treatment_2026)
histories_updated$treatment_2026_v2 <- ifelse(histories_updated$ADMIN0ISO3 %in% c("SEN", "SDN"), "exclude for treatment from 2026: under PIS/non-endemic", histories_updated$treatment_2026_v2)

cbPalette <- c("#9999FF","slateblue4", "indianred1")

unique(histories_updated$treatment_2026_v2)

histories_updated$BAU_label <- ifelse(histories_updated$IU_CODE_MAPPING %in% c("SDN0566466116"), "no intervention", histories_updated$BAU_label)
histories_updated$business_as_usual <- ifelse(histories_updated$IU_CODE_MAPPING %in% c("SDN0566466116"), 0, histories_updated$business_as_usual)

co_endemicity_maps_final2 <- ESPEN_IUs_ALL %>%
  left_join(histories_updated, by = c("IU_ID" = "IU_ID_MAPPING"))

ggplot() +
  geom_sf(data = co_endemicity_maps_final2, aes(fill = treatment_2026_v2), colour = NA, alpha = 0.7) +
  geom_sf(data = ESPEN_IUs_ALL, aes(), colour = NA, size = 1, fill = NA, alpha = 0.1) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
  coord_sf(xlim = c(-20, 50), ylim = c(38, -35)) +
  theme_bw() +
  scale_fill_manual(values = cbPalette, na.value = "gray78") +
  #scale_fill_discrete(na.value = "gray78") +  # Use scale_fill_discrete with na.value
  labs(fill='') +
  theme(
    legend.position = "right",  # Place the legend to the right
    legend.direction = "vertical"
  )

ggplot() +
  geom_sf(data = co_endemicity_maps_final2, aes(fill = treatment_2026_v2), colour = NA, alpha = 0.7) +
  geom_sf(data = ESPEN_IUs_ALL, aes(), colour = NA, size = 1, fill = NA, alpha = 0.1) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
  coord_sf(xlim = c(8, 10), ylim = c(3, 5)) +
  theme_bw() +
  scale_fill_manual(values = cbPalette, na.value = "gray78") +
  #scale_fill_discrete(na.value = "gray78") +  # Use scale_fill_discrete with na.value
  labs(fill='') +
  theme(
    legend.position = "right",  # Place the legend to the right
    legend.direction = "vertical"
  )

# frequency dist plot by country
freq_table_inclfrom26 <- table(histories_updated$treatment_2026_v2, histories_updated$ADMIN0ISO3) # Create a frequency table

freq_df_inclfrom26 <- as.data.frame(as.table(freq_table_inclfrom26)) # Convert the frequency table to a data frame
colnames(freq_df_inclfrom26) <- c("inclfrom26", "ADMIN0ISO3", "Frequency") # Rename the columns for better readability

# Create the frequency distribution plot
ggplot(freq_df_inclfrom26, aes(x = inclfrom26, y = Frequency, fill = inclfrom26)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Frequency Distribution of IUs included from 2026 by ADMIN0ISO3",
       x = "IU status from 2026",
       y = "Frequency",
       fill = "IU status from 2026") +
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

# frequency dist plot
freq_table_inclfrom26_2 <- table(histories_updated$treatment_2026_v2) # Create a frequency table

freq_df_inclfrom26_2 <- as.data.frame(as.table(freq_table_inclfrom26_2)) # Convert the frequency table to a data frame
colnames(freq_df_inclfrom26_2) <- c("inclfrom26", "Frequency") # Rename the columns for better readability

# Create the frequency distribution plot
ggplot(freq_df_inclfrom26_2, aes(x = inclfrom26, y = Frequency, fill = inclfrom26)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Frequency Distribution of treatment status in 2022",
       x = "Treatment status in 2022",
       y = "Frequency",
       fill = "treatment status") +
  scale_fill_manual(values = cbPalette) +
  theme_minimal()+
  theme(axis.text.x = element_blank(),  # Remove x-axis labels
        axis.ticks.x = element_blank(),  # Remove x-axis ticks if needed
        #axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.line = element_line(colour = "black"))


# ggplot() +
#   geom_sf(data = african_countries, aes(), fill = NA, colour = "black", linewidth = 0.5) +
#   geom_sf(data = co_endemicity_maps_final, aes(fill = treatment_2026), colour = NA, alpha = 0.7) +
#   geom_sf(data = ESPEN_IUs_ALL, aes(), colour = NA, size = 1, fill = NA, alpha = 0.1) +
#   coord_sf(xlim = c(-20, 50), ylim = c(38, -35)) +
#   theme_minimal() +
#   scale_fill_manual(values = cbPalette, na.value = "gray78") +
#   labs(fill='') +
#   theme(
#     legend.position = "right",  # Place the legend to the right
#     legend.direction = "vertical",
#     panel.grid.major = element_blank(),  # Remove major gridlines
#     panel.grid.minor = element_blank(),  # Remove minor gridlines
#     panel.grid = element_blank()          # Remove all gridlines
#   )

# ================ #
# BAU status       #
# ================ #

cbPalette<- cbPalette <- c("#CC79A7","#E69F00","#009E73","#F0E442","#0072B2","indianred1")
# Full_histories_df_popinfo_BAU0a <- subset(Full_histories_df_popinfo, business_as_usual == -1) # no intervention (and ABR = 0)
# Full_histories_df_popinfo_BAU0 <- subset(Full_histories_df_popinfo, business_as_usual == 0) # no intervention
# Full_histories_df_popinfo_BAU1 <- subset(Full_histories_df_popinfo, business_as_usual == 1) # annual MDA only
# Full_histories_df_popinfo_BAU2 <- subset(Full_histories_df_popinfo, business_as_usual == 2) # annual MDA + VC
# Full_histories_df_popinfo_BAU3 <- subset(Full_histories_df_popinfo, business_as_usual == 3) # annual MDA + Ve
# Full_histories_df_popinfo_BAU4 <- subset(Full_histories_df_popinfo, business_as_usual == 4) # biannual MDA
# Full_histories_df_popinfo_BAU5 <- subset(Full_histories_df_popinfo, business_as_usual == 5) # biannual MDA + VC
# Full_histories_df_popinfo_BAU6 <- subset(Full_histories_df_popinfo, business_as_usual == 6) # biannual MDA + Ve
ggplot() +
  geom_sf(data = co_endemicity_maps_final2, aes(fill = as.factor(BAU_label)), colour = NA, alpha = 0.7) +
  geom_sf(data = ESPEN_IUs_ALL, aes(), colour = NA, size = 1, fill = NA, alpha = 0.1) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
  coord_sf(xlim = c(-20, 50), ylim = c(38, -35)) +
  theme_bw() +
  scale_fill_manual(values = cbPalette, na.value = "gray") +
  scale_colour_manual(na.value="gray")+
  labs(fill='') +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal")

# Bioko #
ggplot() +
  geom_sf(data = co_endemicity_maps_final, aes(fill = as.factor(BAU_label)), colour = NA, alpha = 0.7) +
  geom_sf(data = ESPEN_IUs_ALL, aes(), colour = NA, size = 1, fill = NA, alpha = 0.1) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
  coord_sf(xlim = c(8, 10), ylim = c(3, 5)) +
  theme_bw() +
  scale_fill_manual(values = cbPalette, na.value = "gray") +
  scale_colour_manual(na.value="gray")+
  labs(fill='') +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal")

# frequency dist plot by country
freq_table_BAU <- table(histories_updated$BAU_label, histories_updated$ADMIN0ISO3) # Create a frequency table

freq_df_BAU <- as.data.frame(as.table(freq_table_BAU)) # Convert the frequency table to a data frame
colnames(freq_df_BAU) <- c("BAU", "ADMIN0ISO3", "Frequency") # Rename the columns for better readability

# Create the frequency distribution plot
ggplot(freq_df_BAU, aes(x = BAU, y = Frequency, fill = BAU)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Frequency Distribution of business-as-usual status from 2023 by ADMIN0ISO3",
       x = "business-as-usual status from 2023",
       y = "Frequency",
       fill = "business-as-usual status from 2023") +
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

# frequency dist plot
freq_table_BAU2 <- table(histories_updated$BAU_label) # Create a frequency table

freq_df_BAU2 <- as.data.frame(as.table(freq_table_BAU2)) # Convert the frequency table to a data frame
colnames(freq_df_BAU2) <- c("BAU", "Frequency") # Rename the columns for better readability

# Create the frequency distribution plot
ggplot(freq_df_BAU2, aes(x = BAU, y = Frequency, fill = BAU)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Frequency Distribution of business-as-usual status from 2023",
       x = "business-as-usual status from 2023",
       y = "Frequency",
       fill = "business-as-usual status from 2023") +
  scale_fill_manual(values = cbPalette) +
  theme_minimal()+
  theme(axis.text.x = element_blank(),  # Remove x-axis labels
        axis.ticks.x = element_blank(),  # Remove x-axis ticks if needed
        #axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.line = element_line(colour = "black"))


# save basic dataframe to share with NTD MC for final selection of IUs to run for 2026-2040 scenarios (August 2024)
final_df_IUstorun <- histories_updated[, c("IU_ID_MAPPING","IUs_NAME_MAPPING","IU_CODE_MAPPING", "IUID", "ADMIN0ISO3","Control_prog",
                                           "endemicity_baseline","biannual_VC_mapping",
                                           "CUM_MDA_modelled","trt_status_2022","trt_status_2022_v2","co_endemicity",
                                           "treatment_2026","business_as_usual", "BAU_label")]

final_df_IUstorun$IUs_to_include_frm2026 <- ifelse(final_df_IUstorun$treatment_2026 %in% c("include for treatment from 2026"), 1, 0)

# write.csv(final_df_IUstorun, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/IUs_status_Endgame_190824.csv")

final_df_IUstorun_select <- subset(final_df_IUstorun, IUs_to_include_frm2026 == 1)
final_df_IUstorun_select_IUCODEMAPPING <- final_df_IUstorun_select$IU_CODE_MAPPING

# write(final_df_IUstorun_select_IUCODEMAPPING, file = "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/inal_df_IUstorun_select_IUCODEMAPPING.txt")

# ============================ #
# ESPEN update status for 2022 #

cbPalette2<- cbPalette <- c("#CC0000","#FF9966","#99FF66")

ggplot() +
  geom_sf(data = co_endemicity_maps_final, aes(fill = new_MDA_IUs_2022), colour = NA, alpha = 0.7) +
  geom_sf(data = ESPEN_IUs_ALL, aes(), colour = NA, size = 1, fill = NA, alpha = 0.1) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
  coord_sf(xlim = c(-20, 50), ylim = c(38, -35)) +
  theme_bw() +
  scale_fill_manual(values = cbPalette2, na.value = "gray") +
  scale_colour_manual(na.value="gray")+
  labs(fill='') +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal")

# Nigeria - ESPEN 2022 update #
ggplot() +
  geom_sf(data = co_endemicity_maps_final, aes(fill = new_MDA_IUs_2022), colour = NA, alpha = 0.7) +
  geom_sf(data = ESPEN_IUs_ALL, aes(), colour = "darkgrey", size = 1, fill = NA, alpha = 0.1) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", linewidth = 1) +
  coord_sf(xlim = c(3, 14), ylim = c(4, 13.5)) +
  theme_bw() +
  scale_fill_manual(values = cbPalette2, na.value = "gray") +
  scale_colour_manual(na.value="gray")+
  labs(fill='') +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal")

# Ethiopia - ESPEN 2022 update #
ggplot() +
  geom_sf(data = co_endemicity_maps_final, aes(fill = new_MDA_IUs_2022), colour = NA, alpha = 0.7) +
  geom_sf(data = ESPEN_IUs_ALL, aes(), colour = "darkgrey", size = 1, fill = NA, alpha = 0.1) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", linewidth = 1) +
  coord_sf(xlim = c(33.5, 40), ylim = c(4.7, 14.3)) +
  theme_bw() +
  scale_fill_manual(values = cbPalette2, na.value = "gray") +
  scale_colour_manual(na.value="gray")+
  labs(fill='') +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal")

# Uganda - ESPEN 2022 update #
ggplot() +
  geom_sf(data = co_endemicity_maps_final, aes(fill = new_MDA_IUs_2022), colour = NA, alpha = 0.7) +
  geom_sf(data = ESPEN_IUs_ALL, aes(), colour = "darkgrey", size = 1, fill = NA, alpha = 0.1) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", linewidth = 1) +
  coord_sf(xlim = c(29.7, 35), ylim = c(-1.2, 4.1)) +
  theme_bw() +
  scale_fill_manual(values = cbPalette2, na.value = "gray") +
  scale_colour_manual(na.value="gray")+
  labs(fill='') +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal")

iu_mapping_count <- Full_histories_df_popinfo %>%
  group_by(new_MDA_IUs_2022) %>%
  summarise(num_iu_mapping = n_distinct(IU_ID_MAPPING))
iu_mapping_count

iu_mapping_count <- Full_histories_df_popinfo %>%
  group_by(ADMIN0ISO3, new_MDA_IUs_2022) %>%
  summarise(num_iu_mapping = n_distinct(IU_ID_MAPPING))
iu_mapping_count

iu_mapping_table <- Full_histories_df_popinfo %>%
  group_by(ADMIN0ISO3, new_MDA_IUs_2022) %>%
  summarise(num_iu_mapping = n_distinct(IU_ID_MAPPING)) %>%
  ungroup() %>%
  spread(new_MDA_IUs_2022, num_iu_mapping, fill = 0)
iu_mapping_table

# ================================================= #
#   Identify all IUs for new endgame runs           #

Full_histories_df_popinfo_3countries_210324 <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_popinfo_3countries_210324.csv")
Full_histories_df_popinfo_210324 <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_popinfo_210324.csv")

IU_ID_3countries <- data.frame(IU_ID = unique(Full_histories_df_popinfo_3countries_210324$IU_ID_MAPPING))
IUID_3countries <- data.frame(IUID = unique(Full_histories_df_popinfo_3countries_210324$IUID))

IU_3countries <- cbind(IU_ID_3countries, IUID_3countries)

IU_ID <- data.frame(IU_ID = unique(Full_histories_df_popinfo_210324$IU_ID_MAPPING))
IUID <- data.frame(IUID = unique(Full_histories_df_popinfo_210324$IUID))

IU <- cbind(IU_ID, IUID)

# write.csv(IU_3countries, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/IU_3countries.csv")
# write.csv(IU, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/IU.csv")



# ================================================= #
#   Identify a and b IUs lists for new endgame runs #

Full_histories_df_popinfo_3countries_210324 <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_popinfo_3countries_210324.csv")

Full_histories_df_popinfo_3countries_minus1IUs <- subset(Full_histories_df_popinfo_3countries_210324, business_as_usual == -1)
Full_histories_df_popinfo_3countries_restIUs <- subset(Full_histories_df_popinfo_3countries_210324, business_as_usual %in% c(0,1,4,5))

IU_ID_minus1IUs_3countries <- data.frame(IU_ID = unique(Full_histories_df_popinfo_3countries_minus1IUs$IU_ID_MAPPING)) # b IUs
IU_ID_restIUs_3countries <- data.frame(IU_ID = unique(Full_histories_df_popinfo_3countries_restIUs$IU_ID_MAPPING)) # a IUs

IUID_minus1IUs_3countries <- data.frame(IUID = unique(Full_histories_df_popinfo_3countries_minus1IUs$IUID)) # b IUs
IUID_restIUs_3countries <- data.frame(IUID = unique(Full_histories_df_popinfo_3countries_restIUs$IUID)) # a IUs

IU_bIUs_3countries <- cbind(IU_ID_minus1IUs_3countries, IUID_minus1IUs_3countries)
IU_aIUs_3countries <- cbind(IU_ID_restIUs_3countries, IUID_restIUs_3countries)

IUcodemapping_minus1IUs_3countries <- data.frame(IUID = unique(Full_histories_df_popinfo_3countries_minus1IUs$IU_CODE_MAPPING)) # b IUs
IUcodemapping_restIUs_3countries <- data.frame(IUID = unique(Full_histories_df_popinfo_3countries_restIUs$IU_CODE_MAPPING)) # a IUs

IU_bIUs_3countries <- cbind(IU_bIUs_3countries, IUcodemapping_minus1IUs_3countries)
IU_aIUs_3countries <- cbind(IU_aIUs_3countries, IUcodemapping_restIUs_3countries)

# write.csv(IU_aIUs_3countries, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/IU_aIUs_3countries.csv")
# write.csv(IU_bIUs_3countries, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/IU_bIUs_3countries.csv")

Full_histories_df_popinfo_3countries_1IUs <- subset(Full_histories_df_popinfo_3countries_210324, business_as_usual == 1)
Full_histories_df_popinfo_3countries_4and5IUs <- subset(Full_histories_df_popinfo_3countries_210324, business_as_usual %in% c(4,5))

IU_ID_1IUs_3countries <- data.frame(IU_ID = unique(Full_histories_df_popinfo_3countries_1IUs$IU_ID_MAPPING)) # b IUs
IU_ID_4and5IUs_3countries <- data.frame(IU_ID = unique(Full_histories_df_popinfo_3countries_4and5IUs$IU_ID_MAPPING)) # a IUs

IUID_1IUs_3countries <- data.frame(IUID = unique(Full_histories_df_popinfo_3countries_1IUs$IUID)) # b IUs
IUID_4and5IUs_3countries <- data.frame(IUID = unique(Full_histories_df_popinfo_3countries_4and5IUs$IUID)) # a IUs

IU_cIUs_3countries <- cbind(IU_ID_1IUs_3countries, IUID_1IUs_3countries)
IU_dIUs_3countries <- cbind(IU_ID_4and5IUs_3countries, IUID_4and5IUs_3countries)

IUcodemapping_1IUs_3countries <- data.frame(IUID = unique(Full_histories_df_popinfo_3countries_1IUs$IU_CODE_MAPPING)) # b IUs
IUcodemapping_4and5IUs_3countries <- data.frame(IUID = unique(Full_histories_df_popinfo_3countries_4and5IUs$IU_CODE_MAPPING)) # a IUs

IU_cIUs_3countries <- cbind(IU_cIUs_3countries, IUcodemapping_1IUs_3countries)
IU_dIUs_3countries <- cbind(IU_dIUs_3countries, IUcodemapping_4and5IUs_3countries)

# write.csv(IU_cIUs_3countries, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/IU_cIUs_3countries.csv")
# write.csv(IU_dIUs_3countries, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/IU_dIUs_3countries.csv")

# ================================================================= #
#   Identify a and b IUs lists for new endgame runs - ALL COUNTRIES #

Full_histories_df_popinfo_210324 <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_popinfo_210324.csv")

Full_histories_df_popinfo_minus1IUs <- subset(Full_histories_df_popinfo_210324, business_as_usual == -1)
Full_histories_df_popinfo_restIUs <- subset(Full_histories_df_popinfo_210324, business_as_usual %in% c(0,1,4,5))

IU_ID_minus1IUs <- data.frame(IU_ID = unique(Full_histories_df_popinfo_minus1IUs$IU_ID_MAPPING)) # b IUs
IU_ID_restIUs <- data.frame(IU_ID = unique(Full_histories_df_popinfo_restIUs$IU_ID_MAPPING)) # a IUs

IUID_minus1IUs <- data.frame(IUID = unique(Full_histories_df_popinfo_minus1IUs$IUID)) # b IUs
IUID_restIUs <- data.frame(IUID = unique(Full_histories_df_popinfo_restIUs$IUID)) # a IUs

IU_bIUs <- cbind(IU_ID_minus1IUs, IUID_minus1IUs)
IU_aIUs <- cbind(IU_ID_restIUs, IUID_restIUs)

# IUcodemapping_minus1IUs <- data.frame(IUID = unique(Full_histories_df_popinfo_minus1IUs$IU_CODE_MAPPING)) # b IUs
# IUcodemapping_restIUs <- data.frame(IUID = unique(Full_histories_df_popinfo_restIUs$IU_CODE_MAPPING)) # a IUs
#
# IU_bIUs <- cbind(IU_bIUs, IUcodemapping_minus1IUs)
# IU_aIUs <- cbind(IU_aIUs, IUcodemapping_restIUs)

# write.csv(IU_aIUs, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/IU_aIUs.csv")
# write.csv(IU_bIUs, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/IU_bIUs.csv")

Full_histories_df_popinfo_1IUs <- subset(Full_histories_df_popinfo_210324, business_as_usual == 1)
Full_histories_df_popinfo_4and5IUs <- subset(Full_histories_df_popinfo_210324, business_as_usual %in% c(4,5))

IU_ID_1IUs <- data.frame(IU_ID = unique(Full_histories_df_popinfo_1IUs$IU_ID_MAPPING)) # b IUs
IU_ID_4and5IUs <- data.frame(IU_ID = unique(Full_histories_df_popinfo_4and5IUs$IU_ID_MAPPING)) # a IUs

IUID_1IUs <- data.frame(IUID = unique(Full_histories_df_popinfo_1IUs$IUID)) # b IUs
IUID_4and5IUs <- data.frame(IUID = unique(Full_histories_df_popinfo_4and5IUs$IUID)) # a IUs

IU_cIUs <- cbind(IU_ID_1IUs, IUID_1IUs)
IU_dIUs <- cbind(IU_ID_4and5IUs, IUID_4and5IUs)

# IUcodemapping_1IUs <- data.frame(IUID = unique(Full_histories_df_popinfo_1IUs$IU_CODE_MAPPING)) # b IUs
# IUcodemapping_4and5IUs <- data.frame(IUID = unique(Full_histories_df_popinfo_4and5IUs$IU_CODE_MAPPING)) # a IUs
#
# IU_cIUs <- cbind(IU_cIUs, IUcodemapping_1IUs)
# IU_dIUs <- cbind(IU_dIUs, IUcodemapping_4and5IUs)

# write.csv(IU_cIUs, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/IU_cIUs.csv")
# write.csv(IU_dIUs, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/IU_dIUs.csv")


# Full_most_recent <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_minimal_lastyr_2022_170524.csv")

unique(Full_most_recent$co_endemicity)
Full_most_recent_loahyper <- subset(Full_most_recent, co_endemicity %in% c("oncho,loa hyper", "oncho,LF,loa hyper"))

