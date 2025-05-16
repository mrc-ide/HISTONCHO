# ==========================================================================
#                   Make plots                                             #
#==========================================================================#

library(ggplot2)
library(cowplot)
library(dplyr)
library(sf)
library(readxl)
library(ragg)


# Set the file path to the shapefile
shapefile_path <- "C:/Users/mad206/OneDrive/Endgame/OCP mapping/African countries/Africa_Boundaries.shp"
# Read the shapefile
african_countries <- st_read(dsn = shapefile_path)
# View the attributes and structure of the shapefile
summary(african_countries)
# oceans shapefile
oceans_shp <- st_read('C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/oceans_shapefile/ne_10m_ocean.shp')



# Calculate the centroids for the african_countries shapefile
# Step 1: Validate and repair the geometry of african_countries
african_countries_valid <- st_make_valid(african_countries)
african_centroids <- st_centroid(african_countries_valid)

african_countries_toinclude <- c("Angola", "Burundi", "Benin", "Burkina Faso", "Central African Republic",
                                "Côte d'Ivoire", "Cameroon", "Democratic Republic of the Congo", "Republic of Congo",
                                "Ethiopia", "Gabon", "Ghana", "Guinea", "Guinea-Bissau","Liberia", "Mali", "Malawi",
                                "Niger", "Nigeria", "Sudan", "Senegal", "Sierra Leone", "South Sudan", "Chad",
                                "Togo", "Tanzania", "Uganda")

african_centroids_oncho <- subset(african_centroids, NAME_0 %in% african_countries_toinclude)

african_centroids_oncho <- african_centroids_oncho %>%
  mutate(NAME_0 = case_when(
    NAME_0 == "Republic of Congo" ~ "Congo",  # Replace "Republic of Congo" with "Congo"
    NAME_0 == "Democratic Republic of the Congo" ~ "Democratic Republic
of the Congo",  # Add a line break in "Democratic Republic of Congo"
    TRUE ~ NAME_0  # Keep other country names unchanged
  ))

# african_centroids_oncho <- african_centroids_oncho %>%
#   mutate(
#     offset_y = ifelse(NAME_0 == "Democratic Republic
# of Congo", 0.5, 0)  # Offset by 0.5 units for DRC
#   )

#Full_histories_df_popinfo_310724 <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_popinfo_310724.csv")
#Full_histories_df_popinfo_111124 <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_popinfo_111124.csv")
#Full_histories_df_popinfo <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_popinfo_260325.csv")

# APRIL 25'
#Full_histories_df_popinfo <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_popinfo_070425.csv")
Full_histories_df_popinfo <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_popinfo_290425.csv") # FINAL 


# ================================== #
#    check where 2139 have come from #

Full_APOC_histories_df_minimal_lastyr_2022_290425 <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_APOC_histories_df_minimal_lastyr_2022_290425.csv")


# # ALL IUS with MDA and TREATMENT NAIVE (APRIL 2025)
# Full_histories_df_popinfo <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_popinfo_ALL_minimal_070425_listlabels.csv")

# Check when last MDA round by MDA status 2022   #
add_last_year_MDA <- function(df) {
  df %>%
    group_by(IU_ID_MAPPING) %>%
    mutate(
      last_year_MDA = {
        valid_years <- Year[!is.na(as.numeric(Cov.in2)) & Year <= 2022]
        if (length(valid_years) > 0) max(valid_years, na.rm = TRUE) else NA
      }
    ) %>%
    ungroup()
}

Full_histories_df_popinfo <- add_last_year_MDA(Full_histories_df_popinfo)

Full_histories_df_popinfo$year_tmp <- Full_histories_df_popinfo$Year
Full_histories_df_popinfo$cov.in2_tmp <- Full_histories_df_popinfo$Cov.in2


Full_histories_df_minimal_lastyr_2022 <- subset(Full_histories_df_popinfo, Year == 2022)
Full_histories_df_minimal_lastyr_2013 <- subset(Full_histories_df_popinfo, Year == 2013)
Full_histories_df_minimal_lastyr_2013_CUMMDA0 <- subset(Full_histories_df_minimal_lastyr_2013, Cum_MDA_ESPEN == 0)

nrow(Full_histories_df_minimal_lastyr_2022) # 2130 (April 25')

table(Full_histories_df_minimal_lastyr_2022$Control_prog)

# NOW (END OF APRIL 25' - FINAL)
# APOC : 1517
# OCP : 613


# =========================================================== #
#         Subset those just in Endgame                        #

Full_histories_df_minimal_lastyr_2022_check <- subset(Full_histories_df_minimal_lastyr_2022, !is.na(Control_prog)) # 2138 IUs
nrow(Full_histories_df_minimal_lastyr_2022_check)
Full_histories_df_minimal_lastyr_2022_check2 <- subset(Full_histories_df_minimal_lastyr_2022, is.na(Control_prog)) # 586 IUs (treatment naive IUs)
nrow(Full_histories_df_minimal_lastyr_2022_check2) # 0 when only endgame IUs included (April 25')

# ================================================================ #
#         Those in Endgame, only keep Treatment naive IUs in Gabon #

Full_histories_df_minimal_lastyr_2022_check_trtnaive <- subset(Full_histories_df_minimal_lastyr_2022_check, trt_status_2022 == "Treatment naive")

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
    !IUID %in% c("ETH19529", "NGA37017") # these two are both revised_cum_MDA == 0 so treatment naive 
  )

Full_histories_df_minimal_lastyr_2022_check_trtnaive <- subset(Full_histories_df_minimal_lastyr_2022_check, trt_status_2022 == "Treatment naive")

table(Full_histories_df_minimal_lastyr_2022_check_trtnaive$ADMIN0ISO3) # 27 in GAB and 2 in SDN (from CC) - April 25'

Full_histories_df_minimal_lastyr_2022 <- Full_histories_df_minimal_lastyr_2022_check # RESET HERE

# NEED TO DO ABOVE TO FULL DATASET (with all years) #

Full_histories_df_popinfo <- Full_histories_df_popinfo %>%
  filter(
    # Keep "Treatment naive" only if ADMIN0ISO3 is "GAB" or "SDN"
    (trt_status_2022 == "Treatment naive" & ADMIN0ISO3 %in% c("GAB", "SDN")) |
      # Keep any row where trt_status_2022 is not "Treatment naive"
      trt_status_2022 != "Treatment naive",
    # Exclude a specific IU_ID
    !IUID %in% c("ETH19529", "NGA37017") # these two are both revised_cum_MDA == 0 so treatment naive 
  )

length(unique(Full_histories_df_popinfo$IU_ID_MAPPING)) 

Full_histories_df_popinfo_lastyr_2022 <- subset(Full_histories_df_popinfo, Year == 2022)
nrow(Full_histories_df_popinfo_lastyr_2022) # 2130 IUs (April 2025): 2137 - 5 - 2

table(Full_histories_df_popinfo_lastyr_2022$Control_prog)

# 1517 APOC
# 613 OCP 
# April 25'

Full_histories_df_popinfo_lastyr_2025 <- subset(Full_histories_df_popinfo, Year == 2025)

# 1517 in APOC
# 613 in OCP
# 2130 TOTAL

# # SAVE ENDGAME: APOC AND OCP COMBINED (end of APRIL 2025)
# write.csv(Full_histories_df_popinfo, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_popinfo_290425.csv")
# write.csv(Full_histories_df_popinfo_lastyr_2022, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_minimal_lastyr_2022_290425.csv")
# write.csv(Full_histories_df_popinfo_lastyr_2025, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_minimal_lastyr_2025_290425.csv")


# =========================================================== #
#         Subset those just                                   # 


Full_histories_df_minimal_lastyr_2022_OCP <- subset(Full_histories_df_minimal_lastyr_2022, Control_prog == "OCP")
nrow(Full_histories_df_minimal_lastyr_2022_OCP) # 609 IUs - 614 IUs (APril 2025)
Full_histories_df_minimal_lastyr_2022_APOC <- subset(Full_histories_df_minimal_lastyr_2022, Control_prog == "APOC")
nrow(Full_histories_df_minimal_lastyr_2022_APOC) # 1529 IUs - 1522 IUs (April 2025)

# check against lists #

list_2 <- read.table("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Planning final Endgame sims (March-May 25)/list_2.txt", quote="\"", comment.char="")

# ==================================== #
# add co-endemicity to endgame outputs #

iu_projections_scenarios_status_updated <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Full Endgame outputs (Sept 24)/iu_projections_scenarios_status_updated.csv")
# table_freq_IUs_endgame <- as.data.frame(sort(table(iu_projections_scenarios_status_updated$iu_name), decreasing = TRUE))
# write.csv(table_freq_IUs_endgame, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Full Endgame outputs (Sept 24)/table_freq_IUs_endgame.csv")

df_coendemicity <- data.frame(IUID = Full_histories_df_minimal_lastyr_2022$IUID, co_endemicity = Full_histories_df_minimal_lastyr_2022$co_endemicity)
df_coendemicity$loa_coendemic <- ifelse(df_coendemicity$co_endemicity %in% c("oncho,loa hyper", "oncho,loa meso", "oncho,loa hypo",
                                                                             "oncho,LF,loa hypo", "oncho,LF,loa meso", "oncho,LF,loa hyper"), "yes", "no")
endgame_coendemicity_merged <- merge(df_coendemicity, iu_projections_scenarios_status_updated,
                   by.x = "IUID", by.y = "iu_name",
                   all = TRUE)  # Use 'all = TRUE' for a full outer join, or adjust as needed

endgame_coendemicity_merged <- endgame_coendemicity_merged[!endgame_coendemicity_merged$IUID %in% c("SDN66024", "SDN66022"), ]
IUs_nonloacoendemic_df <- subset(endgame_coendemicity_merged, loa_coendemic == "no")
unique(IUs_nonloacoendemic_df$co_endemicity)
IUs_nonloacoendemic_vec <- IUs_nonloacoendemic_df$IUID
# writeLines(IUs_nonloacoendemic_vec, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Full Endgame outputs (Sept 24)/IUs_notloa_coendemic.txt")

# maps & frequency distributions ~ country #
ESPEN_IUs <- st_read('C:/Users/mad206/OneDrive/Endgame/Endgame IUs/ESPEN_IU_2021.shp')
all_countries <- unique(Full_histories_df_minimal_lastyr_2022$ADMIN0ISO3)
ESPEN_IUs_ALL <- ESPEN_IUs[which((ESPEN_IUs$ADMIN0ISO3 %in% all_countries)),]
st_geometry_type(ESPEN_IUs_ALL)
st_crs(ESPEN_IUs_ALL)

co_endemicity_maps_final <- ESPEN_IUs_ALL %>%
  left_join(Full_histories_df_minimal_lastyr_2022, by = c("IU_ID" = "IU_ID_MAPPING"))

co_endemicity_maps_final$phase2 <- ifelse(co_endemicity_maps_final$Control_prog == "APOC", "APOC", co_endemicity_maps_final$PHASE)

# =====================# 
# VERSION 1: JUST THOSE (n = 2132) in Endgame - only treatment naive in GAB and SDN
co_endemicity_maps_final_onlyendgame <- co_endemicity_maps_final

# # =================== #
# # VERSION 2: ALL IUS
# Full_histories_df_popinfo <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_popinfo_ALL_minimal_070425_listlabels.csv")
# Full_histories_df_minimal_lastyr_2022 <- subset(Full_histories_df_popinfo, Year == 2022)

# # maps & frequency distributions ~ country #
# ESPEN_IUs <- st_read('C:/Users/mad206/OneDrive/Endgame/Endgame IUs/ESPEN_IU_2021.shp')
# all_countries <- unique(Full_histories_df_minimal_lastyr_2022$ADMIN0ISO3)
# ESPEN_IUs_ALL <- ESPEN_IUs[which((ESPEN_IUs$ADMIN0ISO3 %in% all_countries)),]
# st_geometry_type(ESPEN_IUs_ALL)
# st_crs(ESPEN_IUs_ALL)
# 
# co_endemicity_maps_final <- ESPEN_IUs_ALL %>%
#   left_join(Full_histories_df_minimal_lastyr_2022, by = c("IU_ID" = "IU_ID_MAPPING"))
# 
# co_endemicity_maps_final$phase2 <- ifelse(co_endemicity_maps_final$Control_prog == "APOC", "APOC", co_endemicity_maps_final$PHASE)
# 
# co_endemicity_maps_final_ALLIUs <- co_endemicity_maps_final

# ======================================== #
#        frequency of IUs by country        #
nrow(Full_histories_df_minimal_lastyr_2022)

freq_df <- Full_histories_df_minimal_lastyr_2022 %>%
  group_by(ADMIN0ISO3) %>%   # Group by ADMIN0ISO3
  summarise(
    Frequency = n(),           # Count the frequency of each ADMIN0ISO3
    Proportion = n() / nrow(Full_histories_df_minimal_lastyr_2022) * 100 # Calculate the proportion
  )

View(freq_df)

# ============= #
# OCP vs APOC   #
# ============= #

# Switch between versions here #

# # VERSION 1 - Endgame only
# co_endemicity_maps_final <- co_endemicity_maps_final_onlyendgame # VERSION 1 (ONLY THOSE IN ENDGAME n = 2132)

# # VERSION 2 - ALL IUs
# #co_endemicity_maps_final <- co_endemicity_maps_final_ALLIUs # VERSION 1 (ONLY THOSE IN ENDGAME n = 2132)
# 
# OCP <- c("Mali", "Niger", "Burkina Faso", "Ghana", "Togo", "Benin",
#          "Cote d'Ivoire", "Guinea", "Sierra Leone", "Senegal",
#          "Guinea-Bissau")
# 
# co_endemicity_maps_final_ALLIUs$phase2 <- ifelse(
#   is.na(co_endemicity_maps_final_ALLIUs$phase2) & 
#     co_endemicity_maps_final_ALLIUs$trt_status_2022 == "Treatment naive" & 
#     co_endemicity_maps_final_ALLIUs$ADMIN0ISO3.x %in% c("NER", "GNB", "GIN", "TGO", "SEN"),
#   "Former OCP: Non-Control", 
#   co_endemicity_maps_final_ALLIUs$phase2  # Keep the original value when the condition is not met
# )
# 
# co_endemicity_maps_final_ALLIUs$phase2 <- ifelse(
#   is.na(co_endemicity_maps_final_ALLIUs$phase2) & 
#     co_endemicity_maps_final_ALLIUs$trt_status_2022 == "Treatment naive" & 
#     !co_endemicity_maps_final_ALLIUs$ADMIN0ISO3.x %in% c("NER", "GNB", "GIN", "TGO", "SEN"),
#   "Former APOC", 
#   co_endemicity_maps_final_ALLIUs$phase2  # Keep the original value when the condition is not met
# )
#                                                    
#co_endemicity_maps_final <- co_endemicity_maps_final_ALLIUs      # VERSION 2 (ALL IUs N = 2724)



# ===================================== #
#  Make changes for paper (April 2025)  #

# change label fro PHASE I
unique(co_endemicity_maps_final$phase2)

co_endemicity_maps_final <- co_endemicity_maps_final %>%
  mutate(phase2 = ifelse(phase2 == "PHASE 1 - FEB 1975", "Former OCP Phase I - 1975", phase2))

# change other labels #
co_endemicity_maps_final <- co_endemicity_maps_final %>%
  mutate(phase2 = ifelse(phase2 == "PHASE II - JAN 1976", "Former OCP Phase II - 1976", phase2))

co_endemicity_maps_final <- co_endemicity_maps_final %>%
  mutate(phase2 = ifelse(phase2 == "PHASE III EAST - JUL 1977", "Former OCP Phase III East - 1977", phase2))

co_endemicity_maps_final <- co_endemicity_maps_final %>%
  mutate(phase2 = ifelse(phase2 == "PHASE III WEST - MAR 1977", "Former OCP Phase III West - 1977", phase2))

co_endemicity_maps_final <- co_endemicity_maps_final %>%
  mutate(phase2 = ifelse(phase2 == "PHASE IV - MAR 1979", "Former OCP Phase IV - 1979", phase2))

co_endemicity_maps_final <- co_endemicity_maps_final %>%
  mutate(phase2 = ifelse(phase2 == "SOUTHERN EXTENSION - FEB 1988", "Former OCP Southern Extension - 1988", phase2))

co_endemicity_maps_final <- co_endemicity_maps_final %>%
  mutate(phase2 = ifelse(phase2 == "SOUTHERN FOREST EXTENSION - JAN 1990", "Former OCP Southern Forest Extension- 1990", phase2))

co_endemicity_maps_final <- co_endemicity_maps_final %>%
  mutate(phase2 = ifelse(phase2 == "WESTERN EXTENSION - MAR 1989", "Former OCP Western Extension - 1989", phase2))

co_endemicity_maps_final <- co_endemicity_maps_final %>%
  mutate(phase2 = ifelse(phase2 == "WESTERN EXTENSION - 1990", "Former OCP Western Extension - 1990", phase2))

co_endemicity_maps_final <- co_endemicity_maps_final %>%
  mutate(phase2 = ifelse(phase2 == "WESTERN EXTENSION - MAR 1990", "Former OCP Western Extension - 1990 (March)", phase2))

co_endemicity_maps_final <- co_endemicity_maps_final %>%
  mutate(phase2 = ifelse(phase2 == "NON-CONTROL", "Former OCP: Non-Control", phase2))

co_endemicity_maps_final <- co_endemicity_maps_final %>%
  mutate(phase2 = ifelse(phase2 == "APOC", "Former APOC", phase2))

# ============================================================================= #
#        make different African centroid labels based on black and white colour #

african_centroids_oncho_subset1 <- subset(african_centroids_oncho, NAME_0 %in% c("Guinea-Bissau", "Senegal",
                                                                     "Guinea", "Sierra Leone",
                                                                     "Togo", "Ghana", "Benin"))

african_centroids_oncho_subset2 <- subset(african_centroids_oncho, !NAME_0 %in% c("Guinea-Bissau", "Senegal",
                                                                     "Guinea", "Sierra Leone",
                                                                     "Togo", "Ghana", "Benin"))

cbPalette <- c("#FFCC00","#9999FF","#009E73","#CC0000","#0000FF","#FF6600","#990066","#993333","#003300","#330033","#330000","#006666")

p_map <- ggplot() +
  geom_sf(data = oceans_shp, aes(), fill = "lightblue", colour = NA) +
  geom_sf(data = co_endemicity_maps_final, aes(fill = phase2), colour = NA, alpha = 0.7) +
  #geom_sf(data = co_endemicity_maps_final, aes(fill = phase2), colour = NA, alpha = 0.7) +
  #geom_sf(data = ESPEN_IUs_ALL, aes(), colour = "lightgrey", linewidth = 0.1, size= 0.1, fill = NA, alpha = 0.1) +
  geom_sf(data = ESPEN_IUs_ALL, aes(), colour = NA, size = 1, fill = NA, alpha = 0.1) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
  #geom_sf_text(data = african_centroids_oncho, aes(label = NAME_0), size = 1.5, color = "black", fontface = "bold", alpha = 0.7) + # Add country labelscoord_sf(xlim = c(-15, 45), ylim = c(22, -15)) +
  geom_sf_text(data = african_centroids_oncho_subset1, aes(label = NAME_0), size = 1.5, color = "white", fontface = "bold", alpha = 0.7) +
  geom_sf_text(data = african_centroids_oncho_subset2, aes(label = NAME_0), size = 1.5, color = "black", fontface = "bold", alpha = 0.7) +
  coord_sf(xlim = c(-15, 45), ylim = c(22, -16)) +
  theme_bw() +
  scale_fill_manual(values = cbPalette, na.value = "gray") +
  scale_colour_manual(na.value="gray")+
  #scale_size_identity()+
  labs(fill='', x = " ", y = " ") +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal")

# just bioko #
p_bioko <- ggplot() +
  geom_sf(data = oceans_shp, aes(), fill = "lightblue", colour = NA) +
  geom_sf(data = co_endemicity_maps_final, aes(fill = phase2), colour = NA, alpha = 0.7) +
  geom_sf(data = ESPEN_IUs_ALL, aes(), colour = "black", linewidth = 0.01, fill = NA, alpha = 0.3) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
  coord_sf(xlim = c(8, 10), ylim = c(3, 5)) +
  theme_bw() +
  scale_fill_manual(values = cbPalette, na.value = "gray") +
  scale_colour_manual(na.value = "gray") +
  labs(fill = '') +
  theme(
    legend.position = "none",         # Remove the legend
    axis.text = element_blank(),      # Remove axis text (labels)
    axis.ticks = element_blank(),     # Remove axis ticks
    axis.title = element_blank(),     # Remove axis titles
    panel.grid.major = element_blank(),  # Optionally remove grid lines
    panel.grid.minor = element_blank()   # Optionally remove minor grid lines
  )

# Combine p_map and p_bioko using ggdraw
final_map_plot <- ggdraw() +
  draw_plot(p_map, 0, 0, 1, 1) +  # Main map takes the whole space
  draw_plot(p_bioko, 0.2, 0.32, 0.25, 0.25)  # Inset map in bottom-left corner (adjust size and position as needed)


# ggsave("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Publications/Intervention histories/figures/Bothprojects.pdf",
#        plot = final_map_plot, device = "pdf", width = 11.69, height = 8.27) # A4 portrait size in inches
# 
# APOC_df <- subset(Full_histories_df_minimal_lastyr_2022, Control_prog == "APOC")


# add SIZ in #

# match SIZs to IUs#
SIZs_IUs <- read_excel("C:/Users/mad206/OneDrive/Endgame/OCP mapping/SIZs/SIZs_IUs.xlsx")

SIZ_IU_vec <- as.character(SIZs_IUs$IU)
SIZ_IU_vec # check these IUs against new IUs

# extract out the IUs which should be in the SIZs (function to below to deal with those IUs with accents not matching)

# Define a function to find the closest match
find_closest_match <- function(target, candidates) {
  distances <- stringdist::stringdistmatrix(target, candidates, method = "jw")
  closest_match <- candidates[which.min(distances)]
  return(closest_match)
}

# Apply the function to each value in names_to_keep
matched_names <- sapply(SIZ_IU_vec, function(name) find_closest_match(name, Full_histories_df_minimal_lastyr_2022$IUs_NAME_MAPPING))

Full_histories_df_minimal_lastyr_2022_SIZs <- Full_histories_df_minimal_lastyr_2022

Full_histories_df_minimal_lastyr_2022_SIZs$SIZ_label <- ifelse(Full_histories_df_minimal_lastyr_2022_SIZs$IUs_NAME_MAPPING %in% matched_names, "SIZ", Full_histories_df_minimal_lastyr_2022_SIZs$SIZ_label)

Full_histories_df_minimal_lastyr_2022_SIZs <- subset(Full_histories_df_minimal_lastyr_2022_SIZs, SIZ_label == "SIZ")

co_endemicity_maps_final_SIZ <- ESPEN_IUs_ALL %>%
  left_join(Full_histories_df_minimal_lastyr_2022_SIZs, by = c("IU_ID" = "IU_ID_MAPPING"))

co_endemicity_maps_final_SIZ <- subset(co_endemicity_maps_final_SIZ, SIZ_label == "SIZ")

cbPalette_SIZ <- c("#FFCC00","#9999FF","#009E73","#CC0000","#0000FF","#FF6600","#990066","#993333","#003300","#330033","#330000","#006666","#99FFFF")

p_map2 <- ggplot() +
  geom_sf(data = oceans_shp, aes(), fill = "lightblue", colour = NA) +
  geom_sf(data = co_endemicity_maps_final, aes(fill = phase2), colour = NA, alpha = 0.7) +
  geom_sf(data = ESPEN_IUs_ALL, aes(), colour = "black", linewidth = 0.01, fill = NA, alpha = 0.3) +
  geom_sf(data = co_endemicity_maps_final_SIZ, aes(), colour = "#0000CC", alpha = 0.3) + # Add this layer
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
  #geom_sf_text(data = african_centroids_oncho, aes(label = NAME_0), size = 1.5, color = "black", fontface = "bold", alpha = 0.7) + # Add country labelscoord_sf(xlim = c(-15, 45), ylim = c(22, -15)) +
  geom_sf_text(data = african_centroids_oncho_subset1, aes(label = NAME_0), size = 1.5, color = "white", fontface = "bold", alpha = 1) +
  geom_sf_text(data = african_centroids_oncho_subset2, aes(label = NAME_0), size = 1.5, color = "black", fontface = "bold", alpha = 0.7) +
  theme_bw() +
  #coord_sf(xlim = c(-15, 45), ylim = c(22, -15)) +
  coord_sf(xlim = c(-15, 45), ylim = c(22, -16.5)) + # for All IUs
  scale_fill_manual(values = cbPalette_SIZ, na.value = "gray") +
  scale_colour_manual(na.value="gray") +
  labs(fill='', x = " ", y = " ") +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal",
    legend.text = element_text(size = 5),  # Adjust size of legend text
    legend.title = element_text(size = 10)  # Adjust size of legend title)))
  )


# Combine p_map and p_bioko using ggdraw
final_map_plot_base <- ggdraw() +
  draw_plot(p_map2, 0, 0, 1, 1) +  # Main map takes the whole space
  draw_plot(p_bioko, 0.2, 0.32, 0.25, 0.25)  # Inset map in bottom-left corner (adjust size and position as needed)


# ggsave("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Publications/Intervention histories/figures/APOC_OCP_projects_map.pdf",
#        plot = final_map_plot, device = "pdf", width = 11.69, height = 8.27) # A4 portrait size in inches

# VERSION 1 (endgame IUs , n = 2132)
# increase resolution so that IU lines visible but not too thick
agg_png("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Publications/Intervention histories/figures/APOC_OCP_projects_map_V1.png", 6000, 4000, scaling = 12)
final_map_plot_base
dev.off()

# # VERSION 2 (ALL IUs)
# # increase resolution so that IU lines visible but not too thick
# agg_png("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Publications/Intervention histories/figures/APOC_OCP_projects_map_V2_ALLIUs.png", 6000, 4000, scaling = 12)
# final_map_plot_base
# dev.off()

# ============= #
# Co-endemicity #
# ============= #

# VERSION 1 - Endgame only
co_endemicity_maps_final <- co_endemicity_maps_final_onlyendgame # VERSION 1 (ONLY THOSE IN ENDGAME n = 2132)

## VERSION 2 - all IUs #
#co_endemicity_maps_final <- co_endemicity_maps_final_ALLIUs # VERSION 1 (ONLY THOSE IN ENDGAME n = 2132)

# change labels as per the Variables tabl #

# change label fro PHASE I
unique(co_endemicity_maps_final$co_endemicity)

co_endemicity_maps_final <- co_endemicity_maps_final %>%
  mutate(co_endemicity = ifelse(co_endemicity %in% c("oncho","assumed oncho only"), "Only Oncho", co_endemicity))

co_endemicity_maps_final <- co_endemicity_maps_final %>%
  mutate(co_endemicity = ifelse(co_endemicity == "oncho,LF", "Oncho and LF", co_endemicity))

co_endemicity_maps_final <- co_endemicity_maps_final %>%
  mutate(co_endemicity = ifelse(co_endemicity == "oncho,loa hypo", "Oncho and Loa hypo", co_endemicity))

co_endemicity_maps_final <- co_endemicity_maps_final %>%
  mutate(co_endemicity = ifelse(co_endemicity == "oncho,loa meso", "Oncho and Loa meso", co_endemicity))

co_endemicity_maps_final <- co_endemicity_maps_final %>%
  mutate(co_endemicity = ifelse(co_endemicity == "oncho,loa hyper", "Oncho and Loa hyper", co_endemicity))

co_endemicity_maps_final <- co_endemicity_maps_final %>%
  mutate(co_endemicity = ifelse(co_endemicity == "oncho,LF,loa hypo", "Oncho and LF and Loa hypo", co_endemicity))

co_endemicity_maps_final <- co_endemicity_maps_final %>%
  mutate(co_endemicity = ifelse(co_endemicity == "oncho,LF,loa meso", "Oncho and LF and Loa meso", co_endemicity))

co_endemicity_maps_final <- co_endemicity_maps_final %>%
  mutate(co_endemicity = ifelse(co_endemicity == "oncho,LF,loa hyper", "Oncho and LF and Loa hyper", co_endemicity))

# Define the desired order
desired_order <- c(
  "Only Oncho","Oncho and Loa hypo","Oncho and Loa meso","Oncho and Loa hyper",
  "Oncho and LF","Oncho and LF and Loa hypo","Oncho and LF and Loa meso","Oncho and LF and Loa hyper")

# Reorder the co_endemicity column in your dataframe
co_endemicity_maps_final$co_endemicity <- factor(
  co_endemicity_maps_final$co_endemicity, 
  levels = desired_order
)

# VERSION 2 - ALL IUs

#cbPalette <- c("#99CCFF","#9999FF","#009E73","#990066","#FFCCFF","#CC3399","#CC0000","#FFFF99","#FF6600")
cbPalette <- c("#6699FF","#FFFF99","#FFCC00","#CC0000","#CC3399","#FF9966","#FF6600", "darkred")

p_map <- ggplot() +
  geom_sf(data = oceans_shp, aes(), fill = "lightblue", colour = NA) +
  geom_sf(data = co_endemicity_maps_final, aes(fill = co_endemicity), colour = NA, alpha = 0.7) +
  #geom_sf(data = co_endemicity_maps_final, aes(), colour = "white", linewidth = 0.125, fill = NA, alpha = 0.3) +
  #geom_sf(data = ESPEN_IUs_ALL, aes(), colour = "lightgrey", linewidth = 0.1, size= 0.1, fill = NA, alpha = 0.1) +
  #geom_sf(data = ESPEN_IUs_ALL, aes(), colour = NA, size = 5, fill = NA, alpha = 0.1) +
  geom_sf(data = ESPEN_IUs_ALL, aes(), colour = "black", linewidth = 0.01, fill = NA, alpha = 0.3) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
  geom_sf_text(data = african_centroids_oncho, aes(label = NAME_0), size = 1.5, color = "black", fontface = "bold", alpha = 0.7) + # Add country labels
  #coord_sf(xlim = c(-15, 45), ylim = c(22, -15)) +
  coord_sf(xlim = c(-15, 45), ylim = c(22, -16.5)) + # for All IUs
  theme_bw() +
  scale_fill_manual(values = cbPalette, na.value = "gray") +
  scale_colour_manual(na.value="gray")+
  #scale_size_identity()+
  labs(fill='', x = " ", y = " ") +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal",
    legend.text = element_text(size = 7),  # Adjust size of legend text
    legend.title = element_text(size = 10))  # Adjust size of legend title)

# just bioko #
p_bioko <- ggplot() +
  geom_sf(data = oceans_shp, aes(), fill = "lightblue", colour = NA) +
  geom_sf(data = co_endemicity_maps_final, aes(fill = co_endemicity), colour = NA, alpha = 0.7) +
  #geom_sf(data = ESPEN_IUs_ALL, aes(), colour = NA, size = 1, fill = NA, alpha = 0.1) +
  geom_sf(data = ESPEN_IUs_ALL, aes(), colour = "black", linewidth = 0.01, fill = NA, alpha = 0.3) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
  coord_sf(xlim = c(8, 10), ylim = c(3, 5)) +
  theme_bw() +
  scale_fill_manual(values = cbPalette, na.value = "gray") +
  scale_colour_manual(na.value = "gray") +
  labs(fill = '') +
  theme(
    legend.position = "none",         # Remove the legend
    axis.text = element_blank(),      # Remove axis text (labels)
    axis.ticks = element_blank(),     # Remove axis ticks
    axis.title = element_blank(),     # Remove axis titles
    panel.grid.major = element_blank(),  # Optionally remove grid lines
    panel.grid.minor = element_blank()   # Optionally remove minor grid lines
  )


# # ======================================= #
# # only loa co-endemic (any oncho endemic) #
# 
# unique(co_endemicity_maps_final$co_endemicity)
# 
# co_endemicity_maps_final_onlyloa <- subset(co_endemicity_maps_final, co_endemicity %in% c("oncho,loa hyper",
#                                                                                           "oncho,loa hypo",
#                                                                                           "oncho,loa meso",
#                                                                                           "oncho,LF,loa hypo",
#                                                                                           "oncho,LF,loa meso",
#                                                                                           "oncho,LF,loa hyper"))
# 
# p_map <- ggplot() +
#   geom_sf(data = oceans_shp, aes(), fill = "lightblue", colour = NA) +
#   geom_sf(data = co_endemicity_maps_final_onlyloa, aes(fill = co_endemicity), colour = NA, alpha = 0.7) +
#   #geom_sf(data = ESPEN_IUs_ALL, aes(), colour = "lightgrey", linewidth = 0.1, size= 0.1, fill = NA, alpha = 0.1) +
#   geom_sf(data = ESPEN_IUs_ALL, aes(), colour = NA, size = 1, fill = NA, alpha = 0.1) +
#   geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
#   geom_sf_text(data = african_centroids_oncho, aes(label = NAME_0), size = 3, color = "black", fontface = "bold") + # Add country labels
#   coord_sf(xlim = c(-15, 45), ylim = c(22, -16)) +
#   theme_bw() +
#   scale_fill_manual(values = cbPalette, na.value = "gray") +
#   scale_colour_manual(na.value="gray")+
#   #scale_size_identity()+
#   labs(fill='', x = " ", y = " ") +
#   theme(
#     legend.position = "bottom",  # Place the legend at the bottom
#     legend.direction = "horizontal")
# 
# # just bioko #
# p_bioko <- ggplot() +
#   geom_sf(data = oceans_shp, aes(), fill = "lightblue", colour = NA) +
#   geom_sf(data = co_endemicity_maps_final_onlyloa, aes(fill = co_endemicity), colour = NA, alpha = 0.7) +
#   geom_sf(data = ESPEN_IUs_ALL, aes(), colour = NA, size = 1, fill = NA, alpha = 0.1) +
#   geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
#   coord_sf(xlim = c(8, 10), ylim = c(3, 5)) +
#   theme_bw() +
#   scale_fill_manual(values = cbPalette, na.value = "gray") +
#   scale_colour_manual(na.value = "gray") +
#   labs(fill = '') +
#   theme(
#     legend.position = "none",         # Remove the legend
#     axis.text = element_blank(),      # Remove axis text (labels)
#     axis.ticks = element_blank(),     # Remove axis ticks
#     axis.title = element_blank(),     # Remove axis titles
#     panel.grid.major = element_blank(),  # Optionally remove grid lines
#     panel.grid.minor = element_blank()   # Optionally remove minor grid lines
#   )
# 
# ## Combine p_map and p_bioko using ggdraw
# # final_map_plot <- ggdraw() +
# #   draw_plot(p_map, 0, 0, 1, 1) +  # Main map takes the whole space
# #   draw_plot(p_bioko, 0.2, 0.32, 0.25, 0.25)  # Inset map in bottom-left corner (adjust size and position as needed)
# 
final_map_plot <- ggdraw() +
  draw_plot(p_map, 0, 0, 1, 1) +  # Main map takes the whole space
  draw_plot(p_bioko, 0.2, 0.32, 0.25, 0.25) +  # Inset map in bottom-left corner
  draw_text("a", x = 0.1, y = 1, hjust = 0, vjust = 1, size = 10, fontface = "bold")

#final_map_plot

# # Display the final plot
# print(final_map_plot)

# ggsave("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Publications/Intervention histories/figures/coendemicity_maps.pdf",
#        plot = final_map_plot, device = "pdf", width = 11.69, height = 8.27) # A4 portrait size in inches

# VERSION 1 ; Only endgame IUs (n = 2132)
# increase resolution so that IU lines visible but not too thick
agg_png("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Publications/Intervention histories/figures/coendemicity_map_plot_V1.png", 6000, 4000, scaling = 12)
final_map_plot
dev.off()

# # VERSION 2 ; All IUs 
# # increase resolution so that IU lines visible but not too thick
# agg_png("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Publications/Intervention histories/figures/coendemicity_map_plot_V2_ALLIUs.png", 6000, 4000, scaling = 12)
# final_map_plot
# dev.off()

# ====================================================== #
#       Make FREQUENCY DISTRIBUTION PLOTS                #

# ALL IUS with MDA and TREATMENT NAIVE (APRIL 2025)
#Full_histories_df_popinfo <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_popinfo_ALL_minimal_070425_listlabels.csv")
# Full_histories_df_popinfo <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_popinfo_290425.csv") # FINAL 

Full_histories_df_minimal_lastyr_2022 <- subset(Full_histories_df_popinfo, Year == 2022)
Full_histories_df_minimal_lastyr_2013 <- subset(Full_histories_df_popinfo, Year == 2013)
Full_histories_df_minimal_lastyr_2013_CUMMDA0 <- subset(Full_histories_df_minimal_lastyr_2013, Cum_MDA_ESPEN == 0)

nrow(Full_histories_df_minimal_lastyr_2022) # 2130 IUs (April 25')

# =========================================================== #
#         Subset those just in Endgame                        #

Full_histories_df_minimal_lastyr_2022_check <- subset(Full_histories_df_minimal_lastyr_2022, !is.na(Control_prog)) # 2138 IUs

Full_histories_df_minimal_lastyr_2022_check <- Full_histories_df_minimal_lastyr_2022_check %>%
  filter(
    # Keep "Treatment naive" only if ADMIN0ISO3 is "GAB" or "SDN"
    (trt_status_2022 == "Treatment naive" & ADMIN0ISO3 %in% c("GAB", "SDN")) |
      # Keep any row where trt_status_2022 is not "Treatment naive"
      trt_status_2022 != "Treatment naive",
    # Exclude a specific IU_ID
    IU_CODE_MAPPING != "ETH0194319529"
  )

#Full_histories_df_minimal_lastyr_2022_check_trtnaive <- subset(Full_histories_df_minimal_lastyr_2022_check, trt_status_2022 == "Treatment naive")

Full_histories_df_minimal_lastyr_2022 <- Full_histories_df_minimal_lastyr_2022_check # RESET HERE

nrow(Full_histories_df_minimal_lastyr_2022) # 2130 IUs (April 25')

# # VERSION 2 - RESET ALL IUS dataframe HERE #
# # ALL IUS with MDA and TREATMENT NAIVE (APRIL 2025)
# Full_histories_df_popinfo <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_popinfo_ALL_minimal_070425_listlabels.csv")
# Full_histories_df_minimal_lastyr_2022 <- subset(Full_histories_df_popinfo, Year == 2022)


# RECODE FACTOR LEVELS TO MATCH MAP - update April 2025 #
# Step 1: Recode the existing factor levels
Full_histories_df_minimal_lastyr_2022$co_endemicity <- Full_histories_df_minimal_lastyr_2022$co_endemicity %>%
  forcats::fct_recode(
    "Only Oncho" = "oncho",
    "Only Oncho" = "assumed oncho only",
    "Oncho and LF" = "oncho,LF",
    "Oncho and Loa hypo" = "oncho,loa hypo",
    "Oncho and Loa meso" = "oncho,loa meso",
    "Oncho and Loa hyper" = "oncho,loa hyper",
    "Oncho and LF and Loa hypo" = "oncho,LF,loa hypo",
    "Oncho and LF and Loa meso" = "oncho,LF,loa meso",
    "Oncho and LF and Loa hyper" = "oncho,LF,loa hyper"
  )

# Step 2: Check the updated levels
unique(Full_histories_df_minimal_lastyr_2022$co_endemicity)

# Define the desired order
desired_order <- c(
  "Only Oncho","Oncho and Loa hypo","Oncho and Loa meso","Oncho and Loa hyper",
  "Oncho and LF","Oncho and LF and Loa hypo","Oncho and LF and Loa meso","Oncho and LF and Loa hyper")


# # Combine the desired order with any other levels not already listed
# updated_levels <- unique(c(desired_order, setdiff(current_levels, desired_order)))
# 
# # Reassign the levels to the co_endemicity column with the new order
# Full_histories_df_minimal_lastyr_2022$co_endemicity <- factor(
#   Full_histories_df_minimal_lastyr_2022$co_endemicity, 
#   levels = updated_levels
# )
# 
# unique(Full_histories_df_minimal_lastyr_2022$co_endemicity) # check

# Reorder the co_endemicity column in your dataframe
Full_histories_df_minimal_lastyr_2022$co_endemicity <- factor(
  Full_histories_df_minimal_lastyr_2022$co_endemicity, 
  levels = desired_order
)

nrow(Full_histories_df_minimal_lastyr_2022)
unique(Full_histories_df_minimal_lastyr_2022$co_endemicity) # check


# frequency dist plot
freq_table_coendemicity2 <- table(Full_histories_df_minimal_lastyr_2022$co_endemicity) # Create a frequency table

freq_df_coendemicity2 <- as.data.frame(as.table(freq_table_coendemicity2)) # Convert the frequency table to a data frame
colnames(freq_df_coendemicity2) <- c("co_endemicity", "Frequency") # Rename the columns for better readability

# change label fro PHASE I
unique(freq_df_coendemicity2$co_endemicity)


# Create the frequency distribution plot
p1 <- ggplot(freq_df_coendemicity2, aes(x = co_endemicity, y = Frequency, fill = co_endemicity)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  geom_text(data = subset(freq_df_coendemicity2, Frequency > 0),
            aes(label = Frequency),
            vjust = -0.5,  # Adjusts the position of the text (above the bar)
            size = 3,      # Adjusts the size of the text
            colour = "black") +  # Color of the text
  labs(x = "Co-endemicity",
       y = "Frequency",
       fill = "Co-endemicity") +
  scale_fill_manual(values = cbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Remove x-axis labels
        axis.ticks.x = element_blank(),  # Remove x-axis ticks if needed
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.line = element_line(colour = "black"),
        strip.text = element_text(face = "bold")) +
  guides(fill = guide_legend(title = NULL)) +  # Correct way to remove the legend title
  coord_cartesian(clip = "off")  # Allow labels to go outside the plot area

p1

# frequency dist plot by country
freq_table_coendemicity <- table(Full_histories_df_minimal_lastyr_2022$co_endemicity, Full_histories_df_minimal_lastyr_2022$ADMIN0ISO3) # Create a frequency table

freq_df_coendemicity <- as.data.frame(as.table(freq_table_coendemicity)) # Convert the frequency table to a data frame
colnames(freq_df_coendemicity) <- c("co_endemicity", "ADMIN0ISO3", "Frequency") # Rename the columns for better readability

p2 <- ggplot(freq_df_coendemicity, aes(x = co_endemicity, y = Frequency, fill = co_endemicity)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  geom_text(data = subset(freq_df_coendemicity, Frequency > 0),
            aes(label = Frequency),
            vjust = -0.4,  # Adjusts the position of the text (above the bar)
            size = 3,      # Adjusts the size of the text
            colour = "black") +  # Color of the text
  labs(x = "Co-endemicity",
       y = "Frequency",
       fill = "Co-endemicity") +
  scale_fill_manual(values = cbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Remove x-axis labels
        axis.ticks.x = element_blank(),  # Remove x-axis ticks if needed
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.line = element_line(colour = "black"),
        strip.text = element_text(face = "bold")) +
  guides(fill = guide_legend(title = NULL)) +  # Correct way to remove the legend title
  facet_wrap(~ADMIN0ISO3) +
  coord_cartesian(clip = "off")  # Allow labels to go outside the plot area

# Display the plot
p2

# Assuming p1, p2, and final_map are your ggplot objects

# # Extract the shared legend from one of the plots (assuming they all have the same legend)
# shared_legend <- get_legend(p1 + theme(legend.position = "bottom"))
#
# # Remove the legends from the individual plots
# p1_no_legend <- p1 + theme(legend.position = "none")
# p2_no_legend <- p2 + theme(legend.position = "none")
# final_map_no_legend <- final_map_plot + theme(legend.position = "none")
#
# # Arrange p1 and p2 in a column on the left-hand side
# left_column <- plot_grid(p1_no_legend, p2_no_legend, ncol = 1, rel_heights = c(1/4, 3/4))
#
# # Arrange final_map on the right-hand side
# right_side <- plot_grid(final_map_no_legend, shared_legend, ncol = 1, rel_heights = c(1, 0.1))
#
# # Combine the left column and the right side
# combined_plot <- plot_grid(left_column, right_side, ncol = 2, rel_widths = c(1/3, 2/3))

p1_no_legend <- p1 + theme(legend.position = "none")
p2_no_legend <- p2 + theme(legend.position = "none")

# Extract the shared legend from one of the plots
shared_legend <- get_legend(p1 + theme(legend.position = "bottom"))

# Combine p1 and p2 in a vertical layout, without titles
combined_plots <- plot_grid(
  p1_no_legend, p2_no_legend,
  labels = c("b", "c"),        # Add "A" and "B" labels
  label_size = 18,             # Adjust label size
  ncol = 1,                    # Stack them vertically
  rel_heights = c(1/4, 3/4)    # Set relative heights: p1 takes 1/4, p2 takes 3/4
)

# Add the shared legend at the bottom
final_plot <- plot_grid(
  combined_plots, shared_legend,
  ncol = 1,
  rel_heights = c(1, 0.1)  # Legend takes 10% of the space at the bottom
)

# Display the final plot with the shared legend
#print(final_plot)


# # Optionally, save to PDF in portrait mode (A4 size)
# ggsave("p1_p2_combined_portrait.pdf", combined_plot, width = 8.27, height = 11.69, units = "in")  # A4 portrait dimensions

# Save the combined plot to a landscape A4 PDF

# VERSION 1: only endgame IUs (n = 2131)
# 421+158+66+59+759+325+181+162 = 2131
ggsave("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Publications/Intervention histories/figures/combined_coendemicity_freqdist_plot_V1.pdf", final_plot, width = 8.27, height = 11.69, units = "in")  # A4 landscape dimensions

# 421 + 158 + 66 + 59 + 759 + 324 + 181 + 162 = 2130 (April 25' Panel A)

# VERSION 2: only endgame IUs (n = 2724)
# 666+312+106+136+772+347+194+191

ggsave("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Publications/Intervention histories/figures/combined_coendemicity_freqdist_plot_V2_ALLIUs.pdf", final_plot, width = 8.27, height = 11.69, units = "in")  # A4 landscape dimensions


# # ======================================================= #
# #               Plot oncho hypoendemic with loa           #
# 
# co_endemicity_maps_final_hypo <- subset(co_endemicity_maps_final, endemicity_baseline == "hypoendemic")
# co_endemicity_maps_final_hypo_anyloa <- subset(co_endemicity_maps_final_hypo, co_endemicity %in% c("oncho,loa hyper",
#                                                                                                    "oncho,loa hypo",
#                                                                                                    "oncho,loa meso",
#                                                                                                    "oncho,LF,loa hypo",
#                                                                                                    "oncho,LF,loa meso",
#                                                                                                    "oncho,LF,loa hyper"))
# cbPalette <- c("#990066","#FFCCFF","#CC3399","#CC0000","#FFFF99","#FF6600")
# 
# 
# p_map <- ggplot() +
#   geom_sf(data = oceans_shp, aes(), fill = "lightblue", colour = NA) +
#   geom_sf(data = co_endemicity_maps_final_hypo_anyloa, aes(fill = co_endemicity), colour = NA, alpha = 0.7) +
#   #geom_sf(data = ESPEN_IUs_ALL, aes(), colour = "lightgrey", linewidth = 0.1, size= 0.1, fill = NA, alpha = 0.1) +
#   #geom_sf(data = ESPEN_IUs_ALL, aes(), colour = NA, size = 1, fill = NA, alpha = 0.1) +
#   geom_sf(data = ESPEN_IUs_ALL, aes(), colour = "grey", linewidth = 0.125, fill = NA, alpha = 0.3) +
#   geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
#   geom_sf_text(data = african_centroids_oncho, aes(label = NAME_0), size = 3, color = "black", fontface = "bold") + # Add country labels
#   coord_sf(xlim = c(-15, 45), ylim = c(22, -16)) +
#   theme_bw() +
#   scale_fill_manual(values = cbPalette, na.value = "gray") +
#   scale_colour_manual(na.value="gray")+
#   #scale_size_identity()+
#   labs(fill='', x = " ", y = " ") +
#   theme(
#     legend.position = "bottom",  # Place the legend at the bottom
#     legend.direction = "horizontal")
# 
# # just bioko #
# p_bioko <- ggplot() +
#   geom_sf(data = oceans_shp, aes(), fill = "lightblue", colour = NA) +
#   geom_sf(data = co_endemicity_maps_final_hypo_anyloa, aes(fill = co_endemicity), colour = NA, alpha = 0.7) +
#   geom_sf(data = ESPEN_IUs_ALL, aes(), colour = NA, size = 1, fill = NA, alpha = 0.1) +
#   geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
#   coord_sf(xlim = c(8, 10), ylim = c(3, 5)) +
#   theme_bw() +
#   scale_fill_manual(values = cbPalette, na.value = "gray") +
#   scale_colour_manual(na.value = "gray") +
#   labs(fill = '') +
#   theme(
#     legend.position = "none",         # Remove the legend
#     axis.text = element_blank(),      # Remove axis text (labels)
#     axis.ticks = element_blank(),     # Remove axis ticks
#     axis.title = element_blank(),     # Remove axis titles
#     panel.grid.major = element_blank(),  # Optionally remove grid lines
#     panel.grid.minor = element_blank()   # Optionally remove minor grid lines
#   )
# 
# ## Combine p_map and p_bioko using ggdraw
# # final_map_plot <- ggdraw() +
# #   draw_plot(p_map, 0, 0, 1, 1) +  # Main map takes the whole space
# #   draw_plot(p_bioko, 0.2, 0.32, 0.25, 0.25)  # Inset map in bottom-left corner (adjust size and position as needed)
# 
# final_map_plot <- ggdraw() +
#   draw_plot(p_map, 0, 0, 1, 1) +  # Main map takes the whole space
#   draw_plot(p_bioko, 0.2, 0.32, 0.25, 0.25) +  # Inset map in bottom-left corner
#   draw_text("A", x = 0.03, y = 1, hjust = 0, vjust = 1, size = 18, fontface = "bold")
# 
# final_map_plot
# 
# # frequency dist plot
# freq_table_coendemicity3 <- table(co_endemicity_maps_final_hypo_anyloa$co_endemicity) # Create a frequency table
# 
# freq_df_coendemicity3 <- as.data.frame(as.table(freq_table_coendemicity3)) # Convert the frequency table to a data frame
# colnames(freq_df_coendemicity3) <- c("co_endemicity", "Frequency") # Rename the columns for better readability


# ========================================== #
#    intervention histories                  #
# ========================================== #

# VERSION 1 - Endgame only
co_endemicity_maps_final <- co_endemicity_maps_final_onlyendgame # VERSION 1 (ONLY THOSE IN ENDGAME n = 2132)

co_endemicity_maps_final$biannual_VC_mapping 

## VERSION 2 - all IUs #
#co_endemicity_maps_final <- co_endemicity_maps_final_ALLIUs # VERSION 1 (ONLY THOSE IN ENDGAME n = 2132)


# change labels as per the Variables tabl #

# change label fro PHASE I
unique(co_endemicity_maps_final$biannual_VC_mapping)

co_endemicity_maps_final <- co_endemicity_maps_final %>%
  mutate(biannual_VC_mapping = ifelse(biannual_VC_mapping == "annual only", "Annual MDA", biannual_VC_mapping))

co_endemicity_maps_final <- co_endemicity_maps_final %>%
  mutate(biannual_VC_mapping = ifelse(biannual_VC_mapping == "annual & vector control", "Annual MDA and vector control", biannual_VC_mapping))

co_endemicity_maps_final <- co_endemicity_maps_final %>%
  mutate(biannual_VC_mapping = ifelse(biannual_VC_mapping == "biannual", "Biannual MDA", biannual_VC_mapping))

co_endemicity_maps_final <- co_endemicity_maps_final %>%
  mutate(biannual_VC_mapping = ifelse(biannual_VC_mapping == "biannual & vector control", "Biannual MDA and vector control", biannual_VC_mapping))

co_endemicity_maps_final <- co_endemicity_maps_final %>%
  mutate(biannual_VC_mapping = ifelse(biannual_VC_mapping == "treatment naive", "Treatment-naïve", biannual_VC_mapping))

unique(co_endemicity_maps_final$biannual_VC_mapping)

#check <- subset(co_endemicity_maps_final, biannual_VC_mapping == "Treatment-naïve" & ADMIN0ISO3.x == "NGA")

# Define the desired order
desired_order <- c(
  "Annual MDA","Annual MDA and vector control","Biannual MDA",
  "Biannual MDA and vector control","Treatment-naïve")

# Reorder the co_endemicity column in your dataframe
co_endemicity_maps_final$biannual_VC_mapping <- factor(
  co_endemicity_maps_final$biannual_VC_mapping, 
  levels = desired_order
)


#check <- subset(co_endemicity_maps_final, ADMIN0ISO3.x == "ETH" & biannual_VC_mapping == "Treatment-naive")

cbPalette2 <- c("#CC79A7","#E69F00","#009E73","#F0E442","#0072B2")

p_map <- ggplot() +
  geom_sf(data = oceans_shp, aes(), fill = "lightblue", colour = NA) +
  geom_sf(data = co_endemicity_maps_final, aes(fill = biannual_VC_mapping), colour = NA, alpha = 0.7) +
  #geom_sf(data = ESPEN_IUs_ALL, aes(), colour = "lightgrey", linewidth = 0.1, size= 0.1, fill = NA, alpha = 0.1) +
  #geom_sf(data = ESPEN_IUs_ALL, aes(), colour = NA, size = 1, fill = NA, alpha = 0.1) +
  geom_sf(data = ESPEN_IUs_ALL, aes(), colour = "black", linewidth = 0.01, fill = NA, alpha = 0.3) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
  geom_sf_text(data = african_centroids_oncho, aes(label = NAME_0), size = 1.5, color = "black", fontface = "bold", alpha = 0.7) + # Add country labels
  #coord_sf(xlim = c(-15, 45), ylim = c(22, -15)) +
  coord_sf(xlim = c(-15, 45), ylim = c(22, -16.5)) + # for All IUs
  theme_bw() +
  scale_fill_manual(values = cbPalette2, na.value = "gray") +
  scale_colour_manual(na.value="gray")+
  #scale_size_identity()+
  labs(fill='', x = " ", y = " ") +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal",
    legend.text = element_text(size = 7),  # Adjust size of legend text
    legend.title = element_text(size = 10))  # Adjust size of legend title)

# just bioko #
p_bioko <- ggplot() +
  geom_sf(data = oceans_shp, aes(), fill = "lightblue", colour = NA) +
  geom_sf(data = co_endemicity_maps_final, aes(fill = biannual_VC_mapping), colour = NA, alpha = 0.7) +
  geom_sf(data = ESPEN_IUs_ALL, aes(), colour = "black", linewidth = 0.01, fill = NA, alpha = 0.3) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
  coord_sf(xlim = c(8, 10), ylim = c(3, 5)) +
  theme_bw() +
  scale_fill_manual(values = cbPalette2, na.value = "gray") +
  scale_colour_manual(na.value = "gray") +
  labs(fill = '') +
  theme(
    legend.position = "none",         # Remove the legend
    axis.text = element_blank(),      # Remove axis text (labels)
    axis.ticks = element_blank(),     # Remove axis ticks
    axis.title = element_blank(),     # Remove axis titles
    panel.grid.major = element_blank(),  # Optionally remove grid lines
    panel.grid.minor = element_blank()   # Optionally remove minor grid lines
  )

# Combine p_map and p_bioko using ggdraw
final_map_plot2 <- ggdraw() +
  draw_plot(p_map, 0, 0, 1, 1) +  # Main map takes the whole space
  draw_plot(p_bioko, 0.2, 0.32, 0.25, 0.25) + # Inset map in bottom-left corner (adjust size and position as needed)
  draw_text("a", x = 0.1, y = 1, hjust = 0, vjust = 1, size = 10, fontface = "bold")

# # Display the final plot
# print(final_map_plot)

# ggsave("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Publications/Intervention histories/figures/intervention_histories_maps.pdf",
#        plot = final_map_plot2, device = "pdf", width = 11.69, height = 8.27) # A4 portrait size in inches

# increase resolution so that IU lines visible but not too thick

# VERSION 1: only Endgame IUs (n = 2132 IUs)
agg_png("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Publications/Intervention histories/figures/intervention_histories_maps_V1.png", 6000, 4000, scaling = 12)
final_map_plot2
dev.off()

# # VERSION 2: All IUs
# agg_png("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Publications/Intervention histories/figures/intervention_histories_maps_V2_ALLIUs.png", 6000, 4000, scaling = 12)
# final_map_plot2
# dev.off()


# ====================================================== #
#       Make FREQUENCY DISTRIBUTION PLOTS                #

# # ALL IUS with MDA and TREATMENT NAIVE (APRIL 2025)
# Full_histories_df_popinfo <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_popinfo_ALL_minimal_070425_listlabels.csv")
# Full_histories_df_minimal_lastyr_2022 <- subset(Full_histories_df_popinfo, Year == 2022)
# Full_histories_df_minimal_lastyr_2013 <- subset(Full_histories_df_popinfo, Year == 2013)
# Full_histories_df_minimal_lastyr_2013_CUMMDA0 <- subset(Full_histories_df_minimal_lastyr_2013, Cum_MDA_ESPEN == 0)

# =========================================================== #
#         Subset those just in Endgame                        #

Full_histories_df_minimal_lastyr_2022_check <- subset(Full_histories_df_minimal_lastyr_2022, !is.na(Control_prog)) # 2138 IUs

# only keep SDN and GAB treatment naive IUs
Full_histories_df_minimal_lastyr_2022_check <- Full_histories_df_minimal_lastyr_2022_check %>%
  filter(
    # Keep "Treatment naive" only if ADMIN0ISO3 is "GAB" or "SDN"
    (trt_status_2022 == "Treatment naive" & ADMIN0ISO3 %in% c("GAB", "SDN")) |
      # Keep any row where trt_status_2022 is not "Treatment naive"
      trt_status_2022 != "Treatment naive",
    # Exclude a specific IU_ID
    IU_CODE_MAPPING != "ETH0194319529"
  )

#Full_histories_df_minimal_lastyr_2022_check_trtnaive <- subset(Full_histories_df_minimal_lastyr_2022_check, trt_status_2022 == "Treatment naive")

Full_histories_df_minimal_lastyr_2022 <- Full_histories_df_minimal_lastyr_2022_check # RESET HERE

nrow(Full_histories_df_minimal_lastyr_2022) # 2130 IUs (April 25')

#check <- subset(Full_histories_df_minimal_lastyr_2022, ADMIN0ISO3 == "ETH" & biannual_VC_mapping == "Treatment naive")


# # VERSION 2 - RESET ALL IUS dataframe HERE #
# # ALL IUS with MDA and TREATMENT NAIVE (APRIL 2025)
# Full_histories_df_popinfo <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_popinfo_ALL_minimal_070425_listlabels.csv")
# Full_histories_df_minimal_lastyr_2022 <- subset(Full_histories_df_popinfo, Year == 2022)


# RECODE FACTOR LEVELS TO MATCH MAP - update April 2025 #
# Step 1: Recode the existing factor levels
Full_histories_df_minimal_lastyr_2022$biannual_VC_mapping <- Full_histories_df_minimal_lastyr_2022$biannual_VC_mapping %>%
  forcats::fct_recode(
    "Annual MDA" = "annual only",
    "Annual MDA and vector control" = "annual & vector control",
    "Biannual MDA" = "biannual",
    "Biannual MDA and vector control" = "biannual & vector control",
    "Treatment-naïve" = "treatment naive"
  )

# # Step 2: Check the updated levels
# unique(Full_histories_df_minimal_lastyr_2022$biannual_VC_mapping)
# 
# # Combine the desired order with any other levels not already listed
# updated_levels <- unique(c(desired_order, setdiff(current_levels, desired_order)))
# 
# # Reassign the levels to the co_endemicity column with the new order
# Full_histories_df_minimal_lastyr_2022$biannual_VC_mapping <- factor(
#   Full_histories_df_minimal_lastyr_2022$biannual_VC_mapping, 
#   levels = updated_levels
# )
# 
# unique(Full_histories_df_minimal_lastyr_2022$biannual_VC_mapping) # check

# Define the desired order
desired_order <- c(
  "Annual MDA","Annual MDA and vector control","Biannual MDA","Biannual MDA and vector control",
  "Treatment-naïve")

# Reorder the co_endemicity column in your dataframe
Full_histories_df_minimal_lastyr_2022$biannual_VC_mapping <- factor(
  Full_histories_df_minimal_lastyr_2022$biannual_VC_mapping, 
  levels = desired_order
)

unique(Full_histories_df_minimal_lastyr_2022$biannual_VC_mapping) # check

#check <- subset(Full_histories_df_minimal_lastyr_2022, ADMIN0ISO3 == "ETH" & biannual_VC_mapping == "Treatment-naïve")

# ================================= #
#       frequency dist plot         #
freq_table_histint2 <- table(Full_histories_df_minimal_lastyr_2022$biannual_VC_mapping) # Create a frequency table

freq_df_histint2 <- as.data.frame(as.table(freq_table_histint2)) # Convert the frequency table to a data frame
colnames(freq_df_histint2) <- c("Historical_int", "Frequency") # Rename the columns for better readability

# Create the frequency distribution plot
p1 <- ggplot(freq_df_histint2, aes(x = Historical_int, y = Frequency, fill = Historical_int)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  geom_text(data = subset(freq_df_histint2, Frequency > 0),
            aes(label = Frequency),
            vjust = -0.4,  # Adjusts the position of the text (above the bar)
            size = 3,      # Adjusts the size of the text
            colour = "black") +  # Color of the text
  labs(x = "Historical interventions",
       y = "Frequency",
       fill = "Historical interventions") +
  scale_fill_manual(values = cbPalette2) +
  theme_minimal()+
  theme(axis.text.x = element_blank(),  # Remove x-axis labels
        axis.ticks.x = element_blank(),  # Remove x-axis ticks if needed
        #axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.line = element_line(colour = "black"),
        strip.text = element_text(face = "bold"))+
  guides(fill = guide_legend(title = NULL)) +
  coord_cartesian(clip = "off")  # Allow labels to go outside the plot area

p1

# 1516 + 272 + 167 + 146 + 31 = 2132 IUs
# V2: 1516 + 272 + 167 + 146 + 623 = 2724 IUs

# frequency dist plot by country
freq_table_histint <- table(Full_histories_df_minimal_lastyr_2022$biannual_VC_mapping, Full_histories_df_minimal_lastyr_2022$ADMIN0ISO3) # Create a frequency table

freq_df_histint <- as.data.frame(as.table(freq_table_histint)) # Convert the frequency table to a data frame
colnames(freq_df_histint) <- c("Historical_int", "ADMIN0ISO3", "Frequency") # Rename the columns for better readability

# Create the frequency distribution plot
p2 <- ggplot(freq_df_histint, aes(x = Historical_int, y = Frequency, fill = Historical_int)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  geom_text(data = subset(freq_df_histint, Frequency > 0),
            aes(label = Frequency),
            vjust = -0.4,  # Adjusts the position of the text (above the bar)
            size = 3,      # Adjusts the size of the text
            colour = "black") +  # Color of the text
  labs(x = "Historical interventions",
       y = "Frequency",
       fill = "Historical interventions") +
  scale_fill_manual(values = cbPalette2) +
  theme_minimal()+
  theme(axis.text.x = element_blank(),  # Remove x-axis labels
        axis.ticks.x = element_blank(),  # Remove x-axis ticks if needed
        #axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.line = element_line(colour = "black"),
        strip.text = element_text(face = "bold")) +
  guides(fill = guide_legend(title = NULL)) +
  facet_wrap(~ADMIN0ISO3)+
  coord_cartesian(clip = "off")  # Allow labels to go outside the plot area

p2

# combine
p1_no_legend <- p1 + theme(legend.position = "none")
p2_no_legend <- p2 + theme(legend.position = "none")

# Extract the shared legend from one of the plots
shared_legend <- get_legend(p1 + theme(legend.position = "bottom"))

# Combine p1 and p2 in a vertical layout, without titles
combined_plots <- plot_grid(
  p1_no_legend, p2_no_legend,
  labels = c("b", "c"),        # Add "A" and "B" labels
  label_size = 18,             # Adjust label size
  ncol = 1,                    # Stack them vertically
  rel_heights = c(1/4, 3/4)    # Set relative heights: p1 takes 1/4, p2 takes 3/4
)

# Add the shared legend at the bottom
final_plot <- plot_grid(
  combined_plots, shared_legend,
  ncol = 1,
  rel_heights = c(1, 0.1)  # Legend takes 10% of the space at the bottom
)

# Display the final plot with the shared legend
#print(final_plot)


# # Optionally, save to PDF in portrait mode (A4 size)
# ggsave("p1_p2_combined_portrait.pdf", combined_plot, width = 8.27, height = 11.69, units = "in")  # A4 portrait dimensions

# Save the combined plot to a landscape A4 PDF

# VERSION 1: just Endgame IUs (n = 2131)
# 1516+272+167+146+30 = 2131 (removed one extra trt naive in ETH)
ggsave("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Publications/Intervention histories/figures/combined_inthist_freqdist_plot_V1.pdf", final_plot, width = 8.27, height = 11.69, units = "in")  # A4 landscape dimensions

# 1516 + 272 + 167 + 146 + 29 = 2130 (April 25')

# # VERSION 2: ALL IUs
# ggsave("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Publications/Intervention histories/figures/combined_inthist_freqdist_plot_V2_ALLIUs.pdf", final_plot, width = 8.27, height = 11.69, units = "in")  # A4 landscape dimensions

# ========================================== #
#    intervention histories - with OEM       #
# ========================================== #

# IUs with generic histories, but considered Unknown (under LF MDA) or Unknown (require OEM)
# instead of MDA having stopped, these could be labelled "previous IVM MDA, requiring OEM"

Additional_IUs_MDA_to_trtnaive <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Planning final Endgame sims (March-May 25)/Additional_IUs_MDA_to_trtnaive.csv")

IUs_MDA_to_trtnaive <- Additional_IUs_MDA_to_trtnaive$IU_ID 

co_endemicity_maps_final$biannual_VC_mapping_v2 <- co_endemicity_maps_final$biannual_VC_mapping

co_endemicity_maps_final$biannual_VC_mapping_v2 <- ifelse(co_endemicity_maps_final$IU_ID %in% IUs_MDA_to_trtnaive, "previous IVM MDA, requiring OEM", co_endemicity_maps_final$biannual_VC_mapping_v2)

unique(co_endemicity_maps_final$biannual_VC_mapping_v2)

# change in endgame (trt naive)
co_endemicity_maps_final$status_simple <- ifelse(co_endemicity_maps_final$trt_status_2022 == "Treatment naive", "in Endgame 
(treatment naive)", co_endemicity_maps_final$status_simple)

# change in endgame (MDA)
co_endemicity_maps_final$status_simple <- ifelse(co_endemicity_maps_final$trt_status_2022 != "Treatment naive", "in Endgame 
(intervention history)", co_endemicity_maps_final$status_simple)

co_endemicity_maps_final$status_simple <- ifelse(co_endemicity_maps_final$IU_ID %in% IUs_MDA_to_trtnaive, "switch to treatment naive 
(from intervention history
in Endgame)", co_endemicity_maps_final$status_simple)


cbPalette2 <- c("#CC79A7","#E69F00","#009E73","#F0E442","#0072B2")

p_map <- ggplot() +
  geom_sf(data = oceans_shp, aes(), fill = "lightblue", colour = NA) +
  geom_sf(data = co_endemicity_maps_final, aes(fill = biannual_VC_mapping), colour = NA, alpha = 0.7) +
  #geom_sf(data = ESPEN_IUs_ALL, aes(), colour = "lightgrey", linewidth = 0.1, size= 0.1, fill = NA, alpha = 0.1) +
  #geom_sf(data = ESPEN_IUs_ALL, aes(), colour = NA, size = 1, fill = NA, alpha = 0.1) +
  geom_sf(data = ESPEN_IUs_ALL, aes(), colour = "black", linewidth = 0.01, fill = NA, alpha = 0.3) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
  geom_sf_text(data = african_centroids_oncho, aes(label = NAME_0), size = 1.5, color = "black", fontface = "bold", alpha = 0.7) + # Add country labels
  coord_sf(xlim = c(-15, 45), ylim = c(22, -16)) +
  theme_bw() +
  scale_fill_manual(values = cbPalette2, na.value = "gray") +
  scale_colour_manual(na.value="gray")+
  #scale_size_identity()+
  labs(fill='', x = " ", y = " ") +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal",
    legend.text = element_text(size = 7),  # Adjust size of legend text
    legend.title = element_text(size = 10))  # Adjust size of legend title)

# just bioko #
p_bioko <- ggplot() +
  geom_sf(data = oceans_shp, aes(), fill = "lightblue", colour = NA) +
  geom_sf(data = co_endemicity_maps_final, aes(fill = biannual_VC_mapping), colour = NA, alpha = 0.7) +
  geom_sf(data = ESPEN_IUs_ALL, aes(), colour = "black", linewidth = 0.01, fill = NA, alpha = 0.3) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
  coord_sf(xlim = c(8, 10), ylim = c(3, 5)) +
  theme_bw() +
  scale_fill_manual(values = cbPalette2, na.value = "gray") +
  scale_colour_manual(na.value = "gray") +
  labs(fill = '') +
  theme(
    legend.position = "none",         # Remove the legend
    axis.text = element_blank(),      # Remove axis text (labels)
    axis.ticks = element_blank(),     # Remove axis ticks
    axis.title = element_blank(),     # Remove axis titles
    panel.grid.major = element_blank(),  # Optionally remove grid lines
    panel.grid.minor = element_blank()   # Optionally remove minor grid lines
  )

# Combine p_map and p_bioko using ggdraw
final_map_plot2 <- ggdraw() +
  draw_plot(p_map, 0, 0, 1, 1) +  # Main map takes the whole space
  draw_plot(p_bioko, 0.2, 0.32, 0.25, 0.25) + # Inset map in bottom-left corner (adjust size and position as needed)
  draw_text("a", x = 0.1, y = 1, hjust = 0, vjust = 1, size = 10, fontface = "bold")

# # Display the final plot
# print(final_map_plot)

# ggsave("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Publications/Intervention histories/figures/intervention_histories_maps.pdf",
#        plot = final_map_plot2, device = "pdf", width = 11.69, height = 8.27) # A4 portrait size in inches

# increase resolution so that IU lines visible but not too thick
agg_png("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Publications/Intervention histories/figures/intervention_histories_maps.png", 6000, 4000, scaling = 12)
final_map_plot2
dev.off()


# ========================================== #
#    modelled cumulative MDA rounds          #
# ========================================== #

# VERSION 1 - Endgame only
co_endemicity_maps_final <- co_endemicity_maps_final_onlyendgame # VERSION 1 (ONLY THOSE IN ENDGAME n = 2132)
nrow(co_endemicity_maps_final)

## VERSION 2 - all IUs #
#co_endemicity_maps_final <- co_endemicity_maps_final_ALLIUs # VERSION 1 (ONLY THOSE IN ENDGAME n = 2132)


#cbPalette2 <- c("#CC79A7","#E69F00","#009E73","#F0E442","#0072B2")

p_map <- ggplot() +
  geom_sf(data = oceans_shp, aes(), fill = "lightblue", colour = NA) +
  geom_sf(data = co_endemicity_maps_final, aes(fill = CUM_MDA_modelled), colour = NA, alpha = 0.7) +
  #geom_sf(data = ESPEN_IUs_ALL, aes(), colour = "lightgrey", linewidth = 0.1, size= 0.1, fill = NA, alpha = 0.1) +
  #geom_sf(data = ESPEN_IUs_ALL, aes(), colour = NA, size = 1, fill = NA, alpha = 0.1) +
  geom_sf(data = ESPEN_IUs_ALL, aes(), colour = "black", linewidth = 0.01, fill = NA, alpha = 0.3) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
  geom_sf_text(data = african_centroids_oncho, aes(label = NAME_0), size = 1.5, color = "black", fontface = "bold", alpha = 0.7) + # Add country labels
  #coord_sf(xlim = c(-15, 45), ylim = c(22, -15)) +
  coord_sf(xlim = c(-15, 45), ylim = c(22, -16.5)) + # for All IUs
  theme_bw() +
  scale_fill_gradient(low = "yellow", high = "darkblue", na.value = "gray") +
  scale_colour_manual(na.value="white") +
  #scale_size_identity()+
  labs(fill='Revised number of cumulative MDA rounds', x = " ", y = " ") +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal",
    legend.text = element_text(size = 7),  # Adjust size of legend text
    legend.title = element_text(size = 10))  # Adjust size of legend title))

# just bioko #
p_bioko <- ggplot() +
  geom_sf(data = oceans_shp, aes(), fill = "lightblue", colour = NA) +
  geom_sf(data = co_endemicity_maps_final, aes(fill = CUM_MDA_modelled), colour = NA, alpha = 0.7) +
  geom_sf(data = ESPEN_IUs_ALL, aes(), colour = "black", linewidth = 0.01, fill = NA, alpha = 0.3) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
  coord_sf(xlim = c(8, 10), ylim = c(3, 5)) +
  theme_bw() +
  scale_fill_gradient(low = "yellow", high = "darkblue", na.value = "gray") +
  scale_colour_manual(na.value="white") +
  labs(fill = 'Revised number of cumulative MDA rounds') +
  theme(
    legend.position = "none",         # Remove the legend
    axis.text = element_blank(),      # Remove axis text (labels)
    axis.ticks = element_blank(),     # Remove axis ticks
    axis.title = element_blank(),     # Remove axis titles
    panel.grid.major = element_blank(),  # Optionally remove grid lines
    panel.grid.minor = element_blank()   # Optionally remove minor grid lines
  )

# Combine p_map and p_bioko using ggdraw
final_map_plot3 <- ggdraw() +
  draw_plot(p_map, 0, 0, 1, 1) +  # Main map takes the whole space
  draw_plot(p_bioko, 0.2, 0.32, 0.25, 0.25) + # Inset map in bottom-left corner (adjust size and position as needed)
  draw_text("a", x = 0.1, y = 1, hjust = 0, vjust = 1, size = 10, fontface = "bold")


# # Display the final plot
# print(final_map_plot)
# 
# ggsave("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Publications/Intervention histories/figures/modelledcumMDA_maps.pdf",
#        plot = final_map_plot2, device = "pdf", width = 11.69, height = 8.27) # A4 portrait size in inches

# increase resolution so that IU lines visible but not too thick

# VERSION 1: endgame IUs (n = 2130: April 25')
agg_png("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Publications/Intervention histories/figures/modelledcumMDA_maps_V1.png", 6000, 4000, scaling = 12)
final_map_plot3
dev.off()

# # VERSION 2: ALL IUs (n = )
# agg_png("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Publications/Intervention histories/figures/modelledcumMDA_maps_V2_ALLIUs.png", 6000, 4000, scaling = 12)
# final_map_plot3
# dev.off()

# ====================================================== #
#       Make FREQUENCY DISTRIBUTION PLOTS                #

# # ALL IUS with MDA and TREATMENT NAIVE (APRIL 2025)
# Full_histories_df_popinfo <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_popinfo_ALL_minimal_070425_listlabels.csv")
# Full_histories_df_minimal_lastyr_2022 <- subset(Full_histories_df_popinfo, Year == 2022)
# Full_histories_df_minimal_lastyr_2013 <- subset(Full_histories_df_popinfo, Year == 2013)
# Full_histories_df_minimal_lastyr_2013_CUMMDA0 <- subset(Full_histories_df_minimal_lastyr_2013, Cum_MDA_ESPEN == 0)

# =========================================================== #
#         Subset those just in Endgame                        #

Full_histories_df_minimal_lastyr_2022_check <- subset(Full_histories_df_minimal_lastyr_2022, !is.na(Control_prog)) # 2138 IUs
nrow(Full_histories_df_minimal_lastyr_2022_check) # 2130 IUs (April 25')

# # only keep SDN and GAB treatment naive IUs
# Full_histories_df_minimal_lastyr_2022_check <- Full_histories_df_minimal_lastyr_2022_check %>%
#   filter(
#     # Keep "Treatment naive" only if ADMIN0ISO3 is "GAB" or "SDN"
#     (trt_status_2022 == "Treatment naive" & ADMIN0ISO3 %in% c("GAB", "SDN")) |
#       # Keep any row where trt_status_2022 is not "Treatment naive"
#       trt_status_2022 != "Treatment naive",
#     # Exclude a specific IU_ID
#     IU_CODE_MAPPING != "ETH0194319529"
#   )

#Full_histories_df_minimal_lastyr_2022_check_trtnaive <- subset(Full_histories_df_minimal_lastyr_2022_check, trt_status_2022 == "Treatment naive")

Full_histories_df_minimal_lastyr_2022 <- Full_histories_df_minimal_lastyr_2022_check # RESET HERE

# # VERSION 2 - RESET ALL IUS dataframe HERE #
# # ALL IUS with MDA and TREATMENT NAIVE (APRIL 2025)
# Full_histories_df_popinfo <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_popinfo_ALL_minimal_070425_listlabels.csv")
# Full_histories_df_minimal_lastyr_2022 <- subset(Full_histories_df_popinfo, Year == 2022)


# # Plot the histogram with a gradient fill
# ggplot(Full_histories_df_minimal_lastyr_2022, aes(x = CUM_MDA_modelled, fill = CUM_MDA_modelled)) +
#   geom_histogram(binwidth = 1, color = "black", position = "identity", alpha = 0.7) +
#   scale_fill_gradient(low = "yellow", high = "darkblue", na.value = "gray") +
#   labs(x = "Revised number of cumulative MDA rounds",
#        y = "Frequency",
#        fill = "Cumulative MDA") +
#   theme_minimal() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.border = element_rect(colour = "black", fill = NA),
#         axis.line = element_line(colour = "black")) +
#   facet_wrap(~ADMIN0ISO3, scales = "free_y")  # Separate histograms by ADMIN0ISO3 with free y-axis scaling

# Get the min and max of CUM_MDA_modelled
min_value <- min(Full_histories_df_minimal_lastyr_2022$CUM_MDA_modelled, na.rm = TRUE)
max_value <- max(Full_histories_df_minimal_lastyr_2022$CUM_MDA_modelled, na.rm = TRUE)

# Create the plot with a customized color scale
p1 <- ggplot(Full_histories_df_minimal_lastyr_2022, aes(x = CUM_MDA_modelled, fill = ..x..)) +
  geom_histogram(binwidth = 1, color = "black", position = "identity", alpha = 0.7) +
  scale_fill_gradient(low = "yellow", high = "darkblue", 
                      limits = c(min_value, max_value),  # Set the range of the legend
                      breaks = seq(min_value, max_value, by = 10)) +  # Set breaks every 10 units
  scale_x_continuous(breaks = seq(0, 50, by = 5)) +  # Set x-axis ticks every 10 units
  labs(x = "Revised number of cumulative MDA rounds",
       y = "Frequency",
       fill = "Revised number of cumulative MDA rounds") +
  guides(fill = guide_legend(title = NULL)) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = 12))  # Change the size of x-axis labels) +)

p1

# Create the plot with a customized color scale
# p2 <- ggplot(Full_histories_df_minimal_lastyr_2022, aes(x = CUM_MDA_modelled, fill = ..x..)) +
#   geom_histogram(binwidth = 2, color = "black", position = "identity", alpha = 0.7) +
#   scale_fill_gradient(low = "yellow", high = "darkblue", 
#                       limits = c(min_value, max_value),  # Set the range of the legend
#                       breaks = seq(min_value, max_value, by = 10)) +  # Set breaks every 10 units
#   scale_x_continuous(breaks = seq(0, 50, by = 10)) +  # Set x-axis ticks every 10 units
#   labs(x = "Revised number of cumulative MDA rounds",
#        y = "Frequency",
#        fill = "Revised number of cumulative MDA rounds") +
#   theme_minimal() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.border = element_rect(colour = "black", fill = NA),
#         axis.line = element_line(colour = "black"),
#         axis.text.x = element_text(size = 8, angle = 45, hjust = 1))+
#   guides(fill = guide_legend(title = NULL)) +
#   # Change the size of x-axis labels) +
#   facet_wrap(~ADMIN0ISO3, scales = "free_y")  # Separate histograms by ADMIN0ISO3 with free y-axis scaling
# 
# p2

# frequency dist plot by country
freq_table_cumMDA <- table(Full_histories_df_minimal_lastyr_2022$CUM_MDA_modelled, Full_histories_df_minimal_lastyr_2022$ADMIN0ISO3) # Create a frequency table

freq_df_cumMDA <- as.data.frame(as.table(freq_table_cumMDA)) # Convert the frequency table to a data frame
colnames(freq_df_histint) <- c("Cum_MDA", "ADMIN0ISO3", "Frequency") # Rename the columns for better readability

freq_df_cumMDA <- subset(freq_df_cumMDA, Freq > 0)

p2 <- ggplot(Full_histories_df_minimal_lastyr_2022, aes(x = CUM_MDA_modelled, fill = ..x..)) +
  # geom_text(data = subset(CUM_MDA_modelled, Frequency > 0),
  #           aes(label = Frequency),
  #           vjust = -0.4,  # Adjusts the position of the text (above the bar)
  #           size = 3,      # Adjusts the size of the text
  #           colour = "black") +  # Color of the text
    # For CUM_MDA_modelled == 0, apply red border
  geom_histogram(data = subset(Full_histories_df_minimal_lastyr_2022, CUM_MDA_modelled == 0),
                 binwidth = 2, color = "red", position = "identity", alpha = 0.7) +
  
  # For non-zero CUM_MDA_modelled values, apply black border
  geom_histogram(data = subset(Full_histories_df_minimal_lastyr_2022, CUM_MDA_modelled != 0),
                 binwidth = 2, color = "black", position = "identity", alpha = 0.7) +
  
  scale_fill_gradient(low = "yellow", high = "darkblue", 
                      limits = c(min_value, max_value),  # Set the range of the legend
                      breaks = seq(min_value, max_value, by = 10)) +  # Set breaks every 10 units
  scale_x_continuous(breaks = seq(0, 50, by = 10)) +  # Set x-axis ticks every 10 units
  labs(x = "Revised number of cumulative MDA rounds",
       y = "Frequency",
       fill = "Revised number of cumulative MDA rounds") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.line = element_line(colour = "black"),  # Ensures that the other axis lines are black
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
        axis.line.x = element_blank(),  # Removes the x-axis line at the bottom
        legend.position = "bottom",
        legend.text = element_text(size = 8)) +
  guides(fill = guide_legend(title = NULL)) +  # Remove legend title
  facet_wrap(~ADMIN0ISO3, scales = "free_y")  # Separate histograms by ADMIN0ISO3 with free y-axis scaling

# Print the plot
print(p2)


# combine
p1_no_legend <- p1 + theme(legend.position = "none")
p2_no_legend <- p2 + theme(legend.position = "none")

# Extract the shared legend from one of the plots
shared_legend <- get_legend(p1 + theme(legend.position = "none"))

# Combine p1 and p2 in a vertical layout, without titles
combined_plots <- plot_grid(
  p1_no_legend, p2_no_legend,
  labels = c("b", "c"),        # Add "A" and "B" labels
  label_size = 18,             # Adjust label size
  ncol = 1,                    # Stack them vertically
  rel_heights = c(1/4, 3/4)    # Set relative heights: p1 takes 1/4, p2 takes 3/4
)

# Add the shared legend at the bottom
final_plot <- plot_grid(
  combined_plots, shared_legend,
  ncol = 1,
  rel_heights = c(1, 0.1)  # Legend takes 10% of the space at the bottom
)

# Display the final plot with the shared legend
#print(final_plot)


# # Optionally, save to PDF in portrait mode (A4 size)
# ggsave("p1_p2_combined_portrait.pdf", combined_plot, width = 8.27, height = 11.69, units = "in")  # A4 portrait dimensions

# Save the combined plot to a landscape A4 PDF

# VERSION 1: only Endgame IUs (n = 2132)
ggsave("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Publications/Intervention histories/figures/combined_modelledMDA_freqdist_plot_V1.pdf", final_plot, width = 8.27, height = 11.69, units = "in")  # A4 landscape dimensions


# # VERSION 2: only Endgame IUs (n = 2132)
# ggsave("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Publications/Intervention histories/figures/combined_modelledMDA_freqdist_plot_V2_ALLIUs.pdf", final_plot, width = 8.27, height = 11.69, units = "in")  # A4 landscape dimensions


# ========================================== #
#    MDA status 2022                         #
# ========================================== #

# VERSION 1 - Endgame only
co_endemicity_maps_final <- co_endemicity_maps_final_onlyendgame # VERSION 1 (ONLY THOSE IN ENDGAME n = 2132)

# ## VERSION 2 - all IUs #
# co_endemicity_maps_final <- co_endemicity_maps_final_ALLIUs # VERSION 1 (ONLY THOSE IN ENDGAME n = 2132)

# # NEED TO CHANGE: Adagnehagre chako/ ETH0194319529 (from MDA stopped to Treatment Naive in VERSION 2
# co_endemicity_maps_final$trt_status_2022 <- ifelse(co_endemicity_maps_final$IU_CODE_MAPPING == "ETH0194319529",
#                                                    "Treatment-naive", co_endemicity_maps_final$trt_status_2022)


# # need to recode one IU in NGA as "Treatment-naïve" in trt_status_2022 for next set of plots #
# co_endemicity_maps_final$trt_status_2022 <- ifelse(co_endemicity_maps_final$IUs_NAME == "Wase" & 
#                                                      co_endemicity_maps_final$biannual_VC_mapping == "treatment naive",
#                                                    "Treatment-naive", co_endemicity_maps_final$trt_status_2022)
# change label fro PHASE I
unique(co_endemicity_maps_final$trt_status_2022)

co_endemicity_maps_final <- co_endemicity_maps_final %>%
  mutate(trt_status_2022 = ifelse(trt_status_2022 == "Treatment naive", "Treatment-naïve", trt_status_2022))

unique(co_endemicity_maps_final$trt_status_2022)

#check <- subset(co_endemicity_maps_final, biannual_VC_mapping == "Treatment-naïve" & ADMIN0ISO3.x == "NGA")

# # Define the desired order
# desired_order <- c(
#   "Annual MDA","Annual MDA and vector control","Biannual MDA",
#   "Biannual MDA and vector control","Treatment-naïve")
# 
# # Reorder the co_endemicity column in your dataframe
# co_endemicity_maps_final$biannual_VC_mapping <- factor(
#   co_endemicity_maps_final$biannual_VC_mapping, 
#   levels = desired_order
# )


# cbPalette2 <- c("#CC79A7","#E69F00","#009E73","#F0E442","#0072B2")
# 
# p_map <- ggplot() +
#   geom_sf(data = oceans_shp, aes(), fill = "lightblue", colour = NA) +
#   geom_sf(data = co_endemicity_maps_final, aes(fill = trt_status_2022), colour = NA, alpha = 0.7) +
#   #geom_sf(data = ESPEN_IUs_ALL, aes(), colour = "lightgrey", linewidth = 0.1, size= 0.1, fill = NA, alpha = 0.1) +
#   geom_sf(data = ESPEN_IUs_ALL, aes(),  colour = "black", linewidth = 0.01, fill = NA, alpha = 0.3) +
#   geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
#   geom_sf_text(data = african_centroids_oncho, aes(label = NAME_0), size = 1.5, color = "black", fontface = "bold", alpha = 0.7) + # Add country labels
#   #coord_sf(xlim = c(-15, 45), ylim = c(22, -15)) +
#   coord_sf(xlim = c(-15, 45), ylim = c(22, -16.5)) + # for All IUs
#   theme_bw() +
#   scale_fill_manual(values = cbPalette2, na.value = "gray") +
#   scale_colour_manual(na.value="gray")+
#   #scale_size_identity()+
#   labs(fill='', x = " ", y = " ") +
#   theme(
#     legend.position = "bottom",  # Place the legend at the bottom
#     legend.direction = "horizontal",
#     legend.text = element_text(size = 7),  # Adjust size of legend text
#     legend.title = element_text(size = 10))  # Adjust size of legend title)))
# 
# # just bioko #
# p_bioko <- ggplot() +
#   geom_sf(data = oceans_shp, aes(), fill = "lightblue", colour = NA) +
#   geom_sf(data = co_endemicity_maps_final, aes(fill = trt_status_2022), colour = NA, alpha = 0.7) +
#   geom_sf(data = ESPEN_IUs_ALL, aes(), colour = "black", linewidth = 0.01, fill = NA, alpha = 0.3) +
#   geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
#   coord_sf(xlim = c(8, 10), ylim = c(3, 5)) +
#   theme_bw() +
#   scale_fill_manual(values = cbPalette2, na.value = "gray") +
#   scale_colour_manual(na.value = "gray") +
#   labs(fill = '') +
#   theme(
#     legend.position = "none",         # Remove the legend
#     axis.text = element_blank(),      # Remove axis text (labels)
#     axis.ticks = element_blank(),     # Remove axis ticks
#     axis.title = element_blank(),     # Remove axis titles
#     panel.grid.major = element_blank(),  # Optionally remove grid lines
#     panel.grid.minor = element_blank()   # Optionally remove minor grid lines
#   )
# 
# # Combine p_map and p_bioko using ggdraw
# final_map_plot4 <- ggdraw() +
#   draw_plot(p_map, 0, 0, 1, 1) +  # Main map takes the whole space
#   draw_plot(p_bioko, 0.2, 0.32, 0.25, 0.25) +  # Inset map in bottom-left corner (adjust size and position as needed)
#   draw_text("a", x = 0.1, y = 1, hjust = 0, vjust = 1, size = 10, fontface = "bold")
# 
# # # Display the final plot
# # print(final_map_plot)
# 
# # ggsave("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Publications/Intervention histories/figures/trtstatus2022_maps.pdf",
# #        plot = final_map_plot2, device = "pdf", width = 11.69, height = 8.27) # A4 portrait size in inches
# 
# # increase resolution so that IU lines visible but not too thick
# 
# # VERSION 1: Endgame IUs (n = 2132)
# agg_png("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Publications/Intervention histories/figures/trtstatus2022_maps_V1.png", 6000, 4000, scaling = 12)
# final_map_plot4
# dev.off()

# =================================== #
# FREQUENCY DISTRIBUTION PLOTS        #



# frequency dist plot
freq_table_trtstat22_2 <- table(Full_histories_df_minimal_lastyr_2022$trt_status_2022) # Create a frequency table

freq_df_trtstat22_2 <- as.data.frame(as.table(freq_table_trtstat22_2)) # Convert the frequency table to a data frame
colnames(freq_df_trtstat22_2) <- c("trt_status_2022", "Frequency") # Rename the columns for better readability

# Create the frequency distribution plot
p1 <- ggplot(freq_df_trtstat22_2, aes(x = trt_status_2022, y = Frequency, fill = trt_status_2022)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  geom_text(data = subset(freq_df_trtstat22_2, Frequency > 0),
            aes(label = Frequency),
            vjust = -0.4,  # Adjusts the position of the text (above the bar)
            size = 3,      # Adjusts the size of the text
            colour = "black") +  # Color of the text
  labs(x = "Treatment status in 2022",
       y = "Frequency",
       fill = "treatment status") +
  scale_fill_manual(values = cbPalette2) +
  theme_minimal()+
  theme(axis.text.x = element_blank(),  # Remove x-axis labels
        axis.ticks.x = element_blank(),  # Remove x-axis ticks if needed
        #axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.line = element_line(colour = "black"),
        legend.position = "bottom",
        legend.text = element_text(size = 8),  # Adjust text size
        legend.key.size = unit(0.5, "lines"))+   # Adjust key size)
guides(fill = guide_legend(title = NULL))
# frequency dist plot by country
freq_table_trtstat22 <- table(Full_histories_df_minimal_lastyr_2022$trt_status_2022, Full_histories_df_minimal_lastyr_2022$ADMIN0ISO3) # Create a frequency table

freq_df_trtstat22 <- as.data.frame(as.table(freq_table_trtstat22)) # Convert the frequency table to a data frame
colnames(freq_df_trtstat22) <- c("trt_status_2022", "ADMIN0ISO3", "Frequency") # Rename the columns for better readability

# Create the frequency distribution plot
p2 <- ggplot(freq_df_trtstat22, aes(x = trt_status_2022, y = Frequency, fill = trt_status_2022)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  geom_text(data = subset(freq_df_trtstat22, Frequency > 0),
            aes(label = Frequency),
            vjust = -0.4,  # Adjusts the position of the text (above the bar)
            size = 3,      # Adjusts the size of the text
            colour = "black") +  # Color of the text
  labs(x = "Treatment status in 2022",
       y = "Frequency",
       fill = "treatment status") +
  scale_fill_manual(values = cbPalette2) +
  theme_minimal()+
  theme(axis.text.x = element_blank(),  # Remove x-axis labels
        axis.ticks.x = element_blank(),  # Remove x-axis ticks if needed
        #axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.line = element_line(colour = "black"),
        )+
  guides(fill = guide_legend(title = NULL)) +
  facet_wrap(~ADMIN0ISO3)

# combine
p1_no_legend <- p1 + theme(legend.position = "none")
p2_no_legend <- p2 + theme(legend.position = "none")

# Extract the shared legend from one of the plots
shared_legend <- get_legend(p1 + theme(legend.position = "bottom"))

# Combine p1 and p2 in a vertical layout, without titles
combined_plots <- plot_grid(
  p1_no_legend, p2_no_legend,
  labels = c("b", "c"),        # Add "A" and "B" labels
  label_size = 18,             # Adjust label size
  ncol = 1,                    # Stack them vertically
  rel_heights = c(1/4, 3/4)    # Set relative heights: p1 takes 1/4, p2 takes 3/4
)

# Add the shared legend at the bottom
final_plot <- plot_grid(
  combined_plots, shared_legend,
  ncol = 1,
  rel_heights = c(1, 0.1)  # Legend takes 10% of the space at the bottom
)

# Display the final plot with the shared legend
#print(final_plot)


# # Optionally, save to PDF in portrait mode (A4 size)
# ggsave("p1_p2_combined_portrait.pdf", combined_plot, width = 8.27, height = 11.69, units = "in")  # A4 portrait dimensions

# Save the combined plot to a landscape A4 PDF
ggsave("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Publications/Intervention histories/figures/combined_trtstatus2022_freqdist_plot.pdf", final_plot, width = 8.27, height = 11.69, units = "in")  # A4 landscape dimensions


# # ========================================== #
# #    MDA status 2022 - with OEM       #
# # ========================================== #
# 
# # IUs with generic histories, but considered Unknown (under LF MDA) or Unknown (require OEM)
# # instead of MDA having stopped, these could be labelled "previous IVM MDA, requiring OEM"
# 
Additional_IUs_MDA_to_trtnaive <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Planning final Endgame sims (March-May 25)/Additional_IUs_MDA_to_trtnaive.csv")

IUs_MDA_to_trtnaive <- Additional_IUs_MDA_to_trtnaive$IU_ID

co_endemicity_maps_final$trt_status_2022_OEM <- co_endemicity_maps_final$trt_status_2022

co_endemicity_maps_final$trt_status_2022_OEM <- ifelse(co_endemicity_maps_final$IU_ID %in% IUs_MDA_to_trtnaive, "some previous IVM MDA; 
potentially requiring OEM and/or SM", co_endemicity_maps_final$trt_status_2022_OEM)

unique(co_endemicity_maps_final$trt_status_2022_OEM)

# cbPalette2 <- c("#CC79A7","#E69F00","#009E73","#F0E442","#0072B2")
# 
# p_map <- ggplot() +
#   geom_sf(data = oceans_shp, aes(), fill = "lightblue", colour = NA) +
#   geom_sf(data = co_endemicity_maps_final, aes(fill = trt_status_2022_OEM), colour = NA, alpha = 0.7) +
#   #geom_sf(data = ESPEN_IUs_ALL, aes(), colour = "lightgrey", linewidth = 0.1, size= 0.1, fill = NA, alpha = 0.1) +
#   geom_sf(data = ESPEN_IUs_ALL, aes(),  colour = "black", linewidth = 0.01, fill = NA, alpha = 0.3) +
#   geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
#   geom_sf_text(data = african_centroids_oncho, aes(label = NAME_0), size = 1.5, color = "black", fontface = "bold", alpha = 0.7) + # Add country labels
#   coord_sf(xlim = c(-15, 45), ylim = c(22, -15)) +
#   theme_bw() +
#   scale_fill_manual(values = cbPalette2, na.value = "gray") +
#   scale_colour_manual(na.value="gray")+
#   #scale_size_identity()+
#   labs(fill='', x = " ", y = " ") +
#   theme(
#     legend.position = "bottom",  # Place the legend at the bottom
#     legend.direction = "horizontal",
#     legend.text = element_text(size = 7),  # Adjust size of legend text
#     legend.title = element_text(size = 10))  # Adjust size of legend title)))
# 
# # just bioko #
# p_bioko <- ggplot() +
#   geom_sf(data = oceans_shp, aes(), fill = "lightblue", colour = NA) +
#   geom_sf(data = co_endemicity_maps_final, aes(fill = trt_status_2022_OEM), colour = NA, alpha = 0.7) +
#   geom_sf(data = ESPEN_IUs_ALL, aes(), colour = "black", linewidth = 0.01, fill = NA, alpha = 0.3) +
#   geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
#   coord_sf(xlim = c(8, 10), ylim = c(3, 5)) +
#   theme_bw() +
#   scale_fill_manual(values = cbPalette2, na.value = "gray") +
#   scale_colour_manual(na.value = "gray") +
#   labs(fill = '') +
#   theme(
#     legend.position = "none",         # Remove the legend
#     axis.text = element_blank(),      # Remove axis text (labels)
#     axis.ticks = element_blank(),     # Remove axis ticks
#     axis.title = element_blank(),     # Remove axis titles
#     panel.grid.major = element_blank(),  # Optionally remove grid lines
#     panel.grid.minor = element_blank()   # Optionally remove minor grid lines
#   )
# 
# # Combine p_map and p_bioko using ggdraw
# final_map_plot4_OEM <- ggdraw() +
#   draw_plot(p_map, 0, 0, 1, 1) +  # Main map takes the whole space
#   draw_plot(p_bioko, 0.2, 0.32, 0.25, 0.25) +  # Inset map in bottom-left corner (adjust size and position as needed)
#   draw_text("a", x = 0.1, y = 1, hjust = 0, vjust = 1, size = 10, fontface = "bold")
# 
# # # Display the final plot
# # print(final_map_plot)
# 
# # ggsave("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Publications/Intervention histories/figures/trtstatus2022_maps.pdf",
# #        plot = final_map_plot2, device = "pdf", width = 11.69, height = 8.27) # A4 portrait size in inches
# 
# # increase resolution so that IU lines visible but not too thick
# agg_png("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Publications/Intervention histories/figures/trtstatus2022_maps_wOEM.png", 6000, 4000, scaling = 12)
# final_map_plot4_OEM
# dev.off()


# # ========================================== #
# #    MDA status 2022 - with OEM (+OCP)      #
# # ========================================== #
# 
# # IUs with generic histories, but considered Unknown (under LF MDA) or Unknown (require OEM)
# # instead of MDA having stopped, these could be labelled "previous IVM MDA, requiring OEM"
# 
Additional_IUs_MDA_to_trtnaive_OCP <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Planning final Endgame sims (March-May 25)/Additional_IUs_MDA_to_trtnaive_OCP.csv")

IUs_MDA_to_trtnaive_OCP <- Additional_IUs_MDA_to_trtnaive_OCP$IU_ID

co_endemicity_maps_final$trt_status_2022_OEM_v2 <- co_endemicity_maps_final$trt_status_2022_OEM

co_endemicity_maps_final$trt_status_2022_OEM_v2 <- ifelse(co_endemicity_maps_final$IU_ID %in% IUs_MDA_to_trtnaive_OCP, "some previous IVM MDA; 
potentially requiring OEM and/or SM*", co_endemicity_maps_final$trt_status_2022_OEM_v2)

co_endemicity_maps_final <- co_endemicity_maps_final %>%
  mutate(trt_status_2022_OEM_v2 = ifelse(trt_status_2022_OEM_v2 == "Treatment-naïve", "Treatment-naïve;
potentially requiring OEM and/or SM", trt_status_2022_OEM_v2))


unique(co_endemicity_maps_final$trt_status_2022_OEM_v2)

check_somMDAIUs <- subset(co_endemicity_maps_final, trt_status_2022_OEM_v2 %in% c("some previous IVM MDA; 
potentially requiring OEM and/or SM*", "some previous IVM MDA; 
potentially requiring OEM and/or SM"))

table(check_somMDAIUs$MAX_Endemicity)

#subset_someIVM_APOC <- subset(co_endemicity_maps_final, trt_status_2022_OEM_v2 == "some previous IVM MDA; \npotentially requiring OEM and/or SM")
#all_IUs_someIVM <- c(IUs_MDA_to_trtnaive, IUs_MDA_to_trtnaive_OCP)
# check <- subset(Full_histories_df_popinfo, IU_ID_MAPPING %in% all_IUs_someIVM)
# length(unique(check$IU_ID_MAPPING))

cbPalette2 <- c("#CC79A7","#E69F00","#009E73","#F0E442","#0072B2")

p_map <- ggplot() +
  geom_sf(data = oceans_shp, aes(), fill = "lightblue", colour = NA) +
  geom_sf(data = co_endemicity_maps_final, aes(fill = trt_status_2022_OEM_v2), colour = NA, alpha = 0.7) +
  #geom_sf(data = ESPEN_IUs_ALL, aes(), colour = "lightgrey", linewidth = 0.1, size= 0.1, fill = NA, alpha = 0.1) +
  geom_sf(data = ESPEN_IUs_ALL, aes(),  colour = "black", linewidth = 0.01, fill = NA, alpha = 0.3) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
  geom_sf_text(data = african_centroids_oncho, aes(label = NAME_0), size = 1.5, color = "black", fontface = "bold", alpha = 0.7) + # Add country labels
  coord_sf(xlim = c(-15, 45), ylim = c(22, -16)) +
  #coord_sf(xlim = c(-15, 45), ylim = c(22, -16.5)) + # for All IUs
  theme_bw() +
  scale_fill_manual(values = cbPalette2, na.value = "gray") +
  scale_colour_manual(na.value="gray")+
  #scale_size_identity()+
  labs(fill='', x = " ", y = " ") +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal",
    legend.text = element_text(size = 7),  # Adjust size of legend text
    legend.title = element_text(size = 10))  # Adjust size of legend title)))

# just bioko #
p_bioko <- ggplot() +
  geom_sf(data = oceans_shp, aes(), fill = "lightblue", colour = NA) +
  geom_sf(data = co_endemicity_maps_final, aes(fill = trt_status_2022_OEM_v2), colour = NA, alpha = 0.7) +
  geom_sf(data = ESPEN_IUs_ALL, aes(), colour = "black", linewidth = 0.01, fill = NA, alpha = 0.3) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
  coord_sf(xlim = c(8, 10), ylim = c(3, 5)) +
  theme_bw() +
  scale_fill_manual(values = cbPalette2, na.value = "gray") +
  scale_colour_manual(na.value = "gray") +
  labs(fill = '') +
  theme(
    legend.position = "none",         # Remove the legend
    axis.text = element_blank(),      # Remove axis text (labels)
    axis.ticks = element_blank(),     # Remove axis ticks
    axis.title = element_blank(),     # Remove axis titles
    panel.grid.major = element_blank(),  # Optionally remove grid lines
    panel.grid.minor = element_blank()   # Optionally remove minor grid lines
  )

# Combine p_map and p_bioko using ggdraw
final_map_plot4_OEM_v2 <- ggdraw() +
  draw_plot(p_map, 0, 0, 1, 1) +  # Main map takes the whole space
  draw_plot(p_bioko, 0.2, 0.32, 0.25, 0.25) +  # Inset map in bottom-left corner (adjust size and position as needed)
  draw_text("a", x = 0.1, y = 1, hjust = 0, vjust = 1, size = 10, fontface = "bold")

#final_map_plot4_OEM_v2

# # Display the final plot
# print(final_map_plot)

# ggsave("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Publications/Intervention histories/figures/trtstatus2022_maps.pdf",
#        plot = final_map_plot2, device = "pdf", width = 11.69, height = 8.27) # A4 portrait size in inches

# increase resolution so that IU lines visible but not too thick

# VERSION 1: only Endgame IUs (n = 2130 April 25')
agg_png("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Publications/Intervention histories/figures/trtstatus2022_maps_V1_check.png", 6000, 4000, scaling = 12)
final_map_plot4_OEM_v2
dev.off()

# # VERSION 2: All IUs
# agg_png("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Publications/Intervention histories/figures/trtstatus2022_maps_V2_ALLIUs.png", 6000, 4000, scaling = 12)
# final_map_plot4_OEM_v2
# dev.off()


# ====================================================== #
#    check when last MDA for each MDA status             #

# Count frequency of last_year_MDA per Trt_status_2022
freq_table <- co_endemicity_maps_final %>%
  st_drop_geometry() %>%  # <<< Drop sf geometry first
  filter(!is.na(trt_status_2022)) %>%
  group_by(trt_status_2022, last_year_MDA) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(trt_status_2022, desc(last_year_MDA))

freq_table

# select out those with MDA counties but last mda year = 2012

# ============= #
# MDA continues #
freq_table_MDAcontinues <- co_endemicity_maps_final %>%
  st_drop_geometry() %>%  # <<< Drop sf geometry first
  filter(!is.na(trt_status_2022_v2) & trt_status_2022 == "MDA continues") %>%
  group_by(trt_status_2022_v2, last_year_MDA) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(desc(last_year_MDA))

freq_table_MDAcontinues

freq_table_MDAcontinues_lstMDAyr2022 <- subset(freq_table_MDAcontinues, last_year_MDA == 2022)
freq_table_MDAcontinues_lstMDAyr2021 <- subset(freq_table_MDAcontinues, last_year_MDA == 2021)
freq_table_MDAcontinues_lstMDAyr2020 <- subset(freq_table_MDAcontinues, last_year_MDA == 2020) # check from here
freq_table_MDAcontinues_lstMDAyr2019 <- subset(freq_table_MDAcontinues, last_year_MDA == 2019) 
freq_table_MDAcontinues_lstMDAyr2018 <- subset(freq_table_MDAcontinues, last_year_MDA == 2018) 
freq_table_MDAcontinues_lstMDAyr2017 <- subset(freq_table_MDAcontinues, last_year_MDA == 2017) 
freq_table_MDAcontinues_lstMDAyr2016 <- subset(freq_table_MDAcontinues, last_year_MDA == 2016) 
freq_table_MDAcontinues_lstMDAyr2015 <- subset(freq_table_MDAcontinues, last_year_MDA == 2015) 
freq_table_MDAcontinues_lstMDAyr2014 <- subset(freq_table_MDAcontinues, last_year_MDA == 2014) 
freq_table_MDAcontinues_lstMDAyr2013 <- subset(freq_table_MDAcontinues, last_year_MDA == 2013) 
freq_table_MDAcontinues_lstMDAyr2012 <- subset(freq_table_MDAcontinues, last_year_MDA == 2012) 



check_df <- subset(co_endemicity_maps_final, trt_status_2022 == "MDA continues" & last_year_MDA == 2020)
check_df <- subset(co_endemicity_maps_final, trt_status_2022 == "MDA continues" & last_year_MDA == 2019)
check_df <- subset(co_endemicity_maps_final, trt_status_2022 == "MDA continues" & last_year_MDA == 2018)
check_df <- subset(co_endemicity_maps_final, trt_status_2022 == "MDA continues" & last_year_MDA == 2017)
check_df <- subset(co_endemicity_maps_final, trt_status_2022 == "MDA continues" & last_year_MDA == 2016)
check_df <- subset(co_endemicity_maps_final, trt_status_2022 == "MDA continues" & last_year_MDA == 2015)
check_df <- subset(co_endemicity_maps_final, trt_status_2022 == "MDA continues" & last_year_MDA == 2014)
check_df <- subset(co_endemicity_maps_final, trt_status_2022 == "MDA continues" & last_year_MDA == 2012)

# ============= #
# MDA stopped #
check_df <- subset(co_endemicity_maps_final, trt_status_2022 == "MDA stopped" & last_year_MDA == 2022) # 8 in Senegal & 1 in Sudan
check_df <- subset(co_endemicity_maps_final, trt_status_2022 == "MDA stopped" & last_year_MDA == 2020)
check_df <- subset(co_endemicity_maps_final, trt_status_2022 == "MDA stopped" & last_year_MDA == 2019)
check_df <- subset(co_endemicity_maps_final, trt_status_2022 == "MDA stopped" & last_year_MDA == 2018)
check_df <- subset(co_endemicity_maps_final, trt_status_2022 == "MDA stopped" & last_year_MDA == 2017)
check_df <- subset(co_endemicity_maps_final, trt_status_2022 == "MDA stopped" & last_year_MDA == 2016)
check_df <- subset(co_endemicity_maps_final, trt_status_2022 == "MDA stopped" & last_year_MDA == 2015)
check_df <- subset(co_endemicity_maps_final, trt_status_2022 == "MDA stopped" & last_year_MDA == 2013)

check_df <- subset(co_endemicity_maps_final, trt_status_2022 == "MDA stopped" & last_year_MDA %in% c(2010:2022))


freq_table_MDAstopped <- co_endemicity_maps_final %>%
  st_drop_geometry() %>%  # <<< Drop sf geometry first
  filter(!is.na(trt_status_2022_v2) & trt_status_2022 == "MDA stopped") %>%
  group_by(trt_status_2022_v2, last_year_MDA) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(desc(last_year_MDA))

freq_table_MDAstopped

freq_table_MDAstopped_lstMDAyr2022 <- subset(freq_table_MDAstopped, last_year_MDA == 2022)
freq_table_MDAstopped_lstMDAyr2020 <- subset(freq_table_MDAstopped, last_year_MDA == 2020)
freq_table_MDAstopped_lstMDAyr2019 <- subset(freq_table_MDAstopped, last_year_MDA == 2019)
freq_table_MDAstopped_lstMDAyr2018 <- subset(freq_table_MDAstopped, last_year_MDA == 2018)
freq_table_MDAstopped_lstMDAyr2017 <- subset(freq_table_MDAstopped, last_year_MDA == 2017)
freq_table_MDAstopped_lstMDAyr2016 <- subset(freq_table_MDAstopped, last_year_MDA == 2016)
freq_table_MDAstopped_lstMDAyr2015 <- subset(freq_table_MDAstopped, last_year_MDA == 2015)
freq_table_MDAstopped_lstMDAyr2014 <- subset(freq_table_MDAstopped, last_year_MDA == 2014)
freq_table_MDAstopped_lstMDAyr2013 <- subset(freq_table_MDAstopped, last_year_MDA == 2013)



# ====================================================== #
#       Make FREQUENCY DISTRIBUTION PLOTS                #

# # ALL IUS with MDA and TREATMENT NAIVE (APRIL 2025)
# Full_histories_df_popinfo <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_popinfo_ALL_minimal_070425_listlabels.csv")
# Full_histories_df_minimal_lastyr_2022 <- subset(Full_histories_df_popinfo, Year == 2022)
# Full_histories_df_minimal_lastyr_2013 <- subset(Full_histories_df_popinfo, Year == 2013)
# Full_histories_df_minimal_lastyr_2013_CUMMDA0 <- subset(Full_histories_df_minimal_lastyr_2013, Cum_MDA_ESPEN == 0)


# =========================================================== #
#         Subset those just in Endgame                        #

Full_histories_df_minimal_lastyr_2022_check <- subset(Full_histories_df_minimal_lastyr_2022, !is.na(Control_prog)) # 2138 IUs
nrow(Full_histories_df_minimal_lastyr_2022_check) # 2130 IUs (April 25')

# only keep SDN and GAB treatment naive IUs
Full_histories_df_minimal_lastyr_2022_check <- Full_histories_df_minimal_lastyr_2022_check %>%
  filter(
    # Keep "Treatment naive" only if ADMIN0ISO3 is "GAB" or "SDN"
    (trt_status_2022 == "Treatment naive" & ADMIN0ISO3 %in% c("GAB", "SDN")) |
      # Keep any row where trt_status_2022 is not "Treatment naive"
      trt_status_2022 != "Treatment naive",
    # Exclude a specific IU_ID
    IU_CODE_MAPPING != "ETH0194319529"
  )

#Full_histories_df_minimal_lastyr_2022_check_trtnaive <- subset(Full_histories_df_minimal_lastyr_2022_check, trt_status_2022 == "Treatment naive")

Full_histories_df_minimal_lastyr_2022 <- Full_histories_df_minimal_lastyr_2022_check # RESET HERE

check <- subset(Full_histories_df_minimal_lastyr_2022, ADMIN0ISO3 == "ETH")

# # VERSION 2 - RESET ALL IUS dataframe HERE #
# # ALL IUS with MDA and TREATMENT NAIVE (APRIL 2025)
# Full_histories_df_popinfo <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_histories_df_popinfo_ALL_minimal_070425_listlabels.csv")
# Full_histories_df_minimal_lastyr_2022 <- subset(Full_histories_df_popinfo, Year == 2022)

# ====================================================== #
#  Wase should be removed so shouldnt need code below : 

# 
# # need to recode one IU in NGA as "Treatment-naïve" in trt_status_2022 for next set of plots #
# Full_histories_df_minimal_lastyr_2022$trt_status_2022 <- ifelse(Full_histories_df_minimal_lastyr_2022$IUs_NAME == "Wase" & 
#                                                                   Full_histories_df_minimal_lastyr_2022$biannual_VC_mapping == "treatment naive",
#                                                    "Treatment-naive", Full_histories_df_minimal_lastyr_2022$trt_status_2022)
# # change label fro PHASE I
# unique(Full_histories_df_minimal_lastyr_2022$trt_status_2022)
# 
# Full_histories_df_minimal_lastyr_2022 <- Full_histories_df_minimal_lastyr_2022 %>%
#   mutate(trt_status_2022 = ifelse(trt_status_2022 == "Treatment naive", "Treatment-naive", trt_status_2022))
# 
# unique(Full_histories_df_minimal_lastyr_2022$trt_status_2022)
# 
# # Update 1 Iu in NGA that needs to be changed to treatment-naive from MDA stopped
# Full_histories_df_minimal_lastyr_2022$trt_status_2022 <- ifelse(Full_histories_df_minimal_lastyr_2022$IUs_NAME == "Wase" & 
#                                                                   Full_histories_df_minimal_lastyr_2022$biannual_VC_mapping == "treatment naive",
#                                                                 "Treatment-naive", Full_histories_df_minimal_lastyr_2022$trt_status_2022)

Full_histories_df_minimal_lastyr_2022 <- subset(Full_histories_df_popinfo, Year == 2022)

# convert from factor
Full_histories_df_minimal_lastyr_2022$trt_status_2022 <- as.character(Full_histories_df_minimal_lastyr_2022$trt_status_2022)

# Update IUs that are "MDA stopped" to "some previous MDA ..." from former APOC countries
Full_histories_df_minimal_lastyr_2022$trt_status_2022 <- ifelse(Full_histories_df_minimal_lastyr_2022$IU_ID_MAPPING %in% IUs_MDA_to_trtnaive, "some previous IVM MDA; 
potentially requiring OEM and/or SM", Full_histories_df_minimal_lastyr_2022$trt_status_2022)

unique(Full_histories_df_minimal_lastyr_2022$trt_status_2022)

# Update IUs that are "MDA stopped" to "some previous MDA ..." from former OCP countries
Full_histories_df_minimal_lastyr_2022$trt_status_2022 <- ifelse(Full_histories_df_minimal_lastyr_2022$IU_ID_MAPPING %in% IUs_MDA_to_trtnaive_OCP, "some previous IVM MDA; 
potentially requiring OEM and/or SM*", Full_histories_df_minimal_lastyr_2022$trt_status_2022)

# update IUs considered treatment naive to include mapping label
Full_histories_df_minimal_lastyr_2022$trt_status_2022 <- ifelse(Full_histories_df_minimal_lastyr_2022$trt_status_2022 == "Treatment naive", 
"Treatment-naïve;
potentially requiring OEM and/or SM", Full_histories_df_minimal_lastyr_2022$trt_status_2022)

unique(Full_histories_df_minimal_lastyr_2022$trt_status_2022)

# # NEED TO CHANGE: Adagnehagre chako/ ETH0194319529 (from MDA stopped to Treatment Naive in VERSION 2
# Full_histories_df_minimal_lastyr_2022$trt_status_2022 <- ifelse(Full_histories_df_minimal_lastyr_2022$IU_CODE_MAPPING == "ETH0194319529",
# "Treatment-naive;
# potentially requiring OEM and/or SM", Full_histories_df_minimal_lastyr_2022$trt_status_2022)


# Define the desired order
desired_order <- c(
  "MDA continues","MDA stopped","some previous IVM MDA; \npotentially requiring OEM and/or SM",
  "some previous IVM MDA; \npotentially requiring OEM and/or SM*",
  "Treatment-naïve;\npotentially requiring OEM and/or SM")

# Reorder the co_endemicity column in your dataframe
Full_histories_df_minimal_lastyr_2022$trt_status_2022 <- factor(
  Full_histories_df_minimal_lastyr_2022$trt_status_2022, 
  levels = desired_order
)

unique(Full_histories_df_minimal_lastyr_2022$trt_status_2022) # check

# frequency dist plot
freq_table_trtstat22_2 <- table(Full_histories_df_minimal_lastyr_2022$trt_status_2022) # Create a frequency table

freq_df_trtstat22_2 <- as.data.frame(as.table(freq_table_trtstat22_2)) # Convert the frequency table to a data frame
colnames(freq_df_trtstat22_2) <- c("trt_status_2022", "Frequency") # Rename the columns for better readability

## Create the frequency distribution plot
# p1 <- ggplot(freq_df_trtstat22_2, aes(x = trt_status_2022, y = Frequency, fill = trt_status_2022)) +
#   geom_bar(stat = "identity", position = "dodge", colour = "black") +
#   geom_text(data = subset(freq_df_trtstat22_2, Frequency > 0),
#             aes(label = Frequency),
#             vjust = -0.4,  # Adjusts the position of the text (above the bar)
#             size = 3,      # Adjusts the size of the text
#             colour = "black") +  # Color of the text
#   labs(x = "Treatment status in 2022",
#        y = "Frequency",
#        fill = "treatment status") +
#   scale_fill_manual(values = cbPalette2) +
#   theme_minimal()+
#   theme(axis.text.x = element_blank(),  # Remove x-axis labels
#         axis.ticks.x = element_blank(),  # Remove x-axis ticks if needed
#         #axis.text.x = element_text(angle = 60, hjust = 1),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.border = element_rect(colour = "black", fill = NA),
#         axis.line = element_line(colour = "black"),
#         legend.position = "bottom",
#         legend.title.position = "top",  # Move the legend title to the top
#         legend.text = element_text(size = 8),  # Adjust text size
#         legend.key.size = unit(0.5, "lines"))   # Adjust key size)

p1 <- ggplot(freq_df_trtstat22_2, aes(x = trt_status_2022, y = Frequency, fill = trt_status_2022)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  geom_text(data = subset(freq_df_trtstat22_2, Frequency > 0),
            aes(label = Frequency),
            vjust = -0.2,  # Adjusts the position of the text (above the bar)
            size = 3,      # Adjusts the size of the text
            colour = "black") +  # Color of the text
  labs(x = "Treatment status in 2022",
       y = "Frequency",
       fill = "Treatment status") +
  ylim(0, 1700)+
  scale_fill_manual(values = cbPalette2) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Remove x-axis labels
        axis.ticks.x = element_blank(),  # Remove x-axis ticks if needed
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.line = element_line(colour = "black"),
        legend.position = "bottom",  # Place the legend at the bottom
        legend.text = element_text(size = 8),  # Adjust text size
        legend.key.size = unit(0.5, "lines"),  # Adjust key size
        plot.margin = margin(t = 40, b = 10, l = 10, r = 10)) +  # Increase top margin for text space
  guides(fill = guide_legend(title = NULL))  # Remove the legend title

# frequency dist plot by country
freq_table_trtstat22 <- table(Full_histories_df_minimal_lastyr_2022$trt_status_2022, Full_histories_df_minimal_lastyr_2022$ADMIN0ISO3) # Create a frequency table

freq_df_trtstat22 <- as.data.frame(as.table(freq_table_trtstat22)) # Convert the frequency table to a data frame
colnames(freq_df_trtstat22) <- c("trt_status_2022", "ADMIN0ISO3", "Frequency") # Rename the columns for better readability

# Create the frequency distribution plot
p2 <- ggplot(freq_df_trtstat22, aes(x = trt_status_2022, y = Frequency, fill = trt_status_2022)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  geom_text(data = subset(freq_df_trtstat22, Frequency > 0),
            aes(label = Frequency),
            vjust = -0.2,  # Adjust the position of the text (above the bar)
            size = 3,      # Adjusts the size of the text
            colour = "black",
            #nudge_y = 10,  # Move text higher to overlap the border
            clip = "off") +  # Allow text to go outside the plot area
  labs(x = "Treatment status in 2022",
       y = "Frequency",
       fill = "Treatment status") +
  ylim(0, 450)+
  scale_fill_manual(values = cbPalette2) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Remove x-axis labels
        axis.ticks.x = element_blank(),  # Remove x-axis ticks if needed
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.line = element_line(colour = "black"),
        plot.margin = margin(t = 40, b = 10, l = 10, r = 10),  # Increase top margin for text space
        legend.position = "bottom",
        legend.text = element_text(size = 8),  # Adjust text size
        legend.key.size = unit(0.5, "lines")) +  # Adjust key size
  guides(fill = guide_legend(title = NULL)) +
  facet_wrap(~ADMIN0ISO3)  # Separate by ADMIN0ISO3

#p2

# combine
p1_no_legend <- p1 + theme(legend.position = "none")
p2_no_legend <- p2 + theme(legend.position = "none")

# Extract the shared legend from one of the plots
shared_legend <- get_legend(p1 + theme(legend.position = "none"))

# Combine p1 and p2 in a vertical layout, without titles
combined_plots <- plot_grid(
  p1_no_legend, p2_no_legend,
  labels = c("b", "c"),        # Add "A" and "B" labels
  label_size = 18,             # Adjust label size
  ncol = 1,                    # Stack them vertically
  rel_heights = c(1/4, 3/4)    # Set relative heights: p1 takes 1/4, p2 takes 3/4
)

# Add the shared legend at the bottom
final_plot <- plot_grid(
  combined_plots, shared_legend,
  ncol = 1,
  rel_heights = c(1, 0.1)  # Legend takes 10% of the space at the bottom
)

# Display the final plot with the shared legend
#print(final_plot)
p1
p2
# # Optionally, save to PDF in portrait mode (A4 size)
# ggsave("p1_p2_combined_portrait.pdf", combined_plot, width = 8.27, height = 11.69, units = "in")  # A4 portrait dimensions

# Save the combined plot to a landscape A4 PDF

# VERSION 1: only endgame IUs (n = 2131)
# 1659 + 133 + 143 +166 + 30 = 2131 IUs (removing one trt naive from ETH)
ggsave("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Publications/Intervention histories/figures/combined_trtstatus2022_freqdist_plot_V1.pdf", final_plot, width = 8.27, height = 11.69, units = "in")  # A4 landscape dimensions

# 1659 + 133 + 143 + 166 + 29 = 2130 (April 25')

# # VERSION 2: All IUs
# ggsave("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Publications/Intervention histories/figures/combined_trtstatus2022_freqdist_plot_V2_ALLIUs.pdf", final_plot, width = 8.27, height = 11.69, units = "in")  # A4 landscape dimensions


# get txt lists #

for_list1 <- subset(co_endemicity_maps_final, trt_status_2022_OEM_v2 == "previous IVM MDA, requiring OEM")
for_list2 <- subset(co_endemicity_maps_final, trt_status_2022_OEM_v2 == "previous IVM MDA, requiring OEM*")

for_list_both <- rbind(for_list1, for_list2)

IU_ID_trtnaive <- as.character(unique(for_list_both$IU_ID))
# writeLines(IU_ID_trtnaive, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Planning final Endgame sims (March-May 25)/list_5.txt")


# # ======================================================= #
# #    MDA status 2022 - OEM with all other IU changes      #
# # ======================================================= #
# 
# # IUs with generic histories, but considered Unknown (under LF MDA) or Unknown (require OEM)
# # instead of MDA having stopped, these could be labelled "previous IVM MDA, requiring OEM"
# 

# 1) these need to be 
list_1 <- read.table("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Planning final Endgame sims (March-May 25)/list_1.txt", quote="\"", comment.char="")
list_1 <- list_1 %>% rename(IU_CODE = V1)
list_1_vec <- list_1$IU_CODE

co_endemicity_maps_final$trt_status_2022_OEM_v3 <- co_endemicity_maps_final$trt_status_2022_OEM_v2
co_endemicity_maps_final$trt_status_2022_OEM_v3 <- ifelse(co_endemicity_maps_final$IU_CODE_MAPPING %in% list_1_vec, "Treatment naive", co_endemicity_maps_final$trt_status_2022_OEM_v3)
unique(co_endemicity_maps_final$trt_status_2022_OEM_v3)

# 1) these need to be included in intervention histories with MDA (Not in eEndgame before)
list_3 <- read.table("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Planning final Endgame sims (March-May 25)/list_3.txt", quote="\"", comment.char="")
list_3 <- list_3 %>% rename(IU_CODE = V1)
list_3_vec <- list_3$IU_CODE

co_endemicity_maps_final$trt_status_2022_OEM_v3 <- ifelse(co_endemicity_maps_final$IU_CODE %in% list_3_vec & co_endemicity_maps_final$ADMIN0ISO3.x == "NER", "MDA stopped", co_endemicity_maps_final$trt_status_2022_OEM_v3)
co_endemicity_maps_final$trt_status_2022_OEM_v3 <- ifelse(co_endemicity_maps_final$IU_CODE %in% list_3_vec & co_endemicity_maps_final$ADMIN0ISO3.x == "NER", "MDA continues", co_endemicity_maps_final$trt_status_2022_OEM_v3)
unique(co_endemicity_maps_final$trt_status_2022_OEM_v3)

# 1) these need to be included in intervention histories with MDA (Not in eEndgame before)
list_4 <- read.table("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Planning final Endgame sims (March-May 25)/list_4.txt", quote="\"", comment.char="")
list_4 <- list_4 %>% rename(IU_CODE = V1)
list_4_vec <- list_4$IU_CODE

co_endemicity_maps_final$trt_status_2022_OEM_v3 <- ifelse(co_endemicity_maps_final$IU_CODE_MAPPING %in% list_4_vec, NA, co_endemicity_maps_final$trt_status_2022_OEM_v3)

cbPalette2 <- c("#CC79A7","#E69F00","#009E73","#F0E442","#0072B2")

p_map <- ggplot() +
  geom_sf(data = oceans_shp, aes(), fill = "lightblue", colour = NA) +
  geom_sf(data = co_endemicity_maps_final, aes(fill = trt_status_2022_OEM_v3), colour = NA, alpha = 0.7) +
  #geom_sf(data = ESPEN_IUs_ALL, aes(), colour = "lightgrey", linewidth = 0.1, size= 0.1, fill = NA, alpha = 0.1) +
  geom_sf(data = ESPEN_IUs_ALL, aes(),  colour = "black", linewidth = 0.01, fill = NA, alpha = 0.3) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
  geom_sf_text(data = african_centroids_oncho, aes(label = NAME_0), size = 1.5, color = "black", fontface = "bold", alpha = 0.7) + # Add country labels
  coord_sf(xlim = c(-15, 45), ylim = c(22, -15)) +
  theme_bw() +
  scale_fill_manual(values = cbPalette2, na.value = "gray") +
  scale_colour_manual(na.value="gray")+
  #scale_size_identity()+
  labs(fill='', x = " ", y = " ") +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal",
    legend.text = element_text(size = 7),  # Adjust size of legend text
    legend.title = element_text(size = 10))  # Adjust size of legend title)))

# just bioko #
p_bioko <- ggplot() +
  geom_sf(data = oceans_shp, aes(), fill = "lightblue", colour = NA) +
  geom_sf(data = co_endemicity_maps_final, aes(fill = trt_status_2022_OEM_v3), colour = NA, alpha = 0.7) +
  geom_sf(data = ESPEN_IUs_ALL, aes(), colour = "black", linewidth = 0.01, fill = NA, alpha = 0.3) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
  coord_sf(xlim = c(8, 10), ylim = c(3, 5)) +
  theme_bw() +
  scale_fill_manual(values = cbPalette2, na.value = "gray") +
  scale_colour_manual(na.value = "gray") +
  labs(fill = '') +
  theme(
    legend.position = "none",         # Remove the legend
    axis.text = element_blank(),      # Remove axis text (labels)
    axis.ticks = element_blank(),     # Remove axis ticks
    axis.title = element_blank(),     # Remove axis titles
    panel.grid.major = element_blank(),  # Optionally remove grid lines
    panel.grid.minor = element_blank()   # Optionally remove minor grid lines
  )

# Combine p_map and p_bioko using ggdraw
final_map_plot4_OEM_v3 <- ggdraw() +
  draw_plot(p_map, 0, 0, 1, 1) +  # Main map takes the whole space
  draw_plot(p_bioko, 0.2, 0.32, 0.25, 0.25) +  # Inset map in bottom-left corner (adjust size and position as needed)
  draw_text("a", x = 0.1, y = 1, hjust = 0, vjust = 1, size = 10, fontface = "bold")

# # Display the final plot
# print(final_map_plot)

# ggsave("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Publications/Intervention histories/figures/trtstatus2022_maps.pdf",
#        plot = final_map_plot2, device = "pdf", width = 11.69, height = 8.27) # A4 portrait size in inches

# increase resolution so that IU lines visible but not too thick
agg_png("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Publications/Intervention histories/figures/trtstatus2022_maps_wOEM_v3.png", 6000, 4000, scaling = 12)
final_map_plot4_OEM_v3
dev.off()

# ================================================ #
# check characteristics of IUs with "some IVM ..." #

IUs_someMDA_APOC_df <- subset(co_endemicity_maps_final, IU_ID %in% IUs_MDA_to_trtnaive) # former APOC countries

freq_table_someIVM_APOC <- IUs_someMDA_APOC_df %>%
  st_drop_geometry() %>%  # <<< Drop sf geometry first
  filter(!is.na(trt_status_2022_v2)) %>%
  group_by(trt_status_2022_v2, last_year_MDA) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(desc(last_year_MDA))

IUs_someMDA_OCP_df <- subset(co_endemicity_maps_final, IU_ID %in% IUs_MDA_to_trtnaive_OCP) # former OCP countries

freq_table_someIVM_OCP <- IUs_someMDA_OCP_df %>%
  st_drop_geometry() %>%  # <<< Drop sf geometry first
  filter(!is.na(trt_status_2022_v2)) %>%
  group_by(trt_status_2022_v2, last_year_MDA) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(desc(last_year_MDA))

# ========================================== #
#    Business-as-usual (2023 -)              #
# ========================================== #

cbPalette3 <- cbPalette <- c("#CC79A7","#E69F00","#009E73","#F0E442","#0072B2","indianred1")

p_map <- ggplot() +
  geom_sf(data = oceans_shp, aes(), fill = "lightblue", colour = NA) +
  geom_sf(data = co_endemicity_maps_final, aes(fill = as.factor(BAU_label)), colour = NA, alpha = 0.7) +
  #geom_sf(data = ESPEN_IUs_ALL, aes(), colour = "lightgrey", linewidth = 0.1, size= 0.1, fill = NA, alpha = 0.1) +
  geom_sf(data = ESPEN_IUs_ALL, aes(), colour = NA, size = 1, fill = NA, alpha = 0.1) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
  geom_sf_text(data = african_centroids_oncho, aes(label = NAME_0), size = 3, color = "black", fontface = "bold") + # Add country labels
  coord_sf(xlim = c(-15, 45), ylim = c(22, -16)) +
  theme_bw() +
  scale_fill_manual(values = cbPalette3, na.value = "gray") +
  scale_colour_manual(na.value="gray")+
  #scale_size_identity()+
  labs(fill='', x = " ", y = " ") +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal")

# just bioko #
p_bioko <- ggplot() +
  geom_sf(data = oceans_shp, aes(), fill = "lightblue", colour = NA) +
  geom_sf(data = co_endemicity_maps_final, aes(fill = as.factor(BAU_label)), colour = NA, alpha = 0.7) +
  geom_sf(data = ESPEN_IUs_ALL, aes(), colour = NA, size = 1, fill = NA, alpha = 0.1) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
  coord_sf(xlim = c(8, 10), ylim = c(3, 5)) +
  theme_bw() +
  scale_fill_manual(values = cbPalette3, na.value = "gray") +
  scale_colour_manual(na.value = "gray") +
  labs(fill = '') +
  theme(
    legend.position = "none",         # Remove the legend
    axis.text = element_blank(),      # Remove axis text (labels)
    axis.ticks = element_blank(),     # Remove axis ticks
    axis.title = element_blank(),     # Remove axis titles
    panel.grid.major = element_blank(),  # Optionally remove grid lines
    panel.grid.minor = element_blank(),
    # Optionally remove minor grid lines
  )

# Combine p_map and p_bioko using ggdraw
final_map_plot2 <- ggdraw() +
  draw_plot(p_map, 0, 0, 1, 1) +  # Main map takes the whole space
  draw_plot(p_bioko, 0.2, 0.32, 0.25, 0.25) +  # Inset map in bottom-left corner (adjust size and position as needed)
 draw_text("A", x = 0.03, y = 1, hjust = 0, vjust = 1, size = 18, fontface = "bold")

# # Display the final plot
# print(final_map_plot)

ggsave("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Publications/Intervention histories/figures/BAU_maps.pdf",
       plot = final_map_plot2, device = "pdf", width = 11.69, height = 8.27) # A4 portrait size in inches

# frequency dist plot
freq_table_BAU2 <- table(Full_histories_df_minimal_lastyr_2022$BAU_label) # Create a frequency table

freq_df_BAU2 <- as.data.frame(as.table(freq_table_BAU2)) # Convert the frequency table to a data frame
colnames(freq_df_BAU2) <- c("BAU", "Frequency") # Rename the columns for better readability

# Create the frequency distribution plot
p1 <- ggplot(freq_df_BAU2, aes(x = BAU, y = Frequency, fill = BAU)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  geom_text(data = subset(freq_df_BAU2, Frequency > 0),
            aes(label = Frequency),
            vjust = -0.4,  # Adjusts the position of the text (above the bar)
            size = 3,      # Adjusts the size of the text
            colour = "black") +  # Color of the text
  labs(x = "business-as-usual status from 2023",
       y = "Frequency",
       fill = "business-as-usual
status from 2023") +
  scale_fill_manual(values = cbPalette3) +
  theme_minimal()+
  theme(axis.text.x = element_blank(),  # Remove x-axis labels
        axis.ticks.x = element_blank(),  # Remove x-axis ticks if needed
        #axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.line = element_line(colour = "black"),
        legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9),  # Adjust text size
        legend.key.size = unit(0.5, "lines"),
        strip.text = element_text(face = "bold")) +  # Adjust key size)
  coord_cartesian(clip = "off")  # Allow labels to go outside the plot area


# frequency dist plot by country
freq_table_BAU <- table(Full_histories_df_minimal_lastyr_2022$BAU_label, Full_histories_df_minimal_lastyr_2022$ADMIN0ISO3) # Create a frequency table

freq_df_BAU <- as.data.frame(as.table(freq_table_BAU)) # Convert the frequency table to a data frame
colnames(freq_df_BAU) <- c("BAU", "ADMIN0ISO3", "Frequency") # Rename the columns for better readability

# Create the frequency distribution plot
p2 <- ggplot(freq_df_BAU, aes(x = BAU, y = Frequency, fill = BAU)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  geom_text(data = subset(freq_df_BAU, Frequency > 0),
            aes(label = Frequency),
            vjust = -0.4,  # Adjusts the position of the text (above the bar)
            size = 3,      # Adjusts the size of the text
            colour = "black") +  # Color of the text
  labs(x = "business-as-usual status from 2023",
       y = "Frequency",
       fill = "business-as-usual status from 2023") +
  scale_fill_manual(values = cbPalette3) +
  theme_minimal()+
  theme(axis.text.x = element_blank(),  # Remove x-axis labels
        axis.ticks.x = element_blank(),  # Remove x-axis ticks if needed
        #axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.line = element_line(colour = "black"),
        strip.text = element_text(face = "bold"))+
  facet_wrap(~ADMIN0ISO3)+
  coord_cartesian(clip = "off")  # Allow labels to go outside the plot area


# combine
p1_no_legend <- p1 + theme(legend.position = "none")
p2_no_legend <- p2 + theme(legend.position = "none")

# Extract the shared legend from one of the plots
shared_legend <- get_legend(p1 + theme(legend.position = "bottom"))

# Combine p1 and p2 in a vertical layout, without titles
combined_plots <- plot_grid(
  p1_no_legend, p2_no_legend,
  labels = c("B", "C"),        # Add "A" and "B" labels
  label_size = 18,             # Adjust label size
  ncol = 1,                    # Stack them vertically
  rel_heights = c(1/4, 3/4)    # Set relative heights: p1 takes 1/4, p2 takes 3/4
)

# Add the shared legend at the bottom
final_plot <- plot_grid(
  combined_plots, shared_legend,
  ncol = 1,
  rel_heights = c(1, 0.1)  # Legend takes 10% of the space at the bottom
)

# Display the final plot with the shared legend
#print(final_plot)


# # Optionally, save to PDF in portrait mode (A4 size)
# ggsave("p1_p2_combined_portrait.pdf", combined_plot, width = 8.27, height = 11.69, units = "in")  # A4 portrait dimensions

# Save the combined plot to a landscape A4 PDF
ggsave("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Publications/Intervention histories/figures/combined_BAU_freqdist_plot.pdf", final_plot, width = 8.27, height = 11.69, units = "in")  # A4 landscape dimensions



# ===================================================================== #
#  Check if EpiCov > 100% where co-endemicty predominantly in ESPEN     #

# Filter data for the years 2013 to 2022 and EpiCov > 100
result_greater100cov <- Full_histories_df_popinfo_111124 %>%
  filter(Year >= 2013 & Year <= 2022, EpiCov > 100) %>%
  group_by(co_endemicity) %>%
  summarize(
    IU_count = n_distinct(IU_ID_MAPPING)
  ) %>%
  mutate(
    Total_IU = sum(IU_count),                    # Total IU_ID_MAPPING across all co_endemicity
    Percentage1 = (IU_count / Total_IU) * 100,    # Calculate percentage for each group
    Percentage2 = (IU_count / 2139) * 100    # Calculate percentage for each group

  )

print(result_greater100cov)

535/740 # % of IUs > 100% cov where co-endemic with LF (of all IUs where cov > 100%, 740 of 2139 IUs)

result <- Full_histories_df_popinfo_111124 %>%
  summarize(
    IU_count = n_distinct(IU_ID_MAPPING)
  )

print(result)


# Filter data for the years 2013 to 2022 and EpiCov > 100
result_anyPIS <- Full_histories_df_popinfo_111124 %>%
  filter(Year >= 2013 & Year <= 2022) %>%
  group_by(Endemicity) %>%
  summarize(
    IU_count = n_distinct(IU_ID_MAPPING)
  ) %>%
  mutate(
    Total_IU = sum(IU_count),                    # Total IU_ID_MAPPING across all co_endemicity
    Percentage1 = (IU_count / Total_IU) * 100,    # Calculate percentage for each group
    Percentage2 = (IU_count / 2139) * 100    # Calculate percentage for each group

  )

result_anyPIS <- Full_histories_df_popinfo_111124 %>%
  filter(Year >= 2013 & Year <= 2022, Endemicity == "Endemic (under post-intervention surveillance)") %>%
  group_by(trt_status_2022) %>%
  summarize(
    IU_count = n_distinct(IU_ID_MAPPING)
  )

result_anyunknown <- Full_histories_df_popinfo_111124 %>%
  filter(Year >= 2013 & Year <= 2022, MAX_Endemicity %in% c("Not reported", "Unknown (consider Oncho Elimination Mapping", "Unknown (under LF MDA)")) %>%
  group_by(trt_status_2022) %>%
  summarize(
    IU_count = n_distinct(IU_ID_MAPPING)
  )

print(result_anyPIS)
print(result_anyunknown)


IUs_underPIS <- subset(Full_histories_df_popinfo_111124, Endemicity == "Endemic (under post-intervention surveillance)")
IUs_underPIS_MDAstopped <- subset(Full_histories_df_popinfo_111124, Endemicity == "Endemic (under post-intervention surveillance)" &
                                    trt_status_2022 %in% c())


# =========================================================================================== #
#         Compare to ESPEN (2022) oncho endemicity classification                             #
# =========================================================================================== #

final_df_oncho_Parent_Child <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Cleaning IUs (Paul B)/final_df_oncho_Parent_Child.csv")

final_df_oncho_Parent_Child_2002 <- subset(final_df_oncho_Parent_Child, Year == 2022)

final_df_oncho_Parent_Child_2002$Endemicity_ESPEN <- ifelse(
  final_df_oncho_Parent_Child_2002$Endemicity %in% c("Endemic (under MDA)", "Endemic (MDA not delivered)"), "Endemic (requiring MDA)",
  ifelse(
    final_df_oncho_Parent_Child_2002$Endemicity %in% c("Unknown (under LF MDA)", "Unknown (consider Oncho Elimination Mapping)"), "Unknown (under LF MDA or consider OEM)",
    ifelse(
      final_df_oncho_Parent_Child_2002$Endemicity == "Endemic (under post-intervention surveillance)", "Under post-intervention surveillance",
      ifelse(
        final_df_oncho_Parent_Child_2002$Endemicity == "Not reported", "No data available",
        ifelse(
          final_df_oncho_Parent_Child_2002$Endemicity == "Non-endemic", "Not suitable for Onchocerciasis",
          final_df_oncho_Parent_Child_2002$Endemicity
        )
      )
    )
  )
)

final_df_oncho_Parent_Child_2002$Endemicity_ESPEN <- factor(
  final_df_oncho_Parent_Child_2002$Endemicity_ESPEN,
  levels = c(
    "Not suitable for Onchocerciasis",
    "Endemic (requiring MDA)",
    "Endemic (pending IA)",
    "Unknown (under LF MDA or consider OEM)",
    "Under post-intervention surveillance",
     "No data available"
  ),
  ordered = TRUE
)

# Verify the levels
levels(final_df_oncho_Parent_Child_2002$Endemicity_ESPEN)

freq_table <- final_df_oncho_Parent_Child_2002 %>%
  group_by(Endemicity_ESPEN) %>%
  summarize(Frequency = n()) %>%
  mutate(Percentage = (Frequency / sum(Frequency)) * 100) %>%  # Add percentage column
  arrange(desc(Frequency))  # Sort by descending frequency

freq_table

# maps & frequency distributions ~ country #
ESPEN_IUs <- st_read('C:/Users/mad206/OneDrive/Endgame/Endgame IUs/ESPEN_IU_2021.shp')
all_countries <- unique(Full_histories_df_minimal_lastyr_2022$ADMIN0ISO3)
ESPEN_IUs_ALL <- ESPEN_IUs[which((ESPEN_IUs$ADMIN0ISO3 %in% all_countries)),]
st_geometry_type(ESPEN_IUs_ALL)
st_crs(ESPEN_IUs_ALL)

final_df_oncho_Parent_Child_2002_shp <- ESPEN_IUs_ALL %>%
  left_join(final_df_oncho_Parent_Child_2002, by = c("IU_ID" = "IU_ID_MAPPING"))

cbPalette <- c("green4", "red", "pink", "yellow", "deepskyblue", "grey32")


#cbPalette <- cbPalette <- c("#CC79A7","#E69F00","#009E73","#F0E442","#0072B2","indianred1")

p_map_ESPENEndemicity <- ggplot() +
  geom_sf(data = oceans_shp, aes(), fill = "lightblue", colour = NA) +
  geom_sf(data = final_df_oncho_Parent_Child_2002_shp, aes(fill = Endemicity_ESPEN), colour = NA, alpha = 0.7) +
  #geom_sf(data = co_endemicity_maps_final, aes(fill = phase2), colour = NA, alpha = 0.7) +
  #geom_sf(data = ESPEN_IUs_ALL, aes(), colour = "lightgrey", linewidth = 0.1, size= 0.1, fill = NA, alpha = 0.1) +
  geom_sf(data = final_df_oncho_Parent_Child_2002_shp, aes(), colour = "grey", linewidth = 0.01, fill = NA, alpha = 0.3) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
  geom_sf_text(data = african_centroids_oncho, aes(label = NAME_0), size = 3, color = "black", fontface = "bold") + # Add country labels
  coord_sf(xlim = c(-15, 45), ylim = c(22, -16)) +
  theme_bw() +
  scale_fill_manual(values = cbPalette, na.value = "gray") +
  scale_colour_manual(na.value="gray")+
  scale_size_identity()+
  #scale_size_identity()+
  labs(fill='', x = " ", y = " ") +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal")

p_map_ESPENEndemicity <- ggplot() +
  geom_sf(data = oceans_shp, aes(), fill = "lightblue", colour = NA) +
  geom_sf(data = final_df_oncho_Parent_Child_2002_shp, aes(fill = Endemicity_ESPEN), colour = NA, alpha = 0.7) +
  geom_sf(data = final_df_oncho_Parent_Child_2002_shp, aes(), colour = "grey", linewidth = 0.125, fill = NA, alpha = 0.3) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", linewidth = 1) +
  geom_sf_text(data = african_centroids_oncho, aes(label = NAME_0), size = 2.5, color = "black", fontface = "bold") +
  coord_sf(xlim = c(-15, 45), ylim = c(22, -16)) +
  theme_bw() +
  scale_fill_manual(values = cbPalette, na.value = "gray") +
  labs(fill = '', x = " ", y = " ") +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal"
  )
ggsave("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Publications/Intervention histories/plots for Claudio/p_map_ESPENEndemicity.png", dpi = 300)

p_map_ESPENEndemicity


# our endemicity map #

Full_histories_df_minimal_lastyr_2022$Endemicity_ESPEN <- ifelse(
  Full_histories_df_minimal_lastyr_2022$Endemicity %in% c("Endemic (under MDA)", "Endemic (MDA not delivered)"), "Endemic (requiring MDA)",
  ifelse(
    Full_histories_df_minimal_lastyr_2022$Endemicity %in% c("Unknown (under LF MDA)", "Unknown (consider Oncho Elimination Mapping)"), "Unknown (under LF MDA or consider OEM)",
    ifelse(
      Full_histories_df_minimal_lastyr_2022$Endemicity == "Endemic (under post-intervention surveillance)", "Under post-intervention surveillance",
      ifelse(
        Full_histories_df_minimal_lastyr_2022$Endemicity == "Not reported", "No data available",
        ifelse(
          Full_histories_df_minimal_lastyr_2022$Endemicity == "Non-endemic", "Not suitable for Onchocerciasis",
          Full_histories_df_minimal_lastyr_2022$Endemicity
        )
      )
    )
  )
)

Full_histories_df_minimal_lastyr_2022$Endemicity_ESPEN <- factor(
  Full_histories_df_minimal_lastyr_2022$Endemicity_ESPEN,
  levels = c(
    "Not suitable for Onchocerciasis",
    "Endemic (requiring MDA)",
    "Endemic (pending IA)",
    "Unknown (under LF MDA or consider OEM)",
    "Under post-intervention surveillance",
    "No data available"
  ),
  ordered = TRUE
)

# Verify the levels
levels(Full_histories_df_minimal_lastyr_2022$Endemicity_ESPEN)

freq_table2 <- Full_histories_df_minimal_lastyr_2022 %>%
  group_by(Endemicity_ESPEN) %>%
  summarize(Frequency = n()) %>%
  mutate(Percentage = (Frequency / sum(Frequency)) * 100) %>%  # Add percentage column
  arrange(desc(Frequency))  # Sort by descending frequency

freq_table2

co_endemicity_maps_final <- ESPEN_IUs_ALL %>%
  left_join(Full_histories_df_minimal_lastyr_2022, by = c("IU_ID" = "IU_ID_MAPPING"))

co_endemicity_maps_final$phase2 <- ifelse(co_endemicity_maps_final$Control_prog == "APOC", "APOC", co_endemicity_maps_final$PHASE)


p_map_ourEndemicity <- ggplot() +
  geom_sf(data = oceans_shp, aes(), fill = "lightblue", colour = NA) +
  geom_sf(data = co_endemicity_maps_final, aes(fill = Endemicity_ESPEN), colour = NA, alpha = 0.7) +
  geom_sf(data = co_endemicity_maps_final, aes(), colour = "grey", linewidth = 0.125, fill = NA, alpha = 0.3) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", linewidth = 1) +
  geom_sf_text(data = african_centroids_oncho, aes(label = NAME_0), size = 2.5, color = "black", fontface = "bold") +
  coord_sf(xlim = c(-15, 45), ylim = c(22, -16)) +
  theme_bw() +
  scale_fill_manual(values = cbPalette, na.value = "gray") +
  labs(fill = '', x = " ", y = " ") +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal"
  )
ggsave("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Publications/Intervention histories/plots for Claudio/p_map_ourEndemicity.png", dpi = 300)

#p_map_ourEndemicity


cbPalette <- c("#99CCFF","#9999FF","#009E73","#990066","#FFCCFF","#CC3399","#CC0000","#FFFF99","#FF6600")
p_map_coendemicity <- ggplot() +
  geom_sf(data = oceans_shp, aes(), fill = "lightblue", colour = NA) +
  geom_sf(data = co_endemicity_maps_final, aes(fill = co_endemicity), colour = NA, alpha = 0.7) +
  geom_sf(data = co_endemicity_maps_final, aes(), colour = "grey", linewidth = 0.125, fill = NA, alpha = 0.3) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", linewidth = 1) +
  geom_sf_text(data = african_centroids_oncho, aes(label = NAME_0), size = 2.5, color = "black", fontface = "bold") +
  coord_sf(xlim = c(-15, 45), ylim = c(22, -16)) +
  theme_bw() +
  scale_fill_manual(values = cbPalette, na.value = "gray") +
  labs(fill = '', x = " ", y = " ") +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal"
  )
ggsave("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Publications/Intervention histories/plots for Claudio/p_map_coendemicity.png", dpi = 300)


freq_table3 <- Full_histories_df_minimal_lastyr_2022 %>%
  group_by(co_endemicity) %>%
  summarize(Frequency = n()) %>%
  mutate(Percentage = (Frequency / sum(Frequency)) * 100) %>%  # Add percentage column
  arrange(desc(Frequency))  # Sort by descending frequency

freq_table3

Full_histories_df_minimal_lastyr_2022_masked <- Full_histories_df_minimal_lastyr_2022
Full_histories_df_minimal_lastyr_2022_masked$co_endemicity2 <- ifelse(Full_histories_df_minimal_lastyr_2022_masked$co_endemicity %in% c("oncho,LF,loa hypo",
                                                                                                                  "oncho,LF,loa meso",
                                                                                                                  "oncho,LF,loa hyper",
                                                                                                                  "oncho,loa hypo",
                                                                                                                  "oncho,loa meso",
                                                                                                                  "oncho,loa hyper"),
                                                       NA, Full_histories_df_minimal_lastyr_2022_masked$co_endemicity)


co_endemicity_maps_final_masked <- ESPEN_IUs_ALL %>%
  left_join(Full_histories_df_minimal_lastyr_2022_masked, by = c("IU_ID" = "IU_ID_MAPPING"))


cbPalette <- c("#99CCFF","#9999FF","#009E73","#990066","#FFCCFF","#CC3399","#CC0000","#FFFF99","#FF6600")
p_map_coendemicity2 <- ggplot() +
  geom_sf(data = oceans_shp, aes(), fill = "lightblue", colour = NA) +
  geom_sf(data = co_endemicity_maps_final_masked, aes(fill = co_endemicity2), colour = NA, alpha = 0.7) +
  geom_sf(data = co_endemicity_maps_final_masked, aes(), colour = "grey", linewidth = 0.125, fill = NA, alpha = 0.3) +
  geom_sf(data = african_countries, aes(), fill = NA, colour = "black", linewidth = 1) +
  geom_sf_text(data = african_centroids_oncho, aes(label = NAME_0), size = 2.5, color = "black", fontface = "bold") +
  coord_sf(xlim = c(-15, 45), ylim = c(22, -16)) +
  theme_bw() +
  scale_fill_manual(values = cbPalette, na.value = "gray") +
  labs(fill = '', x = " ", y = " ") +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal"
  )
ggsave("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Publications/Intervention histories/plots for Claudio/p_map_coendemicity2.png", dpi = 300)
