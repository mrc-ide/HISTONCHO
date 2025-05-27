# ================================================================================================================================================= #
#                            Code to reproduce figures for HISTONCHO paper                                                                          #
# ================================================================================================================================================= #

# ============================================================================= #
#             Extract and process shapefiles                                    #
# ============================================================================= #

# call function
shapefile_input <- "/African countries/Africa_Boundaries.shp"
oceans_shp_input <- "/oceans_shapefile/ne_10m_ocean.shp"
ESPEN_IUs_shape_input <- "/ESPEN_IU_shapefile/ESPEN_IU_2021.shp"

african_countries_toinclude <- c("Angola", "Burundi", "Benin", "Burkina Faso", "Central African Republic",
                                 "Côte d'Ivoire", "Cameroon", "Democratic Republic of the Congo", "Republic of Congo",
                                 "Ethiopia", "Gabon", "Ghana", "Guinea", "Guinea-Bissau", "Liberia", "Mali", "Malawi",
                                 "Niger", "Nigeria", "Sudan", "Senegal", "Sierra Leone", "South Sudan", "Chad",
                                 "Togo", "Tanzania", "Uganda")

processed_out <- processing_shapefiles_merge(HISTONCHO_final_cols_df, shapefile_input, oceans_shp_input, 
                                                     african_countries_toinclude, ESPEN_IUs_shape_input)

african_countries <- processed_out[[1]] 
oceans_shp <- processed_out[[2]]
african_centroids_oncho <- processed_out[[3]]
ESPEN_IUs_ALL <- processed_out[[4]]
HISTONCHO_shape <- processed_out[[5]]
HISTONCHO_2022 <- processed_out[[6]]


# ============================================== #
#       FIGURE 1 plotting - OCP phases and APOC  #
# ============================================== #

# Call function #
fig1_out <- process_and_plot_fig1(african_countries, oceans_shp, "/SIZs_IUs.xlsx", 
                     african_centroids_oncho, ESPEN_IUs_ALL, HISTONCHO_final_cols_df, 
                     HISTONCHO_shape)


# Save the final combined plot
output_file_path <- "plots/Figure_1.png"
agg_png(output_file_path, 6000, 4000, scaling = 12)
fig1_out
dev.off()

# =================================================== #
#   FIGURE 5 plotting - intervention distribution     #
# =================================================== #

# Call function #
fig5_out <- process_and_plot_fig5(african_countries, oceans_shp, african_centroids_oncho, 
                                  ESPEN_IUs_ALL, HISTONCHO_shape, HISTONCHO_2022)


# Save map
output_file_path <- "plots/Figure_5a.png"
agg_png(output_file_path, 6000, 4000, scaling = 12)
fig5_out[[1]]
dev.off()

# Save frequency plot
output_file_path <- "plots/Figure_5bc.png"
agg_png(output_file_path, width = 8.27, height = 11.69, units = "in")
fig5_out[[2]]
dev.off()


# =================================================== #
#   FIGURE 6 plotting - Cumulative revised MDA rounds #
# =================================================== #

# Call function #
fig6_out <- process_and_plot_fig6(african_countries, oceans_shp, african_centroids_oncho, 
                                  ESPEN_IUs_ALL, HISTONCHO_shape, HISTONCHO_2022)


# Save map
output_file_path <- "plots/Figure_6a.png"
agg_png(output_file_path, 6000, 4000, scaling = 12)
fig6_out[[1]]
dev.off()

# Save frequency plot
output_file_path <- "plots/Figure_6bc.png"
agg_png(output_file_path, width = 8.27, height = 11.69, units = "in")
fig6_out[[2]]
dev.off()

# ============================================ #
#   FIGURE 7 plotting - Co-endemicity          #
# ============================================ #

# Call function #
fig7_out <- process_and_plot_fig7(african_countries, oceans_shp, african_centroids_oncho, 
                                  ESPEN_IUs_ALL, HISTONCHO_shape, HISTONCHO_2022)


# Save map
output_file_path <- "plots/Figure_7a.png"
agg_png(output_file_path, 6000, 4000, scaling = 12)
fig7_out[[1]]
dev.off()

# Save frequency plot
output_file_path <- "plots/Figure_7bc.png"
agg_png(output_file_path, width = 8.27, height = 11.69, units = "in")
fig7_out[[2]]
dev.off()

# ============================================ #
#   FIGURE 8 plotting - MDA status 2022        #
# ============================================ #

# Call function #
fig8_out <- process_and_plot_fig8(african_countries, oceans_shp, african_centroids_oncho, 
                                  ESPEN_IUs_ALL, HISTONCHO_shape, HISTONCHO_2022)


# Save map
output_file_path <- "plots/Figure_8a.png"
agg_png(output_file_path, 6000, 4000, scaling = 12)
fig8_out[[1]]
dev.off()

# Save frequency plot
output_file_path <- "plots/Figure_8bc.png"
agg_png(output_file_path, width = 8.27, height = 11.69, units = "in")
fig8_out[[2]]
dev.off()

