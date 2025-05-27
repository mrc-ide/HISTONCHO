# ====================================================================================================================== #
#                                 PLOTTING FOR PAPER FUNCTIONs (Figure 1, 5, 6, 7, 8)                                    #
# ====================================================================================================================== #


# =============================================================================== #
#            FUNCTION to load and extract all relevant shapefiles                 #
# =============================================================================== #

processing_shapefiles_merge <- function(df_input, shapefile_input, oceans_shp_input, countries_to_include, ESPEN_IUs_shape_input) {
  
  df <- subset(df_input, Year == 2022) # subset HISTONCHO for Year = 2022
  
  # Read the African countries shapefile
  shapefile_path <- file.path(base_path, shapefile_input)
  african_countries <- st_read(dsn = shapefile_path)
  
  # View the attributes and structure of the shapefile (optional)
  summary(african_countries)
  
  # Read the oceans shapefile
  oceans_shp_path <- file.path(base_path, oceans_shp_input)
  oceans_shp <- st_read(oceans_shp_path)
  
  # Calculate the centroids for the african_countries shapefile
  # Step 1: Validate and repair the geometry of african_countries
  african_countries_valid <- st_make_valid(african_countries)
  african_centroids <- st_centroid(african_countries_valid)
  
  # Filter the African countries that are in the countries_to_include list
  african_centroids_oncho <- subset(african_centroids, NAME_0 %in% countries_to_include)
  
  # Modify specific country names
  african_centroids_oncho <- african_centroids_oncho %>%
    mutate(NAME_0 = case_when(
      NAME_0 == "Republic of Congo" ~ "Congo",  # Replace "Republic of Congo" with "Congo"
      NAME_0 == "Democratic Republic of the Congo" ~ "Democratic Republic of the Congo",  # Correct line break
      TRUE ~ NAME_0  # Keep other country names unchanged
    ))
  
  # maps & frequency distributions ~ country #
  ESPEN_IUs_shape_path <- file.path(base_path, ESPEN_IUs_shape_input)
  ESPEN_IUs <- st_read(ESPEN_IUs_shape_path )
  all_countries <- unique(df$ADMIN0ISO3)
  ESPEN_IUs_ALL <- ESPEN_IUs[which((ESPEN_IUs$ADMIN0ISO3 %in% all_countries)),]
  st_geometry_type(ESPEN_IUs_ALL)
  st_crs(ESPEN_IUs_ALL)
  
  # merge ESPEN shapefile with HISTONCHO
  merged_df <- ESPEN_IUs_ALL %>%
    left_join(df, by = c("IU_ID" = "IU_ID_MAPPING"))
  
  cat("


SUCCESSFULLY MERGED ESPEN (2021) SHAPEFILE WITH HISTONCHO", "\n")
  
  
  # Return the processed dataframe of centroids for the selected countries
  return(list(african_countries, oceans_shp, african_centroids_oncho, ESPEN_IUs_ALL, merged_df, df))
}


# ======================================================================================================= #
#               Function for plotting Figure 1  - OCP phases and APOC distribution of IUs                 #
# ======================================================================================================= #


process_and_plot_fig1 <- function(african_countries_shapefile, oceans_shp, SIZs_file_input, 
                                  african_centroids_oncho, ESPEN_shp, df_input, merged_shp) {
  
  HISTONCHO_merged_allIUs <- merged_shp
  
  HISTONCHO_merged_allIUs$OCP_PHASE <- ifelse(HISTONCHO_merged_allIUs$Control_prog == "APOC", "Former APOC", HISTONCHO_merged_allIUs$OCP_PHASE)
  
  df <- subset(df_input, Year == 2022) # HISTONCHO
  
  # Read the SIZs file
  SIZs_file_path <- file.path(base_path, SIZs_file_input)
  SIZs_IUs <- read_excel(SIZs_file_path)
  SIZ_IU_vec <- as.character(SIZs_IUs$IU)
  
  # Split into two subsets for plotting
  african_centroids_oncho_subset1 <- subset(african_centroids_oncho, NAME_0 %in% c("Guinea-Bissau", "Senegal", "Guinea", "Sierra Leone", "Togo", "Ghana", "Benin"))
  african_centroids_oncho_subset2 <- subset(african_centroids_oncho, !NAME_0 %in% c("Guinea-Bissau", "Senegal", "Guinea", "Sierra Leone", "Togo", "Ghana", "Benin"))
  
  # Find closest match for SIZs to IUs
  find_closest_match <- function(target, candidates) {
    distances <- stringdist::stringdistmatrix(target, candidates, method = "jw")
    closest_match <- candidates[which.min(distances)]
    return(closest_match)
  }
  
  # Apply function to match SIZs to IUs
  matched_names <- sapply(SIZ_IU_vec, function(name) find_closest_match(name, ESPEN_shp$IUs_NAME_MAPPING))
  
  # Unlist the names of matched_names into a vector
  SIZ_vector <- names(matched_names)
  
  df$SIZ_label <- ifelse(df$IUs_NAME_MAPPING %in% SIZ_vector, "SIZ", df$SIZ_label)
  
  df <- subset(df, SIZ_label == "SIZ")
  
  # Create co-endemicity map with SIZs
  merged_shp_SIZ <- ESPEN_shp %>%
    left_join(df, by = c("IU_ID" = "IU_ID_MAPPING"))
  
  merged_shp_SIZ <- subset(merged_shp_SIZ, SIZ_label == "SIZ")
  
  # Set color palette
  cbPalette_SIZ <- c("#FFCC00","#9999FF","#009E73","#CC0000","#0000FF","#FF6600","#990066","#993333","#003300","#330033","#330000","#006666","#99FFFF")
  
  # Create the main map
  p_map <- ggplot() +
    geom_sf(data = oceans_shp, aes(), fill = "lightblue", colour = NA) +
    geom_sf(data = HISTONCHO_merged_allIUs, aes(fill = OCP_PHASE), colour = NA, alpha = 0.7) +
    geom_sf(data = ESPEN_IUs_ALL, aes(), colour = "black", linewidth = 0.01, fill = NA, alpha = 0.3) +
    geom_sf(data = merged_shp_SIZ, aes(), colour = "#0000CC", alpha = 0.3) + # Add this layer
    geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
    geom_sf_text(data = african_centroids_oncho_subset1, aes(label = NAME_0), size = 1.5, color = "white", fontface = "bold", alpha = 1) +
    geom_sf_text(data = african_centroids_oncho_subset2, aes(label = NAME_0), size = 1.5, color = "black", fontface = "bold", alpha = 0.7) +
    theme_bw() +
    coord_sf(xlim = c(-15, 45), ylim = c(22, -16.5)) +  # Adjust for all IUs
    scale_fill_manual(values = cbPalette_SIZ, na.value = "gray") +
    scale_colour_manual(na.value="gray") +
    labs(fill='', x = " ", y = " ") +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.text = element_text(size = 5),
      legend.title = element_text(size = 10)
    )
  
  cbPalette <- c("#FFCC00","#9999FF","#009E73","#CC0000","#0000FF","#FF6600","#990066","#993333","#003300","#330033","#330000","#006666")
  
  # Just Bioko inset map
  p_bioko <- ggplot() +
    geom_sf(data = oceans_shp, aes(), fill = "lightblue", colour = NA) +
    geom_sf(data = HISTONCHO_merged_allIUs, aes(fill = OCP_PHASE), colour = NA, alpha = 0.7) +
    geom_sf(data = ESPEN_IUs_ALL, aes(), colour = "black", linewidth = 0.01, fill = NA, alpha = 0.3) +
    geom_sf(data = merged_shp_SIZ, aes(), colour = "#0000CC", alpha = 0.3) + # Add this layer
    geom_sf(data = ESPEN_IUs_ALL, aes(), colour = "black", linewidth = 0.01, fill = NA, alpha = 0.3) +
    coord_sf(xlim = c(8, 10), ylim = c(3, 5)) +
    theme_bw() +
    scale_fill_manual(values = cbPalette, na.value = "gray") +
    scale_colour_manual(na.value = "gray") +
    labs(fill = '') +
    theme(
      legend.position = "none",
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  
  Figure_1 <- ggdraw() +
    draw_plot(p_map, 0, 0, 1, 1) +  # Main map takes the whole space
    draw_plot(p_bioko, 0.2, 0.32, 0.25, 0.25)  # Inset map in bottom-left corner (adjust size and position as needed)
  
  cat("Figure 1 ready to plot!

      
")
  
  # Return the final map object
  return(Figure_1)
}

# ======================================================================================================= #
#               Function for plotting Figure 5  - Intervention mapping                                    #
# ======================================================================================================= #


process_and_plot_fig5 <- function(african_countries_shapefile, oceans_shp, african_centroids_oncho, 
                                  ESPEN_shp, merged_shp, df_2022){ 
  # shapefiles to plot
  oceans_shp <- oceans_shp
  african_countries <- african_countries_shapefile
  ESPEN_IUs_ALL <- ESPEN_shp
  merged_shp <- merged_shp 
  african_centroids_oncho <- african_centroids_oncho
  df_2022 <- df_2022
  
  cbPalette <- c("#CC79A7","#E69F00","#009E73","#F0E442","#0072B2")
  
  p_map <- ggplot() +
    geom_sf(data = oceans_shp, aes(), fill = "lightblue", colour = NA) +
    geom_sf(data = merged_shp, aes(fill = MDA_VC_Mapping), colour = NA, alpha = 0.7) +
    geom_sf(data = ESPEN_IUs_ALL, aes(), colour = "black", linewidth = 0.01, fill = NA, alpha = 0.3) +
    geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
    geom_sf_text(data = african_centroids_oncho, aes(label = NAME_0), size = 1.5, color = "black", fontface = "bold", alpha = 0.7) + # Add country labels
    coord_sf(xlim = c(-15, 45), ylim = c(22, -16.5)) + # for All IUs
    theme_bw() +
    scale_fill_manual(values = cbPalette, na.value = "gray") +
    scale_colour_manual(na.value="gray")+
    labs(fill='', x = " ", y = " ") +
    theme(
      legend.position = "bottom",  # Place the legend at the bottom
      legend.direction = "horizontal",
      legend.text = element_text(size = 7),  # Adjust size of legend text
      legend.title = element_text(size = 10))  # Adjust size of legend title)
  
  # just bioko #
  p_bioko <- ggplot() +
    geom_sf(data = oceans_shp, aes(), fill = "lightblue", colour = NA) +
    geom_sf(data = merged_shp, aes(fill = MDA_VC_Mapping), colour = NA, alpha = 0.7) +
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
  
  
  Figure_5a <- ggdraw() +
    draw_plot(p_map, 0, 0, 1, 1) +  # Main map takes the whole space
    draw_plot(p_bioko, 0.2, 0.32, 0.25, 0.25)  # Inset map in bottom-left corner (adjust size and position as needed)
  
  cat("Figure 5a ready to plot!
    
    
    
    
", "\n")
  
  
  # ==================== #
  # make frequency plots #
  
  # frequency dist plot
  freq_table_histint2 <- table(df_2022$MDA_VC_Mapping) # Create a frequency table
  
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
    scale_fill_manual(values = cbPalette) +
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
  
  # frequency dist plot by country
  freq_table_histint <- table(df_2022$MDA_VC_Mapping, df_2022$ADMIN0ISO3) # Create a frequency table
  
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
    scale_fill_manual(values = cbPalette) +
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
  Figure_5bc <- plot_grid(
    combined_plots, shared_legend,
    ncol = 1,
    rel_heights = c(1, 0.1)  # Legend takes 10% of the space at the bottom
  )
  
  cat("Figure 5b and 5c ready to plot!
    
    
    
    
", "\n")
  
  # Return the final map object
  return(list(Figure_5a, Figure_5bc))
  
  
}



# ======================================================================================================= #
#               Function for plotting Figure 6  - number of MDA rounds                                    #
# ======================================================================================================= #


process_and_plot_fig6 <- function(african_countries_shapefile, oceans_shp, african_centroids_oncho, 
                                  ESPEN_shp, merged_shp, df_2022){ 
  # shapefiles to plot
  oceans_shp <- oceans_shp
  african_countries <- african_countries_shapefile
  ESPEN_IUs_ALL <- ESPEN_shp
  merged_shp <- merged_shp 
  african_centroids_oncho <- african_centroids_oncho
  df_2022 <- df_2022
  
  p_map <- ggplot() +
    geom_sf(data = oceans_shp, aes(), fill = "lightblue", colour = NA) +
    geom_sf(data = merged_shp, aes(fill =Cum_MDA_Rev), colour = NA, alpha = 0.7) +
    geom_sf(data = ESPEN_IUs_ALL, aes(), colour = "black", linewidth = 0.01, fill = NA, alpha = 0.3) +
    geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
    geom_sf_text(data = african_centroids_oncho, aes(label = NAME_0), size = 1.5, color = "black", fontface = "bold", alpha = 0.7) + # Add country labels
    coord_sf(xlim = c(-15, 45), ylim = c(22, -16.5)) + # for All IUs
    theme_bw() +
    scale_fill_gradient(low = "yellow", high = "darkblue", na.value = "gray") +
    scale_colour_manual(na.value="white") +
    labs(fill='Revised number of cumulative MDA rounds', x = " ", y = " ") +
    theme(
      legend.position = "bottom",  # Place the legend at the bottom
      legend.direction = "horizontal",
      legend.text = element_text(size = 7),  # Adjust size of legend text
      legend.title = element_text(size = 10))  # Adjust size of legend title))
  
  # just bioko #
  p_bioko <- ggplot() +
    geom_sf(data = oceans_shp, aes(), fill = "lightblue", colour = NA) +
    geom_sf(data = merged_shp, aes(fill = Cum_MDA_Rev), colour = NA, alpha = 0.7) +
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
  
  
  Figure_6a <- ggdraw() +
    draw_plot(p_map, 0, 0, 1, 1) +  # Main map takes the whole space
    draw_plot(p_bioko, 0.2, 0.32, 0.25, 0.25) + # Inset map in bottom-left corner (adjust size and position as needed)
    draw_text("a", x = 0.1, y = 1, hjust = 0, vjust = 1, size = 10, fontface = "bold")
  
  cat("Figure 6a ready to plot!
    
    
    
    
", "\n")
  
  
  # ==================== #
  # make frequency plots #
  
  # Get the min and max of CUM_MDA_modelled
  min_value <- min(df_2022$Cum_MDA_Rev, na.rm = TRUE)
  max_value <- max(df_2022$Cum_MDA_Rev, na.rm = TRUE)
  
  # Create the plot with a customized color scale
  p1 <- ggplot(df_2022, aes(x = Cum_MDA_Rev, fill = ..x..)) +
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
  
  
  # frequency dist plot by country
  freq_table_cumMDA <- table(df_2022$Cum_MDA_Rev, df_2022$ADMIN0ISO3) # Create a frequency table
  
  freq_df_cumMDA <- as.data.frame(as.table(freq_table_cumMDA)) # Convert the frequency table to a data frame
  colnames(freq_df_cumMDA) <- c("Cum_MDA", "ADMIN0ISO3", "Frequency") # Rename the columns for better readability
  
  freq_df_cumMDA <- subset(freq_df_cumMDA, Frequency > 0)
  
  p2 <- ggplot(df_2022, aes(x = Cum_MDA_Rev, fill = ..x..)) +
    geom_histogram(data = subset(df_2022, Cum_MDA_Rev == 0),
                   binwidth = 2, color = "red", position = "identity", alpha = 0.7) +
    
    # For non-zero CUM_MDA_modelled values, apply black border
    geom_histogram(data = subset(df_2022, Cum_MDA_Rev != 0),
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
  Figure6bc <- plot_grid(
    combined_plots, shared_legend,
    ncol = 1,
    rel_heights = c(1, 0.1)  # Legend takes 10% of the space at the bottom
  )
  
  cat("Figure 6b and 6c ready to plot!
    
    
    
    
", "\n")
  
  # Return the final map object
  return(list(Figure_6a, Figure6bc))
  
  
}

# ======================================================================================================= #
#               Function for plotting Figure 7  - Co-endemicity                                           #
# ======================================================================================================= #


process_and_plot_fig7 <- function(african_countries_shapefile, oceans_shp, african_centroids_oncho, 
                                  ESPEN_shp, merged_shp, df_2022){ 
  # shapefiles to plot
  oceans_shp <- oceans_shp
  african_countries <- african_countries_shapefile
  ESPEN_IUs_ALL <- ESPEN_shp
  merged_shp <- merged_shp 
  african_centroids_oncho <- african_centroids_oncho
  df_2022 <- df_2022
  
  cbPalette <- c("#6699FF","#FFFF99","#FFCC00","#CC0000","#CC3399","#FF9966","#FF6600", "darkred")
  
  p_map <- ggplot() +
    geom_sf(data = oceans_shp, aes(), fill = "lightblue", colour = NA) +
    geom_sf(data = merged_shp, aes(fill = Co_endemicity), colour = NA, alpha = 0.7) +
    geom_sf(data = ESPEN_IUs_ALL, aes(), colour = "black", linewidth = 0.01, fill = NA, alpha = 0.3) +
    geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
    geom_sf_text(data = african_centroids_oncho, aes(label = NAME_0), size = 1.5, color = "black", fontface = "bold", alpha = 0.7) + # Add country labels
    coord_sf(xlim = c(-15, 45), ylim = c(22, -16.5)) + # for All IUs
    theme_bw() +
    scale_fill_manual(values = cbPalette, na.value = "gray") +
    scale_colour_manual(na.value="gray")+
    labs(fill='', x = " ", y = " ") +
    theme(
      legend.position = "bottom",  # Place the legend at the bottom
      legend.direction = "horizontal",
      legend.text = element_text(size = 7),  # Adjust size of legend text
      legend.title = element_text(size = 10))  # Adjust size of legend title)
  
  # just bioko #
  p_bioko <- ggplot() +
    geom_sf(data = oceans_shp, aes(), fill = "lightblue", colour = NA) +
    geom_sf(data = merged_shp, aes(fill = Co_endemicity), colour = NA, alpha = 0.7) +
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
  
  
  Figure_7a <- ggdraw() +
    draw_plot(p_map, 0, 0, 1, 1) +  # Main map takes the whole space
    draw_plot(p_bioko, 0.2, 0.32, 0.25, 0.25)  # Inset map in bottom-left corner (adjust size and position as needed)
  
  cat("Figure 7a ready to plot!
    
    
    
    
", "\n")
  
  
  # ==================== #
  # make frequency plots #
  
  # frequency dist plot
  freq_table_coendemicity2 <- table(df_2022$Co_endemicity) # Create a frequency table
  
  freq_df_coendemicity2 <- as.data.frame(as.table(freq_table_coendemicity2)) # Convert the frequency table to a data frame
  colnames(freq_df_coendemicity2) <- c("co_endemicity", "Frequency") # Rename the columns for better readability
  
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
  
  # frequency dist plot by country
  freq_table_coendemicity <- table(df_2022$Co_endemicity, df_2022$ADMIN0ISO3) # Create a frequency table
  
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
  Figure_7bc <- plot_grid(
    combined_plots, shared_legend,
    ncol = 1,
    rel_heights = c(1, 0.1)  # Legend takes 10% of the space at the bottom
  )
  
  cat("Figure 7b and 7c ready to plot!
    
    
    
    
", "\n")
  
  # Return the final map object
  return(list(Figure_7a, Figure_7bc))
  
  
}


# ======================================================================================================= #
#               Function for plotting Figure 8  - MDA status 2022                                          #
# ======================================================================================================= #


process_and_plot_fig8 <- function(african_countries_shapefile, oceans_shp, african_centroids_oncho, 
                                  ESPEN_shp, merged_shp, df_2022){ 
  # shapefiles to plot
  oceans_shp <- oceans_shp
  african_countries <- african_countries_shapefile
  ESPEN_IUs_ALL <- ESPEN_shp
  merged_shp <- merged_shp 
  african_centroids_oncho <- african_centroids_oncho
  df_2022 <- df_2022
  
  cbPalette <- c("#CC79A7","#E69F00","#009E73","#F0E442","#0072B2")
  
  p_map <- ggplot() +
    geom_sf(data = oceans_shp, aes(), fill = "lightblue", colour = NA) +
    geom_sf(data = merged_shp, aes(fill = Trt_Status_2022), colour = NA, alpha = 0.7) +
    geom_sf(data = ESPEN_IUs_ALL, aes(), colour = "black", linewidth = 0.01, fill = NA, alpha = 0.3) +
    geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
    geom_sf_text(data = african_centroids_oncho, aes(label = NAME_0), size = 1.5, color = "black", fontface = "bold", alpha = 0.7) + # Add country labels
    coord_sf(xlim = c(-15, 45), ylim = c(22, -16.5)) + # for All IUs
    theme_bw() +
    scale_fill_manual(values = cbPalette, na.value = "gray") +
    scale_colour_manual(na.value="gray")+
    labs(fill='', x = " ", y = " ") +
    theme(
      legend.position = "bottom",  # Place the legend at the bottom
      legend.direction = "horizontal",
      legend.text = element_text(size = 6),  # Adjust size of legend text
      legend.title = element_text(size = 10))  # Adjust size of legend title)
  
  # just bioko #
  p_bioko <- ggplot() +
    geom_sf(data = oceans_shp, aes(), fill = "lightblue", colour = NA) +
    geom_sf(data = merged_shp, aes(fill = Trt_Status_2022), colour = NA, alpha = 0.7) +
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
  
  
  Figure_8a <- ggdraw() +
    draw_plot(p_map, 0, 0, 1, 1) +  # Main map takes the whole space
    draw_plot(p_bioko, 0.2, 0.32, 0.25, 0.25)  # Inset map in bottom-left corner (adjust size and position as needed)
  
  cat("Figure 8a ready to plot!
    
    
    
    
", "\n")
  
  
  # ==================== #
  # make frequency plots #
  
  # frequency dist plot
  freq_table_trtstat22_2 <- table(df_2022$Trt_Status_2022) # Create a frequency table
  
  freq_df_trtstat22_2 <- as.data.frame(as.table(freq_table_trtstat22_2)) # Convert the frequency table to a data frame
  colnames(freq_df_trtstat22_2) <- c("trt_status_2022", "Frequency") # Rename the columns for better readability
  
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
    scale_fill_manual(values = cbPalette) +
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
  freq_table_trtstat22 <- table(df_2022$Trt_Status_2022, df_2022$ADMIN0ISO3) # Create a frequency table
  
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
    scale_fill_manual(values = cbPalette) +
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
  Figure_8bc <- plot_grid(
    combined_plots, shared_legend,
    ncol = 1,
    rel_heights = c(1, 0.1)  # Legend takes 10% of the space at the bottom
  )
  
  cat("Figure 8b and 7c ready to plot!
    
    
    
    
", "\n")
  
  # Return the final map object
  return(list(Figure_8a, Figure_8bc))
  
  
}
