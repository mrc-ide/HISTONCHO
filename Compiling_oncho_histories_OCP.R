# ================================================================================================================================== #
#                                           New OCP histories (NOV 2023)                                                            #
# ================================================================================================================================== #
library(dplyr)
library(sf)
library(sp)
library(tidyverse)
library(broom)
library(ggplot2)
library(readxl)
library(stringdist)

# ================================================= #
# Define the base path for calling all data sources #
base_path <- "C:/Users/mad206/OneDrive - Imperial College London/Documents/HISTONCHO/input data" # set base path specific to your system


# LOAD IN OCP #

# #OCP_DF_lastyr <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Cleaning IUs (Paul B)/OCP_DF.csv")
# 
# load("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Cleaning IUs (Paul B)/OCP_DF_ALL.RData")
# 
# dfOCP_included <- oncho_df_phase_complete
# dfOCP_included_lastyr <- subset(dfOCP_included, Year == 2022) # 873 IUs (April 25')
# 
# # 443 IUs classified as endemic
# nrow(dfOCP_included_lastyr[dfOCP_included_lastyr$MAX_Endemicity %in% c("Endemic (under MDA)",
#                                                                        "Endemic (MDA not delivered)",
#                                                                        "Endemic (under post-intervention surveillance)"), ])
# 
# # 430 IUs are non-endemic, not-reported, unknwon
# nrow(dfOCP_included_lastyr[dfOCP_included_lastyr$MAX_Endemicity %in% c("Unknown (consider Oncho Elimination Mapping)",
#                                                                        "Unknown (under LF MDA)",
#                                                                        "Not reported",
#                                                                        "Non-endemic"), ])
# 
# # 443 + 430 = 873 IuS
# 
# # check (April 25' ) 
# table(dfOCP_included_lastyr$Included) # check : included b/c "endemic" in ESPEN = 449 or 
#                                       # included b/c not "endemic" in OCP shape = 227
#                                       # 449 + 227 = 676 
#                                       # not included = 197
#                                       # 449 + 227 + 197 = 873 total
# # look at each set of IUs under two types of included categories:
# 
# # 1) included = "endemic"
# dfOCP_included_lastyr_incl_endemic <- subset(dfOCP_included_lastyr, Included == "Included Endemic")
# nrow(dfOCP_included_lastyr_incl_endemic) # 449
# unique(dfOCP_included_lastyr_incl_endemic$MAX_Endemicity)
# table(dfOCP_included_lastyr_incl_endemic$MAX_Endemicity) # 245 (endemic - MDA not delivered), 186 (endemic - under MDA), 12 (endemic - under PIS), 
#                                                         # 6 ("not reported" or "non-endemic")
# 
# dfOCP_included_lastyr_incl_endemic_subset <- subset(dfOCP_included_lastyr_incl_endemic, MAX_Endemicity %in% c("Not reported", "Non-endemic")) # 6 
# unique(dfOCP_included_lastyr_incl_endemic_subset$MAX_Endemicity_parent) # all parent IUs = endemic
# 
# # 2) included = Intervention (OCP)
# dfOCP_included_lastyr_incl_OCPshape <- subset(dfOCP_included_lastyr, Included == "Intervention (OCP)")
# nrow(dfOCP_included_lastyr_incl_OCPshape) # 227
# table(dfOCP_included_lastyr_incl_OCPshape$MAX_Endemicity) # 211 (Not reported), 6 (unknown - both consider OEM + under LF MDA), 10 non-endemic
# 
# 
# vec_to_include <- c("Included Endemic", "Intervention (OCP)")
# dfOCP_included1 <- dfOCP_included[dfOCP_included$Included %in% vec_to_include,]
# 
# matches <- dfOCP_included1$Cum_MDA == dfOCP_included1$Cum_MDA_Final
# isFALSE(matches)
# matches_df <- as.data.frame(matches)
# 
# length(unique(dfOCP_included1$IU_ID_MAPPING)) # 676 IUs


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

# call function :
dfOCP_included1 <- load_and_filter_ocp_data("/OCP_DF_ALL.RData")


# # =========================================================================== #
# # Find Ius that have "not-reported" in 2013 and "non-endemic" all other years #
# 
# filtered_ius <- dfOCP_included1 %>%
#   group_by(IU_ID_MAPPING) %>%
#   filter(
#     any(Year == 2013 & Endemicity == "Not reported") &
#       any(Year %in% 2014:2022 & Endemicity == "Non-endemic") &
#       !any(Year %in% 2014:2022 & Endemicity %in% c("Endemic (under MDA)", "Endemic (MDA not delivered)",
#                                                    "Unknown (consider Oncho Elimination Mapping)"))
#   )
# 
# # filtered_ius <- dfOCP_included1 %>%
# #   group_by(IU_ID_MAPPING) %>%
# #   filter(
# #     any(Year %in% c(2013,2014) & Endemicity == "Not reported") &
# #       any(Year %in% 2015:2022 & Endemicity == "Non-endemic") &
# #       !any(Year %in% 2015:2022 & Endemicity %in% c("Endemic (under MDA)", "Endemic (MDA not delivered)",
# #                                                    "Unknown (consider Oncho Elimination Mapping)"))
# #   )
# 
# # Get the unique IU_names that satisfy the conditions
# unique_ius <- unique(filtered_ius$IU_ID_MAPPING) # 223 IUs
# length(unique_ius) # 223 when with not reported in just 2013 or both 2013, 2014 (rest non-endemic)
# 
# # relabel these IUs as not included in overall dataframe (new included column) #
# dfOCP_included$Included_updated <- ifelse(dfOCP_included$IU_ID_MAPPING %in% unique_ius, "Not included (under OCP area but not reported/non-endemic)", dfOCP_included$Included)
# dfOCP_included1$Included_updated <- ifelse(dfOCP_included1$IU_ID_MAPPING %in% unique_ius, "Not included (under OCP area but not reported/non-endemic)", dfOCP_included1$Included)
# dfOCP_included_lastyr$Included_updated <- ifelse(dfOCP_included_lastyr$IU_ID_MAPPING %in% unique_ius, "Not included (under OCP area but not reported/non-endemic)", dfOCP_included_lastyr$Included)
# 
# table(dfOCP_included_lastyr$Included_updated)
# 
# dfOCP_included1_lastyr_notincluded <- subset(dfOCP_included_lastyr, Included_updated == "Not included") # want to exclude these
# length(unique(dfOCP_included1_lastyr_notincluded$IU_ID_MAPPING)) # 197 IUs removed
# dfOCP_included1_lastyr_noncontrol <- subset(dfOCP_included_lastyr, PHASE == "NON-CONTROL") # theres 20 IUs extra here want to keep!
# length(unique(dfOCP_included1_lastyr_noncontrol$IU_ID_MAPPING)) # 217 IUs here (197 of these are specified as "Not included" above - 20 are endemic and need to be included)
# 
# # Assuming df1 and df2 are your two dataframes
# difference_iu <- setdiff(unique(dfOCP_included1_lastyr_noncontrol$IU_ID_MAPPING), unique(dfOCP_included1_lastyr_notincluded$IU_ID_MAPPING)) # these are the 20 extra want to keep!
# 
# dfOCP_included1_check <- subset(dfOCP_included1, IU_ID_MAPPING %in% difference_iu)
# #dfOCP_included1_check2 <- subset(dfOCP_included1, IU_ID_MAPPING %in% difference_iu)
# 
# # want to filter on not included rather than Non-control in Phase
# # above dataframe still has all 873 IUs, but labels some as not to be included (non-endemic in the )
# 
# # map to check #
# 
# # set-up objects #
# ESPEN_IUs <- st_read('C:/Users/mad206/OneDrive/Endgame/Endgame IUs/ESPEN_IU_2021.shp')
# 
# #OCP_histories_updated_lastyr_270923 <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/New Endgame Sept 2023 -/OCP_histories_updated_lastyr_270923.csv")
# #df_OCP_endemic_minimal <- OCP_histories_updated_lastyr_270923[, c("ADMIN0", "ADMIN0ISO3", "ADMIN1", "IU_ID_MAPPING", "IUs_NAME_MAPPING","IU_CODE_MAPPING", "endemicity")]
# OCP_countries <- unique(dfOCP_included1$ADMIN0ISO3)
# ESPEN_IUs_OCP <- ESPEN_IUs[which((ESPEN_IUs$ADMIN0ISO3 %in% OCP_countries)),]
# st_geometry_type(ESPEN_IUs_OCP)
# st_crs(ESPEN_IUs_OCP)
# 
# shapefile_path <- "C:/Users/mad206/OneDrive/Endgame/OCP mapping/African countries/Africa_Boundaries.shp"
# african_countries <- st_read(dsn = shapefile_path)
# summary(african_countries)
# 
# dfOCP_included_lastyr1 <- subset(dfOCP_included1, Year == 2022)
# nrow(dfOCP_included_lastyr1)# IUs = 676
# 
# table(dfOCP_included_lastyr1$Included) # included endemic (449) + included OCP shape (227) = 676 IUs
# table(dfOCP_included_lastyr1$Included_updated) # included endemic (449) + included OCP shape (4) + 
#                                                # included under OCP shapefile but considered not reported/non-endemic (223) = 676 IUs
# 
# OCP_includedIUs_676 <- ESPEN_IUs_OCP %>%
#   left_join(dfOCP_included_lastyr1, by = c("IU_ID" = "IU_ID_MAPPING"))
# 
# #cbPalette <- c("#CC79A7","#E69F00","#009E73","#F0E442","#0072B2")
# 
# ggplot() +
#   geom_sf(data = OCP_includedIUs_676, aes(fill = PHASE), colour = NA, alpha = 0.7) +
#   geom_sf(data = ESPEN_IUs_OCP, aes(), colour = NA, size = 1, fill = NA, alpha = 0.1) +
#   geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
#   coord_sf(xlim = c(-20, 7), ylim = c(4, 20)) +
#   theme_bw() +
#   #scale_fill_manual(na.value = "gray") +
#   scale_colour_manual(na.value="gray")+
#   labs(fill='') +
#   theme(
#     legend.position = "bottom",  # Place the legend at the bottom
#     legend.direction = "horizontal")


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

# call function:
# Assuming dfOCP_included1 is already loaded in the environment
dfOCP_included1 <- further_IU_checks_update_df_OCP(dfOCP_included1)

# # =================================================================== #
# #       backfill histories to 1975                                    #
# # =================================================================== #
# 
# #FinalESPENyr <- 2021
# FinalESPENyr <- 2013
# 
# dfls <- split(dfOCP_included1, dfOCP_included1$IU_ID)
# 
# ## new list of dfs
# newdf <- vector("list", length(dfls))
# 
# lsid <- split(dfOCP_included1$IU_ID, dfOCP_included1$IU_ID)
# lsid <- unique(unlist(lsid))
# 
# # loop to fill in missing years and columns
# 
# 
# for (i in 1:length(dfls)) {
# 
# 
#   newdf[[i]] <- subset(dfls[[i]], select=colnames(dfOCP_included1))
# 
#   FinalESPENyr <- head(newdf[[i]]$Year, 1)
#   # indicator for augmented or ESPEN data
#   #newdf[[i]]$Aug <- 0
# 
#   ## check if any years are missing from ESPEN and if so add these years to the Augmentation
#   start <- min(newdf[[i]]$Year) - FinalESPENyr # should be 0 if starting at 2014
# 
#   ## cumulative rounds of national MDA before 2014
#   ## set equal to national start year if IU start indicates going further back
#   ## set equal to national start year
# 
#   IUStartMDA <- max(min(newdf[[i]]$Year) - min(newdf[[i]]$Cum_MDA_Final) - start)
# 
#   IUstart <- 1975
# 
#   newdf[[i]]$IUstart <- IUstart
#   Prioryrs <- FinalESPENyr - IUstart ## prior years to include
# 
#   PriorMDA <- FinalESPENyr - IUStartMDA ## prior MDA rounds
# 
#   newdf[[i]]$lsID <- i
# 
#   i
# 
#   if (is.na(IUstart)!=T) {
#     if(IUstart<FinalESPENyr) {
#       # if cumulative rounds greater than 1, augment data frame
#       # by repeating the first row
# 
#       tmp <- newdf[[i]][rep(1, each = (min(newdf[[i]]$Year)-IUstart)), ]
# 
#       # fill in the years from start date of IU to first year on ESPEN
#       tmp$Year <- seq(IUstart, min(newdf[[i]]$Year)-1)
# 
#       # set MDA to 0 for all years
# 
#       # tmp$MDA <- 0
#       # tmp$Cum_MDA <- 0
# 
# 
#       # fill in with years that had MDA (where any cum_MDA > 1 in 2013 onwards)
# 
#       if(any(head(newdf[[i]]$Cum_MDA_Final)> 0)) {
#         # tmp$MDA[1:PriorMDA] <- 1 # this should be from last row (2012) to n row (i.e. (nrow(tmp) - priorMDA) : (nrow(tmp)) )
# 
#         val_totest <- head(newdf[[i]]$Cum_MDA_Final,1)
# 
#         # where top cum_MDA value is greater than 1
#         if(val_totest > 1){
# 
#           # tmp$MDA[(nrow(tmp) - (PriorMDA-2)) : (nrow(tmp))] <- 1 # this should be from last row (2012) to n row (i.e. (nrow(tmp) - priorMDA) : (nrow(tmp)) )
#           #
#           # # remove MDA = 1 and Cum_MDA = 1 for 1975 yr (incorrectly included)
#           # ifelse(tmp$year == 1975 & tmp$MDA == 1, 0, tmp$MDA)
# 
#           #tmp$Cum_MDA[1:PriorMDA] <- seq(1, PriorMDA)
#           #tmp$Cum_MDA_Final[(nrow(tmp) - (PriorMDA-2)) : (nrow(tmp))] <- seq(1, PriorMDA)
# 
#           # Determine the number of leading zeros based on your condition
#           leading_zeros <- rep(0, 37 - head(newdf[[i]]$Cum_MDA_Final,1) + 1)
# 
#           # Create the sequence that goes in reverse from 0 to 27
#           tmp$Cum_MDA_Final <- c(leading_zeros, seq(from = 0, to = head(newdf[[i]]$Cum_MDA_Final,1) - 1, by = 1))
# 
# 
#         }
# 
#       }
# 
#       # #enedmicity category given "Augmented Status"
#       tmp$Endemicity <- "Augmented"
#       tmp$MDA_scheme <- "Augmented"
# 
#       newdf[[i]] <- rbind(tmp, newdf[[i]])
# 
# 
#       #rm(tmp)
#     }
#   }
# 
# 
# }
# 
# newdf <- do.call(rbind, newdf)
# 
# dfOCP_included2 <- newdf
# 
# # check all IUs have 1975 - 2022 (48 rows)
# check_result5 <- dfOCP_included2 %>%
#   group_by(IU_ID_MAPPING) %>%
#   filter(n() != 48) %>%
#   ungroup() # check if any IUs with less than 10 rows
# 
# length(unique(dfOCP_included2$IU_ID_MAPPING)) # theres 5 Ius without full 2013 - 2022 history
#                                               # 676 IUs
# 
# # remove geometry column as making dataframe slow?
# dfOCP_included2$geometry <- NULL


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

# call function (note this will take some time)
dfOCP_included2 <- backfill_histories(dfOCP_included1)


# # ======================================= #
# #       Add in SIZ labels                 #
# 
# # match SIZs to IUs#
# SIZs_IUs <- read_excel("C:/Users/mad206/OneDrive/Endgame/OCP mapping/SIZs/SIZs_IUs.xlsx")
# 
# SIZ_IU_vec <- as.character(SIZs_IUs$IU)
# SIZ_IU_vec # check these IUs against new IUs
# 
# # extract out the IUs which should be in the SIZs (function to below to deal with those IUs with accents not matching)
# 
# # Define a function to find the closest match
# find_closest_match <- function(target, candidates) {
#   distances <- stringdist::stringdistmatrix(target, candidates, method = "jw")
#   closest_match <- candidates[which.min(distances)]
#   return(closest_match)
# }
# 
# # Apply the function to each value in names_to_keep
# matched_names <- sapply(SIZ_IU_vec, function(name) find_closest_match(name, dfOCP_included2$IUs_NAME_MAPPING))
# 
# # Subset the dataframe based on the matched names
# dfOCP_included2_SIZs <- dfOCP_included2[dfOCP_included2$IUs_NAME_MAPPING %in% matched_names, ]
# length(unique(dfOCP_included2_SIZs$IUs_NAME))
# 
# # ADD label in to main dataframe #
# 
# # Create a vector of logical values indicating whether the conditions are met
# conditions_met <- dfOCP_included2$IUs_NAME_MAPPING %in% matched_names &
#   !is.na(dfOCP_included2$Year) &  # Check for non-NA values in Year
#   dfOCP_included2$Year %in% 2003:2012
# 
# # Use ifelse to assign "SIZ" where conditions are met and NA otherwise
# dfOCP_included2$SIZ_label <- ifelse(conditions_met, "SIZ", NA)
# 
# length(unique(dfOCP_included2$IU_ID_MAPPING)) # 676 IUs


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

# call function
dfOCP_included2 <- add_SIZ_labels(dfOCP_included2, "/SIZs_IUs.xlsx")



# =============================================================================== #
#   Function to check how many are not endemic in ESPEN but included in OCP shape? #
# =============================================================================== #

# dfOCP_included2_ESPENendemic <- subset(dfOCP_included2, MAX_Endemicity %in% c("Endemic (under MDA)",
#                                                                               "Endemic (MDA not delivered)",
#                                                                               "Endemic (under post-intervention surveillance)"))
# 
# dfOCP_included2_ESPENendemic_lastyr <- subset(dfOCP_included2_ESPENendemic, Year == 2022)
# nrow(dfOCP_included2_ESPENendemic_lastyr) # 443 IUs
# 
# dfOCP_included2_ESPENendemic_notinOCPshape <- subset(dfOCP_included2_ESPENendemic_lastyr, PHASE == "NON-CONTROL")
# nrow(dfOCP_included2_ESPENendemic_notinOCPshape) # 20 Ius are ESPEN endemic but not in the OCP shape file - remove
# 
# dfOCP_included2_onlyOCPshape <- subset(dfOCP_included2, MAX_Endemicity %in% c("Unknown (under LF MDA)",
#                                                                               "Not reported",
#                                                                               "Non-endemic",
#                                                                               "Unknown (consider Oncho Elimination Mapping)"))
# 
# dfOCP_included2_onlyOCPshape_lastyr <- subset(dfOCP_included2_onlyOCPshape, Year == 2022)
# nrow(dfOCP_included2_onlyOCPshape_lastyr) # 233 IUs

# =============================================================================== #
#   Function to check how many are not endemic in ESPEN but included in OCP shape? #
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

# call function:
perform_endemicity_checks(dfOCP_included2)


# # =================================================== #
# #     add baseline endemicity to dataframe            #
# 
# load("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/IU pre-control fitted prevalence/summaries_ocp.Rdata")
# View(summaries_ocp)
# 
# OCP_baseline <- as.data.frame(summaries_ocp) # convert matrix to dataframe
# check <- dfOCP_included2 %>%
#   group_by(ADMIN0ISO3) %>%
#   summarise(unique_numeric_count = length(unique(IU_ID_MAPPING[!is.na(as.numeric(IU_ID_MAPPING))])))
# 
# OCP_baseline$ADMIN0 <- substr(OCP_baseline$IU_CODE, 1, 3)
# 
# OCP_baseline$IU_ID_MAPPING <- substr(OCP_baseline$IU_CODE, nchar(OCP_baseline$IU_CODE) - 4, nchar(OCP_baseline$IU_CODE))
# 
# OCP_baseline$IU_ID_MAPPING <- as.numeric(as.character(OCP_baseline$IU_ID_MAPPING)) # remove leading 0 from IU_ID_MAPPING
# 
# # Merge the two data frames based on the 'IU_CODE' column
# dfOCP_included2  <- merge(dfOCP_included2, OCP_baseline, by.x = "IU_ID_MAPPING", by.y = "IU_ID_MAPPING", all.x = TRUE)
# 
# # omit last 8 cols with merged dataframe (unless want quantiles for pre-control prev)
# dfOCP_included2  <- dfOCP_included2[, 1:(ncol(dfOCP_included2) - 8)]
# 
# dfOCP_included2 <- dfOCP_included2 %>%
#   mutate(endemicity_baseline = case_when(
#     is.na(mean) ~ "non-endemic",
#     mean < 0.40 ~ "hypoendemic",
#     mean < 0.60 ~ "mesoendemic",
#     mean < 0.80 ~ "hyperendemic",
#     TRUE ~ "holoendemic"
#   ))
# 
# # remove any IUs with non-endemic baseline from geostatistical maps (O'Hanlon)
# dfOCP_included2_nonendemic_baseline <- subset(dfOCP_included2, endemicity_baseline == "non-endemic")
# check_df <- subset(dfOCP_included2_nonendemic_baseline, Year == 2022)
# table(check_df$MAX_Endemicity) # 6 IUs  - all "Not reported" status (so these are in the box of 227 in the flow chart (i.e., included b/c in OCP shape))
# 
# non_endemic_baseline_vec <- unique(dfOCP_included2_nonendemic_baseline$IU_ID_MAPPING)
# length(non_endemic_baseline_vec) # n = 6 IUs
# unique(dfOCP_included2_nonendemic_baseline$MAX_Endemicity) # all MAX_endemicity = "Not reported"
# 
# 
# dfOCP_included2 <- dfOCP_included2[!dfOCP_included2$IU_ID_MAPPING %in% non_endemic_baseline_vec, ]
# length(unique(dfOCP_included2$IU_ID_MAPPING)) # 670 Ius left (with 6 IUs removed as no baseline in geostat map)
# 
# check_df <- subset(dfOCP_included2, Year == 2022)
# table(check_df$Included) # included (endemic) = 449 + included OCP area = 221 = 670 IUs

# =========================================================================================== #
#     Function to integrate baseline endemicity and remove IUs without baseline information   #
# =========================================================================================== #

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

# Call function:
dfOCP_included2 <- integrate_baseline_endemicity(dfOCP_included2, "/summaries_ocp.Rdata")


# # =================================================================================================== #
# #  1) identify if treatment naive area                                                                #
# 
# dfOCP_included3 <- dfOCP_included2
# 
# # ============================== #
# #  Pre-ESPEN history to include  #
# 
# dfOCP_included3 <- dfOCP_included3 %>%
#   group_by(IU_ID_MAPPING) %>%
#   mutate(
#     Pre_ESPEN_MDA_history = ifelse(ADMIN0ISO3 == "NER" |
#       (any(MAX_Endemicity %in% c("Endemic (MDA not delivered)", "Endemic (under MDA)", "Endemic (under post-intervention surveillance)")) &
#          any(Cum_MDA > 0 & Year == 2013)) |
#         (any(MAX_Endemicity %in% c("Unknown (consider Oncho Elimination Mapping)", "Unknown (under LF MDA)", "Not reported","Non-endemic")) &
#            any(endemicity_baseline %in% c("hyperendemic","mesoendemic", "hypoendemic")) &
#            any(Cum_MDA > 0 & Year == 2013)),
#       "Include",
#       "Exclude"
#     )
#   ) %>%
#   ungroup()  # Remove grouping information
# 
# 
# # dfOCP_included3 <- dfOCP_included3 %>%
# #   group_by(IU_ID_MAPPING) %>%
# #   mutate(ADMIN0ISO3 == "NER","Include","Exclude") %>%
# #   ungroup()  # Remove grouping information
# 
# 
# dfOCP_included3 <- dfOCP_included3 %>% arrange(IU_ID_MAPPING, Year) # order by ascending year within each IU_ID_MAPPING
# 
# #dfOCP_included3_check <- subset(dfOCP_included3, Pre_ESPEN_MDA_history == "Exclude")
# #dfOCP_included3a_check <- subset(dfOCP_included3_check, Year < 2013)
# 
# # ==============================  #
# #  Post-ESPEN history to include  #
# 
# # note we include non-endemic in the statement below because CUM_MDA is > 0 & within the OCP shape file so believe this is endemic
# dfOCP_included3 <- dfOCP_included3 %>%
#   group_by(IU_ID_MAPPING) %>%
#   mutate(
#     ESPEN_MDA_history = ifelse(
#       (any(Endemicity %in% c("Endemic (MDA not delivered)", "Endemic (under MDA)", "Endemic (under post-intervention surveillance)") &
#              Year > 2012) &
#          any(EpiCov > 0 & Year > 2013)) |
#         (any(MAX_Endemicity %in% c("Unknown (consider Oncho Elimination Mapping)", "Unknown (under LF MDA)", "Not reported", "Non-endemic")) &
#            any(endemicity_baseline %in% c("hyperendemic","mesoendemic", "hypoendemic")) &
#            any(EpiCov > 0 & Year > 2013)),
#       "Include",
#       "Exclude"
#     )
#   ) %>%
#   ungroup()  # Remove grouping information
# 
# #dfOCP_included3_check <- subset(dfOCP_included3, ESPEN_MDA_history == "Exclude")
# #dfOCP_included3a_check <- subset(dfOCP_included3_check, Year > 2012)
# #dfOCP_included3_check <- subset(dfOCP_included3, Pre_ESPEN_MDA_history == "Exclude" & ESPEN_MDA_history == "Exclude")
# #dfOCP_included3_check <- subset(dfOCP_included3, Pre_ESPEN_MDA_history == "Include" & ESPEN_MDA_history == "Include")
# #dfOCP_included3a_check <- subset(dfOCP_included3_check, Year > 2012)
# 
# check_histories_trtnaive <- dfOCP_included3 %>%
#   filter(
#     (Pre_ESPEN_MDA_history == "Exclude" & ESPEN_MDA_history == "Exclude")
#   )
# 
# length(unique(check_histories_trtnaive$IU_ID_MAPPING)) # 57 IUs are treatment naive
# 
# dfOCP_included3_lastyr <- subset(dfOCP_included3, Year == 2022)
# dfOCP_included3_lastyr$MDA_status <- ifelse(dfOCP_included3_lastyr$Pre_ESPEN_MDA_history == "Exclude" &
#                                               dfOCP_included3_lastyr$ESPEN_MDA_history == "Exclude", "Treatment naive", "MDA history")
# 
# OCP_MDAstatus_670 <- ESPEN_IUs_OCP %>%
#   left_join(dfOCP_included3_lastyr, by = c("IU_ID" = "IU_ID_MAPPING"))
# 
# ggplot() +
#   geom_sf(data = OCP_MDAstatus_670, aes(fill = MDA_status), colour = NA, alpha = 0.7) +
#   geom_sf(data = ESPEN_IUs_OCP, aes(), colour = NA, size = 1, fill = NA, alpha = 0.1) +
#   geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.1) +
#   coord_sf(xlim = c(-20, 7), ylim = c(4, 20)) +
#   theme_bw() +
#   #scale_fill_manual(na.value = "gray") +
#   scale_colour_manual(na.value="gray")+
#   labs(fill='') +
#   theme(
#     legend.position = "bottom",  # Place the legend at the bottom
#     legend.direction = "horizontal")
# 
# length(unique(dfOCP_included3_lastyr$IU_ID_MAPPING))
# nrow(dfOCP_included3_lastyr) # 670 Ius


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

# call function:
dfOCP_included3 <- add_pre_post_ESPEN_history(dfOCP_included2)


# ================================================================================================== #
# 2)                                Introduce interventions                                          #
# ================================================================================================== #

# dfOCP_included4 <- dfOCP_included3 # RESET HERE
# 
# dfOCP_included4$MDA_status <- ifelse(dfOCP_included4$Pre_ESPEN_MDA_history == "Exclude" &
#                                        dfOCP_included4$ESPEN_MDA_history == "Exclude", "Treatment naive", "MDA history")
# 
# dfOCP_included4$ADMIN0 <- dfOCP_included4$ADMIN0.x
# 
# OCP_DF3 <- dfOCP_included4
# 
# check_df <- subset(OCP_DF3, Year == 2022)
# nrow(check_df) # 670 IUs
# 
# # Vector Control (from 1975) in OCP period (up to 2002) #
# 
# OCP_DF3$vector_control <- ifelse(OCP_DF3$ADMIN0 %in% "Benin" & OCP_DF3$PHASE %in% "PHASE III EAST - JUL 1977" & OCP_DF3$Year %in% c(1977:2002), 1,
#                                  ifelse(OCP_DF3$ADMIN0 %in% "Benin" & OCP_DF3$PHASE %in% "SOUTHERN EXTENSION - FEB 1988" & OCP_DF3$Year %in% c(1988:2002), 1,
#                                         ifelse(OCP_DF3$ADMIN0 %in% "Burkina Faso" & OCP_DF3$PHASE %in% "PHASE 1 - FEB 1975" & OCP_DF3$Year %in% c(1975:1989), 1,
#                                                ifelse(OCP_DF3$ADMIN0 %in% "Burkina Faso" & OCP_DF3$PHASE %in% "PHASE II - JAN 1976" & OCP_DF3$Year %in% c(1976:1989), 1,
#                                                       ifelse(OCP_DF3$ADMIN0 %in% "Burkina Faso" & OCP_DF3$PHASE %in% "PHASE III EAST - JUL 1977" & OCP_DF3$Year %in% c(1977:1989), 1,
#                                                              ifelse(OCP_DF3$ADMIN0 %in% "Cote d'Ivoire" & OCP_DF3$PHASE %in% "PHASE 1 - FEB 1975" & OCP_DF3$Year %in% c(1975:1991), 1,
#                                                                     ifelse(OCP_DF3$ADMIN0 %in% "Cote d'Ivoire" & OCP_DF3$PHASE %in% "PHASE III WEST - MAR 1977" & OCP_DF3$Year %in% c(1977:1991), 1,
#                                                                            ifelse(OCP_DF3$ADMIN0 %in% "Cote d'Ivoire" & OCP_DF3$PHASE %in% "PHASE IV - MAR 1979" & OCP_DF3$Year %in% c(1979:1991), 1,
#                                                                                   ifelse(OCP_DF3$ADMIN0 %in% "Ghana" & OCP_DF3$PHASE %in% "PHASE 1 - FEB 1975" & OCP_DF3$Year %in% c(1975:1989), 1,
#                                                                                          ifelse(OCP_DF3$ADMIN0 %in% "Ghana" & OCP_DF3$PHASE %in% "PHASE II - JAN 1976" & OCP_DF3$Year %in% c(1976:1989), 1,
#                                                                                                 ifelse(OCP_DF3$ADMIN0 %in% "Ghana" & OCP_DF3$PHASE %in% "PHASE III EAST - JUL 1977" & OCP_DF3$Year %in% c(1977:1989), 1,
#                                                                                                        ifelse(OCP_DF3$ADMIN0 %in% "Ghana" & OCP_DF3$PHASE %in% "SOUTHERN EXTENSION - FEB 1988" & OCP_DF3$Year %in% c(1988:1998), 1,
#                                                                                                               ifelse(OCP_DF3$ADMIN0 %in% "Togo" & OCP_DF3$ADMIN1 %in% c("Kara","Centrale") & OCP_DF3$PHASE %in% "PHASE II - JAN 1976" & OCP_DF3$Year %in% c(1976:2002), 1,
#                                                                                                                      ifelse(OCP_DF3$ADMIN0 %in% "Togo" & OCP_DF3$ADMIN1 %in% "Savanes" & OCP_DF3$PHASE %in% "PHASE II - JAN 1976" & OCP_DF3$Year %in% c(1976:1993), 1,
#                                                                                                                             ifelse(OCP_DF3$ADMIN0 %in% "Togo" & OCP_DF3$ADMIN1 %in% c("Kara","Centrale") & OCP_DF3$PHASE %in% "PHASE III EAST - JUL 1977" & OCP_DF3$Year %in% c(1977:2002), 1,
#                                                                                                                                    ifelse(OCP_DF3$ADMIN0 %in% "Togo" & OCP_DF3$ADMIN1 %in% "Savanes" & OCP_DF3$PHASE %in% "PHASE III EAST - JUL 1977" & OCP_DF3$Year %in% c(1977:1993), 1,
#                                                                                                                                           ifelse(OCP_DF3$ADMIN0 %in% "Togo" & OCP_DF3$PHASE %in% "SOUTHERN EXTENSION - FEB 1988" & OCP_DF3$Year %in% c(1989:2002), 1,
#                                                                                                                                                  ifelse(OCP_DF3$ADMIN0 %in% "Guinea" & OCP_DF3$PHASE %in% "WESTERN EXTENSION - MAR 1989" & OCP_DF3$Year %in% c(1989:2002), 1,
#                                                                                                                                                         ifelse(OCP_DF3$ADMIN0 %in% "Mali" & OCP_DF3$PHASE %in% "PHASE 1 - FEB 1975" & OCP_DF3$Year %in% c(1975:1987), 1,
#                                                                                                                                                                ifelse(OCP_DF3$ADMIN0 %in% "Mali" & OCP_DF3$PHASE %in% "PHASE III EAST - JUL 1977" & OCP_DF3$Year %in% c(1977:1987), 1,
#                                                                                                                                                                       ifelse(OCP_DF3$ADMIN0 %in% "Mali" & OCP_DF3$PHASE %in% "WESTERN EXTENSION - MAR 1989" & OCP_DF3$Year %in% c(1989:2002), 1,
#                                                                                                                                                                           ifelse(OCP_DF3$ADMIN0 %in% "Mali" & OCP_DF3$PHASE %in% "PHASE III WEST - MAR 1977" & OCP_DF3$Year %in% c(1977:1991), 1,
#                                                                                                                                                                              ifelse(OCP_DF3$ADMIN0 %in% "Niger" & OCP_DF3$PHASE %in% c("PHASE III EAST - JUL 1977", "NON-CONTROL") & OCP_DF3$Year %in% c(1977:1989), 1,
#                                                                                                                                                                                     NA)))))))))))))))))))))))
# 
# 
# OCP_DF3$vector_control <- ifelse(OCP_DF3$MDA_status == "Treatment naive", 0, OCP_DF3$vector_control) # update, if in treatment naive area = 0 vector control
# #OCP_DF3$MDA_nonCDTI <- ifelse(OCP_DF3$Pre_ESPEN_MDA_history == "Exclude", 0, OCP_DF3$MDA_nonCDTI) # update, if in treatment naive area = 0 non CDTI
# #BEN0036803224
# 
# # non-CDTI MDA (up to 1996/1997) #
# OCP_DF3$MDA_nonCDTI <- ifelse(OCP_DF3$ADMIN0 %in% "Benin" & OCP_DF3$PHASE %in% c("PHASE III EAST - JUL 1977","SOUTHERN EXTENSION - FEB 1988") & OCP_DF3$Year %in% c(1988:1996), 1,
#                               ifelse(OCP_DF3$ADMIN0 %in% "Burkina Faso" & OCP_DF3$PHASE %in% c("PHASE 1 - FEB 1975", "PHASE II - JAN 1976", "PHASE III EAST - JUL 1977") & OCP_DF3$Year %in% c(1990:1996), 1,
#                                      ifelse(OCP_DF3$ADMIN0 %in% "Cote d'Ivoire" & OCP_DF3$PHASE %in% c("PHASE 1 - FEB 1975", "PHASE III WEST - MAR 1977", "PHASE IV - MAR 1979") & OCP_DF3$Year %in% c(1988:1996), 1,
#                                           ifelse(OCP_DF3$ADMIN0 %in% "Cote d'Ivoire" & OCP_DF3$PHASE %in% c("SOUTHERN FOREST EXTENSION - JAN 1990") & OCP_DF3$Year %in% c(1990:1996), 1,
#                                             ifelse(OCP_DF3$ADMIN0 %in% "Ghana" & OCP_DF3$PHASE %in% c("PHASE 1 - FEB 1975", "PHASE II - JAN 1976", "PHASE III EAST - JUL 1977") & OCP_DF3$Year %in% c(1990:1996), 1,
#                                                    ifelse(OCP_DF3$ADMIN0 %in% "Ghana" & OCP_DF3$PHASE %in% "SOUTHERN EXTENSION - FEB 1988" & OCP_DF3$Year %in% c(1988:1996), 1,
#                                                       ifelse(OCP_DF3$ADMIN0 %in% "Ghana" & OCP_DF3$PHASE %in% "SOUTHERN FOREST EXTENSION - JAN 1990" & OCP_DF3$Year %in% c(1990:1996), 1,
#                                                           ifelse(OCP_DF3$ADMIN0 %in% "Togo" & OCP_DF3$ADMIN1 %in% c("Kara","Centrale", "Savanes") & OCP_DF3$PHASE %in% c("PHASE II - JAN 1976", "PHASE III EAST - JUL 1977") & OCP_DF3$Year %in% c(1991:1996), 1,
#                                                                  ifelse(OCP_DF3$ADMIN0 %in% "Togo" & OCP_DF3$PHASE %in% "SOUTHERN EXTENSION - FEB 1988" & OCP_DF3$Year %in% c(1991:1996), 1,
#                                                                         ifelse(OCP_DF3$ADMIN0 %in% "Guinea" & OCP_DF3$PHASE %in% "WESTERN EXTENSION - MAR 1989" & OCP_DF3$Year %in% c(1989:1996), 1,
#                                                                                ifelse(OCP_DF3$ADMIN0 %in% "Guinea" & OCP_DF3$PHASE %in% c("WESTERN EXTENSION - 1990", "SOUTHERN FOREST EXTENSION - JAN 1990") & OCP_DF3$Year %in% c(1990:1996), 1,
#                                                                                       ifelse(OCP_DF3$ADMIN0 %in% "Guinea-Bissau" & OCP_DF3$PHASE %in% "WESTERN EXTENSION - 1990" & OCP_DF3$Year %in% c(1990:1996), 1,
#                                                                                              ifelse(OCP_DF3$ADMIN0 %in% "Mali" & OCP_DF3$PHASE %in% c("PHASE 1 - FEB 1975", "PHASE III EAST - JUL 1977", "PHASE III WEST - MAR 1977") & OCP_DF3$Year %in% c(1988:1996), 1,
#                                                                                                     ifelse(OCP_DF3$ADMIN0 %in% "Mali" & OCP_DF3$PHASE %in% "WESTERN EXTENSION - MAR 1989" & OCP_DF3$Year %in% c(1989:1996), 1,
#                                                                                                            ifelse(OCP_DF3$ADMIN0 %in% "Mali" & OCP_DF3$PHASE %in% "WESTERN EXTENSION - 1990" & OCP_DF3$Year %in% c(1990:1996), 1,
#                                                                                                                   ifelse(OCP_DF3$ADMIN0 %in% "Niger" & OCP_DF3$PHASE %in% c("PHASE III EAST - JUL 1977", "NON-CONTROL") & OCP_DF3$Year %in% c(1990:1996), 1,
#                                                                                                                          ifelse(OCP_DF3$ADMIN0 %in% "Senegal" & OCP_DF3$PHASE %in% "WESTERN EXTENSION - 1990" & OCP_DF3$Year %in% c(1990:1996), 1,
#                                                                                                                                 NA)))))))))))))))))
# 
# OCP_DF3$MDA_nonCDTI <- ifelse(OCP_DF3$MDA_status == "Treatment naive", 0, OCP_DF3$MDA_nonCDTI) # update, if in treatment naive area = 0 non-CDTI
# #OCP_DF3$MDA_nonCDTI <- ifelse(OCP_DF3$Pre_ESPEN_MDA_history == "Exclude", 0, OCP_DF3$MDA_nonCDTI) # update, if in treatment naive area = 0 non CDTI
# 
# #BEN0036803224
# 
# # CDTI MDA (up to 2002) #
# OCP_DF3$MDA_CDTI <- ifelse(OCP_DF3$ADMIN0 %in% "Benin" & OCP_DF3$PHASE %in% c("PHASE III EAST - JUL 1977","SOUTHERN EXTENSION - FEB 1988") & OCP_DF3$Year %in% c(1997:2002), 1,
#                            ifelse(OCP_DF3$ADMIN0 %in% "Burkina Faso" & OCP_DF3$PHASE %in% c("PHASE 1 - FEB 1975", "PHASE II - JAN 1976", "PHASE III EAST - JUL 1977") & OCP_DF3$Year %in% c(1997:2002), 1,
#                                   ifelse(OCP_DF3$ADMIN0 %in% "Cote d'Ivoire" & OCP_DF3$PHASE %in% c("PHASE 1 - FEB 1975", "PHASE III WEST - MAR 1977", "PHASE IV - MAR 1979") & OCP_DF3$Year %in% c(1997:2000), 1,
#                                          ifelse(OCP_DF3$ADMIN0 %in% "Cote d'Ivoire" & OCP_DF3$PHASE %in% c("SOUTHERN FOREST EXTENSION - JAN 1990") & OCP_DF3$Year %in% c(1997:2002), 1,
#                                          ifelse(OCP_DF3$ADMIN0 %in% "Ghana" & OCP_DF3$PHASE %in% c("PHASE 1 - FEB 1975", "PHASE II - JAN 1976", "PHASE III EAST - JUL 1977") & OCP_DF3$Year %in% c(1997:2002), 1,
#                                                 ifelse(OCP_DF3$ADMIN0 %in% "Ghana" & OCP_DF3$PHASE %in% "SOUTHERN EXTENSION - FEB 1988" & OCP_DF3$Year %in% c(1997:2002), 1,
#                                                     ifelse(OCP_DF3$ADMIN0 %in% "Ghana" & OCP_DF3$PHASE %in% "SOUTHERN FOREST EXTENSION - JAN 1990" & OCP_DF3$Year %in% c(1997:2002), 1,
#                                                        ifelse(OCP_DF3$ADMIN0 %in% "Togo" & OCP_DF3$ADMIN1 %in% c("Kara","Centrale", "Savanes") & OCP_DF3$PHASE %in% c("PHASE II - JAN 1976", "PHASE III EAST - JUL 1977") & OCP_DF3$Year %in% c(1997:2002), 1,
#                                                               ifelse(OCP_DF3$ADMIN0 %in% "Togo" & OCP_DF3$PHASE %in% "SOUTHERN EXTENSION - FEB 1988" & OCP_DF3$Year %in% c(1997:2002), 1,
#                                                                      ifelse(OCP_DF3$ADMIN0 %in% "Guinea" & OCP_DF3$PHASE %in% "WESTERN EXTENSION - MAR 1989" & OCP_DF3$Year %in% c(1997:2002), 1,
#                                                                             ifelse(OCP_DF3$ADMIN0 %in% "Guinea" & OCP_DF3$PHASE %in% c("WESTERN EXTENSION - 1990", "SOUTHERN FOREST EXTENSION - JAN 1990") & OCP_DF3$Year %in% c(1997:2002), 1,
#                                                                                    ifelse(OCP_DF3$ADMIN0 %in% "Guinea-Bissau" & OCP_DF3$PHASE %in% "WESTERN EXTENSION - 1990" & OCP_DF3$Year %in% c(1997:2002), 1,
#                                                                                           ifelse(OCP_DF3$ADMIN0 %in% "Mali" & OCP_DF3$PHASE %in% c("PHASE 1 - FEB 1975", "PHASE III EAST - JUL 1977", "PHASE III WEST - MAR 1977") & OCP_DF3$Year %in% c(1997:2002), 1,
#                                                                                                  ifelse(OCP_DF3$ADMIN0 %in% "Mali" & OCP_DF3$PHASE %in% "WESTERN EXTENSION - MAR 1989" & OCP_DF3$Year %in% c(1997:2002), 1,
#                                                                                                         ifelse(OCP_DF3$ADMIN0 %in% "Mali" & OCP_DF3$PHASE %in% "WESTERN EXTENSION - 1990" & OCP_DF3$Year %in% c(1997:2002), 1,
#                                                                                                                ifelse(OCP_DF3$ADMIN0 %in% "Niger" & OCP_DF3$PHASE %in% c("PHASE III EAST - JUL 1977", "NON-CONTROL") & OCP_DF3$Year %in% c(1997:2002), 1,
#                                                                                                                       ifelse(OCP_DF3$ADMIN0 %in% "Senegal" & OCP_DF3$PHASE %in% "WESTERN EXTENSION - 1990" & OCP_DF3$Year %in% c(1997:2002), 1,
#                                                                                                                              NA)))))))))))))))))
# 
# OCP_DF3$MDA_CDTI <- ifelse(OCP_DF3$MDA_status == "Treatment naive", 0, OCP_DF3$MDA_CDTI) # update, if in treatment naive area = 0 non CDTI
# #OCP_DF3$MDA_CDTI <- ifelse(OCP_DF3$Pre_ESPEN_MDA_history == "Exclude", 0, OCP_DF3$MDA_CDTI) # update, if in treatment naive area = 0 non CDTI
# 
# #BEN0036803224
# #OCP_DF3_Togo <- subset(OCP_DF3, OCP_DF3$ADMIN1 == "Savanes") # check Savanes in Togo


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

# call function:
dfOCP_included4 <- apply_OCP_interventions_up_to_2002(dfOCP_included3)



# # 2003 - 2013 Period SIZs (2003 - 2013) and non-SIZs (2003 - 2013) & Biannual (up to 2022) #
# 
# # ========================================== #
# # vector control (2003 - 2012) ONLY IN SIZs
# # OCP_DF3$vector_control <- ifelse(OCP_DF3$ADMIN0 %in% "Benin" & OCP_DF3$SIZ_label == "SIZ" & OCP_DF3$PHASE %in% c("PHASE III EAST - JUL 1977","SOUTHERN EXTENSION - FEB 1988") & OCP_DF3$Year %in% c(2003:2007), 1,
# #                                  ifelse(OCP_DF3$ADMIN0 %in% "Ghana" & OCP_DF3$SIZ_label == "SIZ" & OCP_DF3$PHASE %in% "PHASE III EAST - JUL 1977" & OCP_DF3$Year %in% c(2003:2007), 1,
# #                                         ifelse(OCP_DF3$ADMIN0 %in% "Togo" & OCP_DF3$SIZ_label == "SIZ" & OCP_DF3$ADMIN1 %in% c("Kara","Centrale") & OCP_DF3$PHASE %in% c("PHASE II - JAN 1976", "PHASE III EAST - JUL 1977") & OCP_DF3$Year %in% c(2003:2007), 1,
# #                                                OCP_DF3$vector_control)))
# 
# OCP_DF3$vector_control <- ifelse(OCP_DF3$ADMIN0 %in% "Benin" & OCP_DF3$SIZ_label == "SIZ" & OCP_DF3$PHASE %in% c("PHASE III EAST - JUL 1977","SOUTHERN EXTENSION - FEB 1988") & OCP_DF3$Year %in% c(2003:2007), 1,
#                                  ifelse(OCP_DF3$ADMIN0 %in% "Ghana" & OCP_DF3$SIZ_label == "SIZ" & OCP_DF3$PHASE %in% "PHASE III EAST - JUL 1977" & OCP_DF3$Year %in% c(2003:2007), 1,
#                                         ifelse(OCP_DF3$ADMIN0 %in% "Togo" & OCP_DF3$SIZ_label == "SIZ" & OCP_DF3$ADMIN1 %in% c("Kara","Centrale") & OCP_DF3$Year %in% c(2003:2007), 1,
#                                                OCP_DF3$vector_control))) # only Oti in Ghana (phase III sliver in the north-east of country) not Pru (small area in central-west) basins
# 
# 
# OCP_DF3$vector_control <- ifelse(OCP_DF3$MDA_status == "Treatment naive", 0, OCP_DF3$vector_control) # update, if in treatment naive area = 0 vector control
# #BEN0036803224
# 
# # annual CDTI (2003 - 2012) in non-SIZs
# OCP_DF3$MDA_CDTI <- ifelse(OCP_DF3$ADMIN0 %in% "Benin" & OCP_DF3$PHASE %in% c("PHASE III EAST - JUL 1977","SOUTHERN EXTENSION - FEB 1988") & OCP_DF3$Year %in% c(2003:2013), 1,
#                            ifelse(OCP_DF3$ADMIN0 %in% "Burkina Faso" & OCP_DF3$PHASE %in% c("PHASE 1 - FEB 1975", "PHASE II - JAN 1976", "PHASE III EAST - JUL 1977") & OCP_DF3$Year %in% c(2003:2013), 1,
#                                   ifelse(OCP_DF3$ADMIN0 %in% "Cote d'Ivoire" & OCP_DF3$PHASE %in% c("PHASE 1 - FEB 1975", "PHASE III WEST - MAR 1977", "PHASE IV - MAR 1979", "SOUTHERN FOREST EXTENSION - JAN 1990") & OCP_DF3$Year %in% c(2008:2013), 1,
#                                          ifelse(OCP_DF3$ADMIN0 %in% "Ghana" & OCP_DF3$PHASE %in% c("PHASE 1 - FEB 1975", "PHASE II - JAN 1976", "PHASE III EAST - JUL 1977", "SOUTHERN EXTENSION - FEB 1988", "SOUTHERN FOREST EXTENSION - JAN 1990") & OCP_DF3$Year %in% c(2003:2013), 1,
#                                                 ifelse(OCP_DF3$ADMIN0 %in% "Togo" & OCP_DF3$ADMIN1 %in% c("Kara","Centrale", "Savanes") & OCP_DF3$PHASE %in% c("PHASE II - JAN 1976", "PHASE III EAST - JUL 1977") & OCP_DF3$Year %in% c(2003:2013), 1,
#                                                        ifelse(OCP_DF3$ADMIN0 %in% "Togo" & OCP_DF3$PHASE %in% "SOUTHERN EXTENSION - FEB 1988" & OCP_DF3$Year %in% c(2003:2013), 1,
#                                                               ifelse(OCP_DF3$ADMIN0 %in% "Guinea" & OCP_DF3$PHASE %in% c("WESTERN EXTENSION - MAR 1989", "WESTERN EXTENSION - 1990", "SOUTHERN FOREST EXTENSION - JAN 1990") & OCP_DF3$Year %in% c(2003:2013), 1,
#                                                                      ifelse(OCP_DF3$ADMIN0 %in% "Guinea-Bissau" & OCP_DF3$PHASE %in% "WESTERN EXTENSION - 1990" & OCP_DF3$Year %in% c(2003:2013), 1,
#                                                                             ifelse(OCP_DF3$ADMIN0 %in% "Mali" & OCP_DF3$PHASE %in% c("PHASE 1 - FEB 1975", "PHASE III EAST - JUL 1977", "WESTERN EXTENSION - MAR 1989", "WESTERN EXTENSION - 1990", "PHASE III WEST - MAR 1977") & OCP_DF3$Year %in% c(2003:2013), 1,
#                                                                                    ifelse(OCP_DF3$ADMIN0 %in% "Niger" & OCP_DF3$PHASE %in% c("PHASE III EAST - JUL 1977", "NON-CONTROL") & OCP_DF3$Year %in% c(2003:2013), 1,
#                                                                                           ifelse(OCP_DF3$ADMIN0 %in% "Senegal" & OCP_DF3$PHASE %in% "WESTERN EXTENSION - 1990" & OCP_DF3$Year %in% c(2003:2013), 1,
#                                                                                                  ifelse(OCP_DF3$ADMIN0 %in% "Sierra Leone" & OCP_DF3$PHASE %in% "WESTERN EXTENSION - MAR 1990" & OCP_DF3$Year %in% c(2003:2013), 1,
#                                                                                                         OCP_DF3$MDA_CDTI))))))))))))
# OCP_DF3$MDA_CDTI <- ifelse(OCP_DF3$MDA_status == "Treatment naive", 0, OCP_DF3$MDA_CDTI) # update, if in treatment naive area = 0 non CDTI
# #OCP_DF3$MDA_CDTI <- ifelse(OCP_DF3$Pre_ESPEN_MDA_history == "Exclude", 0, OCP_DF3$MDA_CDTI) # update, if in treatment naive area = 0 non CDTI
# 
# #BEN0036803224
# 
# # biannual CDTI
# OCP_DF3$MDA_CDTI_Biannual <- ifelse(OCP_DF3$ADMIN0 %in% "Benin" & OCP_DF3$SIZ_label == "SIZ" & OCP_DF3$PHASE %in% c("PHASE III EAST - JUL 1977","SOUTHERN EXTENSION - FEB 1988") & OCP_DF3$Year %in% c(2003:2013), 1,
#                                     ifelse(OCP_DF3$ADMIN0 %in% "Ghana" & OCP_DF3$SIZ_label == "SIZ" & OCP_DF3$PHASE %in% "PHASE III EAST - JUL 1977" & OCP_DF3$Year %in% c(2003:2013), 1,
#                                            ifelse(OCP_DF3$ADMIN0 %in% "Ghana" & OCP_DF3$endemicity_baseline %in% c("mesoendemic", "hyperendemic") & OCP_DF3$Year %in% c(2009:2022), 1,
#                                             ifelse(OCP_DF3$ADMIN0 %in% "Guinea" & OCP_DF3$SIZ_label == "SIZ" & OCP_DF3$PHASE %in% c("WESTERN EXTENSION - MAR 1989", "WESTERN EXTENSION - 1990") & OCP_DF3$Year %in% c(2003:2013), 1,
#                                                   ifelse(OCP_DF3$ADMIN0 %in% "Togo" & OCP_DF3$IUs_NAME_MAPPING %in% c("Anie", "OGOU", "AMOU", "HAHO") & OCP_DF3$Year %in% c(2014:2022), 1,
#                                                          ifelse(OCP_DF3$ADMIN0 %in% "Togo" & OCP_DF3$IUs_NAME_MAPPING %in% c("Kpendjal", "Kpendjal-Ouest", "OTI", "OtI-Sud",
#                                                                                                                              "ASSOLI", "BASSAR", "BINAH", "DANKPEN", "DOUFELGOU", "KERAN", "KOZAH",
#                                                                                                                              "TCHAOUDJO", "MO", "SOTOUBOUA") & OCP_DF3$Year %in% c(2003:2022), 1, NA))))))
# # Ghana needs biannual - all MESO and HYPER beyond 2009! (OCP from 2003 - )
# 
# OCP_DF3$MDA_CDTI_Biannual <- ifelse(OCP_DF3$MDA_status == "Treatment naive", 0, OCP_DF3$MDA_CDTI_Biannual) # update, if in treatment naive area = 0 non CDTI
# 
# #BEN0036803224

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
                                                                                              NA)))))))))))

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

# call function :
dfOCP_included4 <- apply_OCP_interventions_SIZ_2003_2012_biannual(dfOCP_included4)


# # need to remove vector control, annual non-CDTI and CDTI in those IUs where pre_ESPEN = EXCLUDE (expect Ghana meso and hyper endemic IUs)
# OCP_DF3$MDA_nonCDTI <- ifelse(OCP_DF3$Pre_ESPEN_MDA_history == "Exclude" & OCP_DF3$Year < 2014 & OCP_DF3$ADMIN0 != "Ghana", 0, OCP_DF3$MDA_nonCDTI) # update, if in treatment naive area = 0 non CDTI
# OCP_DF3$MDA_nonCDTI <- ifelse(OCP_DF3$Pre_ESPEN_MDA_history == "Exclude" & OCP_DF3$Year < 2014 & OCP_DF3$ADMIN0 == "Ghana" & OCP_DF3$endemicity_baseline == "hypoendemic", 0, OCP_DF3$MDA_nonCDTI) # update, if in treatment naive area = 0 non CDTI
# 
# OCP_DF3$MDA_CDTI <- ifelse(OCP_DF3$Pre_ESPEN_MDA_history == "Exclude" & OCP_DF3$Year < 2014 & OCP_DF3$ADMIN0 != "Ghana", 0, OCP_DF3$MDA_CDTI) # update, if in treatment naive area = 0 non CDTI
# OCP_DF3$MDA_CDTI <- ifelse(OCP_DF3$Pre_ESPEN_MDA_history == "Exclude" & OCP_DF3$Year < 2014 & OCP_DF3$ADMIN0 == "Ghana" & OCP_DF3$endemicity_baseline == "hypoendemic", 0, OCP_DF3$MDA_CDTI) # update, if in treatment naive area = 0 non CDTI
# 
# OCP_DF3$vector_control <- ifelse(OCP_DF3$Pre_ESPEN_MDA_history == "Exclude" & OCP_DF3$Year < 2014 & OCP_DF3$ADMIN0 != "Ghana", 0, OCP_DF3$vector_control) # update, if in treatment naive area = 0 non CDTI
# OCP_DF3$vector_control <- ifelse(OCP_DF3$Pre_ESPEN_MDA_history == "Exclude" & OCP_DF3$Year < 2014 & OCP_DF3$ADMIN0 == "Ghana" & OCP_DF3$endemicity_baseline == "hypoendemic", 0, OCP_DF3$vector_control) # update, if in treatment naive area = 0 non CDTI
# 
# # NOTES:
# # Togo bi-annual in non-SIZ Plateau region : Anie, OGOU, AMOU, HAHO (2014 - 2023)
# # Togo biannual in Savanes : Kpendjal, Kpendjal-Ouest, OTI, OtI-Sud (2003 - 2023)
# # Togo biannual in Kara: all of Kara; ASSOLI, BASSAR, BINAH, DANKPEN, DOUFELGOU, KERAN, KOZAH  (2003-2023)
# # Togo biannual in Centrale: TCHAOUDJO, MO, SOTOUBOUA (2003 - 2023)
# 
# # OCP_DF3_Ghana <- subset(OCP_DF3, OCP_DF3$ADMIN0 == "Ghana") # check biannual in Togo
# #GHA0216421474 (meso) # want to keep pre-ESPEN (even though "Exclude") b/c mesoendemic
# #GHA0216421476 (hypo) #remove preESPEN because "exclude & hypoendemic
# 
# # OCP_DF3_Togo <- subset(OCP_DF3, OCP_DF3$ADMIN0 == "Togo") # check biannual in Togo
# 
# #OCP_DF3_check <- subset(OCP_DF3, Pre_ESPEN_MDA_history == "Exclude" & ESPEN_MDA_history == "Include")
# #OCP_DF3_check <- subset(OCP_DF3, Pre_ESPEN_MDA_history == "Include" & ESPEN_MDA_history == "Include")
# 
# # Filling in MDA presence based on ESPEN data (2013 - 2022)  #
# OCP_DF3$MDA_CDTI <- ifelse(OCP_DF3$EpiCov > 0 & OCP_DF3$Year %in% c(2014:2022), 1, OCP_DF3$MDA_CDTI)
# 
# # need to remove 1 from biannual where no annual CDTI between 2014-2022
# OCP_DF3$MDA_CDTI_Biannual <- ifelse(OCP_DF3$MDA_CDTI == 0 | is.na(OCP_DF3$MDA_CDTI) & OCP_DF3$Year %in% c(2014:2022), 0, OCP_DF3$MDA_CDTI_Biannual)
# 
# #OCP_DF3_Ghana <- subset(OCP_DF3, OCP_DF3$ADMIN0 == "Ghana") # check biannual in Togo
# #OCP_DF3$Year_temp <- OCP_DF3$Year


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

# Call function:
dfOCP_included4 <- update_MDA_including_ESPENyrs(dfOCP_included4)

# # =================================================================================================== #
# #                                   4) extend by 3 years (2023- 2025)                                 #
# 
# dfOCP_included4 <- OCP_DF3
# dfOCP_included4_lastyr <- subset(dfOCP_included4, Year == 2022)
# nrow(dfOCP_included4_lastyr) # 670 IUs
# 
# #FinalESPENyr <- 2013
# FinalESPENyr <- 2022
# 
# dfls <- split(dfOCP_included4, dfOCP_included4$IU_ID_MAPPING)
# 
# ## new list of dfs
# newdf <- vector("list", length(dfls))
# 
# lsid <- split(dfOCP_included4$IU_ID_MAPPING, dfOCP_included4$IU_ID_MAPPING)
# lsid <- unique(unlist(lsid))
# 
# # loop to fill in missing years and columns
# 
# #loop_func <- function(dfls, dfAPOC_included5, FinalESPENyr, newdf, lsid) {
# for (i in 1:length(dfls)) {
# 
#   newdf[[i]] <- subset(dfls[[i]], select=colnames(dfOCP_included4))
# 
#   #IUStartMDA <- max(min(newdf[[i]]$Year) - min(newdf[[i]]$Cum_MDA) - start)
# 
#   IUend <- 2025
# 
#   newdf[[i]]$IUend <- IUend
#   Futureyrs <- IUend - FinalESPENyr ## prior years to include
# 
#   #PriorMDA <- FinalESPENyr - IUStartMDA ## prior MDA rounds
# 
#   newdf[[i]]$lsID <- i
# 
#   i
# 
#   if (is.na(IUend)!=T) {
#     if(IUend>FinalESPENyr) {
#       # if cumulative rounds greater than 1, augment data frame
#       # by repeating the first row
# 
#       tmp <- newdf[[i]][rep(1, each = (min(IUend - newdf[[i]]$Year))), ]
# 
#       # fill in the years from start date of IU to first year on ESPEN
#       #tmp$Year <- seq(IUstart, min(newdf[[i]]$Year)-1)
#       tmp$Year <- seq(FinalESPENyr+1, IUend)
# 
#     }
# 
#     # #enedmicity category given "Augmented Status"
#     tmp$Endemicity <- "Augmented"
#     tmp$MDA_scheme <- "Augmented"
#     # #tmp$Endemicity_ID <- 6
# 
#     newdf[[i]] <- rbind(newdf[[i]], tmp) # want tmp at end
#   }
# }
# 
# #}
# 
# 
# #test <- loop_func(dfls = dfls, dfAPOC_included = dfAPOC_included5, FinalESPENyr = FinalESPENyr, newdf = newdf, lsid = lsid)
# 
# newdf <- do.call(rbind, newdf)
# 
# dfOCP_included5 <- newdf
# 
# dfOCP_included5$Year_tmp <- dfOCP_included5$Year
# 
# dfOCP_included5$Endemicity_tmp <- dfOCP_included5$Endemicity
# 
# dfOCP_included5_lastyr <- subset(dfOCP_included5, Year == 2022)
# nrow(dfOCP_included5_lastyr) # 670 IUs
# 
# #dfOCP_included5_check <- subset(dfOCP_included5, dfOCP_included5$ADMIN0 == "Ghana") # check biannual in Togo

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

# call function:
dfOCP_included5 <- extend_IU_years_2023_2025(dfOCP_included4, FinalESPENyr = 2022)


# # =================================================================================================== #
# #                        Add in Coverage & number of rounds values                                    #
# 
# dfOCP_included5$MDA_CDTI_raw <- NA
# 
# dfOCP_included5$Cov.in2 <- ifelse(dfOCP_included5$MDA_nonCDTI == 1 & dfOCP_included5$Year %in% c(1988:2013), 0.52, NA)
# dfOCP_included5$cov_source <- ifelse(dfOCP_included5$MDA_nonCDTI == 1 & dfOCP_included5$Year %in% c(1988:2013), "assumed", NA)
# dfOCP_included5$cov_specific_source <- ifelse(dfOCP_included5$MDA_nonCDTI == 1 & dfOCP_included5$Year %in% c(1988:2013), "assumed (non-CDTI coverage)", NA)
# 
# dfOCP_included5$Cov.in2 <- ifelse(dfOCP_included5$MDA_CDTI == 1 & dfOCP_included5$Year %in% c(1997:2013), 0.65, dfOCP_included5$Cov.in2)
# dfOCP_included5$cov_source <- ifelse(dfOCP_included5$MDA_CDTI == 1 & dfOCP_included5$Year %in% c(1997:2013), "assumed", dfOCP_included5$cov_source)
# dfOCP_included5$cov_specific_source <- ifelse(dfOCP_included5$MDA_CDTI == 1 & dfOCP_included5$Year %in% c(1997:2013), "assumed (CDTI coverage)", dfOCP_included5$cov_specific_source)
# 
# dfOCP_included5$Cov.in2 <- ifelse(dfOCP_included5$EpiCov >= 65 & dfOCP_included5$Year %in% c(2014:2022), 0.65, dfOCP_included5$Cov.in2)
# dfOCP_included5$cov_source <- ifelse(dfOCP_included5$EpiCov >= 65 & dfOCP_included5$Year %in% c(2014:2022), "IU-level coverage", dfOCP_included5$cov_source)
# dfOCP_included5$cov_specific_source <- ifelse(dfOCP_included5$EpiCov >= 65 & dfOCP_included5$Year %in% c(2014:2022), "ESPEN", dfOCP_included5$cov_specific_source)
# 
# dfOCP_included5$Cov.in2 <- ifelse(dfOCP_included5$EpiCov < 65 & dfOCP_included5$Year %in% c(2014:2022), 0.25, dfOCP_included5$Cov.in2)
# dfOCP_included5$cov_source <- ifelse(dfOCP_included5$EpiCov < 65 & dfOCP_included5$Year %in% c(2014:2022), "IU-level coverage", dfOCP_included5$cov_source)
# dfOCP_included5$cov_specific_source <- ifelse(dfOCP_included5$EpiCov < 65 & dfOCP_included5$Year %in% c(2014:2022), "ESPEN", dfOCP_included5$cov_specific_source)
# 
# dfOCP_included5$Cov.in2 <- ifelse(dfOCP_included5$EpiCov == 0 & dfOCP_included5$Year %in% c(2014:2022), NA, dfOCP_included5$Cov.in2)
# 
# # finally specify number of rounds
# dfOCP_included5$number_rnds <- rowSums(dfOCP_included5[c("MDA_nonCDTI", "MDA_CDTI", "MDA_CDTI_Biannual")], na.rm = TRUE)
# 
# #dfOCP_included5_check <- subset(dfOCP_included5, dfOCP_included5$ADMIN0 == "Ghana") # check biannual in Togo
# 
# dfOCP_included5$vector_control <- ifelse(dfOCP_included5$Year %in% c(2023:2025) & dfOCP_included5$ADMIN0 != "Uganda", 0, dfOCP_included5$vector_control) # remove errenous VC == 1 in 2023 -
# # CIV0163615400 – vector control from 2022? should be 0
# 
# check_df <- subset(dfOCP_included5, Year == 2022)
# nrow(check_df) # 670 IUs

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

# call function:
dfOCP_included5 <- create_edit_intervention_cols(dfOCP_included5)


# # =================================================================================================== #
# # 5) label each IU on forward status: MDA stopped (post intervention surveillance in 2022 or 2021     #
# #    or all EpiCov == 0 for "unknowns" / MDA continue/ treatment naive
# 
# final_endemic_PIS_yr_check <- dfOCP_included5 %>%
#   filter(Endemicity == "Endemic (under post-intervention surveillance)") %>%
#   group_by(IU_ID_MAPPING) %>%
#   summarise(LastYear = max(Year))
# 
# dfOCP_included6 <- dfOCP_included5 # reset here
# length(unique(dfOCP_included6$IU_ID_MAPPING))
# 
# dfOCP_included6 <- dfOCP_included6 %>%
#   group_by(IU_ID_MAPPING) %>%
#   mutate(
#     trt_status_2022 = case_when(
#       all(
#         any(Endemicity %in% c("Endemic (MDA not delivered)", "Endemic (under MDA)") & Year %in% c(2021, 2022) & !all(is.na(Cov.in2)))
#         #& !any(Endemicity == "Endemic (under post-intervention surveillance)" & Year %in% c(2021, 2022) & !all(is.na(Cov.in2)))
#       ) ~ "MDA continues",
#       all(
#         any(MAX_Endemicity %in% c("Unknown (under LF MDA)", "Unknown (consider Oncho Elimination Mapping)", "Not reported", "Non-endemic")) &
#           any(EpiCov > 0) &
#           any(Cum_MDA > 0)
#       ) ~ "MDA continues",
#       any(Endemicity == "Unknown (under LF MDA)" & Year %in% c(2021:2022)) & any(Cov.in2 > 0 & Year %in% c(2013:2022)) ~ "MDA stopped",
#       all(any(MAX_Endemicity %in% c("Unknown (under LF MDA)", "Unknown (consider Oncho Elimination Mapping)", "Not reported")) &
#             all(EpiCov == 0) &
#             any(Cum_MDA > 0) &
#             !all(is.na(Cov.in2))
#       ) ~ "MDA stopped",
#       any(Endemicity == "Endemic (under post-intervention surveillance)" & Year %in% c(2021, 2022) & !all(is.na(Cov.in2))) ~ "MDA stopped",
#       any(Endemicity == "Non-endemic" & Year %in% c(2021, 2022) & !all(is.na(Cov.in2))) ~ "MDA stopped",
#       all(Endemicity %in% c("Non-endemic", "Not reported")) & length(unique(Endemicity)) == 2 ~ "MDA stopped", # check
#       #all(ADMIN0ISO3 == "SDN" & !all(is.na(Cov.in2))) ~ "MDA stopped",
#       all(is.na(Cov.in2)) ~ "Treatment naive",
#       TRUE ~ NA
#     )
#   )
# 
# dfOCP_included6$Endemicity_tmp <- dfOCP_included6$Endemicity
# 
# dfOCP_included6_lastyr <- subset(dfOCP_included6, Year == 2022)
# nrow(dfOCP_included6_lastyr) # 670 IUs
# 
# # =================== #
# # NER code from below #
# 
# dfOCP_included6_lastyr <- subset(dfOCP_included6, Year == 2022)
# merged_OCP <- ESPEN_IUs %>%
#   left_join(dfOCP_included6_lastyr, by = c("IU_ID" = "IU_ID_MAPPING"))
# merged_NER <- subset(merged_OCP, ADMIN0ISO3.x == "NER" & trt_status_2022 == "MDA stopped") # 14 IUs
# NER_all_vec <- unique(merged_NER$IUs_NAME_MAPPING)
# #NER_to_include <- c("Gotheye", "Torodi", "Niamey 1", "Kollo", "Say", "Falmey", "Niamey 2", "Niamey 3", "Niamey 4", "Niamey 5")
# # updated March 2025 - include more NER Ius
# NER_to_include <- c("Gotheye", "Torodi", "Niamey 1", "Kollo", "Say", "Falmey", "Niamey 2", "Niamey 3", "Niamey 4", "Niamey 5",
#                     "Boboye", "Gaya", "Dioundiou", "Tera", "Bankilare")
# # check difference - i.e., those 5 which will be removed
# difference <- setdiff(NER_all_vec, NER_to_include)
# print(difference)
# subset_NER_IUstoremove <- subset(merged_NER, IUs_NAME_MAPPING %in% difference)
# 
# merged_NER <- subset(merged_NER, IUs_NAME_MAPPING %in% NER_to_include)
# pattern <- paste(NER_to_include, collapse="|")
# NER_remaining_vec <- gsub(pattern, "", NER_all_vec)
# 
# # continue here - prune Niger so only border IUs with Nurkina Faso + benin included (with code above isolating those IUs)
# dfOCP_included6 <- subset(dfOCP_included6, !IUs_NAME_MAPPING %in% NER_remaining_vec ) # need to get NER_remaining_vec from below!
# 
# dfOCP_included6$Cum_MDA_tmp <- dfOCP_included6$Cum_MDA
# 
# dfOCP_included6_lastyr <- subset(dfOCP_included6, Year == 2022)
# nrow(dfOCP_included6_lastyr) # 665 IUs (5 removed from NER)
#                             #  670 - none now removed from NER in this step (April 25')
# 
# # testing/ checking labelling over trt_status_2022
# dfOCP_included6_check_MDAstopped <- subset(dfOCP_included6, trt_status_2022 == "MDA stopped")
# length(unique(dfOCP_included6_check_MDAstopped$IU_ID_MAPPING))
# 
# dfOCP_included6_check_MDAcont <- subset(dfOCP_included6, trt_status_2022 == "MDA continues")
# length(unique(dfOCP_included6_check_MDAcont$IU_ID_MAPPING))
# 
# dfOCP_included6_check_trtnaive <- subset(dfOCP_included6, trt_status_2022 == "Treatment naive")
# length(unique(dfOCP_included6_check_trtnaive$IU_ID_MAPPING)) # 57 IUs
# 
# # check those where Cov.in2 is 0 for all years pre 2014
# filtered_df <- dfOCP_included6 %>% filter(Year < 2014) # Filter rows where Year is pre-2014
# unique_subset <- filtered_df %>%
#   group_by(IU_ID_MAPPING) %>%
#   summarise(Cov.in2_sum = sum(Cov.in2, na.rm = TRUE)) %>%
#   filter(Cov.in2_sum == 0) %>%
#   select(IU_ID_MAPPING) # Group by IU_ID_Mapping and check if the sum of Cov.in2 for each group is 0
# 
# result <- unique_subset$IU_ID_MAPPING # Now, unique_subset contains the unique IU_ID_Mapping where Cov.in2 is 0 for all years pre-2014
# dfOCP_included6_check_pre2014_Cov0 <- subset(dfOCP_included6, IU_ID_MAPPING %in% result)
# 
# # added July 2024 #
# 
# dfOCP_included6 <- dfOCP_included6 %>%
#   group_by(IU_ID_MAPPING) %>%
#   mutate(
#     trt_status_2022_v2 = case_when(
#       all(
#         any(Endemicity %in% c("Endemic (MDA not delivered)", "Endemic (under MDA)") & Year %in% c(2021, 2022) & !all(is.na(Cov.in2)))
#         #& !any(Endemicity == "Endemic (under post-intervention surveillance)" & Year %in% c(2021, 2022) & !all(is.na(Cov.in2)))
#       ) ~ "MDA continues",
#       all(
#         any(MAX_Endemicity %in% c("Unknown (under LF MDA)", "Unknown (consider Oncho Elimination Mapping)", "Not reported")) &
#           any(EpiCov > 0) &
#           any(Cum_MDA > 0)
#       ) ~ "MDA continues",
#       any(Endemicity == "Unknown (under LF MDA)" & Year %in% c(2021:2022)) & any(Cov.in2 > 0 & Year %in% c(2013:2022)) ~ "MDA stopped: no MDA in 2021/2022 ESPEN",
#       all(any(MAX_Endemicity %in% c("Unknown (under LF MDA)", "Unknown (consider Oncho Elimination Mapping)", "Not reported")) &
#             all(EpiCov == 0) &
#             any(Cum_MDA > 0) &
#             !all(is.na(Cov.in2))
#       ) ~ "MDA stopped: no MDA in ESPEN years",
#       any(Endemicity == "Endemic (under post-intervention surveillance)" & Year %in% c(2021, 2022) & !all(is.na(Cov.in2))) ~ "MDA stopped: under PIS (ESPEN)",
#       any(Endemicity == "Non-endemic" & Year %in% c(2021, 2022) & !all(is.na(Cov.in2))) ~ "MDA stopped: non-endemic in 2021/2022 ESPEN",
#       all(Endemicity %in% c("Non-endemic", "Not reported")) & length(unique(Endemicity)) == 2 ~ "MDA stopped: non/endemic & not reported in ESPEN", # check
#       all(ADMIN0ISO3 == "SDN" & !all(is.na(Cov.in2)) & IUs_NAME_MAPPING != "El Radoom") ~ "MDA stopped: under PIS (other sources) or eliminated",
#       all(ADMIN0ISO3 == "SDN" & IUs_NAME_MAPPING == "El Radoom") ~ "MDA continues: remove SDN",
#       all(ADMIN0ISO3 == "SEN") ~ "MDA stopped: under PIS (other sources) or eliminated",
#       all(is.na(Cov.in2)) ~ "Treatment naive",
#       TRUE ~ trt_status_2022
#     )
#   )
# 
# unique(dfOCP_included6$trt_status_2022_v2)
# 
# dfOCP_included6_lastyr <- subset(dfOCP_included6, Year == 2022)
# nrow(dfOCP_included6_lastyr) # 665 IUs
#                              # 670 IUs (April 25') - none now removed from NER at this stage


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

# call function:
dfOCP_included6 <- create_trt_status_variable(dfOCP_included5)


# # ==================================================== #
# # MANUALLY PRUNE NIGER (no longer needed- May 2025)    #
# # ==================================================== #
# 
# dfOCP_included6_NER <- subset(dfOCP_included6, ADMIN0ISO3 == "NER")
# NER_IUs_vec <- unique(dfOCP_included6_NER$IU_ID_MAPPING)
# 
# ESPEN_IUs_NER <- ESPEN_IUs[which((ESPEN_IUs$ADMIN0ISO3 == "NER")),]
# st_geometry_type(ESPEN_IUs_NER )
# st_crs(ESPEN_IUs_NER )
# 
# ESPEN_IUs_NER$included_OCP <- ifelse(ESPEN_IUs_NER$IU_ID %in% NER_IUs_vec, "included", NA)
# 
# dfOCP_included6_lastyr <- subset(dfOCP_included6, Year == 2022)
# merged_OCP <- ESPEN_IUs %>%
#   left_join(dfOCP_included6_lastyr, by = c("IU_ID" = "IU_ID_MAPPING"))
# 
# 
# merged_NER <- subset(merged_OCP, ADMIN0ISO3.x == "NER" & trt_status_2022 == "MDA stopped")
# NER_all_vec <- unique(merged_NER$IUs_NAME_MAPPING)
# 
# # Set the file path to the shapefile
# shapefile_path <- "C:/Users/mad206/OneDrive/Endgame/OCP mapping/African countries/Africa_Boundaries.shp"
# # Read the shapefile
# african_countries <- st_read(dsn = shapefile_path)
# # View the attributes and structure of the shapefile
# summary(african_countries)
# 
# 
# # merged_NER <- merged_APOC[which((merged_APOC$ADMIN0ISO3.x == "NER")),]
# # merged_NER2 <- merged_NER[which((merged_NER$included2 %in% c("included", "included as\ntreatment naive/hypoendemic or impact from LF MDA/hypoendemic"))),]
# 
# cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# 
# ggplot() +
#   geom_sf(data = merged_OCP, aes(fill = trt_status_2022), colour = NA, alpha = 0.7) +
#   geom_sf(data = ESPEN_IUs_NER, aes(fill = included_OCP), colour = "grey", size = 1, fill = NA, alpha = 0.1) +
#   geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.3) +
#   geom_sf_text(data = merged_NER, aes(label = IUs_NAME_MAPPING), size = 2.5, color = "red")+
#   coord_sf(xlim = c(0, 4), ylim = c(11.5, 15.2)) +
#   #coord_sf(xlim = c(38, 44), ylim = c(4, 10)) +
#   #coord_sf(xlim = c(38, 44), ylim = c(8, 10)) +
#   #coord_sf(xlim = c(41, 43), ylim = c(8, 10)) +
#   #coord_sf() +
#   theme_bw() +
#   scale_fill_manual(values = cbPalette, na.value = "gray") +
#   scale_colour_manual(na.value="gray")+
#   labs(fill='') +
#   theme(
#     legend.position = "bottom",  # Place the legend at the bottom
#     legend.direction = "horizontal")
# 
# # # older pre March 2025
# # NER_to_include <- c("Gotheye", "Torodi", "Niamey 1", "Kollo", "Say", "Falmey", "Niamey 2", "Niamey 3", "Niamey 4", "Niamey 5")
# 
# # updated March 2025 - include more NER Ius
# NER_to_include <- c("Gotheye", "Torodi", "Niamey 1", "Kollo", "Say", "Falmey", "Niamey 2", "Niamey 3", "Niamey 4", "Niamey 5",
#                     "Boboye", "Gaya", "Dioundiou", "Tera", "Bankilare")
# 
# merged_NER <- subset(merged_NER, IUs_NAME_MAPPING %in% NER_to_include)
# 
# ggplot() +
#   geom_sf(data = merged_NER, aes(fill = trt_status_2022), colour = NA, alpha = 0.7) +
#   geom_sf(data = ESPEN_IUs_NER, aes(fill = included_OCP), colour = "grey", size = 1, fill = NA, alpha = 0.1) +
#   geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.3) +
#   geom_sf_text(data = merged_NER, aes(label = IUs_NAME_MAPPING), size = 2.5, color = "red")+
#   coord_sf(xlim = c(0, 4), ylim = c(11.5, 15.2)) +
#   #coord_sf(xlim = c(38, 44), ylim = c(4, 10)) +
#   #coord_sf(xlim = c(38, 44), ylim = c(8, 10)) +
#   #coord_sf(xlim = c(41, 43), ylim = c(8, 10)) +
#   #coord_sf() +
#   theme_bw() +
#   scale_fill_manual(values = cbPalette, na.value = "gray") +
#   scale_colour_manual(na.value="gray")+
#   labs(fill='') +
#   theme(
#     legend.position = "bottom",  # Place the legend at the bottom
#     legend.direction = "horizontal")
# 
# pattern <- paste(NER_to_include, collapse="|")
# NER_remaining_vec <- gsub(pattern, "", NER_all_vec)
# 
# merged_NER <- subset(merged_NER, !IUs_NAME_MAPPING %in% NER_remaining_vec )
# 
# ggplot() +
#   geom_sf(data = merged_NER, aes(fill = trt_status_2022), colour = NA, alpha = 0.7) +
#   geom_sf(data = ESPEN_IUs_NER, aes(fill = included_OCP), colour = "grey", size = 1, fill = NA, alpha = 0.1) +
#   geom_sf(data = african_countries, aes(), fill = NA, colour = "black", size = 1.3) +
#   geom_sf_text(data = merged_NER, aes(label = IUs_NAME_MAPPING), size = 2.5, color = "red")+
#   coord_sf(xlim = c(0, 4), ylim = c(11.5, 15.2)) +
#   #coord_sf(xlim = c(38, 44), ylim = c(4, 10)) +
#   #coord_sf(xlim = c(38, 44), ylim = c(8, 10)) +
#   #coord_sf(xlim = c(41, 43), ylim = c(8, 10)) +
#   #coord_sf() +
#   theme_bw() +
#   scale_fill_manual(values = cbPalette, na.value = "gray") +
#   scale_colour_manual(na.value="gray")+
#   labs(fill='') +
#   theme(
#     legend.position = "bottom",  # Place the legend at the bottom
#     legend.direction = "horizontal")
# 
# dfOCP_included6_lastyr <- subset(dfOCP_included6, Year == 2022)
# nrow(dfOCP_included6_lastyr) # 665 IUs
#                              # 670 IUs (April 25') - NER now not trimmed

# table(dfOCP_included6_lastyr$Included) # same as earlier 449 included as endemic + 221 included under OCP shape

# # ===========================================================================================#
# # 6) continue MDA (65% coverage) in those IUs with "MDA continuing" or "Treatment naive" (?) #
# 
# dfOCP_included7 <- dfOCP_included6
# 
# dfOCP_included7 <- dfOCP_included7 %>%
#   mutate(
#     MDA_CDTI = if_else(
#       trt_status_2022 == "MDA continues" & Year %in% c(2023:2025),
#       1,
#       MDA_CDTI
#     )
#   )
# 
# dfOCP_included7 <- dfOCP_included7 %>%
#   mutate(
#     Cov.in2 = if_else(
#       trt_status_2022 == "MDA continues" & Year %in% c(2023:2025),
#       0.65,
#       Cov.in2
#     )
#   )
# 
# # check
# dfOCP_included7_check_MDAstopped <- subset(dfOCP_included7, trt_status_2022 == "MDA stopped")
# length(unique(dfOCP_included7_check_MDAstopped$IU_ID_MAPPING))
# dfOCP_included7_check_MDAstopped <- subset(dfOCP_included7_check_MDAstopped, Year %in% c(2023:2025))
# 
# dfOCP_included7_check_MDAcont <- subset(dfOCP_included7, trt_status_2022 == "MDA continues")
# length(unique(dfOCP_included7_check_MDAcont$IU_ID_MAPPING))
# dfOCP_included7_check_MDAcont <- subset(dfOCP_included7_check_MDAcont, Year %in% c(2023:2025))
# 
# dfOCP_included7_check_trtnaive <- subset(dfOCP_included7, trt_status_2022 == "Treatment naive")
# length(unique(dfOCP_included7_check_trtnaive$IU_ID_MAPPING)) # 57 Ius
# 
# # ============================================================================================== #
# # need to continue biannual where any biannual in years 2020 - 2022 (NGA and UGA) for 2023-2025
# 
# # first find IUs where any biannual in 2020 - 2022 (and MDA continues classification with MDA_CDTI in 2023-25)
# 
# filtered_ids <- dfOCP_included7 %>%
#   group_by(IU_ID_MAPPING) %>%
#   filter(
#     any(Year %in% c(2020, 2021, 2022) & MDA_CDTI_Biannual == 1) &
#       any(Year %in% c(2023, 2024, 2025) & MDA_CDTI == 1) &
#       trt_status_2022 == "MDA continues"
#   ) %>%
#   distinct(IU_ID_MAPPING) %>%
#   pull(IU_ID_MAPPING)
# 
# dfOCP_included7$MDA_CDTI_Biannual <- ifelse(dfOCP_included7$IU_ID_MAPPING %in% filtered_ids & dfOCP_included7$Year %in% c(2023,2024,2025), 1, dfOCP_included7$MDA_CDTI_Biannual)

# # ============================================================================================== #
# # need to continue VC status (either 2 = vector eliminated or 1 = under VC) in UGA for 2023-2025
# # where vector control is 1 in 2022 make 1 in 2023 - 2025
# condition <- dfOCP_included7$vector_control == 1 & dfOCP_included7$Year == 2022
# indices <- which(condition)
# selected_rows <- dfOCP_included7[indices, ]
# unique_IDIU <- unique(selected_rows$IU_ID_MAPPING)
# dfOCP_included7$vector_control <- ifelse(dfOCP_included7$IU_ID_MAPPING %in% unique_IDIU & dfOCP_included7$Year %in% c(2023,2024,2025), 1, dfOCP_included7$vector_control)
# # shouldnt be any vector control in 2022?

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

# Example usage:
dfOCP_included7 <- apply_MDA_interventions_2023_2025(dfOCP_included6)

# =============================================================================== #
# 7) create rho parameter column = start all with 0.3                             #

# dfOCP_included7 <- dfOCP_included7 %>%
#   mutate(
#     adherence_par = ifelse(MDA_CDTI == 1 | MDA_nonCDTI == 1, 0.5, NA_real_)
#   ) # for business case

# dfOCP_included7 <- dfOCP_included7 %>%
#   mutate(
#     adherence_par = ifelse(MDA_CDTI == 1 | MDA_nonCDTI == 1, 0.3, NA_real_)
#   ) # for Endgame
# 
# 
# # ===================================================== #
# #  8) create num_rnds and modelled_CUM_MDA cols         #
# 
# dfOCP_included7$number_rnds <- rowSums(dfOCP_included7[c("MDA_nonCDTI", "MDA_CDTI", "MDA_CDTI_Biannual")], na.rm = TRUE)
# 
# dfOCP_included7$any_MDA <- ifelse(dfOCP_included7$number_rnds > 0, 1, 0)
# 
# dfOCP_included7 <- dfOCP_included7 %>%
#   group_by(IU_ID_MAPPING) %>%
#   mutate(CUM_MDA_modelled = cumsum(number_rnds))
# 
# dfOCP_included7$Cum_MDA_ESPEN <- dfOCP_included7$Cum_MDA
# 
# #unique(dfOCP_included7$ADMIN0ISO3)
# 
# dfOCP_included7_lastyr <- subset(dfOCP_included7, Year == 2022)
# nrow(dfOCP_included7_lastyr) # 665 IUs

# # =====================================#
# # 9) label co-endemic IUS with loa     #
# 
# co_endemic_IUs <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Co-endemicity/co_endemic_IUs_OCP.csv")
# 
# unique(dfOCP_included7$ADMIN0ISO3)
# 
# co_endemic_IUs_oncho_LF_loa <- subset(co_endemic_IUs, co_endemicity == "oncho,LF,loa")
# co_endemic_IUs_oncho_LF_loa_vec <- unique(co_endemic_IUs_oncho_LF_loa$IU_ID_MAPPING)
# 
# co_endemic_IUs_oncho_LF <- subset(co_endemic_IUs, co_endemicity == "oncho,LF")
# co_endemic_IUs_oncho_LF_vec <- unique(co_endemic_IUs_oncho_LF$IU_ID_MAPPING)
# 
# co_endemic_IUs_oncho_loa <- subset(co_endemic_IUs, co_endemicity == "oncho,loa")
# co_endemic_IUs_oncho_loa_vec <- unique(co_endemic_IUs_oncho_loa$IU_ID_MAPPING)
# 
# co_endemic_IUs_oncho <- subset(co_endemic_IUs, co_endemicity == "oncho")
# co_endemic_IUs_oncho_vec <- unique(co_endemic_IUs_oncho$IU_ID_MAPPING)
# 
# dfOCP_included7$co_endemicity <- ifelse(dfOCP_included7$IU_ID_MAPPING %in% co_endemic_IUs_oncho_LF_loa_vec, "oncho,LF,loa",
#                                          ifelse(dfOCP_included7$IU_ID_MAPPING %in% co_endemic_IUs_oncho_LF_vec, "oncho,LF",
#                                                 ifelse(dfOCP_included7$IU_ID_MAPPING %in% co_endemic_IUs_oncho_loa_vec, "oncho,loa",
#                                                        ifelse(dfOCP_included7$IU_ID_MAPPING %in% co_endemic_IUs_oncho_vec, "oncho", "assumed oncho only"))))

# ============================================================================ #
#           Funtion to create co-endemicity column (oncho-LF-loa)              #
# ============================================================================ #

create_co_endemicity_column <- function(df, file_path_input) {
  
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

# Example usage:
dfOCP_included7 <- create_co_endemicity_column(dfOCP_included7, "co_endemic_IUs_OCP.csv")



# ==========================================================================
# 9) in co-endemic IUs with loa, increase rho parameter value to 0.5 or 0.6

# dfOCP_included7 <- dfOCP_included7 %>%
#   mutate(
#     adherence_par = case_when(
#       co_endemicity %in% c("oncho,LF,loa", "oncho,loa") & adherence_par == 0.5 ~ 0.6,
#       TRUE ~ adherence_par
#     )
#   )
# should be no change as no loa! - only for Business Case NOT ENDGAME

# ======================================================================================
# 10) make extra empty columns to match OCP dataframe when ready & make FINAL dataframe #

# dfAPOC_included7$PHASE <- NA
# dfAPOC_included7$SIZ_label <- NA
# dfAPOC_included7$MDA_nonCDTI <- NA

# dfOCP_included7$IUID <- dfOCP_included7$IU_ID_MAPPING
# dfOCP_included7$IUID[nchar(dfOCP_included7$IU_ID_MAPPING)==4] <-  paste(0, dfOCP_included7$IUID[nchar(dfOCP_included7$IU_ID_MAPPING)==4], sep="")
# dfOCP_included7$IUID <- paste(dfOCP_included7$ADMIN0ISO3, dfOCP_included7$IUID, sep="")
# 
# # check if any IUs where only MDA_CDTI == 1 found for 2013 (erroneously coded)problem_IUS_2013 <- dfAPOC_included4 %>%
# problem_IUS_2013 <- dfOCP_included7 %>%
#   filter(MDA_CDTI == 1) %>%
#   group_by(IU_ID_MAPPING) %>%
#   filter(n_distinct(Year) == 1 & Year == 2013) %>%
#   ungroup() %>%
#   distinct(IU_ID_MAPPING)
# 
# # check if any IUs where only MDA_CDTI == 1 found for 2013 (erroneously coded)problem_IUS_2013 <- dfAPOC_included4 %>%
# problem_IUS_2014 <- dfOCP_included7 %>%
#   filter(MDA_CDTI == 1) %>%
#   group_by(IU_ID_MAPPING) %>%
#   filter(n_distinct(Year) == 1 & Year == 2014) %>%
#   ungroup() %>%
#   distinct(IU_ID_MAPPING)

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

# Example usage:
dfOCP_included7 <- create_extra_intervention_cols(dfOCP_included7)


# 
# # =====================================================================================
# #              Biannual_VC_ col mapping                                               #
# 
# dfOCP_included7 <- dfOCP_included7 %>%
#   group_by(IU_ID_MAPPING) %>%
#   mutate(biannual_included = any(MDA_CDTI_Biannual == 1))
# 
# dfOCP_included7<- dfOCP_included7 %>%
#   group_by(IU_ID_MAPPING) %>%
#   mutate(VC_included = any(vector_control == 1))
# 
# dfOCP_included7 <- dfOCP_included7 %>%
#   group_by(IU_ID_MAPPING) %>%
#   mutate(annual_only_included = all(is.na(biannual_included)) & any(MDA_CDTI == 1))
# 
# dfOCP_included7 <- dfOCP_included7 %>%
#   group_by(IU_ID_MAPPING) %>%
#   mutate(
#     biannual_VC_mapping = case_when(
#       all(biannual_included) & (all(is.na(annual_only_included)) | all(annual_only_included == FALSE)) & all(is.na(VC_included | VC_included == FALSE)) ~ "biannual",
#       all(biannual_included) & (all(is.na(annual_only_included)) | all(annual_only_included == FALSE)) & all(VC_included) ~ "biannual & vector control",
#       all(is.na(biannual_included)) & all(annual_only_included) & (all(is.na(VC_included | VC_included == FALSE)) | all(VC_included == FALSE)) ~ "annual only",
#       all(is.na(biannual_included)) & all(annual_only_included) & all(VC_included) ~ "annual & vector control",
#       TRUE ~ "treatment naive"
#     )
#   )
# 
# dfOCP_included7_lastyr <- subset(dfOCP_included7, Year == 2022)
# nrow(dfOCP_included7_lastyr) # 665 IUs
#                              # 670 IUs (April 25')


# ================================================================================================= #
#   Function to create biannual_vc_mapping (intervention distribution) variable                     #
# ================================================================================================= #

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

# call function:
dfOCP_included7 <- create_biannual_vc_mapping(dfOCP_included7)

# # ================================================================ #
# #   remove treatment naive IUs (with not-reported etc)  - MAY 2024 #
# 
# # condition <- dfAPOC_included7$trt_status_2022 == "Treatment naive" &
# #   dfAPOC_included7$MAX_Endemicity == "Not reported"
# # condition <- dfOCP_included7$trt_status_2022 == "Treatment naive" &
# #   dfOCP_included7$MAX_Endemicity %in% c("Non-endemic","Not reported","Unknown (under LF MDA)","Unknown (consider Oncho Elimination Mapping)")
# # indices <- which(condition)
# # dfOCP_included7$exclude <- NA
# # dfOCP_included7$exclude[indices] <- "exclude"
# 
# condition <- dfOCP_included7$trt_status_2022 == "Treatment naive"
# indices <- which(condition)
# dfOCP_included7$exclude <- NA
# dfOCP_included7$exclude[indices] <- "exclude"
# 
# 
# # check these IUs labelled as "exclude"
# dfOCP_included7_exclude_subset <- subset(dfOCP_included7, exclude == "exclude")
# dfOCP_included7_exclude_subset_lastyr <- subset(dfOCP_included7_exclude_subset, Year == 2022)
# nrow(dfOCP_included7_exclude_subset_lastyr) # 57 IUs considered treatment naive (56 + 1 considered "endemic" in ESPEN)
# # dfOCP_included7_exclude_subset_lastyr <- subset(dfOCP_included7_exclude_subset, Year == 2022 & !Endemicity == "Non-endemic")
# # nrow(dfOCP_included7_exclude_subset_lastyr) # 5 IUs
# # trt_naive_IUs_OCP_exclude <- unique(dfOCP_included7_exclude_subset_lastyr$IU_ID_MAPPING)
# # trt_naive_IUs_OCP_exclude
# #writeLines(as.character(trt_naive_IUs_OCP_exclude), "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/trt_naive_IUs_OCP_exclude_n4.txt")
# 
# unique(dfOCP_included7_exclude_subset_lastyr$MAX_Endemicity)
# 
# table(dfOCP_included7_exclude_subset_lastyr$MAX_Endemicity)
# check_df <- subset(dfOCP_included7, Year == 2022)
# table(check_df$exclude) # 57 to 
# 
# # filter these out so no longer included as IUs
# dfOCP_included7 <- subset(dfOCP_included7, is.na(dfOCP_included7$exclude))
# 
# dfOCP_included7$Control_prog <- "OCP"
# 
# dfOCP_included7_lastyr <- subset(dfOCP_included7, Year == 2022)
# nrow(dfOCP_included7_lastyr) # now 609 IUs (56 removed)
#                              # now 613 IUs (670 - 57) in April 25'
# 
# unique(dfOCP_included7_lastyr$MAX_Endemicity)
# 
# dfOCP_included7_lastyr_subset <- subset(dfOCP_included7_lastyr, PHASE == "NON-CONTROL")
# nrow(dfOCP_included7_lastyr_subset) # 15 IUs considered "endemic" in ESPEN but not in OCP phase shape file
# 
# dfOCP_included7_lastyr_subset2 <- subset(dfOCP_included7_lastyr, MAX_Endemicity %in% c("Endemic (under MDA)",
#                                                                                       "Endemic (MDA not delivered)",
#                                                                                       "Endemic (under post-intervention surveillance)"))
# nrow(dfOCP_included7_lastyr_subset2) # 438 IUs (instead of 443 IUs - are these endemic ones in NER that have been pruned?)
# 
# dfOCP_included7$Cov_raw <- dfOCP_included7$MDA_CDTI_raw
# 
# check_df <- subset(dfOCP_included7, Year == 2022)
# nrow(check_df) # 613 IUs in April 25'

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

# call function:
dfOCP_included7 <- remove_treatment_naive_IUs_and_check(dfOCP_included7)


# =========================================================================
# 11) final minimal dataframe to share

# Full_OCP_histories_df_popinfo <- dfOCP_included7[, c("IU_ID_MAPPING","IUs_NAME_MAPPING","IU_CODE_MAPPING", "IUID", "ADMIN0ISO3","Endemicity","MAX_Endemicity",
#                                                        "PHASE", "SIZ_label", "endemicity_baseline","Year","PopTot","PopPreSAC","PopSAC","PopAdult","PopReq",
#                                                        "PopTrg","PopTreat", "MDA_scheme","Cum_MDA_ESPEN","Cov","EpiCov",
#                                                        "vector_control","biannual_VC_mapping","MDA_nonCDTI", "MDA_CDTI", "MDA_CDTI_Biannual","number_rnds",
#                                                        "Cov.in2","CUM_MDA_modelled","trt_status_2022","trt_status_2022_v2","adherence_par","co_endemicity", "Control_prog")]


# # adding in raw coverages and source of data (Feb 25)
# Full_OCP_histories_df_popinfo <- dfOCP_included7[, c("IU_ID_MAPPING","IUs_NAME_MAPPING","IU_CODE_MAPPING", "IUID", "ADMIN0ISO3","Endemicity","MAX_Endemicity",
#                                                      "PHASE", "SIZ_label", "endemicity_baseline","Year","PopTot","PopPreSAC","PopSAC","PopAdult","PopReq",
#                                                      "PopTrg","PopTreat", "MDA_scheme","Cum_MDA_ESPEN","Cov","EpiCov",
#                                                      "vector_control","biannual_VC_mapping","MDA_nonCDTI", "MDA_CDTI", "MDA_CDTI_Biannual","number_rnds",
#                                                      "Cov.in2","Cov_raw","cov_source","cov_specific_source","CUM_MDA_modelled","trt_status_2022","trt_status_2022_v2","adherence_par","co_endemicity", "Control_prog")]

# Full_OCP_histories_df_popinfo <- dfOCP_included7[, c("IU_ID_MAPPING","IUs_NAME_MAPPING","IU_CODE_MAPPING", "IUID", "ADMIN0ISO3","Endemicity","MAX_Endemicity",
#                                                      "PHASE", "SIZ_label", "endemicity_baseline","Year", "MDA_scheme","Cum_MDA_ESPEN","Cov","EpiCov",
#                                                      "vector_control","biannual_VC_mapping","MDA_nonCDTI", "MDA_CDTI", "MDA_CDTI_Biannual","number_rnds",
#                                                      "Cov.in2","CUM_MDA_modelled","trt_status_2022","trt_status_2022_v2","adherence_par","co_endemicity", "Control_prog")]


#Full_OCP_histories_df_popinfo$Control_prog <- "OCP"


# Full_OCP_histories_df_minimal <- dfOCP_included7[, c("IU_ID_MAPPING","IUs_NAME_MAPPING","IU_CODE_MAPPING", "IUID", "ADMIN0ISO3","Endemicity",
#                                                        "PHASE", "SIZ_label", "endemicity_baseline","Year","MDA_scheme","Cum_MDA_ESPEN","Cov","EpiCov",
#                                                        "vector_control","biannual_VC_mapping","MDA_nonCDTI", "MDA_CDTI", "MDA_CDTI_Biannual","number_rnds",
#                                                        "Cov.in2","CUM_MDA_modelled","trt_status_2022","trt_status_2022_v2","adherence_par","co_endemicity", "Control_prog")]

# # adding in raw coverages and source of data (Feb 25)
# Full_OCP_histories_df_minimal <- dfOCP_included7[, c("IU_ID_MAPPING","IUs_NAME_MAPPING","IU_CODE_MAPPING", "IUID", "ADMIN0ISO3","Endemicity",
#                                                      "PHASE", "SIZ_label", "endemicity_baseline","Year","MDA_scheme","Cum_MDA_ESPEN","Cov","EpiCov",
#                                                      "vector_control","biannual_VC_mapping","MDA_nonCDTI", "MDA_CDTI", "MDA_CDTI_Biannual","number_rnds",
#                                                      "Cov.in2","Cov_raw","cov_source","cov_specific_source","CUM_MDA_modelled","trt_status_2022","trt_status_2022_v2","adherence_par","co_endemicity", "Control_prog")]
# 
# 
# Full_OCP_histories_df_minimal_lastyr_2022 <- subset(Full_OCP_histories_df_popinfo, Year == 2022)
# Full_OCP_histories_df_minimal_lastyr_2025 <- subset(Full_OCP_histories_df_popinfo, Year == 2025)

# ========================================================================================== #
#                        Create final dataframes for OCP IUs                                 #
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

# call function:
OCP_histories_data <- create_OCP_histories_dataframes(dfOCP_included7)

# Access individual dataframes:
Full_OCP_histories_df_popinfo <- OCP_histories_data[[1]]
Full_OCP_histories_df_minimal <- OCP_histories_data[[2]]
Full_OCP_histories_df_minimal_lastyr_2022 <- OCP_histories_data[[3]]
Full_OCP_histories_df_minimal_lastyr_2025 <- OCP_histories_data[[4]]


# Save - endgame runs in March 2024 #
# write.csv(Full_OCP_histories_df_minimal, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_OCP_histories_df_minimal_150324.csv")
# write.csv(Full_OCP_histories_df_minimal_lastyr_2022, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_OCP_histories_df_minimal_lastyr_2022_150324.csv")
# write.csv(Full_OCP_histories_df_minimal_lastyr_2025, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_OCP_histories_df_minimal_lastyr_2025_150324.csv")

# Save - endgame runs in March 2024 - July additions #
# write.csv(Full_OCP_histories_df_popinfo, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_OCP_histories_df_popinfo_150324_v2.csv")
# write.csv(Full_OCP_histories_df_minimal, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_OCP_histories_df_minimal_150324_v2.csv")
# write.csv(Full_OCP_histories_df_minimal_lastyr_2022, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_OCP_histories_df_minimal_lastyr_2022_150324_v2.csv")
# write.csv(Full_OCP_histories_df_minimal_lastyr_2025, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_OCP_histories_df_minimal_lastyr_2025_150324_v2.csv")

# # Save - Feb 2025 with raw coverage and soruce of information cols #
# write.csv(Full_OCP_histories_df_popinfo, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_OCP_histories_df_popinfo_070225.csv")
# write.csv(Full_OCP_histories_df_minimal, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_OCP_histories_df_minimal_070225.csv")
# write.csv(Full_OCP_histories_df_minimal_lastyr_2022, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_OCP_histories_df_minimal_lastyr_2022_070225.csv")
# write.csv(Full_OCP_histories_df_minimal_lastyr_2025, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_OCP_histories_df_minimal_lastyr_2025_070225.csv")


# Save - March 2025 - updated with additional  #
write.csv(Full_OCP_histories_df_popinfo, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_OCP_histories_df_popinfo_260325.csv")
write.csv(Full_OCP_histories_df_minimal, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_OCP_histories_df_minimal_260325.csv")
write.csv(Full_OCP_histories_df_minimal_lastyr_2022, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_OCP_histories_df_minimal_lastyr_2022_260325.csv")
write.csv(Full_OCP_histories_df_minimal_lastyr_2025, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_OCP_histories_df_minimal_lastyr_2025_260325.csv")

# Save - April 2025 - updated with additional  #
write.csv(Full_OCP_histories_df_popinfo, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_OCP_histories_df_popinfo_290425.csv")
write.csv(Full_OCP_histories_df_minimal, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_OCP_histories_df_minimal_290425.csv")
write.csv(Full_OCP_histories_df_minimal_lastyr_2022, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_OCP_histories_df_minimal_lastyr_2022_290425.csv")
write.csv(Full_OCP_histories_df_minimal_lastyr_2025, "C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_OCP_histories_df_minimal_lastyr_2025_290425.csv")

nrow(Full_OCP_histories_df_minimal_lastyr_2022) # 613 IUs in OCP (Apilr 2025')
# Full_OCP_histories_df_minimal_290425 <- read.csv("C:/Users/mad206/OneDrive - Imperial College London/NTD-MC current/Endgame/Improving histories/Full_OCP_histories_df_minimal_290425.csv")

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

