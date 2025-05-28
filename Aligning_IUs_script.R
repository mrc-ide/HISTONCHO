library(tidyverse)
library(readxl)
library(stringr)
library(sf)
library(readr)

filter_countries <- c("Seychelles", "Mauritius", "Eswatini", "Madagascar", "Gambia",
                      "Botswana", "Namibia", "Tanzania (Zanzibar)",
                      "Sao Tome and Principe", "Cabo Verde", "South Africa",
                      "Lesotho", "Comoros", "Algeria", "Mauritania", "Egypt",
                      "Djibouti", "Somalia", "Yemen", "Zimbabwe", "Eritrea")

oncho <- read_csv("Oncho_IU_MDA_202308.csv")  %>%
  mutate(pID = row_number()) # Add a unique id in case it is useful

ius<- read_sf("ESPEN_IU_2021.shp")
ius_valid <- st_make_valid(ius)

list_new_ius <- read_csv("List_New_IU.csv")

oncho_parent <- oncho %>%
  left_join(list_new_ius, by = c("IU_ID" = "ESPEN_IU_ID")) # Joining up the index file

oncho_parent_slice <- oncho_parent %>%
  filter(!is.na(Parent1_IU_ID)) %>%
  group_by(IU_ID, Parent1_IU_ID) %>%
  slice_min(Year) %>% # For those that have a parent we select the first year in which the IU has a record
  filter(Year > 2013)

yearDiff <- oncho_parent_slice$Year - 2013 # Create a vector of the number of years to be filled in

yearSeq <- unlist(sapply(oncho_parent_slice$Year, function(x) 2013:(x-1))) #  the years that go into the inserted years

newDF <- oncho_parent_slice[rep(1:nrow(oncho_parent_slice), yearDiff),] # the dummy data inserted to back faill
newDF$Year <- yearSeq

newDF_Join <- newDF %>% # Create the new columns in the data frame
  left_join(oncho, by = c("Year" = "Year", "Parent1_IU_ID" = "IU_ID"), suffix = c("", "_P")) %>%
  group_by(Parent1_IU_ID, Year) %>%
  mutate(Sisters = n()) = # The number of other children from that parent


oncho_bind <- oncho %>% # Put the data back into the main data frame
  bind_rows(newDF_Join) %>%
  arrange(IU_ID, Year) %>%
  mutate("SHP_Present" = ifelse(IU_ID %in% ius$IU_ID, "Present", "Absent")) %>% # Is the IU recorded in the spatial data
  mutate(Included_Ctry = ifelse(ADMIN0 %in% filter_countries, "Exclude", "Include")) # Is the countyr one thta we are including in the oncho list

write_excel_csv(oncho_bind, file = "oncho_bind_parents.csv", na = "")


# Inferring the historical status of child IUs ----------------------------

oncho_bind %>% # Removing some noise
  dplyr::select(c(1:48, 93:96), Endemicity_P, MDA_scheme_P, Cum_MDA_P) %>%
  dplyr::select(-c("Endemicity_org", "Endemicity_ID", "pID"))

rankings <- read_excel("Rankings.xlsx") # attaching rankings
rankings$Status <- iconv(rankings$Status, to = 'ASCII//TRANSLIT')
oncho_df2 <- oncho_df %>%
  left_join(rankings %>% dplyr::select(-Variable), by = c("Endemicity" = "Status"), suffix = c("", "_Endemicity")) %>%
  left_join(rankings %>% dplyr::select(-Variable), by = c("MDA_scheme" = "Status"), suffix = c("", "_MDA_scheme"))

oncho_df_max_endemicity <- oncho_df2 %>% # Inferring the maximum endemicity
  group_by(IU_ID_MAPPING) %>%
  slice_max(Rank, with_ties = FALSE) %>%
  left_join(rankings %>% filter(Variable == "Endemicity"), by = "Rank") %>%
  rename("MAX_Endemicity" = "Status") %>%
  dplyr::select(-Rank)

oncho_df_max_MDA <- oncho_df2 %>% # Inferirng the maximum MDA status
  group_by(IU_ID_MAPPING) %>%
  slice_max(Rank_MDA_scheme, with_ties = FALSE) %>%
  left_join(rankings %>% filter(Variable == "MDA"), by = c("Rank_MDA_scheme" ="Rank")) %>%
  rename("MAX_MDA" = "Status") %>%
  dplyr::select(-Rank_MDA_scheme)

oncho_df_join1 <- oncho_df2 %>% # Joining it up
  ungroup() %>%
  left_join(oncho_df_max_endemicity %>%
              dplyr::select(IU_ID_MAPPING, MAX_Endemicity), by = "IU_ID_MAPPING") %>%
  left_join(oncho_df_max_MDA %>%
              dplyr::select(IU_ID_MAPPING, MAX_MDA), by = "IU_ID_MAPPING")



# Parent Status -----------------------------------------------------------
# Attaching and post-processing the status of the parent

oncho_df_2 <- oncho_df %>%
  left_join(rankings %>% dplyr::select(-Variable), by = c("Endemicity_P" = "Status"), suffix = c("", "_Endemicity")) %>%
  left_join(rankings %>% dplyr::select(-Variable), by = c("MDA_scheme_P" = "Status"), suffix = c("", "_MDA_scheme"))

oncho_df_max_endemicity <- oncho_df_2 %>%
  group_by(IU_ID_MAPPING) %>%
  slice_max(Rank, with_ties = FALSE) %>%
  left_join(rankings %>% filter(Variable == "Endemicity"), by = "Rank") %>%
  rename("MAX_Endemicity_Parent" = "Status") %>%
  dplyr::select(-Rank)

oncho_df_max_MDA <- oncho_df_2 %>%
  group_by(IU_ID_MAPPING) %>%
  slice_max(Rank_MDA_scheme, with_ties = FALSE) %>%
  left_join(rankings %>% filter(Variable == "MDA"), by = c("Rank_MDA_scheme" ="Rank")) %>%
  rename("MAX_MDA_Parent" = "Status") %>%
  dplyr::select(-Rank_MDA_scheme)

oncho_df_join2 <- oncho_df_join1 %>%
  ungroup() %>%
  left_join(oncho_df_max_endemicity %>%
              dplyr::select(IU_ID_MAPPING, MAX_Endemicity_Parent), by = "IU_ID_MAPPING") %>%
  left_join(oncho_df_max_MDA %>%
              dplyr::select(IU_ID_MAPPING, MAX_MDA_Parent), by = "IU_ID_MAPPING") %>%
  mutate(Cum_MDA_Final = ifelse(is.na(Cum_MDA_P), Cum_MDA, Cum_MDA_P),
         Inferred = ifelse(is.na(Parent1_IU_ID), "Original", "Inferred"))

