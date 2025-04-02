# FILE: KelpDataCompilationNC.R
# PURPOSE: This file programatically compiles and merges Tier 2 sampling files from 
#   the North Coast region.
# AUTHOR: Mairin Deith
# DATE CREATED: 2025-03-30
# DATE LAST MODIFIED: 2025-04-01

###
# Load libraries
library(readxl) # For relative path use
library(dplyr) # For cleaning data
library(tidyr)
library(stringr) # String manipulation
library(janitor)
library(lubridate) # Reasonable date-time manipulation
library(parsedate) # Parses dates of varying formats
library(sf) # convert UTM to lat/long
library(measurements) # Conversion of feet to m

# Function to calculate average stipe diameter from "," separated list

avgStipeDiameter <- function(x) {
  # Ignore 0 diameter entries
  x[which(x == 0)] <- NA
  sapply(x, FUN = function(X) {
    mean(as.numeric(stringr::str_split(X, ", ")[[1]]), na.rm = T)
  })
}

# File path where data are located (Dropbox link)
# No data to be uploaded to GitHub
kelp_data_folder <- file.path("..", "..", "Data", "MaPP North Coast", "Data")

# File paths to data sheets to be compiled
site_metadata_tier2_file <- file.path(kelp_data_folder, "T2FormExport_20250302-180442", 
  "T2Form_20250302-180442.csv")
bullkelp_tier2_file <- file.path(kelp_data_folder, "T2FormExport_20250302-180442", 
  "T2BullKelpQuadratData_20250302-180442.csv")
giantkelp_tier2_file <- file.path(kelp_data_folder, "T2FormExport_20250302-180442", 
  "T2GiantKelpQuadratData_20250302-180442.csv")
perimeterdata_file <- file.path(kelp_data_folder, "T2FormExport_20250302-180442", 
  "T2PerimeterData_20250302-180442.csv")

site_metadata_tier2 <- read.csv(site_metadata_tier2_file)
bullkelp_tier2 <- read.csv(bullkelp_tier2_file)
giantkelp_tier2 <- read.csv(giantkelp_tier2_file)
perimeterdata <- read.csv(perimeterdata_file)

colnames(site_metadata_tier2)
colnames(bullkelp_tier2)

# Identify if any linkages are missing
which(!(bullkelp_tier2$T2Form_FormId %in% site_metadata_tier2$FormId))
which(!(giantkelp_tier2$T2Form_FormId %in% site_metadata_tier2$FormId))
which(!(perimeterdata$T2Form_FormId %in% site_metadata_tier2$FormId))

bullkelp_data <- bullkelp_tier2 |>
  dplyr::left_join(site_metadata_tier2, by = join_by(
    T2Form_FormId == FormId,
    Date == Date,
    T2Form_WaypointName == WaypointName
  )) 
giantkelp_data <- giantkelp_tier2 |>
  dplyr::left_join(site_metadata_tier2, by = join_by(
    T2Form_FormId == FormId,
    Date == Date,
    T2Form_WaypointName == WaypointName
  )) 

# View(bullkelp_data)
# View(giantkelp_data)

# ----------------------------------------------------------------------------------------
# HARMONIZE WITH 2022 NVI DATA
# Standardize colnames to match with Device Magic input names from NVI

bullkelp_data_NVInames <- bullkelp_data |>
  janitor::clean_names() |>
  mutate(site_id = paste0(site, sub_site, collapse = " ")) |>
  dplyr::rowwise() |>
  mutate(
    urchin_records1 = paste0(na.omit(urchin_species1), na.omit(urchin_density1), collapse = ": "),
    urchin_records2 = paste0(na.omit(urchin_species2), na.omit(urchin_density2), collapse = ": "),
    urchin_records3 = paste0(na.omit(urchin_species3), na.omit(urchin_density3), collapse = ": "),
    urchin_records4 = paste0(na.omit(urchin_species4), na.omit(urchin_density4), collapse = ": ")) |>
  mutate(sea_urchin_observations = paste(urchin_records1, urchin_records2, urchin_records3, urchin_records4, collapse = "; ")) |>
  ungroup() |>
  dplyr::rename( 
    site_id = site_id, 
    quadrat = quadrat_number,                                                
    nereocystis_uw_bulbs = uw_bulbs,
    quadrat_data_bull_kelp_kelp_bulb_count = surface_bulbs,
    quadrat_data_bull_kelp_stipe_diameter_measurements = surface_stripe_diameter,
    weather = general_comments,
    tier_1_or_2 = form_indicator,
    kelp_species = species,
    tides_and_currents_low_tide_height_m = low_tide_m,
    tides_and_currents_time_at_low_tide = low_tide_time,
    tides_and_currents_survey_start_time = start_time,
    tides_and_currents_tide_height_at_survey_start_m = start_tide_height_m,
    survey_end_time = end_time,
    tides_and_currents_tide_height_at_survey_end = end_tide_height_m, 
    survey_notes = observation_comments,
    oceanographic_data_secchi_disk_depth_m = secchi_disk_m,
    oceanographic_data_temperature_at_1m = oceanographic_1m_temp_c,
    oceanographic_data_temperature_at_5m = oceanographic_5m_temp_c,
    oceanographic_data_temperature_at_10m = oceanographic_10m_temp_c,
    oceanographic_data_salinity_at_1m = oceanographic_1m_salinity_ppt,
    oceanographic_data_salinity_at_5m = oceanographic_5m_salinity_ppt,
    oceanographic_data_salinity_at_10m = oceanographic_10m_salinity_ppt,
    submissionid = t2form_form_id,
    tides_and_currents_survey_end_time = end_time,
    quadrat_data_bull_kelp_bryozoan_cover = bryozoan_percent_cover,
  )

# Same for giant kelp
# Standardize names to match with Device Magic input names from NVI
giantkelp_data_NVInames <- giantkelp_data |>
  janitor::clean_names() |>
  mutate(site_id = paste0(site, sub_site, collapse = " ")) |>
  dplyr::rowwise() |>
  mutate(
    urchin_records1 = paste0(na.omit(urchin_species1), na.omit(urchin_density1), collapse = ": "),
    urchin_records2 = paste0(na.omit(urchin_species2), na.omit(urchin_density2), collapse = ": "),
    urchin_records3 = paste0(na.omit(urchin_species3), na.omit(urchin_density3), collapse = ": "),
    urchin_records4 = paste0(na.omit(urchin_species4), na.omit(urchin_density4), collapse = ": ")) |>
  mutate(sea_urchin_observations = paste(urchin_records1, urchin_records2, urchin_records3, urchin_records4, collapse = "; ")) |>
  ungroup() |>
  dplyr::rename( 
    site_id = site_id, 
    quadrat = quadrat_number,                                                
    quadrat_data_giant_kelp_kelp_frond_count = frond_count,
    quadrat_data_giant_kelp_bryozoan_cover = bryozoan_percent_cover,
    weather = general_comments,
    tier_1_or_2 = form_indicator,
    kelp_species = species,
    tides_and_currents_low_tide_height_m = low_tide_m,
    tides_and_currents_time_at_low_tide = low_tide_time,
    tides_and_currents_survey_start_time = start_time,
    tides_and_currents_tide_height_at_survey_start_m = start_tide_height_m,
    survey_end_time = end_time,
    tides_and_currents_tide_height_at_survey_end = end_tide_height_m, 
    survey_notes = observation_comments,
    oceanographic_data_secchi_disk_depth_m = secchi_disk_m,
    oceanographic_data_temperature_at_1m = oceanographic_1m_temp_c,
    oceanographic_data_temperature_at_5m = oceanographic_5m_temp_c,
    oceanographic_data_temperature_at_10m = oceanographic_10m_temp_c,
    oceanographic_data_salinity_at_1m = oceanographic_1m_salinity_ppt,
    oceanographic_data_salinity_at_5m = oceanographic_5m_salinity_ppt,
    oceanographic_data_salinity_at_10m = oceanographic_10m_salinity_ppt,
    submissionid = t2form_form_id,
    tides_and_currents_survey_end_time = end_time
  )

# Other fields may be aggregated in other ways; here, we include only those columns which 
# are easily translated.