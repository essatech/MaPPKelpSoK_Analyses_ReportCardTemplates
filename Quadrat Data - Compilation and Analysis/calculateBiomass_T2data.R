# FILE: calculateBiomass_T2data.R
# DESCRIPTION: This script takes spatial extent data produced by the script, 
#   "UAV Data - Compilation and Analysis > UAVSpatialMetricsCalculation" and combines 
#   these with Tier 2 data from MaPP sites in NVI. After combining the data, this script
#   estimates biomass of the kelp beds based on morphometric relationships (see the 2025 
#   MaPP Monitoring Report, Thompson 2025). 
# The script includes functions to calculate average stipe diameter from strings of varying
#   formats (e.g., "31, 23, 25" and "41 20 30" are both acceptable formats) and to 
#   estimate kelp bed biomass for each of bull and giant kelps based on UAV-derived extent
#   and surface metrics like bulb density and stipe diameter. 
# AUTHOR: Mairin Deith
# DATE CREATED: Mar 6, 2025
# LAST MODIFIED: March 30, 2025

# Step 0: Load libraries and custom functions --------------------------------------------
library(dplyr)
library(ggplot2)
library(colorspace)

# Function to calculate average stipe diameter from "," separated list
avgStipeDiameter <- function(x) {
  # Ignore 0 diameter entries
  x[which(x == 0)] <- NA
  sapply(x, FUN = function(X) {
      mean(as.numeric(gsub(x = stringr::str_split(X, "[,\\s-]+")[[1]], pattern = " ", replace = "")), na.rm = T)
  })
}
# Test the stipe diameter function
# avgStipeDiameter(c("31", "30 31 32", "54, 43 50, 10", "10,15,12,20"))

# Functions to estimate bed biomass (in kilograms) based on kelp and kelp bed attributes
# Relationships from the 2025 MaPP Monitoring Report (Thompson 2025). 
estimateBullKelpBiomasKG <- function(diameter, density, bedarea) {
  0.07*exp(0.08 * diameter) * density * bedarea
}
estimateGiantKelpBiomassKG <- function(frondlength, density, bedarea) {
  0.15*exp(0.23 * frondlength) * density * bedarea
}

# Step 1: Data compilation ---------------------------------------------------------------
# Read the compiled data; this is an output from the KelpDataCompilationNVI_post2022 script
post2022_data <- readRDS(file = file.path("Quadrat Data - Compilation and Analysis/preliminaryCompiledTier2Data.Rds"))

# Read the aerial extent data - this was generated from previous analysis. 
# REQUIRED COLUMNS: site_id, year, area_m2
kelp_bed_data <- read.csv(file = "Quadrat Data - Compilation and Analysis/nonQAd spt metrics.csv")

# Merge the quadrat level and kelp bed extent data by site and year
post2022_data_merged <- post2022_data |>
  dplyr::full_join(kelp_bed_data, 
    by = join_by("site_id" == "siteID", "year" == "year"))

# Step 2: Estimate biomass ---------------------------------------------------------------
post2022_data_biomass_summaries <- post2022_data_merged |>
  mutate(avg_stipe_diameter_mm_quad = avgStipeDiameter(quadrat_data_bull_kelp_stipe_diameter_measurements)) |>
  group_by(site_id, year, kelp_species) |>
  summarize(
    avg_avg_stipe_d = mean(avg_stipe_diameter_mm_quad, na.rm = T),
    avg_kelp_count = mean(as.numeric(quadrat_data_bull_kelp_kelp_bulb_count), na.rm = T),
    bedarea_m = mean(area_m2, na.rm = T)
  ) |>
  ungroup() |>
  mutate(bed_biomass = case_when(
    is.na(kelp_species) | kelp_species == "Bull" ~ estimateBullKelpBiomasKG(
      diameter = avg_avg_stipe_d,
      density = avg_kelp_count,
      bedarea = bedarea_m
    )
  ))

# Step 3: Plot results -------------------------------------------------------------------

eg_plot <- ggplot(post2022_data_biomass_summaries |> filter(site_id %in% c("CR-K03", "TL-K05")), 
  aes(x = year, y = bed_biomass)) +
  scale_color_continuous_sequential(palette = "Terrain 2", begin = 0.5) + 
  geom_line(group = "site_id") + 
  geom_point(aes(size = bedarea_m, color = avg_kelp_count)) +
  scale_x_log10() + 
  facet_wrap(~site_id, scales = "free_y") +
  theme_classic()

eg_plot + 
  xlab("Year") + ylab("Bed biomass (wet weight in kg)") + 
  labs(color = "Avg. bulb count\nper quadrat", 
    size = "Bed area (m^2)") +
  theme(axis.text.x = element_text(angle = 90))

# 
post2022_data_biomass_summaries |> 
  ggplot() + 
  geom_point(aes(x = avg_avg_stipe_d, y = bed_biomass/(avg_kelp_count * bedarea_m))) + 
  expand_limits(x = 0)

# post2022_data_merged |>
#   filter(site_id == "TL-K05")
