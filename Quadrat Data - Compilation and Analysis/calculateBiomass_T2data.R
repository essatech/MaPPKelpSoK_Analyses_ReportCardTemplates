# FILE: calculateBiomass_T2data.R
# AUTHOR: Mairin Deith
# DATE: Mar 6, 2025

library(dplyr)
library(ggplot2)

# Function to calculate average stipe diameter from "," separated list

avgStipeDiameter <- function(x) {
  # Ignore 0 diameter entries
  x[which(x == 0)] <- NA
  sapply(x, FUN = function(X) {
    # if(grepl(x, pattern = ",")) {
      mean(as.numeric(gsub(x = stringr::str_split(X, "[,\\s-]+")[[1]], pattern = " ", replace = "")), na.rm = T)
    # } else {
      # mean(as.numeric(stringr::str_split(X, " ")[[1]]), na.rm = T)
    # }
  })
}
# Test
avgStipeDiameter(c("31", "30 31 32", "54, 43 50, 10", "10,15,12,20"))

proj_folder <- file.path("~", "..", "ESSA Dropbox","d_global","Projects","EN2900to2999",
  "EN2955 - MaPP Kelp State of Knowledge")

post2022_data <- readRDS(file = "C:/Users/mdeith/ESSA Dropbox/Mairin Deith/ProjectNotes/EN2955 - MaPP Kelp Knowledge Consolidation and Workshop/Git_MaPP_KelpMonitoring/data/tier2_quadratdata/preliminaryCompiledTier2Data.Rds")
# dir(kelp_data_folder)
kelp_bed_data <- read.csv(file = "c:/Users/mdeith/ESSA Dropbox/d_global/Projects/EN2900to2999/EN2955 - MaPP Kelp State of Knowledge/Data/Spatial data repo/csv summaries/nonQAd spt metrics.csv")

post2022_data_merged <- post2022_data |>
  dplyr::full_join(kelp_bed_data, 
    by = join_by("site_id" == "siteID", "year" == "year"))

estimateBullKelpBiomasKG <- function(diameter, density, bedarea) {
  0.06*exp(0.09 * diameter) * density * bedarea
}
estimateGiantKelpBiomassKG <- function(frondlength, density, bedarea) {
  0.15*exp(0.23 * frondlength) * density * bedarea
}

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

ggplot(post2022_data_biomass_summaries |> filter(site_id %in% c("CR-K03", "TL-K05")), 
  aes(x = year, y = bed_biomass)) +
  scale_color_continuous_sequential(palette = "Terrain 2", begin = 0.5) + 
  geom_line(group = "site_id") + 
  geom_point(aes(size = bedarea_m, color = avg_kelp_count)) +
  scale_x_log10() + 
  facet_wrap(~site_id, scales = "free_y") +
  theme_classic()

post2022_data_biomass_summaries |> 
  ggplot() + 
  geom_point(aes(x = avg_avg_stipe_d, y = bed_biomass/(avg_kelp_count * bedarea_m))) + 
  expand_limits(x = 0)

# post2022_data_merged |>
#   filter(site_id == "TL-K05")