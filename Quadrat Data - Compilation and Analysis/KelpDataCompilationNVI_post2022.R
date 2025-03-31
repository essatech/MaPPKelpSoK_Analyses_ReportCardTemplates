# FILE: KelpDataCompilationNVI_post2022.R
# PURPOSE: This file programatically merges data files from different monitoring programs
#   conducted in the North Vancouver Island region.
# AUTHOR: Mairin Deith
# DATE CREATED: 2025-02-17
# DATE LAST MODIFIED: 2025-02-17

### TIDYING NOTES
# Column-by-column to do: 
#   tides_and_currents: only time matters, date is wrong
# Stipe diameter: 
#    33% missing; this is when 0 kelp in quadrat.
#    Diameter is 0 if 0 kelp; change to NA?
# Urchin observations: 51% missing
# Bryozoan cover: 
#    Should be NA when 0 kelp in quadrat
# Transect information 
#    Depth at start: 99% missing
#    End location: 89% missing
#    Depth at end: 99% missing
# There are 18 unique site_notes; these often contain important info about GPS coordinates, site conditions, bryozoans, transect coordinates, 
#    start and end times, site names, oceanographic data location
#    Sometimes in lat/long; other times UTM-based
# 2020-2021 WWK data DO NOT include bryozoan coverage

### TIDYING TO DO ITEMS
# Convert urchin observations into multiple columns: red, green, purple; rare, common, abundant
# Animal species observed: could split into multiple columns with 0/1 for each species
# Tides and currents: Low tide is 615m? High tide 845m? Are these times?
# Convert transect locations into lat/long
# Convert oceanographic locations into lat/long
# Consolidate survey_notes into other data fields

### SUGGESTIONS: 
# Dictate what kind of info in the survey_notes?
# Not sure why info about bryozoans etc. is not in the correct fields

###
# Load libraries
library(readxl) # For relative path use
library(dplyr) # For cleaning data
library(tidyr)
library(stringr) # String manipulation
library(janitor)
library(lubridate) # Reasonable date-time manipulation
library(parsedate)
library(sf) # convert UTM to lat/long
library(purrr) # For iterating through datasets
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

kelp_data_folder <- file.path("~", "..", "ESSA Dropbox","d_global","Projects","EN2900to2999",
  "EN2955 - MaPP Kelp State of Knowledge","Data","MaPP NVI","Kelp")
dir(kelp_data_folder)

# List all .csv and .xlsx files
allfiles <- list.files(path = kelp_data_folder,
                        pattern = ".csv|.xlsx",
                        full.names = TRUE,
                        recursive = TRUE)

workbook <- sheetFormat <- c(
  grep(allfiles, pattern = "WKUM 2020 Kelp Data", value = T)
)

# 30 files
# Each file name/path includes year; fetch these
  
df_quadrat_year <- (lapply(
  str_split(allfiles, " NVI Kelp"),
  function(x) str_split(x[[1]], "Kelp/")[[1]][2])
)

### START WITH MOST RECENT: 2022-2024
# Monitoring data from these years are the most harmonized
# Loading example, setting up a column dictionary, and checking for completeness
post2022_example <- readxl::read_xlsx(allfiles[[28]]) %>%
  janitor::clean_names()
(column_name_dict <- colnames(post2022_example))
# test_question is going to appear infrequently

tables_list <- list()
row_sum <- 0

for (y in as.character(2022:2024)) {
  file_idx <- allfiles[which(unlist(df_quadrat_year) == y)]
  tables_list[[file_idx]] <- readxl::read_xlsx(file_idx) %>%
    janitor::clean_names()
  message("...nrows: ", nrow(tables_list[[file_idx]]))
  # message(file_idx)
  # message("...Missing in column_name_dict: ", paste0(collapse = "; ", 
  # setdiff(colnames(tables_list[[file_idx]]), column_name_dict)), 
  # "\n...Missing in file: ", paste0(collapse = "; ", 
  # setdiff(column_name_dict, colnames(tables_list[[file_idx]]))))
  row_sum <- row_sum + nrow(tables_list[[file_idx]])
}

# There are some datasets in the same format: 2021 KMKS
same_format <- c(
  grep(allfiles, pattern = "Kmks Kelp Survey Tier 2", value = TRUE))

for (f in same_format) {
  file_idx <- allfiles[which(allfiles == f)]
  tables_list[[file_idx]] <- readxl::read_xlsx(file_idx) %>%
    janitor::clean_names() |>
    mutate(site_id = case_when(
      site_id == "Km-02" ~ "KM-K02",
      TRUE ~ site_id
    ))
  message("...nrows: ", nrow(tables_list[[file_idx]]))
  # message(file_idx)
  # message("...Missing in column_name_dict: ", paste0(collapse = "; ", 
  # setdiff(colnames(tables_list[[file_idx]]), column_name_dict)), 
  # "\n...Missing in file: ", paste0(collapse = "; ", 
  # setdiff(column_name_dict, colnames(tables_list[[file_idx]]))))
  row_sum <- row_sum + nrow(tables_list[[file_idx]])
}


# Notes: 
#    2022 does not include: location (site_location included); test_question
#    2023 does not include lat; long; test_question (lat/lon may be fill-in-able given transect data)
#    2024 does not include lat; long; 

# Merge and maintain new columns: 
post2022_alldata <- do.call(plyr::rbind.fill, tables_list) 
# nrow(post2022_alldata)
# No rows lost

View(post2022_alldata)

# Clean the data: 
#   COLUMNS TO ADJUST: 
#   submissiondatetime does not matter; use date - it is nearly 100% complete
#   lat/long often missing; these are in site_location as: 
#       e.g. lat=50.031377013, long=-125.248293232, alt=5.219285, hAccuracy=15.454243, vAccuracy=23.571648, timestamp=2025-01-16T21:25:07Z
#   often missing transect start/stop locations
#
# Ml-K12 versus ML-K12 - approx. same lat/lon?
# Ml-K17 versus ML-K17?


post2022_alldata <- post2022_alldata |>
  filter(site_id != "Testing") |>  
  rowwise() |>
  # Rename sites
  mutate(
    site_id = toupper(site_id),
    # Create site prefixes and convert to all-upper names
    site_prefix = stringr::str_split(site_id, "-")[[1]][1]
  ) |>
  dplyr::relocate(site_prefix, .before = site_id) |>
  # Pull lat and long out of site_locations which do not have them
  mutate(site_loc_lat = as.numeric(stringr::str_replace(
    # Split into "lat=...", "[long=]..." and replace "lat=" with ""
    stringr::str_split(site_location, pattern = ", long=", n = 2)[[1]][1], 
    pattern = "lat=", replacement = ""
  ))) |>
  # Intermediate column
  mutate(site_loc_nonlat = stringr::str_replace(
    # Split into "lat=...", "[long=]..." and replace "lat=" with ""
    stringr::str_split(site_location, pattern = ", long=", n = 2)[[1]][2], 
    pattern = "lat=", replacement = ""
  )) |>
  # Use the intermediate column to fetch long
  mutate(site_loc_long = as.numeric(
    stringr::str_split(site_loc_nonlat, pattern = ", alt=", n = 2)[[1]][1]
  )) |>
  dplyr::relocate(site_loc_long, .after = long) |>
  dplyr::relocate(site_loc_lat, .after = long) |>
  # Remove rowwise groupings
  # Re-integrate these into the lat/long columns
  mutate(
    lat = case_when(
      is.na(lat) & !is.na(site_loc_lat) ~ site_loc_lat,
      is.na(site_loc_lat) & is.na(lat) ~ NA,
      TRUE ~ as.numeric(lat)
    ),
    long = case_when(
      is.na(long) & !is.na(site_loc_long) ~ site_loc_long,
      is.na(site_loc_long) & is.na(long) ~ NA,
      TRUE ~ as.numeric(long)  
    )
  ) |>
  mutate(
    year = lubridate::year(date),
    month = lubridate::month(date)
  )
View(post2022_alldata)
# NAs introduced by coercion expected here in the lat/long pulling steps
# Now the only lat-longs that are NA are those without site_location information
# message(paste0(colnames(post2022_alldata), sep = "; "))

# Write out data: sites by years
# sink("dataCompilation/KelpQuadratDataPost2022_SiteByYearTable.txt")
# knitr::kable(table(post2022_alldata$site_id, post2022_alldata$year, useNA = "ifany"))
# sink()

### Other "low hanging" fruit: WWK 2020 AND 2021 data
wwk_2020_2021_file <- grep(allfiles, pattern = "Wei Wai Kum_Bull Kelp Field Data_2020-2021",
  value = T)

wwk_2020_2021 <- list(
  # Sheet 1
  site_info = readxl::read_excel(wwk_2020_2021_file, sheet = "Site Information") |>
    janitor::clean_names() |>
    rename(
      site_id = site,
      site_location = location,
      lat = site_latitude,
      long = site_longitude_nb_negative,
      weather = site_conditions,
      sea_urchin_observations = urchins,
      tides_and_currents_low_tide_height_m = low_tide_m,
      tides_and_currents_time_at_low_tide = time_low_tide,
      tides_and_currents_survey_start_time = survey_start_time,
      tides_and_currents_tide_height_at_survey_start_m = survey_start_tide_m,
      tides_and_currents_tide_height_at_survey_end = survey_end_tide_m,
      survey_notes = notes,
      submissiondatetime = date_data_entered
      # username = data_entry_by
    ),
  # Sheet 2
  perimeter_info = readxl::read_excel(wwk_2020_2021_file, sheet = "Perimeter Data") |>
    janitor::clean_names() |>
    rename(
      site_id = site,
      map_the_kelp_bed_with_gps_points_gps_point_number = point,
      map_the_kelp_bed_with_gps_points_kelp_bed_map_gps_point = gps
    ),
  # Sheet 3
  quadrats = readxl::read_excel(wwk_2020_2021_file, sheet = "Quadrat Data") |>
    janitor::clean_names() |>
    rename(
      site_id = site,
      quadrat_data_bull_kelp_kelp_bulb_count = nereocystis_sfc_bulbs
    ),
  # Sheet 4
  diameter_data = readxl::read_excel(wwk_2020_2021_file, sheet = "Nereocystis diameter data") |>
    janitor::clean_names() |>
    group_by(quadrat, date, site) |>
    summarize(
      quadrat_data_bull_kelp_stipe_diameter_measurements = paste0(
        nereocystis_sfc_bulb_diameter_mm, collapse = ", "
      )
    ) |> 
    ungroup() |>
    rename(site_id = site),
  # Sheet 5
  oceanographic = readxl::read_excel(wwk_2020_2021_file, sheet = "Oceanographic Data") |>
      janitor::clean_names() |>
      mutate(site = case_when(
        site == "K03" ~ "CR-K03",
        TRUE ~ site
      )) |>
      group_by(date, site) |>
      pivot_wider(names_from = depth_m, values_from = c(temp, salinity)) |>
      rename(
        site_id = site,
        oceanographic_data_secchi_disk_depth_m = secchi_disk_m,
        oceanographic_data_temperature_at_1m = temp_1,
        oceanographic_data_temperature_at_5m = temp_5,
        oceanographic_data_temperature_at_10m = temp_10,
        oceanographic_data_salinity_at_1m = salinity_1, 
        oceanographic_data_salinity_at_5m = salinity_5, 
        oceanographic_data_salinity_at_10m = salinity_10 
      )
)

wwk_2020_2021_df <- wwk_2020_2021$quadrats |>
  left_join(wwk_2020_2021$diameter_data, by = c("site_id", "date", "quadrat"))

# Perimeter data messes up the data structure; turns into one row per point 
  # whereas the other post-2022 data includes one row per quad.
wwk_2020_2021_df <- wwk_2020_2021_df |> 
  # left_join(wwk_2020_2021$perimeter_info |>
  #   rename(perimeter_depth_m = depth_m) |>
  #   group_by(site_id, date) |>
  #   summarize()
  #   , by = c("site_id", "date")) |>
  left_join(wwk_2020_2021$site_info, by = c("site_id", "date")) |>
  left_join(wwk_2020_2021$oceanographic, by = c("site_id", "date")) |>
  mutate(site_prefix = stringr::str_split(site_id, "-")[[1]][1])

post2022_alldata_v2 <- plyr::rbind.fill(wwk_2020_2021_df, post2022_alldata)
post2022_alldata_v2

post2022_alldata_v2$date <- parsedate::parse_date(post2022_alldata_v2$date)
post2022_alldata_v2$year <- lubridate::year(post2022_alldata_v2$date)

lubridate::date(post2022_alldata_v2$tides_and_currents_time_at_low_tide) <- 
  lubridate::date(post2022_alldata_v2$tides_and_currents_survey_start_time) <- 
  lubridate::date(post2022_alldata_v2$time_low_tide_mf_tidal_charts) <- 
  lubridate::date(post2022_alldata_v2$tides_and_currents_time_at_low_tide) <- 
  post2022_alldata_v2$date

# Function to calculate mean stipe diameter
post2022_alldata_v2$avg_stipe_diameter_mm <- avgStipeDiameter(
  post2022_alldata_v2$quadrat_data_bull_kelp_stipe_diameter_measurements
)

saveRDS(post2022_alldata_v2, file = "data/tier2_quadratdata/preliminaryCompiledTier2Data.Rds")

hist(post2022_alldata_v2$avg_stipe_diameter_mm)

# Wow, seems almost normally distributed...? I'm happily shocked!

# mama_2021_file <- grep(allfiles, pattern = "Mama Kelp Survey Tier 2", value = T)

# mama_2021_df <- readxl::read_excel(mama_2021_file, sheet = "Sheet1") |>
#   janitor::clean_names() |>
#   group_by(site_id, date) |>
#   pivot_longer(cols = transect_locations_transect_1_start_location_t1a:quadrat_data_bull_kelp_q50_bryozoan_cover,
#   names_to = c("quadrat", "kelp_bulbs", ""), 
#   names_pattern = "quadrat_data_bull_kelp_q(.*)_(.*)"
# )

# PLOTTING -------------------------------------------------------------------------------

library(ggplot2)

theme_custom <- theme(
  text = element_text(size = 12),
  panel.grid = element_blank(),
  panel.grid.major.y = element_line(colour = "#e3e1e1"),
  plot.title.position = 'plot',
  legend.position = 'bottom',
  legend.title = element_blank()
  )
  
theme_set(theme_bw() + theme_custom)

# post2022_alldata_v2 <- post2022_alldata_v2 |>
#   group_by(site_id, date, year, site_prefix, .drop = FALSE) |>
#   mutate(avg_quadrat_data_bull_kelp_kelp_bulb_count = mean(
#     as.numeric(quadrat_data_bull_kelp_kelp_bulb_count), na.rm = TRUE
#   )) |>
#   ungroup() |>
#   group_by(site_id, year) |>
#   mutate(avg_avg_bulb_count = mean(
#     avg_quadrat_data_bull_kelp_kelp_bulb_count,
#     na.rm = T)) |>
#   mutate(year = factor(year, levels = c(2020:2024))) |>
#   ungroup()
#   # group_by(site_prefix, site_id) |>

post2022_alldata_v2_plot <- post2022_alldata_v2 |>
  group_by(site_id, site_prefix, year = factor(year, levels = c(2019:2025))) |>
  summarize(
    avg_kelpcount = mean(
      as.numeric(quadrat_data_bull_kelp_kelp_bulb_count), na.rm = T),
    lwr_kelpcount = quantile(as.numeric(quadrat_data_bull_kelp_kelp_bulb_count), na.rm = T, probs = 0.05),
    upr_kelpcount = quantile(as.numeric(quadrat_data_bull_kelp_kelp_bulb_count), na.rm = T, probs = 0.95),
    avg_avg_stipe_diameter = mean(
      avg_stipe_diameter_mm, na.rm = T
    ),
    lwr_avg_stipe_diameter = quantile(avg_stipe_diameter_mm, na.rm = T, probs = 0.05),
    upr_avg_stipe_diameter = quantile(avg_stipe_diameter_mm, na.rm = T, probs = 0.95),
  n = n()) |>
  ungroup()
  

bulbdensity_site_plots <- list()
for (p in unique(post2022_alldata_v2_plot$site_prefix)) {
  message(p)
# }
  bulbdensity_site_plots[[p]] <- ggplot(post2022_alldata_v2_plot |>
      filter(site_prefix == p), 
      aes(x = year, 
        y =avg_kelpcount,
        color = site_id)) +
      ggtitle(p) + 
      geom_point() +
      geom_errorbar(aes(ymin = lwr_kelpcount, ymax = upr_kelpcount), width = 0, alpha = 0.2) + 
      geom_line(aes(group = site_id), lwd = 1.5) + 
      # geom_line(aes(y = mean(as.numeric(quadrat_data_bull_kelp_kelp_bulb_count))), lwd = 2) + 
      facet_wrap(~site_id, axes = "all_x") + 
      geom_text(aes(y = upr_kelpcount + 0.7, label = 
        paste0(n)), color = "grey50") +
    labs(x = "Year", y = "Average kelp density diameter (bulbs per quadrat)") +
    # theme_classic() + 
    expand_limits(y = 0) +
    theme(axis.text.x = element_text(angle = 90))
}

bulbdensity_site_plots[[1]] + theme(legend.position = "none")
bulbdensity_site_plots[[2]]
bulbdensity_site_plots[[3]]
bulbdensity_site_plots[[4]]

stipediameter_site_plots <- list()
for (p in unique(post2022_alldata_v2_plot$site_prefix)) {
  message(p)
# }
  stipediameter_site_plots[[p]] <- ggplot(post2022_alldata_v2_plot |>
      filter(site_prefix == p), 
      aes(x = year, 
        y =avg_avg_stipe_diameter,
        color = site_id)) +
      ggtitle(p) + 
      geom_point() +
      geom_errorbar(aes(ymin = lwr_avg_stipe_diameter, ymax = upr_avg_stipe_diameter), width = 0, alpha = 0.2) + 
      geom_line(aes(group = site_id), lwd = 1.5) + 
      # geom_line(aes(y = mean(as.numeric(quadrat_data_bull_kelp_kelp_bulb_count))), lwd = 2) + 
      facet_wrap(~site_id) + 
      geom_text(aes(y = upr_avg_stipe_diameter + 0.5, label = paste0("n:", n)), color = "black") +
      # geom_line(aes(y = mean(as.numeric(quadrat_data_bull_kelp_kelp_bulb_count))), lwd = 2) + 
      facet_wrap(~site_id) + 
    theme_classic() + 
    labs(x = "Year", y = "Average stipe diameter (mm)") +
    expand_limits(x = 0) +
    theme(axis.text.x = element_text(angle = 90))
}
stipediameter_site_plots[[1]]
stipediameter_site_plots[[2]]
stipediameter_site_plots[[3]]
stipediameter_site_plots[[4]]

site_by_year_by_gps <- post2022_alldata_v2 |>
  group_by(site_id, lat, long) |>
  dplyr::slice_head(n = 1) |>
  select(site_id, date, lat, long)

write.csv(site_by_year_by_gps,
  "data/quadratDataLatLongByDate_post2022.csv"
)
