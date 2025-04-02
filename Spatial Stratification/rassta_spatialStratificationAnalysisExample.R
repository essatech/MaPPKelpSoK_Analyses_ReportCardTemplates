# FILE: rassta kelp analysis
# PURPOSE: This script performs stratification of an example Sub-region, Haida Gwaii,
# using the rassta R package. Two approaches are shown for example: strata created using
# pre-defined categories, and strata created using algorithmic detection of the optimal
# number of bins.
# The ultimate goal of this stratification work is to increase the amount of information
# that could be made available by monitoring sites with under-represented environmental
# conditions.
# AUTHOR: Mairin Deith
# DATE CREATED: Mar 30, 2025
# LAST MODIFIED: Apr 1, 2025

# Spatial manipulation and stratification libraries
library(rassta)
library(terra)
library(sf)
library(gstat) # For IDW 
# Plotting libraries
library(stars)
library(ggplot2)
# Others: Data download and manipulation
library(dplyr)
library(tidyterra) # Allows for ggplot2 style plotting of terra SpatRasters
library(rerddap) # SST download
library(lubridate) # Date parsing

# USE HAIDA GWAII AS AN EXAMPLE

# STEP 0: Prepare spatial layers
# STACKED URCHIN SDMs: Value represents the maximum likelihood of occurance
#   considering green, red, purple urchins
data_dir <- file.path("../../../Data/Urchin, exposure, SST layers")
urchins_hg <- rast(file.path(data_dir, "Urchin SDM/hg-nearshoreSDM-datapackage/stacked_hg_urchinlayers_max.tif"))

# REI Haida Gwaii
rei_hg <- rast(file.path(data_dir, "Relative_exposure/rei_hg.tif"))
crs(rei_hg) # 3005
rei_extent <- terra::ext(rei_hg)

# DOWNLOAD SST DATA
# Use the rerddap package
# rerddap::ed_search(query = 'SST', which = "table")
# Dataset ID
dataset_id <- "jplMURSST41" #Multi-scale Ultra-high Resolution (MUR) SST Analysis fv04.1, Global, 0.01°, 2002-present,Daily

sst_data_aug17 <- rerddap::griddap(
  dataset_id,
  longitude = c(-139.04, -114.08),  # Replace with your site's longitude range
  latitude = c(48.25, 60.01),       # Replace with your site's latitude range
  time = c("2017-08-01", "2017-08-31"),  # August chosen following Starko et al. 2024
  stride = 5, # Limit the number of time slices
  fields = "analysed_sst"
)

hg_rei_bbox <- st_bbox(rei_hg) %>% 
  st_as_sfc()

sst_data_meansst_sf <- sf::st_as_sf(sst_data_aug17$data, coords = c("longitude", "latitude")) |>
  sf::st_set_crs("EPSG:4326") |>
  sf::st_transform("EPSG:3005") |>
  # Clip to the HG bounding box
  sf::st_intersection(hg_rei_bbox) |>
  mutate(datetime = lubridate::parse_date_time(time, orders = "%Y-%m-%d*%H:%M:%s")) |>
  dplyr::group_by(geometry, lubdate = lubridate::date(datetime)) |>
  summarize(meanDailySST = mean(analysed_sst)) |>
  ungroup(lubdate) |>
  summarize(meanAugSST = mean(meanDailySST))
# Save as a temporary output
# st_write(sst_data_meansst_sf, dsn = file.path("intermediateSpatialFiles", 
#   "MUR_SST_August2017MeanDailyTemperatures.shp"))

# When working through the script, the interpolation is quite time-consuming (more than 
# an hour) even when using multiple cores
run_interpolation <- FALSE

if (run_interpolation) {
  ## Interpolate point-based SST measurements onto the HG REI
  ###  Use inverse distance weighted (IDW) interpolation
  idw_fn <- gstat::gstat(id = "meanAugSST", 
    formula = meanAugSST~1, data=sst_data_meansst_sf,  nmax=7, set=list(idp = .5))

  # Custom function because I'm working with an sf object; from the terra::interpolate 
  # documentation https://search.r-project.org/CRAN/refmans/terra/html/interpolate.html
  interpolate_gstat <- function(model, x, crs, ...) {
    v <- st_as_sf(x, coords=c("x", "y"), crs=crs)
    # Predict raster values for each cell, using 8 cores
    p <- terra::predict(model, v, cores = 12, ...)
    as.data.frame(p)[,1:2]
  }

  interp_sst <- interpolate(rei_hg, idw_fn, debug.level=0, 
    fun=interpolate_gstat, crs=crs(rei_hg), index=1)
  terra::writeRaster(interp_sst, file.path("intermediateSpatialFiles", 
  "interpolatedMUR_SST_August2017MeanDailyTemperatures.tif"))
} else {
  # Read from disk
  interp_sst <- terra::rast(file.path("intermediateSpatialFiles", 
    "interpolatedMUR_SST_August2017MeanDailyTemperatures.tif"))
}
# Just in case there is a change in the extent or resolution due to interpolation; 
#   resample then mask to the REI index data (also removes cells which do not overlap
#   with the REI dataset)
interp_sst_resamp <- terra::resample(interp_sst, rei_hg)
interp_sst_mask <- terra::mask(interp_sst_resamp, rei_hg)

#-----------------------------------------------------------------------------------------
# OPTION 1: Algorithmic creation of classification units
#
# rassta allows the creation of classification units based on dimension reduction and cluster analysis.
# reduce the feature space and estimate the optimum k using four terrain variables with the som_gap

# Step 1: som_gap():  Self-Organizing Map and Selection of k
# Set seed
set.seed(963)
# Create a stacked raster of the variables to stratify by
spat.var <- c(rei_hg, urchins_hg, interp_sst_mask)

all_layer_classification <- TRUE # Create classification layers based on 3 attributes?
if (all_layer_classification) {
  # Scale variables to mean = 0 and standard deviation = 1
  spat.var.scale <- terra::scale(spat.var)

  # With terra::aggregate(), reduce spatial resolution to reduce computing time
  spat.var.scale.agg <- terra::aggregate(spat.var.scale, fact = 3)
  # Dimensionality reduction and estimation of optimum k (max k to evaluate: 9)
  spat.var.som <- som_gap(spat.var.scale.agg, K.max = 12)

  # Plot results
  terra::plot(spat.var.som$SOM, main = "SOM Codes") # Self-Organizing Map's codes
  par(mar = c(4.5, 4.5, 2, 1))
  plot(spat.var.som$SOMgap, main = "Gap Statistic") # Gap statistic
  abline(v = spat.var.som$Kopt) # Optimum k

  # Step 2: som_pam(): Rasterization of Self-Organizing Map and Partitioning Around Medoids
  # Rasterization of terrain SOM grid and terrain PAM clustering
  # Create reference SpatRaster
  rastref <- spat.var.scale.agg
  # The rasterized PAM represents the final set of classification units for a landscape factor
  spat.var.sompam <- som_pam(ref.rast = rastref, kohsom = spat.var.som$SOM,
    # Use the identified Kopt value
    k = spat.var.som$Kopt)
}

saveRDS(list(
  spat.var = spat.var,
  spat.var.sompam = spat.var.sompam,
  spat.var.som = spat.var.som,
  spat.var.scale.agg = spat.var.scale.agg), file = file.path("intermediateSpatialFiles", 
  "selfOrganizingMapOutputs_threeLayer_30032025.Rds"))
terra::writeRaster(spat.var.sompam$sompam.rast, 
  filename = file.path("intermediateSpatialFiles", 
  "selfOrganizingMapOutputs_threeLayer_30032025.tif"))
terra::writeRaster(spat.var.sompam6$sompam.rast, 
  filename = file.path("intermediateSpatialFiles", 
  "selfOrganizingMapOutputs_threeLayer_30032025_k=6.tif"))
  
  # Otherwise, generate classification units for each layer individually
bylayer <- TRUE
if (bylayer) {
  lyr.som <- list()
  lyr.sompam <- list()
  
  # Do this for each layer to create stratification units
  for (i in 1:3) {
    lyr <- spat.var[[i]]
    lyr.name <- names(lyr)
    message(lyr.name)
    lyr.scale <- terra::scale(lyr)
    lyr.agg <- terra::aggregate(lyr.scale, fact = 3)
    message("...som_gap")
    # Dimensionality reduction and estimation of optimum k (max k to evaluate: 12)
    lyr.som[[lyr.name]] <- som_gap(lyr.agg, K.max = 12, method = "globalmax")
    message("...som_pam")
    lyr.sompam[[lyr.name]] <- som_pam(ref.rast = lyr.agg, kohsom = lyr.som[[lyr.name]]$SOM,
      k = ifelse(lyr.som[[lyr.name]]$Kopt == 1, 2, lyr.som[[lyr.name]]$Kopt))
  }
  
  saveRDS(list(
    spat.var = spat.var,
    lyr.som = lyr.som,
    lyr.sompam = lyr.sompam,
    lyr.agg = lyr.agg), file = file.path("intermediateSpatialFiles", 
    "selfOrganizingMapOutputs_31032025.Rds"))
    # Lyaer 1: REI
    plot(lyr.sompam[[1]]$sompam.rast[[1]], main = "Self-Organizing Map",
         mar = c(1.5, 1.3, 1.5, 3.3), col = hcl.colors(100, "spectral", rev = TRUE))
    plot(lyr.sompam[[1]]$sompam.rast[[2]], main = "Classification Units",
         type = "classes", col = hcl.colors(lyr.som[[1]]$Kopt, "spectral", rev = TRUE),
         mar = c(1.5, 2, 1.5, 2.5))
    # Layer 3: SST
    plot(lyr.sompam[[3]]$sompam.rast[[1]], main = "Self-Organizing Map",
          mar = c(1.5, 1.3, 1.5, 3.3), col = hcl.colors(100, "spectral", rev = TRUE))
    plot(lyr.sompam[[3]]$sompam.rast[[2]], main = "Classification Units",
         type = "classes", 
         col = hcl.colors(2, "spectral", rev = TRUE),
         mar = c(1.5, 2, 1.5, 2.5))
    plot(lyr.sompam[[3]]$sompam.rast[[1]], main = "Self-Organizing Map",
         mar = c(1.5, 1.3, 1.5, 3.3), col = hcl.colors(100, "spectral", rev = TRUE))
    plot(lyr.sompam[[3]]$sompam.rast[[2]], main = "Classification Units",
         type = "classes", col = hcl.colors(lyr.som[[1]]$Kopt, "spectral", rev = TRUE),
         mar = c(1.5, 2, 1.5, 2.5))
  
}

#-----------------------------------------------------------------------------------------
# OPTION 2: Simpler approach
# Pre-created quantiles of each landscape factor
# 4 categories for each of the three layers

rei_hg_quad <- rei_hg |> 
  mutate(
    q1 = quantile(rei_hg, 0, na.rm = T),
    q2 = quantile(rei_hg, 0.25, na.rm = T),
    q3 = quantile(rei_hg, 0.5, na.rm = T),
    q4 = quantile(rei_hg, 0.75, na.rm = T),
    q5 = quantile(rei_hg, 1, na.rm = T)
  ) |>
  mutate(
    quantile_REI = case_when(
      rei_hg >= q4 ~ 1L,
      rei_hg >= q3 ~ 2L,
      rei_hg >= q2 ~ 3L,
      rei_hg >= q1 ~ 4L
    )
  )

interp_sst_mask_quad <- interp_sst_mask |> 
  mutate(
    q1 = quantile(interpolatedMUR_SST_August2017MeanDailyTemperatures, 0, na.rm = T),
    q2 = quantile(interpolatedMUR_SST_August2017MeanDailyTemperatures, 0.25, na.rm = T),
    q3 = quantile(interpolatedMUR_SST_August2017MeanDailyTemperatures, 0.5, na.rm = T),
    q4 = quantile(interpolatedMUR_SST_August2017MeanDailyTemperatures, 0.75, na.rm = T),
    q5 = quantile(interpolatedMUR_SST_August2017MeanDailyTemperatures, 1, na.rm = T)
  ) |>
  mutate(
    quantile_SST = case_when(
      interpolatedMUR_SST_August2017MeanDailyTemperatures >= q4 ~ 4L,
      interpolatedMUR_SST_August2017MeanDailyTemperatures >= q3 ~ 3L,
      interpolatedMUR_SST_August2017MeanDailyTemperatures >= q2 ~ 2L,
      interpolatedMUR_SST_August2017MeanDailyTemperatures >= q1 ~ 1L
    )
  )
# plot(interp_sst_mask_quad$quantile)

urchins_hg_quad <- urchins_hg |> 
  mutate(
    q1 = quantile(stacked_hg_urchinlayers_max, 0, na.rm = T),
    q2 = quantile(stacked_hg_urchinlayers_max, 0.25, na.rm = T),
    q3 = quantile(stacked_hg_urchinlayers_max, 0.5, na.rm = T),
    q4 = quantile(stacked_hg_urchinlayers_max, 0.75, na.rm = T),
    q5 = quantile(stacked_hg_urchinlayers_max, 1, na.rm = T)
  ) |>
  mutate(
    quantile_urchin = case_when(
      stacked_hg_urchinlayers_max >= q4 ~ 4L,
      stacked_hg_urchinlayers_max >= q3 ~ 3L,
      stacked_hg_urchinlayers_max >= q2 ~ 2L,
      stacked_hg_urchinlayers_max >= q1 ~ 1L
    )
  )

kelp_layers_strata <- strata(cu.rast = c(
  urchins_hg_quad[["quantile_urchin"]],
  interp_sst_mask_quad[["quantile_SST"]],
  rei_hg_quad[["quantile_REI"]]
  ))

saveRDS(kelp_layers_strata, file.path("intermediateSpatialFiles", 
    "manualStrataMaps_31032025.Rds"))
terra::writeRaster(kelp_layers_strata$su.rast, filename = file.path("intermediateSpatialFiles", 
  "strata_presetQuantiles_01042025.tif"), overwrite = TRUE)

#-----------------------------------------------------------------------------------------
# Plot the results

# Load
algorithmic_strata <- terra::rast(file.path("intermediateSpatialFiles", 
  "selfOrganizingMapOutputs_threeLayer_30032025.tif"))
preset_strata <- terra::rast(file.path("intermediateSpatialFiles", 
    "strata_presetQuantiles_01042025.tif"))
 
# Plot the resulting stratification layers
library(bcmaps)
bc_bound_layer <- st_as_sf(bc_bound_hres())

bc_bound_layer_clip <- st_intersection(
  bc_bound_layer, st_as_sfc(st_bbox(algorithmic_strata)))

preset_quants <- ggplot() + 
  geom_sf(data = bc_bound_layer_clip) + 
  geom_spatraster(data = algorithmic_strata) + 
  scale_fill_viridis_d()

kelp_layers_strata$code.mult

spat.var.sompam$sompam.rast