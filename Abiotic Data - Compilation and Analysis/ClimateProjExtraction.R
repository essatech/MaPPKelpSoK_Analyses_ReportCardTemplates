## --- MaPP Kelp Monitoring Program ---
## Extracting future climate vraibles
## Author: Brenton Twist
## Date Created: 2025-03-03  
#-------------------------------------------------

# This script contains code for extracting future climate condions at MaPP sites
# using the following dataset
# https://open.canada.ca/data/en/dataset/33974622-3acf-4f22-86c1-09b92fa91b2d

#############################
#   Load packages & Data   ##
#############################

library(ncdf4)
library(raster)

# Open the NetCDF file
nc_temp_proj <- nc_open("./data/climatemodel/NEP36-CanOE_temp_RCP45_2046-2065_monthly.nc")

# Check the contents of the NetCDF file
print(nc_temp_proj)

names(nc_temp_proj $var)


test<- raster("nc_temp_proj",varname="temp")

# Extract the latitudes and longitudes (nav_lat and nav_lon)
latitudes <- ncvar_get(nc_temp_proj, "nav_lat")
longitudes <- ncvar_get(nc_temp_proj, "nav_lon")
latitudes <- as.vector(latitudes)
longitudes <- as.vector(longitudes)

deptht_bound<- ncvar_get(nc_temp_proj, "deptht_bounds")
# Extract the temperature data (temp)
temperature_data <- ncvar_get(nc_temp_proj, "temp")

# Check the dimensions of the temperature data
dim(temperature_data)
# Should be [x, y, deptht, t] as mentioned earlier

# Check the min and max values of latitudes and longitudes
min(latitudes)
max(latitudes)
min(longitudes)
max(longitudes)


# Example latitude and longitude (replace with the GPS point you're interested in)
lat_target <- 50.0  # Example latitude
lon_target <- -125.0  # Example longitude

# Find the row (lat) and column (lon) indices with the closest match to the target values
lat_idx <- which.min(abs(latitudes - lat_target))  # Find the closest latitude index
lon_idx <- which.min(abs(longitudes - lon_target))  # Find the closest longitude index

# Check the indices to ensure they are valid
print(paste("Latitude index: ", lat_idx))
print(paste("Longitude index: ", lon_idx))

# Ensure that lat_idx and lon_idx are within bounds
lat_idx <- min(max(lat_idx, 1), dim(latitudes)[1])  # Ensure lat_idx is within range [1, 1021]
lon_idx <- min(max(lon_idx, 1), dim(longitudes)[2])  # Ensure lon_idx is within range [1, 715]












# Example: Extract temperature for a specific latitude and longitude at the first time slice (t = 1)
lat_target <- 45.06429 # Example latitude
lon_target <- -142.2249   # Example longitude

# Find the closest lat and lon indices
lat_idx <- which.min(abs(latitudes - lat_target))
lon_idx <- which.min(abs(longitudes - lon_target))

# Extract the temperature for the closest point at time slice t = 1 and depth level 1
temperature_at_point <- temperature_data[lon_idx, lat_idx, 1, 1]  # For time slice t = 1 and deptht = 1
print(temperature_at_point)

# Close the NetCDF file
nc_close(nc_temp_proj)


