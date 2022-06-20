# Format ONA Connectivity Raster Maps: ------------------------------------
  ## Here, we will be bringing in our omniscape outputs for the three resistance layers that we ran analyses on.
  ## We will be cropping these down to the ONA extent, masking them to just the ONA territory outline, and
  ## making nice maps with them.


# Load Packages: ----------------------------------------------------------
library(sf)
library(tidyverse)
library(dplyr)
library(raster)
library(terra)
library(dismo)
library(stars)
library(measurements)
library(viridis)

# Bring in Data: ----------------------------------------------------------
  # Biophysical Omniscape Map:
#biophys.normalized.cs <- rast("Data/processed/biophysical_normalized_cum_currmap.tif")
biophys.flow.potential <- rast("Data/processed/biophysical_flow_potential.tif")
biophys.cum.current <- rast("Data/processed/biophysical_cum_currmap.tif")

  # Probability of Bear Conflict Omniscape Map: NEED UPDATED
#prob.bear.conf.normalized <- rast("Data/processed/p_conflict_normalized_cum_currmap.tif")
prob.conf.flow.potential <- rast("Data/processed/p_conflict_flow_potential.tif")
prob.conf.cum.current <- rast("Data/processed/p_conflict_cum_currmap.tif")

  # ONA Template Raster:
ona.template <- rast("Data/processed/ona_bound.tif")
ona.bound <- st_read("Data/original/ONA_TerritoryBound.shp") 
ona.reproj <- st_transform(ona.bound, st_crs(crs(ona.template)))
ona.vect <- vect(ona.reproj)


# Normalize Probability of Conflict: --------------------------------------------
# Here we divide the raw biophys+prob conflict by biophys flow potential:
biophys.normalized <- biophys.cum.current / biophys.flow.potential
prob.conf.normalized <- prob.conf.cum.current / biophys.flow.potential # normalize this by biophys for baseline comparison


# Resample Rasters: -------------------------------------------------------
biophys.rsample <- resample(biophys.normalized, ona.template)
biophys.curmap.rsample <- resample(biophys.cum.current, ona.template)

prob.conf.rsample <- resample(prob.conf.normalized, ona.template)
prob.conf.cum.curmap.rsample <- resample(prob.conf.cum.current, ona.template)

# Mask Rasters: -----------------------------------------------------------
biophys.normalized.ona <- terra::mask(biophys.rsample, ona.template) 
biophys.cum.current.ona <- terra::mask(biophys.curmap.rsample, ona.template) 

prob.conf.normalized.ona <- terra::mask(prob.conf.rsample, ona.template)
prob.conf.cum.curmap.ona <- terra::mask(prob.conf.cum.curmap.rsample, ona.template)


# Plot the Outputs: -------------------------------------------------------

plot(biophys.normalized.ona, col=plasma(256), axes = TRUE, main = "Biophysical Normalized Connectivity Map")
plot(biophys.cum.current.ona, col=plasma(256), axes = TRUE, main = "Biophysical Cumulartive Connectivity Map")

plot(prob.conf.normalized.ona, col=plasma(256), axes = TRUE, main = "Probability of Bear Conflict Normalized Connectivity Map")
plot(prob.conf.cum.curmap.ona, col=plasma(256), axes = TRUE, main = "Probability of Bear Conflict Cumulative Connectivity Map")


# Calculate Difference of Biophys and Conflict: ---------------------------
# p.conf.diff <- prob.conf.normalized.ona - biophys.normalized.ona
# biophys.diff <- biophys.normalized.ona - prob.conf.normalized.ona

p.conf.diff <- prob.conf.cum.curmap.ona - biophys.cum.current.ona

# Save Cropped Rasters: ---------------------------------------------------
writeRaster(biophys.normalized.ona, "Data/processed/biophys_normalized_ona.tif", overwrite=TRUE)
writeRaster(biophys.cum.current.ona, "Data/processed/biophys_cum_current_ona.tif", overwrite=TRUE)

writeRaster(prob.conf.normalized.ona, "Data/processed/p_conflict_normalized_ona.tif", overwrite=TRUE)
writeRaster(prob.conf.cum.curmap.ona, "Data/processed/p_conflict_cum_current_ona.tif", overwrite=TRUE)
writeRaster(p.conf.diff, "Data/processed/p_conflict_diff_ona.tif", overwrite=TRUE)


# # Make Zoomed In Maps: ---------------------------------------
#   # Bring in Census Data:
# can.census.regions <- st_read("Data/original/lcd_000b16a_e.shp")
# 
# bc.census.regions <- can.census.regions %>%
#   filter(., PRNAME == "British Columbia / Colombie-Britannique") %>%
#   st_make_valid()
#   # Filter to the Census Regions we want:
# okanagan.census.regions <- bc.census.regions %>%
#   filter(.,  CDUID == "5907" | CDUID == "5905" | CDUID == "5933" | CDUID == "5935" | CDUID == "5937")
# st_write(okanagan.census.regions, "Data/processed/okanagan_census_crop.shp")
#   
#   # Reproject:
# census.zoom.reproj <- st_transform(okanagan.census.regions, st_crs(crs(ona.template)))
# census.zoom.vect <- vect(census.zoom.reproj)
# 
#   # Crop Rasters to this Area:
# biophys.zoom <- terra::mask(biophys.normalized.ona, census.zoom.vect) 
# social.biophys.zoom <- terra::mask(social.biophys.normalized.ona, census.zoom.vect) 
# p.conf.zoom <- terra::mask(prob.conf.normalized.ona, census.zoom.vect) 
# 
# 
# plot(biophys.zoom, col=plasma(256), axes = TRUE, main = "Biophysical Normalized Connectivity Map")
# plot(social.biophys.zoom, col=plasma(256), axes = TRUE, main = "Social Biophysical Normalized Connectivity Map")
# plot(p.conf.zoom, col=plasma(256), axes = TRUE, main = "Probability of Bear Conflict Normalized Connectivity Map")
# 
# 
# # Save our Cropped Outputs: -----------------------------------------------
# writeRaster(biophys.zoom, "Data/processed/biophys_zoom.tif", overwrite=TRUE)
# writeRaster(social.biophys.zoom, "Data/processed/social_biophys_zoom.tif", overwrite=TRUE)
# writeRaster(p.conf.zoom, "Data/processed/p_conflict_zoom.tif", overwrite=TRUE)


