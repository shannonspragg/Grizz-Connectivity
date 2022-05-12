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
biophys.normalized.cs <- rast("/Users/shannonspragg/Grizz-Connectivity/Data/original/biophysical_normalized_cum_currmap.tif")

  # Social + Biophysical Omniscape Map: (Social values of grizzlies) NEED UPDATED
social.bio.normalized <- rast("/Users/shannonspragg/Grizz-Connectivity/Data/original/social_biophys_normalized_cum_currmap.tif")

  # Probability of Bear Conflict Omniscape Map: NEED UPDATED
prob.bear.conf.normalized <- rast("/Users/shannonspragg/Grizz-Connectivity/Data/original/p_conflict_normalized_cum_currmap.tif")


  # ONA Template Raster:
ona.template <- rast("/Users/shannonspragg/Grizz-Connectivity/Data/processed/ona_bound.tif")
ona.bound <- st_read("/Users/shannonspragg/Grizz-Connectivity/Data/original/ONA_TerritoryBound.shp") 
ona.reproj <- st_transform(ona.bound, st_crs(crs(ona.template)))
ona.vect <- vect(ona.reproj)

# Resample Rasters: -------------------------------------------------------
biophys.ona.rsample <- resample(biophys.normalized.cs, ona.template)
social.biophys.ona.rsample <- resample(social.bio.normalized, ona.template)
prob.conf.ona.rsample <- resample(prob.bear.conf.normalized, ona.template)

# Mask Rasters: -----------------------------------------------------------
biophys.normalized.ona <- terra::mask(biophys.ona.rsample, ona.template) 
social.biophys.normalized.ona <- terra::mask(social.biophys.ona.rsample, ona.template) 
prob.conf.normalized.ona <- terra::mask(prob.conf.ona.rsample, ona.template)


# Save Cropped Rasters: ---------------------------------------------------
writeRaster(biophys.normalized.ona, "/Users/shannonspragg/Grizz-Connectivity/Data/processed/biophys_normalized_ona.tif", overwrite=TRUE)
writeRaster(social.biophys.normalized.ona, "/Users/shannonspragg/Grizz-Connectivity/Data/processed/social_biophys_normalized_ona.tif", overwrite=TRUE)
writeRaster(prob.conf.normalized.ona, "/Users/shannonspragg/Grizz-Connectivity/Data/processed/p_conflict_normalized_ona.tif", overwrite=TRUE)


# Plot the Outputs: -------------------------------------------------------

plot(biophys.normalized.ona, col=plasma(256), axes = TRUE, main = "Biophysical Normalized Connectivity Map")

plot(social.biophys.normalized.ona, col=plasma(256), axes = TRUE, main = "Social Values + Biophysical Normalized Connectivity Map")

plot(prob.conf.normalized.ona, col=plasma(256), axes = TRUE, main = "Probability of Bear Conflict Normalized Connectivity Map")


# Make Zoomed In Maps for Figure 5: ---------------------------------------
  # Bring in Census Data:
can.census.regions <- st_read("/Users/shannonspragg/Grizz-Connectivity/Data/original/lcd_000b16a_e.shp")

bc.census.regions <- can.census.regions %>%
  filter(., PRNAME == "British Columbia / Colombie-Britannique") %>%
  st_make_valid()

okanagan.census.regions <- bc.census.regions %>%
  filter(.,  CDUID == "5907" | CDUID == "5905" | CDUID == "5933" | CDUID == "5935" | CDUID == "5937")

  # Reproject:
census.zoom.reproj <- st_transform(okanagan.census.regions, st_crs(crs(ona.template)))
census.zoom.vect <- vect(census.zoom.reproj)

  # Crop Rasters to this Area:
biophys.zoom <- terra::mask(biophys.normalized.ona, census.zoom.vect) 


plot(biophys.zoom, col=plasma(256), axes = TRUE, main = "Biophysical Normalized Connectivity Map")
