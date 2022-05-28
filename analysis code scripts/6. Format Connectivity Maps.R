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
biophys.normalized.cs <- rast("Data/original/biophysical_normalized_cum_currmap.tif")
biophys.flow.potential <- rast("Data/original/biophysical_flow_potential.tif")
biophys.cum.current <- rast("Data/original/biophys_ona_cum_currmap.tif")

  # Social + Biophysical Omniscape Map: (Social values of grizzlies) NEED UPDATED
social.bio.normalized <- rast("Data/original/social_biophys_normalized_cum_currmap.tif")
social.bio.cum.current <- rast("Data/original/social_biophys_ona_cum_currmap.tif")
social.bio.flow.potential <- rast("Data/original/social_bio_flow_potential.tif")

  # Probability of Bear Conflict Omniscape Map: NEED UPDATED
prob.bear.conf.normalized <- rast("Data/original/p_conflict_normalized_cum_currmap.tif")
prob.conf.flow.potential <- rast("Data/original/p_conflict_flow_potential.tif")
prob.conf.cum.current <- rast("Data/original/p_conflict_cum_currmap.tif")

  # ONA Template Raster:
ona.template <- rast("Data/processed/ona_bound.tif")
ona.bound <- st_read("Data/original/ONA_TerritoryBound.shp") 
ona.reproj <- st_transform(ona.bound, st_crs(crs(ona.template)))
ona.vect <- vect(ona.reproj)

# Resample Rasters: -------------------------------------------------------
biophys.ona.rsample <- resample(biophys.normalized.cs, ona.template)
biophys.flow.rsample <- resample(biophys.flow.potential, ona.template)
biophys.curmap.rsample <- resample(biophys.cum.current, ona.template)

social.biophys.ona.rsample <- resample(social.bio.normalized, ona.template)
prob.conf.ona.rsample <- resample(prob.bear.conf.normalized, ona.template)
prob.conf.cum.curmap.rsample <- resample(prob.conf.cum.current, ona.template)

# Mask Rasters: -----------------------------------------------------------
biophys.normalized.ona <- terra::mask(biophys.ona.rsample, ona.template) 
biophys.flow.potential.ona <- terra::mask(biophys.flow.rsample, ona.template) 
biophys.cum.current.ona <- terra::mask(biophys.curmap.rsample, ona.template) 

social.biophys.normalized.ona <- terra::mask(social.biophys.ona.rsample, ona.template) 
prob.conf.normalized.ona <- terra::mask(prob.conf.ona.rsample, ona.template)
prob.conf.cum.curmap.ona <- terra::mask(prob.conf.cum.curmap.rsample, ona.template)


# Normalize Probability of Conflict: --------------------------------------------
  # Here we divide the raw biophys+prob conflict by biophys flow potential:
prob.conf.difference <- prob.conf.cum.curmap.ona / biophys.flow.potential.ona


# Plot the Outputs: -------------------------------------------------------

plot(biophys.normalized.ona, col=plasma(256), axes = TRUE, main = "Biophysical Normalized Connectivity Map")
plot(biophys.cum.current.ona, col=plasma(256), axes = TRUE, main = "Biophysical Normalized Connectivity Map")

plot(social.biophys.normalized.ona, col=plasma(256), axes = TRUE, main = "Social Values + Biophysical Normalized Connectivity Map")

plot(prob.conf.normalized.ona, col=plasma(256), axes = TRUE, main = "Probability of Bear Conflict Normalized Connectivity Map")

plot(prob.conf.difference, col=plasma(256), axes = TRUE, main = "Probability of Bear Conflict Normalized Connectivity Map")

# Save Cropped Rasters: ---------------------------------------------------
writeRaster(biophys.normalized.ona, "Data/processed/biophys_normalized_ona.tif", overwrite=TRUE)
writeRaster(social.biophys.normalized.ona, "Data/processed/social_biophys_normalized_ona.tif", overwrite=TRUE)
writeRaster(biophys.cum.current.ona, "Data/processed/biophys_cum_current_ona.tif", overwrite=TRUE)


writeRaster(prob.conf.difference, "Data/processed/p_conflict_normalized_ona.tif", overwrite=TRUE)


# Make Zoomed In Maps for Figure 5: ---------------------------------------
  # Bring in Census Data:
can.census.regions <- st_read("Data/original/lcd_000b16a_e.shp")

bc.census.regions <- can.census.regions %>%
  filter(., PRNAME == "British Columbia / Colombie-Britannique") %>%
  st_make_valid()
  # Filter to the Census Regions we want:
okanagan.census.regions <- bc.census.regions %>%
  filter(.,  CDUID == "5907" | CDUID == "5905" | CDUID == "5933" | CDUID == "5935" | CDUID == "5937")
st_write(okanagan.census.regions, "Data/processed/okanagan_census_crop.shp")
  
  # Reproject:
census.zoom.reproj <- st_transform(okanagan.census.regions, st_crs(crs(ona.template)))
census.zoom.vect <- vect(census.zoom.reproj)

  # Crop Rasters to this Area:
biophys.zoom <- terra::mask(biophys.normalized.ona, census.zoom.vect) 
social.biophys.zoom <- terra::mask(social.biophys.normalized.ona, census.zoom.vect) 
p.conf.zoom <- terra::mask(prob.conf.normalized.ona, census.zoom.vect) 


plot(biophys.zoom, col=plasma(256), axes = TRUE, main = "Biophysical Normalized Connectivity Map")
plot(social.biophys.zoom, col=plasma(256), axes = TRUE, main = "Social Biophysical Normalized Connectivity Map")
plot(p.conf.zoom, col=plasma(256), axes = TRUE, main = "Probability of Bear Conflict Normalized Connectivity Map")


# Save our Cropped Outputs: -----------------------------------------------
writeRaster(biophys.zoom, "Data/processed/biophys_zoom.tif", overwrite=TRUE)
writeRaster(social.biophys.zoom, "Data/processed/social_biophys_zoom.tif", overwrite=TRUE)
writeRaster(p.conf.zoom, "Data/processed/p_conflict_zoom.tif", overwrite=TRUE)


