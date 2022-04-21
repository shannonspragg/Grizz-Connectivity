# Prepping our Rasters: -------------------------------------------------------
  # Here we bring and produce in our predictor rasters and make sure they are all extended to ONA and equally projected:

# Load Packages: ----------------------------------------------------------
library(sf)
library(tidyverse)
library(dplyr)
library(raster)
library(terra)
library(dismo)
library(stars)
library(measurements)

# Load Data: --------------------------------------------------------------

  # Grizzinc:  UPDATE THIS WITH NEW DATA
grizzinc.rast <- terra::rast("/Users/shannonspragg/Grizz-Connectivity/Data/original/grizz.increase.map.fixed.tif") 

  # Bear Density - Bear Habitat Suitability (BHS):
bhs.rast <- rast("/Users/shannonspragg/Grizz-Connectivity/Data/original/grizz_dens.tif")

  # Biophysical Current Map (Cumulative current flow shows the total current for each landscape pixel):
biophys.rast <- rast("/Users/shannonspragg/Grizz-Connectivity/Data/original/cum_currmap.tif") 

  # SOI Region for plotting:
ona.bound <- st_read("/Users/shannonspragg/Grizz-Connectivity/Data/original/ONA_TerritoryBound.shp") 
ona.rast <- rast("/Users/shannonspragg/Grizz-Connectivity/Data/processed/ona_bound.tif")

# PA and Metro Data: (need to be cropped)
ona.PAs <- st_read("/Users/shannonspragg/Grizz-Connectivity/Data/original/ona_PAs.shp") 

# Extent Grizzly Populations:
extent.grizz <- st_read("/Users/shannonspragg/Grizz-Connectivity/Data/processed/Extent Grizzly Pop Units.shp")


