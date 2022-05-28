

# Load Packages: ----------------------------------------------------------
library(sf)
library(tidyverse)
library(dplyr)
library(raster)
library(terra)
library(dismo)
library(stars)
library(measurements)


# Bring in Data: ----------------------------------------------------------
# Import the grizzly inc files
grizz.inc.bc <- rast("Data/original/grizz.increase.map.fixed.tif") #  the proportion of people within a census that 
grizz.inc.wa <- rast("Data/original/griz.increase.wa.tif") 



# Prep Grizzinc Raster: ------------------------------------------------------
ona_proj <- ona_bdry %>% st_transform(., crs(grizz.inc.bc)) %>% st_buffer(., dist=5000) %>% as(., "Spatial")
ona_proj.vect <- vect(ona_proj)

# Combine GrizzInc Maps:
grizz.inc.combine <- terra::merge(grizz.inc.bc, grizz.inc.wa)

# Crop to ONA:
grizz.inc.crop <- terra::crop(grizz.inc.combine, ona_proj.vect)

# Reproject Raster:
ona_proj <- ona_bdry %>% st_transform(., crs(griz_dens)) %>% st_buffer(., dist=5000) %>% as(., "Spatial")
ona.proj.vect <- vect(ona_proj)

grizzinc.reproj <- terra::project(grizz.inc.crop, griz_dens)

# Resample
grizzinc.rsmple <- resample(grizzinc.reproj, griz_dens, method='bilinear')

# Crop to ONA:
grizzinc.crop <- crop(grizzinc.rsmple, ona.proj.vect)
#grizzinc.ona <- terra::mask(grizzinc.crop, ona.proj.vect)

# Project it back to match others:
grizz.resist.crop <- terra::project(grizzinc.crop, ona_proj.vec)