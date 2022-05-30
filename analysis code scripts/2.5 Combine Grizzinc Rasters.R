

# Load Packages: ----------------------------------------------------------
library(sf)
library(tidyverse)
library(raster)
library(terra)
library(dismo)
library(stars)
library(measurements)


# Bring in Data: ----------------------------------------------------------
# Import the grizzly inc files
grizz.inc.bc <- rast("Data/original/grizz.increase.map.fixed.tif") #  the proportion of people within a census that 
grizz.inc.wa <- rast("Data/original/griz.increase.wa.tif") 
ona_bdry <- st_read(here("Data/original/ONA_TerritoryBound.shp")) 
griz_dens <- rast(here("Data/original/grizz_dens.tif"))

# Prep Grizzinc Raster: ------------------------------------------------------
ona_proj <- ona_bdry %>% st_transform(., crs(grizz.inc.bc)) %>% st_buffer(., dist=5000) %>% as(., "Spatial")
ona_proj.vect <- vect(ona_proj)

# Combine GrizzInc Maps:
grizz.inc.combine <- terra::merge(grizz.inc.bc, grizz.inc.wa)

writeRaster(grizz.inc.combine, "Data/processed/grizz.inc.comb.tif", overwrite=TRUE)


