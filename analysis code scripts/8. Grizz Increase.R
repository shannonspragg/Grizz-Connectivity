
# get the griz increase data ----------------------------------------------

# Load packages:
library(terra)

# Bring in our MRP data from the survey:
griz.inc.can <- rast('Data/original/griz.increase.bcab.update.tif')
griz.inc.us <- rast('Data/original/griz.increase.waid.update.tif')

griz.inc.all <- terra::mosaic(griz.inc.can, griz.inc.us)

#load template and crop
dist2met <- rast("Data/processed/dist2met_km_ONA.tif")

griz.ONA <- project(griz.inc.all, dist2met)

writeRaster(griz.ONA, "Data/processed/griz_inc_ONA.tif")
