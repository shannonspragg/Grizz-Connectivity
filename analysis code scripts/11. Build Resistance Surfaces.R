
# prep biophys resist surface ---------------------------------------------

# Load packages:
library(terra)
library(sf)
library(tidyverse)
library(raster)

# Data:
bhs.ona <- rast("Data/processed/bhs_ONA.tif")
ghm1 <- rast("Data/original/gHMv1_300m_2017_static/gHMv1_300m_2017_static-0000000000-0000000000.tif")
ghm2 <- rast("Data/original/gHMv1_300m_2017_static/gHMv1_300m_2017_static-0000046592-0000000000.tif")

ona.bound <- st_read("Data/original/ONA_TerritoryBound.shp") %>% 
  st_buffer(., 100000) %>% st_transform(., crs=crs(bhs)) %>% 
  as(., "SpatVector")

ghm1.crp <- project(ghm1, bhs.ona)
ghm2.crp <- project(ghm2, bhs.ona)
ghm.mos <- mosaic(ghm1.crp, ghm2.crp, fun="max")
ghm.conv <- ghm.mos/65536

#ghm.crop <- crop(ghm.conv, project(ona.bound, ghm.conv))
elev.can <- rast(raster::getData('alt', country = 'CAN'))
elev.us <- rast(raster::getData('alt', country = 'USA')[[1]])
elev.can.crop <- crop(elev.can, project(ona.bound, elev.can))
elev.us.crop <- crop(elev.us, project(ona.bound, elev.us))
elev.mos <- mosaic(elev.can.crop, elev.us.crop, fun="mean")

rough <- terrain(elev.mos, v="TRI")
rough.max <-  global(rough, "max", na.rm=TRUE)[1,]
rough.min <-  global(rough, "min", na.rm=TRUE)[1,]
rough.rescale <- (rough - rough.min)/(rough.max - rough.min)
rough.proj <- project(rough.rescale, bhs.ona)

fuzzysum2 <- function(r1, r2) {
  rc1.1m <- (1-r1)
  rc2.1m <- (1-r2)
  fuz.sum <- 1-(rc1.1m*rc2.1m)
}
# Add together our biophys attributes: gHM and roughness
biophys_fuzsum <- fuzzysum2(ghm.conv, rough.proj)
writeRaster(biophys_fuzsum,"Data/processed/biophys_fuzsum_ona.tif" )
biophys_resistance <- (1+biophys_fuzsum)^10
writeRaster(biophys_resistance, "Data/processed/biophys_resist_ona.tif")


# Add prob conflict for biophys + social surface --------------------------

prob.bear.conf <- rast("Data/processed/prob_conflict_bears_ONA.tif")

fuzzysum3 <- function(r1, r2, r3) {
   rc1.1m <- (1-r1)
   rc2.1m <- (1-r2)
   rc3.1m <- (1-r3)
   fuz.sum <- 1-(rc1.1m*rc2.1m*rc3.1m)
 }
# # Add together our biophys attributes + grizz inc resist: gHM, and roughness + grizz resist
bio_social_fuzzysum <- fuzzysum3(ghm.conv, rough.proj, prob.bear.conf)
writeRaster(bio_social_fuzzysum, "Data/processed/biosocial_fuzsum.tif",overwrite=TRUE)
biosocial_resistance <- (1+bio_social_fuzzysum)^10
writeRaster(biosocial_resistance, "Data/processed/biosocial_resist.tif", overwrite=TRUE)
