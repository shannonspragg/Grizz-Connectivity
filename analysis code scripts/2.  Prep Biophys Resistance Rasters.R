# Prepping Biophys Resistance Rasters: ------------------------------------
  ## This is our second script, where we prep the biophysical raster input for our omniscape model, as well as
  # our grizz increase raster for the next script.


# Load Packages: ----------------------------------------------------------
library(here)
library(terra)
library(sf)
library(rgdal)
library(raster)
library("sp")
library(ggmap)
library(maptools)
library(viridis)

# Bring in Data: ----------------------------------------------------------
  #Creating the raster file objects pulled down from the data folder
ona_bdry <- st_read(here("Data/original/ONA_TerritoryBound.shp")) 
griz_dens <- rast(here("Data/original/grizz_dens.tif"))
hii <- rast("Data/original/hii_n_amer/")
hmi <- rast("Data/original/gHMv1_300m_2017_static/gHMv1_300m_2017_static-0000000000-0000000000.tif")

  # Obtain the elevation values for CAN and US, merge them together
elev.can <- rast(raster::getData('alt', country = 'CAN'))
elev.us <- rast(raster::getData('alt', country = 'USA')[[1]])
elev <- mosaic(elev.can, elev.us)

  # Import the grizzly resistance file
grizz.inc.bc <- rast("Data/original/grizz.increase.map.fixed.tif") #  the proportion of people within a census that 
grizz.inc.wa <- rast("Data/original/griz.increase.wa.tif") 


  # Reproject the ONA shapefile boundary
griz_proj <- terra::project(griz_dens, hmi)
ona_proj.sp <- ona_bdry %>% st_transform(., crs(griz_proj)) %>% st_buffer(., dist=5000) %>% as(., "Spatial")
ona_proj.vec <- vect(ona_proj.sp)

ona_proj <- ona_bdry %>% st_transform(., crs(griz_dens)) %>% st_buffer(., dist=5000) %>% as(., "Spatial")
ona_proj.vect <- vect(ona_proj)


# Prep Other Rasters: -----------------------------------------------------

  # Expand grizz_dens extent:
griz.ext <- terra::extend(griz_proj, ona_proj.vec, filename=here("data/processed/griz_ext.tif"), overwrite=TRUE)
#griz.ext[is.nan(griz.ext)] <- 0

  # Project & Crop HMI:
hmi.crop <- crop(hmi, ona_proj.vec)
grizz.crop <- crop(griz.ext, ona_proj.vec)

  # Apply Mean to BHS NA's:
# Replace NA Values with Mean for BHS:
griz.raster <- grizz.crop %>% raster() # change to raster for cellstats
cellStats(griz.raster, mean) #Find the mean --> [1] 0.01646741

griz.crop.mean <- classify(grizz.crop, cbind(NA, 0.01646741))

  # Rescale HMI:
hmi.rescale <- hmi.crop / 65536

  # Project & Crop Elev:
elev.proj <- terra::project(elev, griz.ext)
elev.crop <- crop(elev.proj, ona_proj.vec)
rough <- terrain(elev.crop, v="TRI")
rough.max <-  global(rough, "max", na.rm=TRUE)[1,]
rough.min <-  global(rough, "min", na.rm=TRUE)[1,]
rough.rescale <- (rough - rough.min)/(rough.max - rough.min)
rough.rescale[rough.rescale==0] <- 0.000000001
rough.rescale[is.nan(rough.rescale)] <- 1


# # Prep Grizzinc Raster: ------------------------------------------------------
# ona_proj <- ona_bdry %>% st_transform(., crs(grizz.inc.bc)) %>% st_buffer(., dist=5000) %>% as(., "Spatial")
# ona_proj.vect <- vect(ona_proj)
# 
#   # Combine GrizzInc Maps:
# grizz.inc.combine <- terra::merge(grizz.inc.bc, grizz.inc.wa)
# grizzinc.resample <- terra::resample(grizz.inc.combine, hmi.crop)
# 
# # Project grizz resistance:
# griz.resist <- 1-grizzinc.resample
# griz.resist[is.nan(griz.resist)] <- 1

# Fuzzysum Our Rasters: ---------------------------------------------------

  # Fuzzy sum approach to combine them from Theobald 2013:
fuzzysum2 <- function(r1, r2) {
  rc1.1m <- (1-r1)
  rc2.1m <- (1-r2)
  fuz.sum <- 1-(rc1.1m*rc2.1m)
}
  # Add together our biophys attributes: gHM and roughness
biophys_fuzsum <- fuzzysum2(hmi.rescale, rough.rescale)
plot(biophys_fuzsum, col=plasma(256), axes = TRUE, main = "BHS+gHM Resistance Layer")

# fuzzysum3 <- function(r1, r2, r3) {
#   rc1.1m <- (1-r1)
#   rc2.1m <- (1-r2)
#   rc3.1m <- (1-r3)
#   fuz.sum <- 1-(rc1.1m*rc2.1m*rc3.1m)
# }
# # Add together our biophys attributes + grizz inc resist: gHM, and roughness + grizz resist
# bio_social_fuzzysum <- fuzzysum3(hmi.rescale, rough.rescale, griz.resist)

  # Make into resistance surface
biophys_resistance <- (1+biophys_fuzsum)^10
plot(biophys_resistance, col=plasma(256), axes = TRUE, main = "Biophysical Resistance Layer")


# Reproject other Raster to BC Albers: ---------------------------------------
biophys.resist.reproj <- terra::project(biophys_resistance, griz_dens)
grizz.crop.reproj <- terra::project(griz.crop.mean, griz_dens)

biophys.resist.crop <- crop(biophys.resist.reproj, ona.proj.vect)
grizz.crop <- crop(grizz.crop.reproj, ona.proj.vect)

writeRaster(grizz.inc.combine, "Data/processed/grizz_inc_comb.tif")
writeRaster(rough.rescale, filename=here("data/processed/rough_rescale.tif"), overwrite=TRUE)
writeRaster(hmi.rescale, filename=here("data/processed/hmi_resist.tif"), overwrite=TRUE)
  # Omniscape Inputs:
writeRaster(grizz.crop, filename=here("data/processed/griz_source_ona.tif"), overwrite=TRUE) # source input
writeRaster(biophys.resist.crop, filename=here("data/processed/biophys_resist.tif"), overwrite=TRUE) # resistance input

