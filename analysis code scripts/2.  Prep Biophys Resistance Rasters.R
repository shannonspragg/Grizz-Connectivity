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
ona_bdry <- st_read(here("/Users/shannonspragg/Grizz-Connectivity/Data/original/ONA_TerritoryBound.shp")) 
griz_dens <- rast(here("/Users/shannonspragg/Grizz-Connectivity/Data/original/grizz_dens.tif"))
hii <- rast("/Users/shannonspragg/Grizz-Connectivity/Data/original/hii_n_amer/")
hmi <- rast("/Users/shannonspragg/Grizz-Connectivity/Data/original/gHMv1_300m_2017_static/gHMv1_300m_2017_static-0000000000-0000000000.tif")

  # Obtain the elevation values for CAN and US, merge them together
elev.can <- rast(raster::getData('alt', country = 'CAN'))
elev.us <- rast(raster::getData('alt', country = 'USA')[[1]])
elev <- mosaic(elev.can, elev.us)

  # Import the grizzly resistance file
grizz.inc.bc <- rast("/Users/shannonspragg/Grizz-Connectivity/Data/original/grizz.increase.map.fixed.tif") #  the proportion of people within a census that 
grizz.inc.wa <- rast("/Users/shannonspragg/Grizz-Connectivity/Data/original/griz.increase.wa.tif") 


  # Reproject the ONA shapefile boundary
ona_proj.sp <- ona_bdry %>% st_transform(., crs(griz_dens)) %>% st_buffer(., dist=5000) %>% as(., "Spatial")
ona_proj.vec <- vect(ona_proj.sp)
#griz_crop <- crop(griz_dens, ona_proj.sp)


# Create Rescale Function: ------------------------------------------------
rescale01 <- function(r1) {
  r.rescale <- (r1 - cellStats(r1, min))/(cellStats(r1, max) - cellStats(r1, min))
}


# Prep Grizzinc Raster: ------------------------------------------------------
  # GrizzInc Map:
grizz.inc.wa.reproj <- terra::project(grizz.inc.wa, crs(ona_proj.vec))  
grizz.inc.bc.reproj <- terra::project(grizz.inc.bc, crs(ona_proj.vec))  


# Resample to match: ------------------------------------------------------
grizzinc.bc.rsmple <- resample(grizz.inc.bc.reproj, griz_dens, method='bilinear')
grizzinc.wa.rsmple <- resample(grizz.inc.wa.reproj, griz_dens, method='bilinear')


# Merge our BC and WA rasters: -----------------------------------------------------
grizz.inc.comb <- terra::merge(grizzinc.bc.rsmple, grizzinc.wa.rsmple)

#griz.resist <- grizz.inc.comb

# Prep Other Rasters: -----------------------------------------------------

  # Expand grizz_dens extent:
griz.ext <- terra::extend(griz_dens, ona_proj.vec, filename=here("data/processed/griz_ext.tif"), overwrite=TRUE)
#griz.crop <- crop(griz.ext, ona_proj.vec)
griz.ext[is.nan(griz.ext)] <- 0

  # Project & Crop HII: SKIP
#hii.proj <- terra::project(hii, griz.ext, method="bilinear")
#hii.crop <- crop(hii.proj, griz.ext)

  # Project & Crop HMI:
hmi.proj <- terra::project(hmi, griz.ext, method="bilinear")
hmi.crop <- crop(hmi.proj, griz.ext)

  # Rescale HMI:
hmi.rescale <- hmi.crop / 65536

  # Project & Crop Elev:
elev.proj <- terra::project(elev, griz.ext, method="bilinear")
elev.crop <- crop(elev.proj, griz.ext)
rough <- terrain(elev.crop, v="TRI")
rough.max <-  global(rough, "max", na.rm=TRUE)[1,]
rough.min <-  global(rough, "min", na.rm=TRUE)[1,]
rough.rescale <- (rough - rough.min)/(rough.max - rough.min)
rough.rescale[rough.rescale==0] <- 0.000000001
rough.rescale[is.nan(rough.rescale)] <- 1

  # Rescale HII:
#hii.min <- global(hii.crop, "min", na.rm=TRUE)[1,]
#hii.max <- global(hii.crop, "max", na.rm=TRUE)[1,]
#hii.rescale <- (hii.crop - hii.min) / (hii.max - hii.min)
#hii.rescale[hii.rescale == 0] <- 0.000000001
#hii.rescale[is.nan(hii.rescale)] <- 1




  # Project grizz resistance:
griz.comb.proj <- terra::project(grizz.inc.comb, griz.ext, method="bilinear")
griz.inc.crop <- elev.crop <- crop(griz.comb.proj, griz.ext)
griz.resist <- 1-griz.inc.crop
griz.resist[is.nan(griz.resist)] <- 1

  # Invert Grizz dens raster:
griz.ext.inv <- 1/(griz.ext)

# Scale Grizz Extents: don't need this bc this is in our source strength input
griz.ext.min <-global(griz.ext.inv, "min", na.rm=TRUE)[1,]
griz.ext.max <- global(griz.ext.inv, "max", na.rm=TRUE)[1,] 
griz.rescale <- (griz.ext.inv - griz.ext.min) / (griz.ext.max - griz.ext.min)
griz.rescale[griz.rescale == 0] <- 0.000000001
griz.rescale[is.nan(griz.rescale)] <- 1


# Fuzzysum Our Rasters: ---------------------------------------------------

  # Fuzzy sum approach to combine them from Theobald 2013:
fuzzysum2 <- function(r1, r2) {
  rc1.1m <- (1-r1)
  rc2.1m <- (1-r2)
  fuz.sum <- 1-(rc1.1m*rc2.1m)
}
  # Add together our biophys attributes: grizz density, gHM, and roughness
biophys_fuzsum <- fuzzysum2(hmi.rescale, rough.rescale)
plot(biophys_fuzsum, col=plasma(256), axes = TRUE, main = "BHS+gHM Resistance Layer")

fuzzysum3 <- function(r1, r2, r3) {
  rc1.1m <- (1-r1)
  rc2.1m <- (1-r2)
  rc3.1m <- (1-r3)
  fuz.sum <- 1-(rc1.1m*rc2.1m*rc3.1m)
}
# Add together our biophys attributes + grizz inc resist: grizz density, gHM, and roughness + grizz resist
bio_social_fuzzysum <- fuzzysum3(hmi.rescale, rough.rescale, griz.resist)

  # Make into resistance surface
biophys_resistance <- (1+biophys_fuzsum)^10
plot(biophys_resistance, col=plasma(256), axes = TRUE, main = "Biophysical Resistance Layer")

biophys_social_resistance <- (1+bio_social_fuzzysum)^10
plot(biophys_social_resistance, col=plasma(256), axes = TRUE, main = "Biophys + Social Resistance Layer")


 
writeRaster(grizz.inc.comb, "/Users/shannonspragg/Grizz-Connectivity/Data/processed/grizz_inc_comb.tif")
writeRaster(rough.rescale, filename=here("data/processed/rough_rescale.tif"), overwrite=TRUE)
writeRaster(hmi.rescale, filename=here("data/processed/hmi_resist.tif"), overwrite=TRUE)
writeRaster(griz.rescale, filename=here("data/processed/griz_rescale_resist.tif"), overwrite=TRUE)
writeRaster(griz.ext, filename=here("data/processed/griz_source.tif"), overwrite=TRUE)
writeRaster(griz.ext.invert, filename=here("data/processed/griz_resist.tif"), overwrite=TRUE)
writeRaster(griz.ext.inv, filename=here("data/processed/griz_resist_recip.tif"), overwrite=TRUE)

writeRaster(biophys_resistance, filename=here("data/processed/biophys_resist.tif"), overwrite=TRUE)
writeRaster(biophys_social_resistance, filename=here("data/processed/biophys_social_resist.tif"), overwrite=TRUE)
writeRaster(biophys_comb_resistance, "data/raster_layers/biophys_comb_resistance_layer.tif", overwrite = TRUE)

writeRaster(biophys.hii, filename=here("data/processed/biophys_hii_resist.tif"), overwrite=TRUE)
writeRaster(biophys.hmi, filename=here("data/processed/biophys_hmi_resist.tif"), overwrite=TRUE)
