# Prepping Biophys Resistance Rasters: ------------------------------------
  ## This is our second script, where we prep the biophysical raster input for our omniscape model, as well as
  # our grizz increase raster for the next script.


# Load Packages: ----------------------------------------------------------
library(here)
library(terra)
library(sf)
library(here)
library(rgdal)
library(raster)
library("sp")


# Bring in Data: ----------------------------------------------------------
  #Creating the raster file objects pulled down from the data folder
ona_bdry <- st_read(here("/Users/shannonspragg/Grizz-Connectivity/Data/original/ONA_TerritoryBound.shp")) 
griz_dens <- rast(here("/Users/shannonspragg/Grizz-Connectivity/Data/original/grizz_dens.tif"))
hii <- rast("/Users/shannonspragg/Grizz-Connectivity/Data/original/hii_n_amer/")
hmi <- rast("/Users/shannonspragg/Google Drive/My Drive/Data/Original Data/Global_HII_NAmerica/hii_n_amer/")

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


# Prep Grizzinc Raster: ------------------------------------------------------
  # GrizzInc Map:
grizz.inc.wa.reproj <- terra::project(grizz.inc.wa, crs(ona_proj.vec))  
grizz.inc.bc.reproj <- terra::project(grizz.inc.bc, crs(ona_proj.vec))  


# Resample to match: ------------------------------------------------------
grizzinc.bc.rsmple <- resample(grizz.inc.bc.reproj, ona.rast, method='bilinear')
grizzinc.wa.rsmple <- resample(grizz.inc.wa.reproj, ona.rast, method='bilinear')


# Merge our BC and WA rasters: -----------------------------------------------------
grizz.inc.comb <- terra::merge(grizzinc.bc.rsmple, grizzinc.wa.rsmple)

grizz_resist <- grizz.inc.comb

# Prep Other Rasters: -----------------------------------------------------

  # Expand grizz_dens extent:
griz.ext <- terra::extend(griz_dens, ona_proj.vec, filename=here("data/processed/griz_ext.tif"), overwrite=TRUE)
#griz.crop <- crop(griz.ext, ona_proj.vec)
griz.ext[is.nan(griz.ext)] <- 0

  # Project & Crop HII:
hii.proj <- terra::project(hii, griz.ext, method="bilinear")
hii.crop <- crop(hii.proj, griz.ext)

# Project & Crop HMI:
hmi.proj <- terra::project(hmi, griz.ext, method="bilinear")
hmi.crop <- crop(hmi.proj, griz.ext)

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
hii.min <- global(hii.crop, "min", na.rm=TRUE)[1,]
hii.max <- global(hii.crop, "max", na.rm=TRUE)[1,]
hii.rescale <- (hii.crop - hii.min) / (hii.max - hii.min)
hii.rescale[hii.rescale == 0] <- 0.000000001
hii.rescale[is.nan(hii.rescale)] <- 1

# Rescale HII:
hmi.min <- global(hmi.crop, "min", na.rm=TRUE)[1,]
hmi.max <- global(hmi.crop, "max", na.rm=TRUE)[1,]
hmi.rescale <- (hmi.crop - hmi.min) / (hmi.max - hmi.min)
hmi.rescale[hmi.rescale == 0] <- 0.000000001
hmi.rescale[is.nan(hmi.rescale)] <- 1

  # Project grizz resistance:
griz.resist.proj <- terra::project(griz.resist, griz.ext, method="bilinear")
griz.resist.crop <- elev.crop <- crop(griz.resist.proj, griz.ext)
griz.resist.1m <- 1-griz.resist.crop
griz.resist.1m[is.nan(griz.resist.1m)] <- 1

  # Scale Grizz Extents:
griz.ext.min <-global(griz.ext, "min", na.rm=TRUE)[1,]
griz.ext.max <- global(griz.ext, "max", na.rm=TRUE)[1,] 
griz.ext.invert <- ((griz.ext - griz.ext.max)*-1) + griz.ext.min
griz.ext.invert[griz.ext.invert == 0] <- 0.000000001

griz.ext.nozero <- griz.ext
griz.ext.nozero[griz.ext.nozero==0] <- 0.0000000001
griz.ext.inv <- (griz.ext.nozero)^-1


  # Make our Biophys & SocialBiophys Layers:
biophys.hii <- hii.rescale + rough.rescale
biophys.hmi <- hmi.rescale + rough.rescale

biophys.combined <- hii.rescale + griz.ext.invert + rough.rescale
#social.biophys <- hii.rescale + griz.ext.invert + rough.rescale + griz.resist.1m

  # Mask to ONA:
biophys.hii.ona <- terra::mask(biophys.hii, ona_proj.vec) 
biophys.hmi.ona <- terra::mask(biophys.hmi, ona_proj.vec) 

biophys.comb.ona <- terra::mask(biophys.combined, ona_proj.vec)

writeRaster(grizz.inc.comb, "/Users/shannonspragg/Grizz-Connectivity/Data/processed/grizz_inc_comb.tif")
writeRaster(hii.rescale, filename=here("data/processed/hii_resist.tif"), overwrite=TRUE)
writeRaster(griz.ext, filename=here("data/processed/griz_source.tif"), overwrite=TRUE)
writeRaster(griz.ext.invert, filename=here("data/processed/griz_resist.tif"), overwrite=TRUE)
writeRaster(griz.ext.inv, filename=here("data/processed/griz_resist_recip.tif"), overwrite=TRUE)

writeRaster(biophys.combined, filename=here("data/processed/bio_combined_resist.tif"), overwrite=TRUE)
writeRaster(biophys.hii, filename=here("data/processed/biophys_hii_resist.tif"), overwrite=TRUE)
writeRaster(biophys.hmi, filename=here("data/processed/biophys_hmi_resist.tif"), overwrite=TRUE)
