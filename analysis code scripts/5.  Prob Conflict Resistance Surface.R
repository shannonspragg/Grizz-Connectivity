# Prepping our Probability of Conflict Resistance Surface: ----------------
  ## Here, we will prep the probability of conflict resistance surface for inputting into omniscape to run our
  # connectivity model. We bring in our probability of conflict raster and then rescale it for standardization.


# Load Packages: ----------------------------------------------------------
library(raster)
library(terra)
library(dplyr)
library(sf)
library(spatialEco)
library(ggmap)
library(rgdal)
library(maptools)
library(viridis)


# Create our Rescale Function: --------------------------------------------
rescale01 <- function(r1) {
  r.rescale <- (r1 - cellStats(r1, min))/(cellStats(r1, max) - cellStats(r1, min))
}


# Bring in Rasters: -----------------------------------------------

bhs.ona <- rast("Data/processed/bhs_ona.tif")
r <- raster("Data/processed/ona_buf_bound.tif") 
ona.buf <- st_read("/Users/shannonspragg/Grizz-Connectivity/Data/processed/ona_buffer_bound.shp") 
p.bear.conf <- raster("Data/processed/ona_p_bear_conf.tif")
grizz.dens <- raster("Data/processed/grizz_dens_mean.tif")
rough.rescale <- raster("Data/processed/rough_rescale.tif")
hmi.rescale <- raster("Data/processed/hmi_resist.tif")


  # Resample to match the extent and resolution of template raster
p.conf.resample <- resample(p.bear.conf, grizz.dens)

  # Mask our Biohysical to ONA:
ona.buf.vect <- vect(ona.buf)
hmi.rescale.rast <- terra::rast(hmi.rescale)
rough.rescale.rast <- terra::rast(rough.rescale)

hmi.ona.buf <- terra::mask(hmi.rescale.rast, ona.buf.vect)
rough.ona.buf <- terra::mask(rough.rescale.rast, ona.buf.vect)

  # Change this to terra for fuzzy sum:
p.conf.rescale.rast <- rast(p.conf.resample)
p.conf.rescale.rast[is.na(p.conf.rescale.rast)]=1

plot(p.conf.rescale.rast, col=plasma(256), axes = TRUE, main = "Probability of Bear Conflict Resistance Layer")

  # Match our biophys variables:
hmi.rescale.resamp <- resample(hmi.ona.buf, p.conf.rescale.rast)
rough.rescale.resamp <- resample(rough.ona.buf, p.conf.rescale.rast)

hmi.ona.crop <- terra::crop(hmi.rescale.resamp, p.conf.rescale.rast)
hmi.ona.crop[is.nan(hmi.ona.crop)] <- 1

rough.ona.crop <- terra::crop(rough.rescale.resamp, p.conf.rescale.rast)
rough.ona.crop[is.nan(rough.ona.crop)] <- 1


# Fuzzysum Our Rasters: ---------------------------------------------------

# Fuzzy sum approach to combine them from Theobald 2013:

fuzzysum3 <- function(r1, r2, r3) {
  rc1.1m <- (1-r1)
  rc2.1m <- (1-r2)
  rc3.1m <- (1-r3)
  fuz.sum <- 1-(rc1.1m*rc2.1m*rc3.1m)
}
# Add together our biophys attributes + grizz inc resist: grizz density, gHM, and roughness + grizz resist
prob_conf_fuzzysum <- fuzzysum3(hmi.ona.crop, rough.ona.crop, p.conf.rescale.rast)

writeRaster(prob_conf_fuzzysum, "Data/processed/p_conflict_raw_sum.tif", overwrite = TRUE)

# Make this a Resistance Surface: -----------------------------------------
p_conflict_resistance <- (1+prob_conf_fuzzysum)^10
plot(p_conflict_resistance, col=plasma(256), axes = TRUE, main = "Probability of Bear Conflict Resistance Layer")

  # Put the resistance and source into 1000 x 1000 resolution for omniscape:
p.conflict.resistance <- resample(p_conflict_resistance, hmi.rescale.rast)
p.conflict.resistance <- crop(p.conflict.resistance, p.conf.rescale.rast)
names(p.conflict.resistance)[names(p.conflict.resistance) == "hmi_resist"] <- "Probability of Conflict Resistance"

bhs.ona.rsmpl <- resample(bhs.ona, hmi.rescale.rast)
bhs.ona.crop <- crop(bhs.ona.rsmpl, p.conf.rescale.rast)


# Save our Raster: --------------------------------------------------------
  # Write raster (saving both gdrive and local computer):
writeRaster(p.conflict.resistance, "Data/processed/p_conflict_resistance_layer.tif", overwrite = TRUE)
writeRaster(bhs.ona.crop, "Data/processed/bhs_ona.tif", overwrite = TRUE)

