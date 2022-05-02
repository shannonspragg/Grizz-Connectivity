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

r <- raster("Data/processed/ona_bound.tif") 
p.bear.conf <- raster("Data/processed/ona_p_bear_conf.tif")
grizz.dens <- raster("Data/original/grizz_dens.tif")


# NOTE: need to check p(conflict values so highest resistance is highest p(conflict))

  # Resample to match the extent and resolution of template raster
p.conf.resample <- resample(p.bear.conf, grizz.dens)
plot(p.conf.resample)
plot(grizz.dens, add=TRUE)

# Rescale our Raster for Standardization: ---------------------------------
  # Rescale to 0-1 for standardization
  # set the NA values to 1 for highest resistance (won't run in CS otherwise)
p.conf.rescale <- rescale01(p.conf.resample)
p.conf.rescale[is.na(p.conf.rescale)]=1
p.conf.rescale
plot(p.conf.rescale, col=plasma(256), axes = TRUE, main = "Probability of Bear Conflict Resistance Layer")



# Make this a Resistance Surface: -----------------------------------------
p_conflict_resistance <- (1+p.conf.rescale)^10
plot(p_conflict_resistance, col=plasma(256), axes = TRUE, main = "Probability of Bear Conflict Resistance Layer")


# Save our Raster: --------------------------------------------------------
  # Write raster (saving both gdrive and local computer):
writeRaster(p_conflict_resistance, "Data/processed/p_conflict_resistance_layer.tif", overwrite = TRUE)

