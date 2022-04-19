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
library(googledrive)

# Load our Data with GoogleDrive: -----------------------------------------
options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = TRUE
)


# Download Original Data --------------------------------------------
folder_url <- "https://drive.google.com/drive/u/0/folders/1TOBdzYCxFlHDiWAjl094Bp59_DSFa8BK" # rasters and omniscape data
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("Data/original/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))


# Create our Rescale Function: --------------------------------------------
rescale01 <- function(r1) {
  r.rescale <- (r1 - cellStats(r1, min))/(cellStats(r1, max) - cellStats(r1, min))
}


# Bring in Rasters: -----------------------------------------------

r <- raster("Data/original/SOI_10km.tif") 
p.bear.conf <- raster("Data/original/prob_bear_conflict.tif")
grizz.dens <- raster("Data/original/grizz_dens.tif")


# Crop and Resample Source Raster: ----------------------------------------



# Rescale our Raster for Standardization: ---------------------------------
  # Rescale to 0-1 for standardization
  # set the NA values to 1 for highest resistance (won't run in CS otherwise)
p.conf.rescale <- rescale01(p.bear.conf)
p.conf.rescale[is.na(p.conf.rescale)]=1
p.conf.rescale
plot(p.conf.rescale, col=plasma(256), axes = TRUE, main = "Probability of Bear Conflict Resistance Layer")


# Make this a Resistance Surface: -----------------------------------------
p_conflict_resistance <- (1+p.conf.rescale)^10
plot(p_conflict_resistance, col=plasma(256), axes = TRUE, main = "Probability of Bear Conflict Resistance Layer")


# Save our Raster: --------------------------------------------------------
  # Write raster (saving both gdrive and local computer):
writeRaster(p_conflict_resistance, "Data/processed/p_conflict_resistance_layer.tif", overwrite = TRUE)

