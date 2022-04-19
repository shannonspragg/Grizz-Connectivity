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


# Bring in Template Raster: -----------------------------------------------

#template raster
r <- raster("data/template_raster.tif")
herds <- st_read("data/processed/herd_shapefile_outline.shp")
# resample the hsi layer to match the extent and resolution of template raster
hsi <- raster("data/original/SUMMER_HSI_clip/SUMMER_HSI_clip.tif")
hsi.resample <- resample(hsi, r)
plot(hsi.resample)
table(is.na(hsi.resample[]))
plot(hsi.resample, colNA="red")
#writeRaster(hsi.resample, "data/processed/hsi_resample_wrongmax.tif", overwrite = TRUE)
#write this for future use so I won't have to resample again!

# according to Brent (creater of hsi layer) the max value should be 73 NOT 128
# fix the max value here
hsi.resample[hsi.resample>73] <- NA
hsi.resample
writeRaster(hsi.resample, "data/processed/hsi_resample.tif")

# take the inverse of habitat suitability for resistance
hsi.resample <- raster("data/processed/hsi_resample.tif")
plot(hsi.resample)
plot(st_geometry(herds), add = TRUE)
hsi.inverse <- 1/hsi.resample
plot(hsi.inverse)
table(is.na(hsi.resample[]))
plot(hsi.resample, colNA="red")


# Rescale our Raster for Standardization: ---------------------------------
  # Rescale to 0-1 for standardization
  # set the NA values to 1 for highest resistance (won't run in CS otherwise)
hsi.rescale <- rescale01(hsi.inverse)
hsi.rescale[is.na(hsi.rescale)]=1
hsi.rescale
plot(hsi.rescale, col=plasma(256), axes = TRUE, main = "Habitat Suitability Resistance Layer")

# bring in the human modification layer
hmi <- raster("data/processed/hmi.crop.tif")
plot(hmi, col=plasma(256), axes = TRUE, main = "Human Modification Layer")

# fuzzy sum approach to combine them from Theobald 2013
fuzzysum <- function(r1, r2) {
  rc1.1m <- (1-r1)
  rc2.1m <- (1-r2)
  fuz.sum <- 1-(rc1.1m*rc2.1m)
}
biophys_fuzsum <- fuzzysum(hsi.rescale, hmi)
plot(biophys_fuzsum, col=plasma(256), axes = TRUE, main = "HSI+HMI Resistance Layer")



# Make this a Resistance Surface: -----------------------------------------
biophys_resistance <- (1+biophys_fuzsum)^10
plot(biophys_resistance, col=plasma(256), axes = TRUE, main = "HSI+HMI Resistance Layer")



# Save our Raster: --------------------------------------------------------
  # Write raster (saving both gdrive and local computer):
writeRaster(biophys_resistance, "data/raster_layers/biophys_resistance_layer.tif", overwrite = TRUE)

