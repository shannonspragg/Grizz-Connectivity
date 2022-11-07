# Dowload Biophysical Connectivity Models: --------------------------------

### Here we download our biophysical connectivity outputs for the conflict analysis

# Load Packages -------------------------------------------------------
library(googledrive)
library(tidyverse)

# Load our Data with GoogleDrive: -----------------------------------------
options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = TRUE
)

# Biophysical Connectivity Models:
folder_url <- "https://drive.google.com/drive/folders/1GczDMCboJDcK-UScWkG2foRjVY0Cia39" # biophys outputs male b bears 
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("Data/processed/BiophysONA/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

# Conflict Connectivity Models: (completed after conflict analysis)
folder_url <- "https://drive.google.com/drive/folders/1HACXhYpazNAMFLDID2gVuQAhKcbpsZvW" # biophys outputs male b bears 
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("Data/processed/bioSocialONA/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

# Let's crop these to the BHW boundary and 50km buffer --------------------
library(terra)

# Bring in covariate data: -------------------------------------------------------------
bhs.ona <- rast("Data/processed/bhs_ONA.tif")
ona.bound <- st_read("Data/original/ONA_TerritoryBound.shp") %>% 
  st_transform(., crs=crs(bhs.ona)) %>% 
  as(., "SpatVector")

biophys_cumcurr <- rast("data/processed/biophysONA/cum_currmap.tif")
biophys_norm <- rast("data/processed/biophysONA/normalized_cum_currmap.tif")

biosocial_cumcurr <- rast("data/processed/bioSocialONA/cum_currmap.tif")
biosocial_norm <- rast("data/processed/bioSocialONA/normalized_cum_currmap.tif")


# Crop our rasters to the ONA boundary:

biophys_cumcurr_ona <- terra::mask(biophys_cumcurr, ona.bound)
biophys_norm_ona <- terra::mask(biophys_norm, ona.bound)

biosocial_cumcurr_ona <- terra::mask(biosocial_cumcurr, ona.bound)
biosocial_norm_ona <- terra::mask(biosocial_norm, ona.bound)

# Save our connectivity outputs:
writeRaster(biophys_cumcurr_ona, "data/processed/biophys_ona_cumcurr.tif", overwrite=TRUE)
writeRaster(biosocial_cumcurr_ona, "data/processed/biosocial_ona_cumcurr.tif", overwrite=TRUE)
writeRaster(biophys_norm_ona, "data/processed/biophys_ona_norm.tif", overwrite=TRUE)
writeRaster(biosocial_norm_ona, "data/processed/biosocial_ona_norm.tif", overwrite=TRUE)


