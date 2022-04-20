# Original Data Prep for Ch.2: --------------------------------------------
  ## Here we will be prepping the data for our variables from the Ch.1 analysis, extending them to the entirety 
  # of the ONA territory in order to prep them for fitting our probability of conflict prediction raster.

# Loadport Packages -------------------------------------------------------
library(tidyverse)
library(dplyr)
library(sf)
library(sp)
library(rgeos)
library(raster)
library(rgdal)
library(fasterize)
library(terra)
library(stars)
library(units)
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

# Bring in our Original Data --------------------------------------------


