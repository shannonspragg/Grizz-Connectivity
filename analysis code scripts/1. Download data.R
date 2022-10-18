library(googledrive)
library(tidyverse)
options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = TRUE
)

# Download files not in folders -------------------------------------------

folder_url <- "https://drive.google.com/drive/folders/11b1N2yjqqvBVDZXDm_Zzx8bdBqbm_-Np" 
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)[-c(27,28),] #don't grab folders; note that the indices change anytime something new gets added
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("Data/original/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))


# Download Canadian PAs dataset -------------------------------------------

folder_url <- "https://drive.google.com/drive/folders/11bDP30UM22sZ7ngu8wHNvDu-yMuqd03w" 
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("Data/original/CPCAD-BDCAPC_Dec2020.gdb/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

# Download the ghm dataset ------------------------------------------------
folder_url <- "https://drive.google.com/drive/folders/164ggbZ61lLKgvgIRxGhXHO4UozvJmC7p" 
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("Data/original/gHMv1_300m_2017_static/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

#Download PADUS dataset
folder_url <- "https://drive.google.com/drive/folders/15Z1A96UW43uvfi3O3DZDDerfhgr-lqES"
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("Data/original/PAD_US2_1.gdb/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

#Download US Griz Distr zones
folder_url <- "https://drive.google.com/drive/folders/1Bcrlfh0yY6wK7AIBiM9_1XBPlFG6nQkL"
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("Data/original/GrizzlyDistribRecoveryZones.gdb/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))
