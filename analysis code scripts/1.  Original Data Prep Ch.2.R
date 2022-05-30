# Original Data Prep for Ch.2: --------------------------------------------
  ## Here we will be prepping the data for our variables from the Ch.1 analysis, extending them to the entirety 
  # of the ONA territory in order to prep them for fitting our probability of conflict prediction raster.

# Load Packages -------------------------------------------------------
library(tidyverse)
library(sf)
library(sp)
library(rgeos)
#library(raster)
library(rgdal)
#library(fasterize)
library(terra)
library(units)
library(googledrive)
library(geosphere)
library(lakemorpho)
library(here)
library(measurements)

# Load our Data with GoogleDrive: -----------------------------------------
options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = TRUE
)


# Download Original Data -------------------------------------------- I don't know why, but this won't bring in all the date that's in the google drive folder
folder_url <- "https://drive.google.com/drive/u/0/folders/1TOBdzYCxFlHDiWAjl094Bp59_DSFa8BK" # rasters and omniscape data
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("Data/original/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

# Bring in our Original Data --------------------------------------------
  # Ag Data:
farm.type <- read.csv("Data/original/farm type_32100403.csv")
animal.farm.wa <- read.csv("Data/original/Animal Farming WA.csv")
crop.farm.wa <- read.csv("Data/original/Crop Farming WA.csv")

  # Census Divisions (CCS and county):
can.ccs.shp<-st_make_valid(st_read("Data/original/lccs000b16a_e.shp"))
wa.county.shp <- st_read("Data/original/tl_2016_53_cousub.shp")

  #Protected Areas:
#bc.pas <- st_read(st_make_valid("Data/original/Parks_Combined2.shp"))
wa.desig <- st_read("Data/original/PADUS2_1Designation_StateWA.shp")
wa.procs <- st_read("Data/original/PADUS2_1Proclamation_StateWA.shp")

  # Grizzly Population Units:
grizz.units <- st_read("Data/original/GBPU_BC_polygon.shp")

  # Grizz Inc:
grizz.inc.bc <- rast("Data/original/grizz.increase.map.fixed.tif") #  the proportion of people within a census that 
# grizz.inc.wa <- rast("/Users/shannonspragg/Grizz-Connectivity/Data/original/grizz.increase.map.fixed.tif") #  the proportion of people within a census that 

  # Bear Density - Bear Habitat Suitability (BHS):
can.provs <- st_read("Data/original/lpr_000b21a_e.shp")

# Global Human Density:
world.hum.dens <- terra::rast("Data/original/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec.tif")

  # ONA Territory:
ona.bound <- st_read("Data/original/ONA_TerritoryBound.shp") 

# Check Validity:
any(!st_is_valid(wa.county.shp)) #FALSE
any(!st_is_valid(wa.desig)) #FALSE
st_make_valid(wa.desig)
any(!st_is_valid(wa.procs)) # FALSE
st_make_valid(wa.procs)
any(!st_is_valid(grizz.units)) # FALSE
any(!st_is_valid(ona.bound )) # FALSE
st_make_valid(can.provs)

# Download PA Data: -------------------------------------------------------
# Publication Data: Load in Canada Spatial Data ---------------------------------------------
fgdb <- "Data/original/CPCAD-BDCAPC_Dec2020.gdb"

# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
print(fc_list)

# Read the feature class for PA shapes
fc <- readOGR(dsn=fgdb,layer="CPCAD_Dec2020")
fc.sf <- as(fc, "sf") 

#  Filter to just BC:
bc.PAs <- fc.sf %>% 
  filter(., LOC_E == "British Columbia") %>% 
  st_make_valid()

# Filter by IUCN status (Muise et al., 2022 https://esajournals.onlinelibrary.wiley.com/doi/10.1002/eap.2603)
bc.PAs.iucn.filtered <- bc.PAs %>% 
  filter(., IUCN_CAT == "Ia" | IUCN_CAT == "Ib" | IUCN_CAT == "II" | IUCN_CAT == "IV")

# Filter by PA's larger than 100 ha:
bc.PAs.100.ha <- filter(bc.PAs.iucn.filtered, O_AREA > 100) 


# Reproject & Prep ONA Boundary -------------------------------------------
  # We want to match our data to BC Albers
ona.reproj <- st_transform(ona.bound, 3005)
bc.pas.reproj <- st_transform(st_make_valid(bc.PAs.100.ha), st_crs(ona.reproj))
ccs.reproj <- st_transform(can.ccs.shp, st_crs(ona.reproj))
wa.county.reproj <- st_transform(wa.county.shp, st_crs(ona.reproj))
wa.desig.reproj <- st_transform(wa.desig, st_crs(ona.reproj))
wa.proc.reproj <- st_transform(wa.procs, st_crs(ona.reproj))
grizzpop.reproj <- st_transform(grizz.units, st_crs(ona.reproj))
can.provs.reproj <- st_transform(st_make_valid(can.provs), st_crs(ona.reproj))

st_crs(ccs.reproj) == st_crs(bc.pas.reproj) #TRUE
st_crs(wa.county.reproj) == st_crs(wa.proc.reproj) #TRUE
st_crs(grizzpop.reproj) == st_crs(can.provs.reproj) #TRUE


  # Buffer ONA for Omniscape inputs:
ona.buffer <- ona.reproj %>% 
  st_buffer(., 25000)

  # Make our ONA template raster:
ona.vect <- vect(ona.reproj)
ona.buf <- vect(ona.buffer)
ona.vect.p <- terra::project(ona.vect, crs(grizz.inc.bc))
ona.buf.p <- terra::project(ona.buf, crs(grizz.inc.bc))
griz.inc.ext <- terra::extend(grizz.inc.bc, ona.buf.p, overwrite=TRUE)
griz.inc.ext[is.nan(griz.inc.ext)] <- 0

grizzinc.crop.t <- terra::crop(griz.inc.ext, ona.vect.p)  
grizzinc.crop.b <- terra::crop(griz.inc.ext, ona.buf.p)  

ona.rast <- terra::rasterize(ona.vect.p, grizzinc.crop.t, field = "ENTITY")
ona.rast <- resample(ona.rast, grizzinc.crop.t, method='bilinear')

ona.buf.rast <- terra::rasterize(ona.buf.p, grizzinc.crop.b, field = "ENTITY")
ona.buf.rast <- resample(ona.buf.rast, grizzinc.crop.b, method='bilinear')

  # Reproject to BC Albers:
ona.buf.rast <- terra::project(ona.buf.rast, ona.buf)
ona.rast <- terra::project(ona.rast, ona.buf)

  # Export as tiff:
terra::writeRaster(ona.rast, "Data/processed/ona_bound.tif", overwrite=TRUE)
terra::writeRaster(ona.buf.rast, "Data/processed/ona_buf_bound.tif", overwrite=TRUE)
st_write(ona.buffer, "Data/processed/ona_buffer_bound.shp", append=FALSE)

  # Extract BC Boundary:
bc.bound <-can.provs.reproj %>%
  filter(., PRNAME == "British Columbia / Colombie-Britannique") 

st_write(bc.bound, "Data/processed/bc_bound.shp", append=FALSE)

############################# Prep Agriculture Variables:
####################### Now, we will filter the CCS regions and Agriculture Data to BC:

# Filter CCS and Ag Files to BC Only ---------------------------------------------------
# Make sf and filter down to only British Columbia for Census SubDivs (CCS):
can.ccs.sf<- as(ccs.reproj, "sf")
unique(can.ccs.sf$PRNAME) # Shows that the name for BC is "British Columbia / Colombie-Britannique"

# Filter down to just BC:
bc.ccs<-can.ccs.sf %>%
  filter(., PRNAME == "British Columbia / Colombie-Britannique") 

# Save this for later:
st_write(bc.ccs, "Data/processed/BC CCS.shp", append=FALSE)

# Filter the Ag Files down to just BC districts: --------------------------
# See here: https://www.statology.org/filter-rows-that-contain-string-dplyr/  searched: 'Return rows with partial string, filter dplyr'
farm.type.bc <- farm.type %>% filter(grepl("British Columbia", farm.type$GEO)) 

# Filtering to just the BC regions with a CCS number (so we can join to the CCS spatial data):
bc.farm.filter.ccs<-farm.type.bc %>%
  filter(., grepl("*CCS59*", farm.type.bc$GEO))

# Check to see what specific farm types exist in BC:
unique(farm.type.bc$North.American.Industry.Classification.System..NAICS.) # There are 43 unique farm types in BC

# Filter for just the 2016 census results (the data had 2011 and 2016):

bc.farm.2016.ccs<-bc.farm.filter.ccs %>%
  filter(., REF_DATE == "2016") 

# Editing CCS Code into new column for join -------------------------------
  # Here we separate out the CCS code into new column for join with CCS .shp:
bc.ccs$CCSUID.crop<- str_sub(bc.ccs$CCSUID,-5,-1) # Now we have a matching 6 digits
unique(bc.ccs$CCSUID.crop) #This is a 5 digit code
bc.farm.2016.ccs$CCSUID.crop<- str_sub(bc.farm.2016.ccs$GEO,-7,-3) # Now we have a matching 6 digits
unique(bc.farm.2016.ccs$CCSUID.crop) #This is a 5 digit code

str(bc.farm.2016.ccs) # Check the structure before joining
unique(animal.farm.wa$County.ANSI)

crop.farm.wa$County.ANSI <- str_pad(crop.farm.wa$County.ANSI, 3, side = c("left"), pad = "0")
animal.farm.wa$County.ANSI <- str_pad(animal.farm.wa$County.ANSI, 3, side = c("left"), pad = "0")

wa.county.reproj$CountyID<- str_sub(wa.county.reproj$GEOID,3,5) 

unique(wa.county.reproj$CountyID) #This is a 3 digit code
unique(animal.farm.wa$County.ANSI) # Now these match
unique(crop.farm.wa$County.ANSI)


# Joining the CCS with the Farm Type: -------------------------------------
head(wa.county.reproj) # Check out our WA county data

  # Join the BC CCS with Ag Files:
#farm.ccs.join <- merge(bc.farm.2016.ccs, bc.ccs, by.x = "CCSUID.crop", by.y = "CCSUID.crop") 
#wa.animal.join <- merge(animal.farm.wa, wa.county.reproj, by.x = "County.ANSI", by.y = "CountyID") 
# wa.crop.join <- merge(crop.farm.wa, wa.county.reproj, by.x = "County.ANSI", by.y = "CountyID") 

farm.ccs.join <- bc.ccs %>%   # HELP: this drops the NAIC and value columns.. how do we keep these?? (everything else runs smooth to the end)
  left_join(., bc.farm.2016.ccs, by = c("CCSUID.crop" = "CCSUID.crop"))
# farm.ccs.join<- farm.ccs.join %>% 
#   left_join(., bc.farm.2016.ccs %>% dplyr::select("North.American.Industry.Classification.System..NAICS.", "VALUE", "CCSUID.crop"), by = c("CCSUID.crop" = "CCSUID.crop"))
wa.animal.join <- wa.county.reproj %>% 
  left_join(., animal.farm.wa, by = c("CountyID" = "County.ANSI"))
wa.crop.join <- wa.county.reproj %>% 
  left_join(., crop.farm.wa, by = c("CountyID" = "County.ANSI"))


  # Double check that this is the correct structure:
# farm.ccs.sf <- st_as_sf(farm.ccs.join)
# wa.animal.sf <- st_as_sf(wa.animal.join)
# wa.crop.sf <- st_as_sf(wa.crop.join)

# head(farm.ccs.sf) # Here we have a farm type data frame with Multi-polygon geometry - check!
# head(wa.animal.sf)

# Here we subset the farm data to ONA, and pull out the total farm counts: ---------------------------------
  # Start by cropping the data down to ONA buffer:
# farm.ccs.sf <- st_transform(farm.ccs.sf, st_crs(ona.reproj))
# wa.animal.sf <- st_transform(wa.animal.sf, st_crs(ona.reproj))
# wa.crop.sf <- st_transform(wa.crop.sf, st_crs(ona.reproj))

farm.ccs.sf <- st_transform(farm.ccs.join, st_crs(ona.reproj))
wa.animal.sf <- st_transform(wa.animal.join, st_crs(ona.reproj))
wa.crop.sf <- st_transform(wa.crop.join, st_crs(ona.reproj))

  # Crop these to ONA:
# farm.ccs.bc.ona <- st_intersection(farm.ccs.sf, ona.buffer) 
# animal.farms.ona <- st_intersection(wa.animal.sf, ona.buffer) 
# crop.farms.ona <- st_intersection(wa.crop.sf, ona.buffer) 
# crop.farms.ona$Value <- as.integer(crop.farms.ona$Value)

farm.ccs.ona <- farm.ccs.sf[st_intersects(ona.reproj, farm.ccs.sf, sparse =  FALSE),]
animal.farms.ona <- wa.animal.sf[st_intersects(ona.reproj, wa.animal.sf, sparse =  FALSE),]
crop.farms.ona <- wa.crop.sf[st_intersects(ona.reproj, wa.crop.sf, sparse =  FALSE),]
crop.farms.ona$Value <- as.integer(crop.farms.ona$Value)

plot(st_geometry(animal.farms.ona)) # plot to check

  # Subset the data - separate total farms out of NAIC:
farm.ona.subset <- subset(farm.ccs.ona, North.American.Industry.Classification.System..NAICS. != "Total number of farms")
names(farm.ccs.ona)[names(farm.ccs.ona) == "North.American.Industry.Classification.System..NAICS."] <- "N_A_I_C"

  # Condense Farm Types to Animal & Ground Crop Production:
animal.product.farming <- dplyr::filter(farm.ccs.ona,  N_A_I_C == "Cattle ranching and farming [1121]" | N_A_I_C == "Hog and pig farming [1122]" | N_A_I_C == "Poultry and egg production [1123]"| N_A_I_C == "Sheep and goat farming [1124]" | N_A_I_C =="Other animal production [1129]") 


ground.crop.production <- dplyr::filter(farm.ona.subset, N_A_I_C == "Fruit and tree nut farming [1113]" | N_A_I_C == "Greenhouse, nursery and floriculture production [1114]" | N_A_I_C == "Vegetable and melon farming [1112]"
                                        | N_A_I_C == "Oilseed and grain farming [1111]" | N_A_I_C == "Other crop farming [1119]")

  # Total the counts of these farm categories by CCS region:
# animal.prod.bc.counts <- aggregate(cbind(VALUE) ~ CCSUID, data= animal.product.farming, FUN=sum)
# ground.crop.bc.counts <- aggregate(cbind(VALUE) ~ CCSUID, data= ground.crop.production, FUN=sum)
# 
# animal.prod.wa.counts <- aggregate(cbind(Value) ~ County.ANSI, data= animal.farms.ona, FUN=sum)
# ground.crop.wa.counts <- aggregate(cbind(Value) ~ County.ANSI, data= crop.farms.ona, FUN=sum)

##MW: Using the group_by; summarize avoids having to rejoin data
animal.prod.bc.sf <- animal.product.farming %>% 
  group_by(CCSUID) %>% 
  summarise(., "Total Farms in CCS" = sum(VALUE))
ground.crop.bc.sf <- ground.crop.production %>% 
  group_by(CCSUID) %>% 
  summarise(., "Total Farms in CCS" = sum(VALUE))

animal.prod.wa.sf <- animal.farms.ona %>% 
  group_by(CountyID) %>% 
  summarise(., "Total Farms in CCS" = sum(Value))
ground.crop.wa.sf <- crop.farms.ona %>% 
  group_by(CountyID) %>% 
  summarise(., "Total Farms in CCS" = sum(Value))


# names(animal.prod.bc.counts)[names(animal.prod.bc.counts) == "VALUE"] <- "Total Farms in CCS"
# names(ground.crop.bc.counts)[names(ground.crop.bc.counts) == "VALUE"] <- "Total Farms in CCS"
# names(animal.prod.wa.counts)[names(animal.prod.wa.counts) == "Value"] <- "Total Farms in County"
# names(ground.crop.wa.counts)[names(ground.crop.wa.counts) == "Value"] <- "Total Farms in County"
# 
#   # Join this back to our data as a total column:
# animal.prod.bc.join <- merge(animal.prod.bc.counts, animal.product.farming, by.x = "CCSUID", by.y = "CCSUID") 
# ground.crop.bc.join <- merge(ground.crop.bc.counts, ground.crop.production, by.x = "CCSUID", by.y = "CCSUID") 
# animal.prod.wa.join <- merge(animal.prod.wa.counts, animal.farms.ona, by.x = "County.ANSI", by.y = "County.ANSI") 
# ground.crop.wa.join <- merge(ground.crop.wa.counts, crop.farms.ona, by.x = "County.ANSI", by.y = "County.ANSI") 
# 
# animal.prod.bc.sf <- st_as_sf(animal.prod.bc.join)
# ground.crop.bc.sf <- st_as_sf(ground.crop.bc.join)
# animal.prod.wa.sf <- st_as_sf(animal.prod.wa.join)
# ground.crop.wa.sf <- st_as_sf(ground.crop.wa.join)

# Calculate the Density of Farm Types: ------------------------------------
  # We do so by dividing the count of farms by the overall area of the farm type categories (for our 10km buffered area, but save this to the 50km dataset 
  # so that we have values on the edge of our 10km zone):

# Make our area units kilometers:
animal.prod.bc.sf$AREA_SQ_KM <- set_units(st_area(animal.prod.bc.sf), km^2)
ground.crop.bc.sf$AREA_SQ_KM <- set_units(st_area(ground.crop.bc.sf), km^2)
animal.prod.wa.sf$AREA_SQ_KM <- set_units(st_area(animal.prod.wa.sf), km^2)
ground.crop.wa.sf$AREA_SQ_KM <- set_units(st_area(ground.crop.wa.sf), km^2)

#   # Calculate our areas for the two objects: 
# animal.prod.bc.sf$AREA_SQM <- st_area(animal.prod.bc.sf)
# ground.crop.bc.sf$AREA_SQM <- st_area(ground.crop.bc.sf)
# animal.prod.wa.sf$AREA_SQM <- st_area(animal.prod.wa.sf)
# ground.crop.wa.sf$AREA_SQM <- st_area(ground.crop.wa.sf)


  # Now we make a new col with our farms per sq km:
animal.prod.bc.sf$Farms_per_sq_km <- animal.prod.bc.sf$`Total Farms in CCS` / animal.prod.bc.sf$AREA_SQ_KM
head(animal.prod.bc.sf)

ground.crop.bc.sf$Farms_per_sq_km <- ground.crop.bc.sf$`Total Farms in CCS` / ground.crop.bc.sf$AREA_SQ_KM
head(ground.crop.bc.sf)

animal.prod.wa.sf$Farms_per_sq_km <- animal.prod.wa.sf$`Total Farms in CCS` / animal.prod.wa.sf$AREA_SQ_KM
head(animal.prod.wa.sf)

ground.crop.wa.sf$Farms_per_sq_km <- ground.crop.wa.sf$`Total Farms in CCS` / ground.crop.wa.sf$AREA_SQ_KM
head(ground.crop.wa.sf)

  # Make this col numeric:
animal.prod.bc.sf$Farms_per_sq_km <- as.numeric(as.character(animal.prod.bc.sf$Farms_per_sq_km))
ground.crop.bc.sf$Farms_per_sq_km <- as.numeric(as.character(ground.crop.bc.sf$Farms_per_sq_km))
animal.prod.wa.sf$Farms_per_sq_km <- as.numeric(as.character(animal.prod.wa.sf$Farms_per_sq_km))
ground.crop.wa.sf$Farms_per_sq_km <- as.numeric(as.character(ground.crop.wa.sf$Farms_per_sq_km))

  # Subset to only the columns we want:
animal.prod.bc <- animal.prod.bc.sf[, c("CCSUID", "Total Farms in CCS", "GEO", "VALUE", "CCSNAME", "AREA_SQ_KM"
                                        , "Farms_per_sq_km", "geometry")]
ground.crop.bc <- ground.crop.bc.sf[, c("CCSUID", "Total Farms in CCS", "GEO", "VALUE", "CCSNAME", "AREA_SQ_KM"
                                        , "Farms_per_sq_km", "geometry")]

# animal.prod.wa <- animal.prod.wa.sf[, c( "County.ANSI", "Total Farms in County", "GEOID", "Value", "NAME", "AREA_SQ_KM"
#                                          , "Farms_per_sq_km", "geometry")]
#                                     
# ground.crop.wa <- ground.crop.wa.sf[, c("County.ANSI", "Total Farms in County", "GEOID", "Value", "NAME", "AREA_SQ_KM"
#                                         , "Farms_per_sq_km", "geometry" )]

#   # Need to match column names for rbind:
# names(animal.prod.bc)[names(animal.prod.bc) == "CCSUID"] <- "CensusID"
# names(ground.crop.bc)[names(ground.crop.bc) == "CCSUID"] <- "CensusID"
# names(animal.prod.wa)[names(animal.prod.wa) == "County.ANSI"] <- "CensusID"
# names(ground.crop.wa)[names(ground.crop.wa) == "County.ANSI"] <- "CensusID"
# names(animal.prod.bc)[names(animal.prod.bc) == "Total Farms in CCS"] <- "Total Farms in Census Region"
# names(ground.crop.bc)[names(ground.crop.bc) == "Total Farms in CCS"] <- "Total Farms in Census Region"
# names(animal.prod.wa)[names(animal.prod.wa) == "Total Farms in County"] <- "Total Farms in Census Region"
# names(ground.crop.wa)[names(ground.crop.wa) == "Total Farms in County"] <- "Total Farms in Census Region"
# names(animal.prod.wa)[names(animal.prod.wa) == "GEOID"] <- "GEO"
# names(ground.crop.wa)[names(ground.crop.wa) == "GEOID"] <- "GEO"
# names(animal.prod.wa)[names(animal.prod.wa) == "Value"] <- "VALUE"
# names(ground.crop.wa)[names(ground.crop.wa) == "Value"] <- "VALUE"
# names(animal.prod.bc)[names(animal.prod.bc) == "CCSNAME"] <- "NAME"
# names(ground.crop.bc)[names(ground.crop.bc) == "CCSNAME"] <- "NAME"

# Merge these to make ONA animal and crop farms: --------------------------
animal.prod.ona <- rbind(animal.prod.bc.sf, animal.prod.wa.sf)
ground.crop.ona <- rbind(ground.crop.bc.sf, ground.crop.wa.sf)

  # Check once more:
plot(st_geometry(animal.prod.ona))
plot(st_geometry(ground.crop.ona))

# Save these as .shp's for later:
st_write(animal.prod.ona,"Data/processed/ONA Animal Product Farming.shp", overwrite=TRUE)

st_write(ground.crop.ona, "Data/processed/ONA Ground Crop Production.shp", overwrite=TRUE) 

st_write(animal.prod.ona,"Data/processed/ONA Census Districts.shp", overwrite=TRUE)

################################ Combine our Protected Area Datasets:

# Start by Filtering to Gap Status 1 & 2: ------------------------------
wa.desig.filter <- dplyr::filter(wa.desig.reproj, GAP_Sts == "1" | GAP_Sts == "2" )
head(wa.desig.filter)
#wa.proc.filter <- dplyr::filter(wa.desig.reproj, Mang_Name == "NPS" | Mang_Name == "USFS" ) # I don't think we need these

# Crop our PA's to ONA Boundary: ------------------------------------------

bc.ona.pas <- st_intersection(bc.pas.reproj, ona.buffer) 
wa.ona.pas <- st_intersection(wa.desig.filter, ona.buffer) 

# Subset only the columns we need: ----------------------------------------
wa.pas <- wa.ona.pas[, c("Unit_Nm", "d_Des_Tp", "Mang_Type", "geometry" )]
bc.pa <- bc.ona.pas[, c("NAME_E", "TYPE_E", "OWNER_E", "geometry" )]

names(wa.pas)[names(wa.pas) == "Unit_Nm"] <- "NAME_E"
names(wa.pas)[names(wa.pas) == "d_Des_Tp"] <- "TYPE_E"
names(wa.pas)[names(wa.pas) == "Mang_Type"] <- "OWNER_E"


# Join the WA and BC PAs Data: -------------------------------------------------
ona.pas <- rbind(bc.pa, wa.pas)

  # Check this:
plot(st_geometry(ona.pas))

st_write(ona.pas, "Data/processed/ona_PAs.shp", append = FALSE) 


################################# Prep Grizzly Population Units:

# Check Projections: ------------------------------------------------------
st_crs(grizzpop.reproj) == st_crs(ona.buffer) #TRUE

# Plot these together to see overlap:
plot(st_geometry(grizzpop.reproj))


# Filter these to just the extant populations: ----------------------------

extant.grizz <- filter(grizzpop.reproj, POP_NAME == "South Chilcotin Ranges" | POP_NAME == "Squamish-Lillooet" | POP_NAME == "Columbia-Shuswap"
                       | POP_NAME == "Central Monashee" | POP_NAME == "Valhalla" | POP_NAME == "Kettle-Granby" | POP_NAME == "Central Selkirk"
                       | POP_NAME == "Wells Gray" | POP_NAME == "South Selkirk")

# Plot with our boundary to see overlap/position
plot(st_geometry(extant.grizz))
plot(st_geometry(ona.buffer), add=TRUE)

# Save this for later:
st_write(extant.grizz, "Data/processed/Extant Grizzly Pop Units.shp", append=FALSE) 

################################# Prep Human Density Predictor:

# Reproject the Data: --------------------------------------------------
ona.buf.reproj <- project(ona.buf, world.hum.dens)
world.dens.crop <- crop(world.hum.dens, ona.buf.reproj)


# Save Raster as .tif for later: ----------------------------------------------------
terra::writeRaster(world.dens.crop, "Data/processed/human_dens_ona.tif")


# ################################## Combine the Hydrology (Lake) Datasets:
# bc.lakes <- st_read("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/BC Hydrology/BCGW_7113060B_165298896733_5124/FWA_LAKES_POLY/FWLKSPL_polygon.shp")
# 
# fgdb <- "/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/WA Hydrology/hydro.gdb"
# 
#   # List all feature classes in a file geodatabase
# subset(ogrDrivers(), grepl("GDB", name))
# fc_list <- ogrListLayers(fgdb)
# print(fc_list)
# 
#   # Read the feature class for PA shapes
# fc <- readOGR(dsn=fgdb,layer="wbhydro")
# fc.sf <- as(fc, "sf") 
# 
#   #  Filter to just LAKES:
# wa.lakes <- fc.sf %>% 
#   filter(., WB_HYDR_FTR_LABEL_NM == "Lake") %>% 
#   st_make_valid()
# 
#   # Reproject data:
# wa.lakes.reproj <- st_transform(wa.lakes, st_crs(bc.pas))
# bc.lakes.reproj <- st_transform(bc.lakes, st_crs(bc.pas))
# 
#   # Crop to ONA:
# bc.ona.lakes <- st_intersection(bc.lakes.reproj, ona.buffer) 
# wa.ona.lakes <- st_intersection(wa.lakes.reproj, ona.buffer) 
# 
#   # Subset only the columns we need: ----------------------------------------
# wa.ona.lakes.subs <- wa.ona.lakes[, c("WB_HYDR_FTR_LABEL_NM", "SHAPE_Area", "ELEVATION" , "geometry" )]
# bc.ona.lakes.subs <- bc.ona.lakes[, c("WTRBDTP", "AREA_SQM", "ELEVATION", "geometry" )]
# 
# names(wa.ona.lakes.subs)[names(wa.ona.lakes.subs) == "WB_HYDR_FTR_LABEL_NM"] <- "Hydro Type"
# names(wa.ona.lakes.subs)[names(wa.ona.lakes.subs) == "SHAPE_Area"] <- "AREA_SQM"
# names(bc.ona.lakes.subs)[names(bc.ona.lakes.subs) == "WTRBDTP"] <- "Hydro Type"
# 
#   # Join the WA and BC PAs Data: -------------------------------------------------
# ona.lakes <- rbind(wa.ona.lakes.subs, bc.ona.lakes.subs)
# 
#   # Filter size of lakes: for both female and male bears
#   # Female bears will cross up to 1.5km, male bears sometimes up to 6km
# female.resist.lakes <- ona.lakes %>% 
#   filter(., AREA_SQM > 1500) 
# 
# male.resist.lakes <- ona.lakes %>% 
#   filter(., AREA_SQM > 6000) 
# 
# average.resist.lakes <- ona.lakes %>% 
#   filter(., AREA_SQM > 3500) 
# 
# 
# #   Calculating Lake Width & Length: --------------------------------------
# 
#   # Convert to lake object:
# ona.lakes.sp <- as(ona.lakes, "Spatial")
# ona.lakemorpho <- lakeMorphoClass(ona.lakes.sp)
# 
#   # Prep Elevation Rasters:
# griz_dens <- rast(here("/Users/shannonspragg/Grizz-Connectivity/Data/original/grizz_dens.tif"))
# ona_vec <- vect(ona.buffer)
# 
# elev.can <- rast(raster::getData('alt', country = 'CAN'))
# elev.us <- rast(raster::getData('alt', country = 'USA')[[1]])
# elev <- mosaic(elev.can, elev.us)
# 
# elev.proj <- terra::project(elev, griz_dens, method="bilinear")
# elev.crop <- crop(elev.proj, ona_vec)
# 
#   # Calculate Lake Metrics:
# ona.laketopo <- lakeSurroundTopo(ona.lakes.sp, elev.crop) # This only takes one lake input at a time....
# lakeMaxWidth(ona.lakemorpho, 50)
# 
# 
#   # Check this:
# plot(st_geometry(ona.lakes))
# 
# st_write(ona.lakes, "/Users/shannonspragg/Grizz-Connectivity/Data/processed/ona_lakes.shp", overwrite=TRUE) 
# st_write(female.resist.lakes, "/Users/shannonspragg/Grizz-Connectivity/Data/processed/ona_lakes_female_resist.shp", overwrite=TRUE) 
# st_write(male.resist.lakes, "/Users/shannonspragg/Grizz-Connectivity/Data/processed/ona_lakes_male_resist.shp", overwrite=TRUE) 
# st_write(average.resist.lakes, "/Users/shannonspragg/Grizz-Connectivity/Data/processed/ona_lakes_avg_resist.shp", overwrite=TRUE) 



