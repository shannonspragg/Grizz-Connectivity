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
  # Ag Data:
farm.type <- read.csv("/Users/shannonspragg/Grizz-Connectivity/Data/original/farm type_32100403.csv")
animal.farm.wa <- read.csv("/Users/shannonspragg/Grizz-Connectivity/Data/original/Animal Farming WA.csv")
crop.farm.wa <- read.csv("/Users/shannonspragg/Grizz-Connectivity/Data/original/Crop Farming WA.csv")

  # Census Divisions (CCS and county):
can.ccs.shp<-st_read("/Users/shannonspragg/Grizz-Connectivity/Data/original/lccs000b16a_e.shp")
wa.county.shp <- st_read("/Users/shannonspragg/Grizz-Connectivity/Data/original/tl_2016_53_cousub.shp")

  #Protected Areas:
bc.pas <- st_read("/Users/shannonspragg/Grizz-Connectivity/Data/original/Parks_Combined2.shp")
wa.desig <- st_read("/Users/shannonspragg/Grizz-Connectivity/Data/original/PADUS2_1Designation_StateWA.shp")
wa.procs <- st_read("/Users/shannonspragg/Grizz-Connectivity/Data/original/PADUS2_1Proclamation_StateWA.shp")

  # Grizzly Population Units:
grizz.units <- st_read("/Users/shannonspragg/Grizz-Connectivity/Data/original/GBPU_BC_polygon.shp")

  # Grizz Inc:
grizz.inc.bc <- rast("/Users/shannonspragg/Grizz-Connectivity/Data/original/grizz.increase.map.fixed.tif") #  the proportion of people within a census that 
# grizz.inc.wa <- rast("/Users/shannonspragg/Grizz-Connectivity/Data/original/grizz.increase.map.fixed.tif") #  the proportion of people within a census that 

  # ONA Territory:
ona.bound <- st_read("/Users/shannonspragg/Grizz-Connectivity/Data/original/ONA_TerritoryBound.shp") 

# Reproject & Prep ONA Boundary -------------------------------------------
  # We want to match our data to BC Albers
ccs.reproj <- st_transform(can.ccs.shp, st_crs(bc.pas))
wa.county.reproj <- st_transform(wa.county.shp, st_crs(bc.pas))
wa.desig.reproj <- st_transform(wa.desig, st_crs(bc.pas))
wa.proc.reproj <- st_transform(wa.procs, st_crs(bc.pas))
grizzpop.reproj <- st_transform(grizz.units, st_crs(bc.pas))
ona.reproj <- st_transform(ona.bound, st_crs(bc.pas))
bc.pas.reproj <- st_transform(st_make_valid(bc.pas), st_crs(ona.reproj))

st_crs(ccs.reproj) == st_crs(bc.pas) #TRUE
st_crs(wa.county.reproj) == st_crs(wa.proc.reproj) #TRUE
st_crs(grizzpop.reproj) == st_crs(ona.reproj) #TRUE

  # Make our SOI template raster:
ona.vect <- vect(ona.reproj)
grizz.inc.templ <- terra::project(grizz.inc.bc, crs(ona.vect))
grizzinc.crop.t <- terra::crop(grizz.inc.templ, ona.vect)  

plot(grizzinc.crop.t)
plot(ona.vect, add=TRUE)

ona.rast.templ <- rast(ona.vect, nrows= 2027, ncols=1175, nlyrs=1, xmin=1340199, xmax=1658767, ymin=306186.8, ymax=855751.3)
ona.rast <- terra::rasterize(ona.vect, ona.rast.templ, field = "HANDLE")
ona.rast <- resample(ona.rast, grizzinc.crop.t, method='bilinear')
ona.rast[ona.rast == 27] <- 0

  # Export as tiff:
terra::writeRaster(ona.rast, "/Users/shannonspragg/Grizz-Connectivity/Data/processed/ona_bound.tif")

############################# Prep Agriculture Variables:
####################### Now, we will filter the CCS regions and Agriculture Data to BC:

# Filter CCS and Ag Files to BC Only ---------------------------------------------------
# Make sf and filter down to only British Columbia for Census SubDivs (CCS):
can.ccs.sf<- as(ccs.reproj, "sf")
unique(can.ccs.sf$PRNAME) # Shows that the name for BC is "British Columbia / Colombie-Britannique"

# Filter down to just BC:
bc.ccs<-can.ccs.sf %>%
  filter(., PRNAME == "British Columbia / Colombie-Britannique") %>%
  st_make_valid()

# Save this for later:
st_write(bc.ccs, "/Users/shannonspragg/Grizz-Connectivity/Data/processed/BC CCS.shp")

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
  filter(., grepl("2016", bc.farm.filter.ccs$REF_DATE)) # Now there are 344 observations

bc.farm.2016.ccs$CCSUID.crop<- str_sub(bc.farm.2016.ccs$GEO,-6,-2) # Now we have a matching 6 digits
unique(bc.farm.2016.ccs$CCSUID.crop) #This is a 5 digit code

# Editing CCS Code into new column for join -------------------------------
  # Here we separate out the CCS code into new column for join with CCS .shp:
bc.ccs$CCSUID.crop<- str_sub(bc.ccs$CCSUID,-5,-1) # Now we have a matching 6 digits
unique(bc.ccs$CCSUID.crop) #This is a 5 digit code
str(bc.farm.2016.ccs) # Check the structure before joining
nique(animal.farm.wa$County.ANSI)
unique(crop.farm.wa$County)
unique(wa.county.reproj$COUNTYFP)

wa.county.reproj$CountyID<- str_sub(wa.county.reproj$GEOID,4,5) 
unique(wa.county.reproj$CountyID) #This is a 3 digit code

# Joining the CCS with the Farm Type: -------------------------------------
head(wa.county.reproj) # Check out our WA county data

  # Join the BC CCS with Ag Files:
farm.ccs.join <- merge(bc.farm.2016.ccs, bc.ccs, by.x = "CCSUID.crop", by.y = "CCSUID.crop") 
wa.animal.join <- merge(animal.farm.wa, wa.county.reproj, by.x = "County.ANSI", by.y = "CountyID") 
wa.crop.join <- merge(crop.farm.wa, wa.county.reproj, by.x = "County.ANSI", by.y = "CountyID") 

  # Double check that this is the correct structure:
farm.ccs.sf <- st_as_sf(farm.ccs.join)
wa.animal.sf <- st_as_sf(wa.animal.join)
wa.crop.sf <- st_as_sf(wa.crop.join)

head(farm.ccs.sf) # Here we have a farm type data frame with Multi-polygon geometry - check!
head(wa.animal.sf)
plot(st_geometry(wa.animal.sf)) # plot to check


# Here we subset the farm data to ONA, and pull out the total farm counts: ---------------------------------
  # Start by cropping the data down to SOI buffer:
farm.ccs.sf <- st_transform(farm.ccs.sf, st_crs(ona.reproj))
wa.animal.sf <- st_transform(wa.animal.sf, st_crs(ona.reproj))
wa.crop.sf <- st_transform(wa.crop.sf, st_crs(ona.reproj))

farm.ccs.bc.ona <- st_intersection(farm.ccs.sf, ona.reproj) 
animal.farms.ona <- st_intersection(wa.animal.sf, ona.reproj) 
crop.farms.ona <- st_intersection(wa.crop.sf, ona.reproj) 
crop.farms.ona$Value <- as.integer(crop.farms.ona$Value)

  # Subset the data - separate total farms out of NAIC:
farm.ona.subset <- subset(farm.ccs.bc.ona, North.American.Industry.Classification.System..NAICS. != "Total number of farms")
names(farm.ona.subset)[names(farm.ona.subset) == "North.American.Industry.Classification.System..NAICS."] <- "N_A_I_C"

  # Condense Farm Types to Animal & Ground Crop Production:
animal.product.farming <- dplyr::filter(farm.ona.subset, N_A_I_C == "Beef cattle ranching and farming, including feedlots [112110]" | N_A_I_C == "Cattle ranching and farming [1121]" 
                                        | N_A_I_C == "Dairy cattle and milk production [112120]" | N_A_I_C == "Hog and pig farming [1122]" | N_A_I_C == "Poultry and egg production [1123]"
                                        | N_A_I_C == "Chicken egg production [112310]" | N_A_I_C == "Broiler and other meat-type chicken production [112320]" | N_A_I_C == "Turkey production [112330]"
                                        | N_A_I_C == "Poultry hatcheries [112340]" | N_A_I_C == "Combination poultry and egg production [112391]" | N_A_I_C == "All other poultry production [112399]"
                                        | N_A_I_C == "Sheep and goat farming [1124]" | N_A_I_C == "Sheep farming [112410]" | N_A_I_C == "Goat farming [112420]" | N_A_I_C =="Other animal production [1129]"
                                        | N_A_I_C == "Apiculture [112910]" | N_A_I_C == "Horse and other equine production [112920]" | N_A_I_C == "Fur-bearing animal and rabbit production [112930]"
                                        | N_A_I_C == "Animal combination farming [112991]" | N_A_I_C == "All other miscellaneous animal production [112999]") 


ground.crop.production <- dplyr::filter(farm.ona.subset, N_A_I_C == "Fruit and tree nut farming [1113]" | N_A_I_C == "Greenhouse, nursery and floriculture production [1114]" | N_A_I_C == "Vegetable and melon farming [1112]"
                                        | N_A_I_C == "Oilseed and grain farming [1111]" | N_A_I_C == "Soybean farming [111110]" | N_A_I_C == "Oilseed (except soybean) farming [111120]"
                                        | N_A_I_C == "Dry pea and bean farming [111130]" | N_A_I_C == "Wheat farming [111140]" | N_A_I_C == "Corn farming [111150]" | N_A_I_C == "Other grain farming [111190]"
                                        | N_A_I_C == "Potato farming [111211]" | N_A_I_C == "Other vegetable (except potato) and melon farming [111219]" | N_A_I_C == "Mushroom production [111411]" 
                                        | N_A_I_C == "Other food crops grown under cover [111419]" | N_A_I_C == "Nursery and tree production [111421]" | N_A_I_C == "Floriculture production [111422]" 
                                        | N_A_I_C == "Other crop farming [1119]" | N_A_I_C == "Tobacco farming [111910]" | N_A_I_C == "Hay farming [111940]" | N_A_I_C == "Fruit and vegetable combination farming [111993]"
                                        | N_A_I_C == "Maple syrup and products production [111994]" | N_A_I_C == "All other miscellaneous crop farming [111999]" )

  # Total the counts of these farm categories by CCS region:
animal.prod.bc.counts <- aggregate(cbind(VALUE) ~ CCSUID, data= animal.product.farming, FUN=sum)
ground.crop.bc.counts <- aggregate(cbind(VALUE) ~ CCSUID, data= ground.crop.production, FUN=sum)

animal.prod.wa.counts <- aggregate(cbind(Value) ~ County.ANSI, data= animal.farms.ona, FUN=sum)
ground.crop.wa.counts <- aggregate(cbind(Value) ~ County.ANSI, data= crop.farms.ona, FUN=sum)


names(animal.prod.bc.counts)[names(animal.prod.bc.counts) == "VALUE"] <- "Total Farms in CCS"
names(ground.crop.bc.counts)[names(ground.crop.bc.counts) == "VALUE"] <- "Total Farms in CCS"
names(animal.prod.wa.counts)[names(animal.prod.wa.counts) == "Value"] <- "Total Farms in County"
names(ground.crop.wa.counts)[names(ground.crop.wa.counts) == "Value"] <- "Total Farms in County"

  # Join this back to our data as a total column:
animal.prod.bc.join <- merge(animal.prod.bc.counts, animal.product.farming, by.x = "CCSUID", by.y = "CCSUID") 
ground.crop.bc.join <- merge(ground.crop.bc.counts, ground.crop.production, by.x = "CCSUID", by.y = "CCSUID") 
animal.prod.wa.join <- merge(animal.prod.wa.counts, animal.farms.ona, by.x = "County.ANSI", by.y = "County.ANSI") 
ground.crop.wa.join <- merge(ground.crop.wa.counts, crop.farms.ona, by.x = "County.ANSI", by.y = "County.ANSI") 

animal.prod.bc.sf <- st_as_sf(animal.prod.bc.join)
ground.crop.bc.sf <- st_as_sf(ground.crop.bc.join)
animal.prod.wa.sf <- st_as_sf(animal.prod.wa.join)
ground.crop.wa.sf <- st_as_sf(ground.crop.wa.join)

# Calculate the Density of Farm Types: ------------------------------------
  # We do so by dividing the count of farms by the overall area of the farm type categories (for our 10km buffered area, but save this to the 50km dataset 
  # so that we have values on the edge of our 10km zone):

  # Calculate our areas for the two objects: 
animal.prod.bc.sf$AREA_SQM <- st_area(animal.prod.bc.sf)
ground.crop.bc.sf$AREA_SQM <- st_area(ground.crop.bc.sf)
animal.prod.wa.sf$AREA_SQM <- st_area(animal.prod.wa.sf)
ground.crop.wa.sf$AREA_SQM <- st_area(ground.crop.wa.sf)


  # Make our area units kilometers:
animal.prod.bc.sf$AREA_SQ_KM <- set_units(animal.prod.bc.sf$AREA_SQM, km^2)
ground.crop.bc.sf$AREA_SQ_KM <- set_units(ground.crop.bc.sf$AREA_SQM, km^2)
animal.prod.wa.sf$AREA_SQ_KM <- set_units(animal.prod.wa.sf$AREA_SQM, km^2)
ground.crop.wa.sf$AREA_SQ_KM <- set_units(ground.crop.wa.sf$AREA_SQM, km^2)


  # Now we make a new col with our farms per sq km:
animal.prod.bc.sf$Farms_per_sq_km <- animal.prod.bc.sf$`Total Farms in CCS` / animal.prod.bc.sf$AREA_SQ_KM
head(animal.prod.bc.sf)

ground.crop.bc.sf$Farms_per_sq_km <- ground.crop.bc.sf$`Total Farms in CCS` / ground.crop.bc.sf$AREA_SQ_KM
head(ground.crop.bc.sf)

animal.prod.wa.sf$Farms_per_sq_km <- animal.prod.wa.sf$`Total Farms in County` / animal.prod.wa.sf$AREA_SQ_KM
head(animal.prod.wa.sf)

ground.crop.wa.sf$Farms_per_sq_km <- ground.crop.wa.sf$`Total Farms in County` / ground.crop.wa.sf$AREA_SQ_KM
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

animal.prod.wa <- animal.prod.wa.sf[, c( "County.ANSI", "Total Farms in County", "GEOID", "Value", "NAME", "AREA_SQ_KM"
                                         , "Farms_per_sq_km", "geometry")]
                                    
ground.crop.wa <- ground.crop.wa.sf[, c("County.ANSI", "Total Farms in County", "GEOID", "Value", "NAME", "AREA_SQ_KM"
                                        , "Farms_per_sq_km", "geometry" )]

  # Need to match column names for rbind:
names(animal.prod.bc)[names(animal.prod.bc) == "CCSUID"] <- "CensusID"
names(ground.crop.bc)[names(ground.crop.bc) == "CCSUID"] <- "CensusID"
names(animal.prod.wa)[names(animal.prod.wa) == "County.ANSI"] <- "CensusID"
names(ground.crop.wa)[names(ground.crop.wa) == "County.ANSI"] <- "CensusID"
names(animal.prod.bc)[names(animal.prod.bc) == "Total Farms in CCS"] <- "Total Farms in Census Region"
names(ground.crop.bc)[names(ground.crop.bc) == "Total Farms in CCS"] <- "Total Farms in Census Region"
names(animal.prod.wa)[names(animal.prod.wa) == "Total Farms in County"] <- "Total Farms in Census Region"
names(ground.crop.wa)[names(ground.crop.wa) == "Total Farms in County"] <- "Total Farms in Census Region"
names(animal.prod.wa)[names(animal.prod.wa) == "GEOID"] <- "GEO"
names(ground.crop.wa)[names(ground.crop.wa) == "GEOID"] <- "GEO"
names(animal.prod.wa)[names(animal.prod.wa) == "Value"] <- "VALUE"
names(ground.crop.wa)[names(ground.crop.wa) == "Value"] <- "VALUE"
names(animal.prod.bc)[names(animal.prod.bc) == "CCSNAME"] <- "NAME"
names(ground.crop.bc)[names(ground.crop.bc) == "CCSNAME"] <- "NAME"

# Merge these to make ONA animal and crop farms: --------------------------
animal.prod.ona <- rbind(animal.prod.bc, animal.prod.wa)
ground.crop.ona <- rbind(ground.crop.bc, ground.crop.wa)

# Save these as .shp's for later:
st_write(animal.prod.ona,"/Users/shannonspragg/Grizz-Connectivity/Data/processed/ONA Animal Product Farming.shp")

st_write(ground.crop.ona, "/Users/shannonspragg/Grizz-Connectivity/Data/processed/ONA Ground Crop Production.shp") 

################################ Combine our Protected Area Datasets:

# Start by Filtering to Gap Status 1 & 2: ------------------------------
wa.desig.filter <- dplyr::filter(wa.desig.reproj, GAP_Sts == "1" | GAP_Sts == "2" )
head(wa.desig.filter)
#wa.proc.filter <- dplyr::filter(wa.desig.reproj, Mang_Name == "NPS" | Mang_Name == "USFS" ) # I don't think we need these

# Crop our PA's to ONA Boundary: ------------------------------------------

bc.ona.pas <- st_intersection(bc.pas.reproj, ona.reproj) 
wa.ona.pas <- st_intersection(wa.desig.filter, ona.reproj) 

# Subset only the columns we need: ----------------------------------------
wa.pas <- wa.ona.pas[, c("Unit_Nm", "d_Des_Tp", "Mang_Type", "geometry" )]
bc.pa <- bc.ona.pas[, c("UNIT_NAME", "UNIT_TYPE", "TYPE", "geometry" )]

names(wa.pas)[names(wa.pas) == "Unit_Nm"] <- "UNIT_NAME"
names(wa.pas)[names(wa.pas) == "d_Des_Tp"] <- "UNIT_TYPE"
names(wa.pas)[names(wa.pas) == "Mang_Type"] <- "TYPE"


# Join the WA and BC PAs Data: -------------------------------------------------
ona.pas <- rbind(bc.pa, wa.pas)

  # Check this:
plot(st_geometry(ona.pas))

st_write(ona.pas, "/Users/shannonspragg/Grizz-Connectivity/Data/processed/ona_PAs.shp") 


################################# Prep Grizzly Population Units:

# Check Projections: ------------------------------------------------------
st_crs(grizzpop.reproj) == st_crs(ona.reproj) #TRUE

# Plot these together to see overlap:
plot(st_geometry(grizzpop.reproj))


# Filter these to just the extant populations: ----------------------------

extant.grizz <- filter(grizzpop.reproj, POP_NAME == "South Chilcotin Ranges" | POP_NAME == "Squamish-Lillooet" | POP_NAME == "Columbia-Shuswap"
                       | POP_NAME == "Central Monashee" | POP_NAME == "Valhalla" | POP_NAME == "Kettle-Granby" | POP_NAME == "Central Selkirk"
                       | POP_NAME == "Wells Gray" | POP_NAME == "South Selkirk")

# Plot with our boundary to see overlap/position
plot(st_geometry(extant.grizz))
plot(st_geometry(ona.reproj), add=TRUE)

# Save this for later:
st_write(extant.grizz, "/Users/shannonspragg/Grizz-Connectivity/Data/processed/Extent Grizzly Pop Units.shp") 


