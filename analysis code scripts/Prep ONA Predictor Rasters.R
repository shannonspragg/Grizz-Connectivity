# Prepping our Rasters: -------------------------------------------------------
  # Here we bring and produce in our predictor rasters and make sure they are all extended to ONA and equally projected:

# Load Packages: ----------------------------------------------------------
library(sf)
library(tidyverse)
library(dplyr)
library(raster)
library(terra)
library(dismo)
library(stars)
library(measurements)

# Load Data: --------------------------------------------------------------

  # Animal & Crop Farms:
animal.prod.ona <- st_read("/Users/shannonspragg/Grizz-Connectivity/Data/processed/ONA Animal Product Farming.shp")
ground.crop.ona <- st_read("/Users/shannonspragg/Grizz-Connectivity/Data/processed/ONA Ground Crop Production.shp")

  # Grizzinc: 
grizz.inc.comb <- rast("/Users/shannonspragg/Grizz-Connectivity/Data/processed/grizz.inc.combtif") #  the proportion of people within a census that 

  # Bear Density - Bear Habitat Suitability (BHS):
bhs.rast <- rast("/Users/shannonspragg/Grizz-Connectivity/Data/original/grizz_dens.tif")

  # Biophysical Current Map (Cumulative current flow shows the total current for each landscape pixel):
biophys.rast <- rast("/Users/shannonspragg/Grizz-Connectivity/Data/original/biophys_cum_currmap.tif") 

  # SOI Region for plotting:
ona.bound <- st_read("/Users/shannonspragg/Grizz-Connectivity/Data/original/ONA_TerritoryBound.shp") 
ona.rast <- rast("/Users/shannonspragg/Grizz-Connectivity/Data/processed/ona_bound.tif")

# PA and Metro Data: (need to be cropped)
ona.PAs <- st_read("/Users/shannonspragg/Grizz-Connectivity/Data/original/ona_PAs.shp") 

# Extent Grizzly Populations:
extant.grizz <- st_read("/Users/shannonspragg/Grizz-Connectivity/Data/processed/Extent Grizzly Pop Units.shp")


# Check Projections: ------------------------------------------------------
ona.animal.reproj <- st_make_valid(animal.prod.ona) %>% 
  st_transform(crs=crs(ona.rast))
ona.crop.reproj <- st_make_valid(ground.crop.ona) %>% 
  st_transform(crs=crs(ona.rast))
ona.PAs.reproj <- st_make_valid(ona.pas) %>% 
  st_transform(crs=crs(ona.rast))
ona.bound.reproj <- st_make_valid(ona.bound) %>% 
  st_transform(crs=crs(ona.rast))
grizz.pop.reproj <- st_make_valid(extant.grizz) %>% 
  st_transform(crs=crs(ona.rast))

# Check to see if they match:
st_crs(ona.bound.reproj) == st_crs(ona.PAs.reproj) # [TRUE] 
st_crs(grizz.pop.reproj) == st_crs(ona.bound.reproj) # [TRUE]
st_crs(ona.animal.reproj) == st_crs(ona.crop.reproj) # [TRUE]


################################# First, we need to produce our Agriculture Density Rasters:


# Rasterize Farm Data & WARP Points ---------------------------------------
  ## Here we make rasters for the farm type categories within our SOI region:

  # Make these spat vectors:
animal.prod.sv <- vect(ona.animal.reproj)
ground.crop.sv <- vect(ona.crop.reproj)

  # Rasterize our subset rasters:
animal.prod.rast <- terra::rasterize(animal.prod.sv, ona.rast, field = "Frms___")
ground.crop.rast <- terra::rasterize(ground.crop.sv, ona.rast, field = "Frms___")

  # Our extreme values are preventing others from showing (see hist(animal.prod.rast))
animal.prod.rast[animal.prod.rast > 10] <- 0
plot(animal.prod.rast)
ground.crop.rast[ground.crop.rast > 20] <- 0
plot(ground.crop.rast)

  # Fix the column names:
names(animal.prod.rast)[names(animal.prod.rast) == "Frms___"] <- "Density of Animal Product & Meat Farming"
names(ground.crop.rast)[names(ground.crop.rast) == "Frms___"] <- "Density of Ground Crop & Produce Farming"

  # Save these Farm Rasters:
terra::writeRaster(animal.prod.rast, "/Users/shannonspragg/Grizz-Connectivity/Data/processed/animal_production_ona_raster.tif")
terra::writeRaster(ground.crop.rast, "/Users/shannonspragg/Grizz-Connectivity/Data/processed/ground_crop_ona_raster.tif" )


########################################### Next, we make our Distance to PA and Grizzly Pop Rasters:
# Rasterize our Points & Polygons: ----------------------------------------

# Make our data spatvectors:
PAs.ona <- vect(ona.PAs.reproj) 
grizz.pop.sv <- vect(extant.grizz)

# Create a Continuous Raster for Cell Distance to PA's: -------------------

# Do this for our variables:
dist.pa.raster <- terra::distance(ona.rast, PAs.ona) 

dist.grizz.pop.raster <- terra::distance(ona.rast, grizz.pop.sv) 

# Check this to see if it looks right:
plot(dist.pa.raster) # Plot our PAs
plot(dist.grizz.pop.raster)  # Plot our grizz pops

# Make sure our rasters are in km:
dist.pa.raster <- conv_unit(dist.pa.raster,"m","km") # There we go
dist.grizz.pop.raster <- conv_unit(dist.grizz.pop.raster,"m","km")

names(dist.pa.raster)[names(dist.pa.raster) == "HANDLE"] <- "Distance to Nearest PA (km)"
names(dist.grizz.pop.raster)[names(dist.grizz.pop.raster) == "HANDLE"] <- "Distance to Nearest Extent Grizzly Pop (km)"


######################################## Check all our Rasters:
# Check Projections: ------------------------------------------------------
   # Bear Density (BHS) Estimate:
bhs.reproj <- terra::project(bhs.rast, crs(ona.rast))
  # Biophys Map:
biophys.reproj <- terra::project(biophys.rast, crs(ona.rast))
  # Grizzinc:
grizzinc.reproj <- terra::project(grizz.inc.comb, crs(ona.rast))


crs(ona.rast) == crs(grizzinc.reproj) #TRUE
crs(bhs.reproj) == crs(biophys.reproj) #TRUE


  # Crop these Rasters to ONA extant:
biophys.crop <- terra::crop(biophys.reproj, ona.rast)

  # Expand the grizz_dens extant to include WA:
bhs.crop <- terra::crop(bhs.reproj, ona.rast)

  # Make all other values zero:
bhs.no.na <- classify(bhs.crop, cbind(NA, 0))

# Resample to match extents and res:
biophys.rsmple <- resample(biophys.crop, ona.rast, method='bilinear')
bhs.rsmple <- resample(bhs.no.na, ona.rast, method='bilinear')
grizzinc.comb.rsmple <- resample(grizz.inc.comb, ona.rast, method='bilinear')


# Plot Check:
ona.bound.vect <- vect(ona.bound.reproj)

plot(grizz.inc.comb)
plot(ona.bound.vect, add=TRUE)

plot(biophys.rsmple)
plot(ona.bound.vect, add=TRUE)

plot(bhs.rsmple)
plot(ona.bound.vect, add=TRUE)


# Cut these down to the SOI Boundary: -------------------------------------

grizzinc.ona <- terra::mask(grizz.inc.comb, ona.bound.vect) 
biophys.ona <- terra::mask(biophys.rsmple, ona.bound.vect) 
bhs.ona <- terra::mask(bhs.rsmple, ona.bound.vect) 
d2pa.ona <- terra::mask(dist.pa.raster, ona.bound.vect) 
d2grizzpop.ona <- terra::mask(dist.grizz.pop.raster, ona.bound.vect) 

plot(grizzinc.ona)
plot(biophys.ona)
plot(bhs.ona)
plot(d2pa.ona)
plot(d2grizzpop.ona)

# Fix the column names:
names(grizzinc.ona)[names(grizzinc.ona) == "grizz.increase.map.fixed"] <- "Support for Grizzly Increase"
names(biophys.ona)[names(biophys.ona) == "biophys_cum_currmap"] <- "Biophysical Connectivity Current Map"
names(bhs.ona)[names(bhs.ona) == "Height"] <- "Bear Habitat Suitability (BHS)"


# Save our Cropped Rasters: -----------------------------------------------
terra::writeRaster(grizzinc.ona, "/Users/shannonspragg/Grizz-Connectivity/Data/processed/grizz_inc_ona.tif")
terra::writeRaster(biophys.ona, "/Users/shannonspragg/Grizz-Connectivity/Data/processed/biophys_ona.tif")
terra::writeRaster(bhs.ona, "/Users/shannonspragg/Grizz-Connectivity/Data/processed/bhs_ona.tif")
terra::writeRaster(d2pa.ona, "/Users/shannonspragg/Grizz-Connectivity/Data/processed/dist2pa_ona.tif") # already saved
terra::writeRaster(d2grizzpop.ona, "/Users/shannonspragg/Grizz-Connectivity/Data/processed/dist2grizz_pop_ona.tif") # already saved


