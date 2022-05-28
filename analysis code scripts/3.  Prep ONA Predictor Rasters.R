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
animal.prod.ona <- st_read("Data/processed/ONA Animal Product Farming.shp")
ground.crop.ona <- st_read("Data/processed/ONA Ground Crop Production.shp")

  # Grizzinc: 
grizz.inc.comb <- rast("Data/processed/grizz.inc.comb.tif") #  the proportion of people within a census that 

  # Bear Density - Bear Habitat Suitability (BHS):
bhs.rast <- rast("Data/processed/grizz_source_ona.tif")

  # Biophysical Current Map (Cumulative current flow shows the total current for each landscape pixel):
biophys.rast <- rast("Data/original/ona_cum_currmap.tif") 

  # SOI Region for plotting:
ona.buffer <- st_read("Data/processed/ona_buffer_bound.shp") 
ona.buf.rast <- rast("Data/processed/ona_buf_bound.tif")

  # PA and Metro Data: (need to be cropped)
ona.PAs <- st_read("Data/processed/ona_PAs.shp") 

  # Extent Grizzly Populations:
extant.grizz <- st_read("Data/processed/Extant Grizzly Pop Units.shp")

  # Human Density:
hm.dens <- rast("Data/processed/human_dens_ona.tif")

# Check Projections: ------------------------------------------------------
ona.animal.reproj <- st_make_valid(animal.prod.ona) %>% 
  st_transform(crs=crs(ona.buf.rast))
ona.crop.reproj <- st_make_valid(ground.crop.ona) %>% 
  st_transform(crs=crs(ona.buf.rast))
ona.PAs.reproj <- st_make_valid(ona.pas) %>% 
  st_transform(crs=crs(ona.buf.rast))
ona.bound.reproj <- st_make_valid(ona.bound) %>% 
  st_transform(crs=crs(ona.buf.rast))
grizz.pop.reproj <- st_make_valid(extant.grizz) %>% 
  st_transform(crs=crs(ona.buf.rast))

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
animal.prod.rast <- terra::rasterize(animal.prod.sv, ona.buf.rast, field = "Frms___")
ground.crop.rast <- terra::rasterize(ground.crop.sv, ona.buf.rast, field = "Frms___")

  # Our extreme values are preventing others from showing (see hist(animal.prod.rast))
animal.prod.rast[animal.prod.rast > 10] <- 0
plot(animal.prod.rast)
ground.crop.rast[ground.crop.rast > 20] <- 0
plot(ground.crop.rast)

  # Fix the column names:
names(animal.prod.rast)[names(animal.prod.rast) == "Frms___"] <- "Density of Animal Product & Meat Farming"
names(ground.crop.rast)[names(ground.crop.rast) == "Frms___"] <- "Density of Ground Crop & Produce Farming"

  # Save these Farm Rasters:
terra::writeRaster(animal.prod.rast, "Data/processed/animal_production_ona_raster.tif", overwrite=TRUE)
terra::writeRaster(ground.crop.rast, "Data/processed/ground_crop_ona_raster.tif", overwrite=TRUE )


########################################### Next, we make our Distance to PA and Grizzly Pop Rasters:
# Rasterize our Points & Polygons: ----------------------------------------

# Make our data spatvectors:
PAs.ona <- vect(ona.PAs.reproj) 
grizz.pop.sv <- vect(extant.grizz)

# Create a Continuous Raster for Cell Distance to PA's: -------------------

# Do this for our variables:
dist.pa.raster <- terra::distance(ona.buf.rast, PAs.ona) 

dist.grizz.pop.raster <- terra::distance(ona.buf.rast, grizz.pop.sv) 

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
bhs.reproj <- terra::project(bhs.rast, crs(ona.buf.rast))
  # Biophys Map:
biophys.reproj <- terra::project(biophys.rast, crs(ona.buf.rast))
  # Grizzinc:
grizzinc.reproj <- terra::project(grizz.inc.comb, crs(ona.buf.rast))
  # Human Density:

crs(ona.buf.rast) == crs(grizzinc.reproj) #TRUE
crs(bhs.reproj) == crs(biophys.reproj) #TRUE


  # Crop these Rasters to ONA extant:
biophys.crop <- terra::crop(biophys.reproj, ona.buf.rast)

  # Expand the grizz_dens extant to include WA:
bhs.crop <- terra::crop(bhs.reproj, ona.buf.rast)

# Resample to match extents and res:
biophys.rsmple <- resample(biophys.crop, ona.buf.rast, method='bilinear')

bhs.rsmple <- resample(bhs.crop, ona.buf.rast, method='bilinear')
grizzinc.comb.rsmple <- resample(grizz.inc.comb, ona.buf.rast, method='bilinear')


# Plot Check:
ona.bound.vect <- vect(ona.buffer)

plot(grizz.inc.comb)
plot(ona.bound.vect, add=TRUE)

plot(biophys.rsmple)
plot(ona.bound.vect, add=TRUE)

plot(bhs.rsmple)
plot(ona.bound.vect, add=TRUE)


# Cut these down to the SOI Boundary: -------------------------------------

grizzinc.ona <- terra::mask(grizzinc.comb.rsmple, ona.bound.vect) 
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
names(biophys.norm.ona)[names(biophys.norm.ona) == "biophys_normalized_cum_currmap"] <- "Biophysical Connectivity Normalized Current Map"

names(bhs.ona)[names(bhs.ona) == "Height"] <- "Bear Habitat Suitability (BHS)"


# Save our Cropped Rasters: -----------------------------------------------
terra::writeRaster(grizzinc.ona, "Data/processed/grizz_inc_ona.tif", overwrite=TRUE)
terra::writeRaster(biophys.ona, "Data/processed/biophys_ona.tif", overwrite=TRUE)
terra::writeRaster(biophys.norm.ona, "Data/processed/biophys_normalized_ona.tif", overwrite=TRUE)

terra::writeRaster(bhs.ona, "Data/processed/bhs_ona.tif", overwrite=TRUE)
terra::writeRaster(d2pa.ona, "Data/processed/dist2pa_ona.tif", overwrite=TRUE) # already saved
terra::writeRaster(d2grizzpop.ona, "Data/processed/dist2grizz_pop_ona.tif", overwrite=TRUE) # already saved


