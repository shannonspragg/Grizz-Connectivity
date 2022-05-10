# Produce Probability of Bear Conflict ONA Raster: ------------------------


# Load Packages: ----------------------------------------------------------
library(sf)
library(tidyverse)
library(dplyr)
library(raster)
library(terra)
library(dismo)
library(sjPlot)
library(nloptr)
library(sjmisc)
library(rsq)
library(tidybayes)
library(pROC)
library(bayestestR)
library(corrplot)
library(tidyverse)
library(caret)
library(GGally)
library(ggplot2)
library(rstanarm)
library(loo)
library(projpred)

# Bring in Data: ----------------------------------------------------------
  # WARP Data:
warp.df <- st_read("/Users/shannonspragg/Grizz-Connectivity/Data/original/warp_df_complete.shp")
post.co.offset <- readRDS(file = "/Users/shannonspragg/Grizz-Connectivity/Data/original/post_co_offset.rds")

  # Predictor Rasters:
grizz.inc <- rast( "/Users/shannonspragg/Grizz-Connectivity/Data/processed/grizz_inc_ona.tif")
biophys <- rast( "/Users/shannonspragg/Grizz-Connectivity/Data/processed/biophys_ona.tif")
biophys.norm <- rast( "/Users/shannonspragg/Grizz-Connectivity/Data/processed/biophys_normalized_ona.tif")

bhs <- rast("/Users/shannonspragg/Grizz-Connectivity/Data/processed/bhs_ona.tif")
dist2pa <- rast("/Users/shannonspragg/Grizz-Connectivity/Data/processed/dist2pa_ona.tif") 
dist2grizzpop <- rast("/Users/shannonspragg/Grizz-Connectivity/Data/processed/dist2grizz_pop_ona.tif") 
animal.prod <- rast("/Users/shannonspragg/Grizz-Connectivity/Data/processed/animal_production_ona_raster.tif")
ground.crop <- rast("/Users/shannonspragg/Grizz-Connectivity/Data/processed/ground_crop_ona_raster.tif" )


####################################### Prep WARP Data:
# Scale the Variables: ----------------------------------------------------------
scale2sd <-function(variable){(variable - mean(variable, na.rm=TRUE))/(2*sd(variable, na.rm=TRUE))}

dist.2.pa.co <- scale2sd(warp.df$dst__PA)
animal.farm.dens.co <- warp.df$Anml_Fr
ground.crop.dens.co <- warp.df$Grnd_Cr
dist.2.grizzpop.co <- scale2sd(warp.df$dst__GP)
grizzinc.co <- scale2sd(warp.df$GrizzInc)
bhs.co <- scale2sd(warp.df$BHS)
biophys.co <- scale2sd(warp.df$Biophys)

bears_presence_co <- warp.df$bears # Binomial bears
prob.gen.conf <- warp.df$ProbGeneralConf  # This was already scaled

warp.df$CCSNAME <- as.factor(warp.df$CCSNAME)

CCSNAME.co <- warp.df$CCSNAME

which(is.na(warp.df$CCSNAME.co)) # none


################################# Create P(Conflict) Raster for ONA: 


## Make CCS Varying Intercept Raster: ----------------------------------------------------------

ona.ccs.crop <- st_read( "/Users/shannonspragg/Grizz-Connectivity/Data/processed/ONA Census Districts.shp")
# Bring in one of our rasters for rasterizing polygon data later:
ona.buf.rast <- terra::rast("/Users/shannonspragg/Grizz-Connectivity/Data/processed/ona_buf_bound.tif") # ONA Region 

# Reproject the Data:
ona.ccs.reproj <- st_make_valid(ona.ccs.crop) %>% 
  st_transform(crs=crs(ona.buf.rast))

# Check to see if they match:
st_crs(warp.df) == st_crs(ona.ccs.reproj) # [TRUE] 

# Extracting CCS Varying Intercept from Posterior: ------------------------

# Need to make the ranef(post.pa.full) into a dataframe and join by CCS name to our warp.pres.abs:
varying.int.means.co <- as.data.frame(ranef(post.co.offset))
vary.int.subset.co <- varying.int.means.co[ , c("grp", "condval")]

# Join the tab data with spatial:  
ccs.varint.join.co <- merge(ona.ccs.crop, vary.int.subset.co, by.x = "NAME", by.y = "grp")

# Now that it's spatial, do a spatial join to assign a varying intercept mean to each point:
warp.varint.join <- warp.df %>%
  st_join(., ccs.varint.join.co[, c("NAME", "CenssID", "condval")], left = TRUE) # This gives us duplicate rows!!!

# Merge the multiple rows down to just one for each obs:
warp.varint.merged <- warp.varint.join[!duplicated(warp.varint.join$encontr_d),] # This worked!! but did it do what you wanted? the duplicate rows seem like they are a function of the fact that collisions and pas span multiple counties. Your code doesn't merge things, it drops things. That might be fine, but you want to be aware of the difference


# Make our CCS Raster: ----------------------------------------------------
# Make these spatvectors:
ona.ccs.sv <- vect(ona.ccs.reproj)
warp.co.sv <- vect(warp.varint.join)

# Make our CCS Post Mean Raster: ------------------------------------------
ona.ccs.rast <- terra::rasterize(ona.ccs.sv, ona.buf.rast, field = "NAME")

# Make Raster for our Posterior Means for CCS Varying Intercept: ---------------------
varint.means.rast.co <- terra::rasterize(warp.co.sv, ona.buf.rast, field = "condval")
names(varint.means.rast.co)[names(varint.means.rast.co) == "HANDLE"] <- "CCS Varying Intercept Mean Estimate"

  # Extract Values of Posterior Means to CCS regions: 
warp.varint.mean.ext.co <- terra::extract(varint.means.rast.co, ona.ccs.sv, mean, na.rm = TRUE) 

  # Create New Column(s) for Extracted Values:
ona.ccs.sv$CCSMean <- warp.varint.mean.ext.co[,2] 

# Make our CCS Post Mean Raster: ------------------------------------------
ccs.varint.means.rast.co <- terra::rasterize(ona.ccs.sv, ona.buf.rast, field = "CCSMean")
names(ccs.varint.means.rast.co)[names(ccs.varint.means.rast.co) == "CCSMean"] <- "CCS Varying Intercept Means"

  # Check this:
plot(ccs.varint.means.rast.co) 

  # Merge back to other regions:
ccs.means.merge <- terra::merge(ccs.varint.means.rast.co, ona.ccs.rast)
ccs.means.merge[ccs.means.merge > 1.219815] <- -1.5 #make our max ccs mean value the cutoff

# Save our CCS Post Means Raster: -----------------------------------------
terra::writeRaster(ccs.means.merge, "/Users/shannonspragg/Grizz-Connectivity/Data/processed/CCS_varint_raster_co.tif", overwrite = TRUE )

# Scale our Predictor Rasters: --------------------------------------------
# We can do this Step by Step:
  
  # Distance to PA:
  d2pa.mean.co <- mean(warp.df$dst__PA)
  d2pa.sub.mean.co <- dist2pa - d2pa.mean.co
  d2pa.sd.co <- sd(warp.df$dst__PA)
  dist2pa.rast.co.sc <- d2pa.sub.mean.co / ( 2 * d2pa.sd.co)
  
  # Distance to Grizzly Pops:
  d2grizz.mean.co <- mean(warp.df$dst__GP)
  d2grizz.sub.mean.co <- dist2grizzpop - d2grizz.mean.co
  d2grizz.sd.co <- sd(warp.df$dst__GP)
  dist2grizzpop.rast.co.sc <- d2grizz.sub.mean.co / ( 2 * d2grizz.sd.co)
  
  # Animal Farm Density:
  animal.farm.mean.co <- mean(warp.df$Anml_Fr)
  anim.f.sub.mean.co <- animal.prod - animal.farm.mean.co
  anim.f.sd.co <- sd(warp.df$Anml_Fr)
  animal.farm.rast.co.sc <- anim.f.sub.mean.co / ( 2 * anim.f.sd.co)
  
  # Ground Crop Density:
  ground.crop.mean.co <- mean(warp.df$Grnd_Cr)
  ground.c.sub.mean.co <- ground.crop - ground.crop.mean.co
  ground.c.sd.co <- sd(warp.df$Grnd_Cr)
  ground.crop.rast.co.sc <- ground.c.sub.mean.co / ( 2 * ground.c.sd.co)
  
  # Grizz Increase:
  grizzinc.mean.co <- mean(warp.df$GrzzInc)
  grizz.sub.mean.co <- grizz.inc - grizzinc.mean.co
  grizzinc.sd.co <- sd(warp.df$GrzzInc)
  grizzinc.rast.co.sc <- grizz.sub.mean.co / ( 2 * grizzinc.sd.co)
  
  # Biophys:
  biophys.mean.co <- mean(warp.df$Biophys)
  bio.sub.mean.co <- biophys.norm - biophys.mean.co
  biophys.sd.co <- sd(warp.df$Biophys)
  biophys.norm.rast.co.sc <- bio.sub.mean.co / ( 2 * biophys.sd.co)
  
  # BHS:
  bhs.mean.co <- mean(warp.df$BHS)
  bhs.sub.mean.co <- bhs - bhs.mean.co
  bhs.sd.co <- sd(warp.df$BHS)
  bhs.rast.co.sc <- bhs.sub.mean.co / ( 2 * bhs.sd.co)
  
  # Produce our P(Bear Conflict) Raster: ------------------------------------
  
  # Make sure extents match:
  ext(grizzinc.rast.co.sc) == ext(bhs.rast.co.sc) # TRUE
  ext(biophys.norm.rast.co.sc) == ext(animal.farm.rast.co.sc) #TRUE
  ext(animal.farm.rast.co.sc) == ext(ground.crop.rast.co.sc) #TRUE
  
 
  # Stack these spatrasters:
  ona.conf.rast.stack <- c(grizzinc.rast.co.sc, bhs.rast.co.sc, biophys.norm.rast.co.sc, dist2pa.rast.co.sc, dist2grizzpop.rast.co.sc, animal.farm.rast.co.sc, ground.crop.rast.co.sc, ccs.means.merge)
  plot(ona.conf.rast.stack) # plot these all to check
  
  # Create P(all conflict) raster with our regression coefficients and rasters:
  
  # Our full model with general conflict offset:
  ona_bear_conf_offset_rast <- -1.2012335 + ccs.means.merge + (0.1940000 * dist2pa.rast.co.sc) + (0.8533170  * grizzinc.rast.co.sc) + (0.4597530 * biophys.norm.rast.co.sc) + ( -0.2396923 * bhs.rast.co.sc) + (-0.4936497 * dist2grizzpop.rast.co.sc) + (-1.1418043 * animal.farm.rast.co.sc) + 
    ( 2.7521941 * ground.crop.rast.co.sc) 
  
  # Convert the Raster to the Probability Scale:
  p_BEAR_conf_ona_rast <- app(ona_bear_conf_offset_rast, fun=plogis)
  
  plot(p_BEAR_conf_ona_rast) # Our p(bear conflict) with offset for general conflict
  

# Save Raster: ------------------------------------------------------------
writeRaster(p_BEAR_conf_ona_rast, "/Users/shannonspragg/Grizz-Connectivity/Data/processed/ona_p_bear_conf.tif", overwrite = TRUE)

