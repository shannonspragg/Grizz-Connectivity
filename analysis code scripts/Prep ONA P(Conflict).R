# Produce Probability of Bear Conflict ONA Raster: ------------------------




################################# Create P(Conflict) Raster for ONA: 

# Scale our Predictor Rasters: --------------------------------------------
We can do this Step by Step:
  
  # Distance to PA:
  d2pa.mean.co <- mean(warp.df$dst__PA)
  d2pa.sub.mean.co <- dist2pa.rast - d2pa.mean.co
  d2pa.sd.co <- sd(warp.df$dst__PA)
  dist2pa.rast.co.sc <- d2pa.sub.mean.co / ( 2 * d2pa.sd.co)
  
  # Distance to Grizzly Pops:
  d2grizz.mean.co <- mean(warp.df$dst__GP)
  d2grizz.sub.mean.co <- dist2grizzpop.rast - d2grizz.mean.co
  d2grizz.sd.co <- sd(warp.df$dst__GP)
  dist2grizzpop.rast.co.sc <- d2grizz.sub.mean.co / ( 2 * d2grizz.sd.co)
  
  # Animal Farm Density:
  animal.farm.mean.co <- mean(warp.df$Anml_Fr)
  anim.f.sub.mean.co <- animal.farming.rast - animal.farm.mean.co
  anim.f.sd.co <- sd(warp.df$Anml_Fr)
  animal.farm.rast.co.sc <- anim.f.sub.mean.co / ( 2 * anim.f.sd.co)
  
  # Ground Crop Density:
  ground.crop.mean.co <- mean(warp.df$Grnd_Cr)
  ground.c.sub.mean.co <- ground.crop.rast - ground.crop.mean.co
  ground.c.sd.co <- sd(warp.df$Grnd_Cr)
  ground.crop.rast.co.sc <- ground.c.sub.mean.co / ( 2 * ground.c.sd.co)
  
  # Grizz Increase:
  grizzinc.mean.co <- mean(warp.df$GrizzInc)
  grizz.sub.mean.co <- grizzinc.rast - grizzinc.mean.co
  grizzinc.sd.co <- sd(warp.df$GrizzInc)
  grizzinc.rast.co.sc <- grizz.sub.mean.co / ( 2 * grizzinc.sd.co)
  
  # Biophys:
  biophys.mean.co <- mean(warp.df$Biophys)
  bio.sub.mean.co <- biophys.rast - biophys.mean.co
  biophys.sd.co <- sd(warp.df$Biophys)
  biophys.rast.co.sc <- bio.sub.mean.co / ( 2 * biophys.sd.co)
  
  # BHS:
  bhs.mean.co <- mean(warp.df$BHS)
  bhs.sub.mean.co <- bhs.rast - bhs.mean.co
  bhs.sd.co <- sd(warp.df$BHS)
  bhs.rast.co.sc <- bhs.sub.mean.co / ( 2 * bhs.sd.co)
  
  # Produce our P(Bear Conflict) Raster: ------------------------------------
  
  # Make sure extents match:
  ext(grizzinc.rast.co.sc) == ext(bhs.rast.co.sc) # TRUE
  ext(biophys.rast.co.sc) == ext(animal.farm.rast.co.sc) #TRUE
  ext(animal.farm.rast.co.sc) == ext(ground.crop.rast.co.sc) #TRUE
  
  # View our Full Model Coefficients:
  summary(post.co.offset)
  fixef(post.co.offset)
  
  # Stack these spatrasters:
  bear.conf.rast.stack <- c(grizzinc.rast.co.sc, bhs.rast.co.sc, biophys.rast.co.sc, dist2pa.rast.co.sc, dist2grizzpop.rast.co.sc, animal.farm.rast.co.sc, ground.crop.rast.co.sc, ccs.varint.means.rast.co)
  plot(bear.conf.rast.stack) # plot these all to check
  
  # Create P(all conflict) raster with our regression coefficients and rasters:
  
  # Our full model with general conflict offset:
  bear_conf_offset_rast <- -1.2012335 + ccs.varint.means.rast.co + (0.1940000 * dist2pa.rast.co.sc) + (0.8533170  * grizzinc.rast.co.sc) + (0.4597530 * biophys.rast.co.sc) + ( -0.2396923 * bhs.rast.co.sc) + (-0.4936497 * dist2grizzpop.rast.co.sc) + (-1.1418043 * animal.farm.rast.co.sc) + 
    ( 2.7521941 * ground.crop.rast.co.sc) 
  
  # Convert the Raster to the Probability Scale:
  p_BEAR_conf_offset_rast <- app(bear_conf_offset_rast, fun=plogis)
  
  plot(p_BEAR_conf_offset_rast) # Our p(bear conflict) with offset for general conflict
  
  
