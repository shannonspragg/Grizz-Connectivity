#Prepare Human Density Raster
library(terra)

hu.dens <- rast("Data/original/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec.tif")
dist2metkm <- rast("Data/processed/dist2met_km_SOI.tif")

hu.dens.crp.rep <- project(hu.dens, dist2metkm)

writeRaster(hu.dens.crp.rep, "Data/processed/hudens_ONA.tif")
