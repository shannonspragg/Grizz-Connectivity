# Format ONA Connectivity Raster Maps: ------------------------------------
  ## Here, we will be bringing in our circuitscape outputs for the three resistance layers that we ran analyses on.
  ## We will be cropping these down to the ONA extent, masking them to just the ONA territory outline, and
  ## making nice maps with them.


# Load Packages: ----------------------------------------------------------
library(sf)
library(tidyverse)
library(dplyr)
library(raster)
library(terra)
library(dismo)
library(stars)
library(measurements)


# Bring in Data: ----------------------------------------------------------




