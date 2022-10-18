# Grizz-Connectivity
The connectivity analysis scripts for the connectivity modeling of grizzly bear conflict. In this analysis, we produce a series of predictions based on the model results for our first analysis on British Columbia's Southern Interior Ecoprovince. We create a connectivity map to represent the predicted probability of bear conflict across the Okanagan Nation Alliance (ONA) historic Territory. This analysis serves to produce a map showcasing the pathways of potential bear movement, and how they may differ when taking into account the way that social and conflict variables influence bears navigating this territory.

This repository includes the scripts that we used to prep our original data, resistance surfaces, predictor rasters, and format omniscape results. The order of scripts (in the analysis code scripts folder) is as follows:

############################### 1. Download data.R #######################################
This script is where we download all of our data from googledrive.

############################### 2. Distance to Metro.R ###############################
Here, we create the distance to metropolitan areas variable for the ONA Territory extent.

############################### 3. Distance to PAs.R ####################################
In this script, we prepare our distance to protected areas variable for the ONA territory extent.

############################### 4. Agriculture Data Prep.R ###########################################
This script is where we calculate the density of livestock and row crop farms per ccs region for the ONA territory extent.

############################### 5. Human Density.R #####################
In this script, we prepare our human population density variable for the ONA territory extent.

############################### 6. Prob General Conflict.R #############################
In this  script, we produce and scale the general conflict predictor rasters for the ONA territory extent.

############################### 7. Distance to Grizz Pops.R #############################
Here, we produce the distance to extant grizzly bear population raster for the ONA territory extent.

############################### 8. Grizz Increase.R #############################
In this  script, we bring in the survey data responses for support for grizzly bear increase for the ONA territory extent.

############################### 9. Bear Habitat Suitability.R #############################
Here, we bring in the bear habitat suitability for BC and extrapolate habitat suitability for the US portion of the ONA territory extent.

############################### 10. Bear Conflict.R #############################
In this  script, we produce and scale the bear conflict predictor rasters for the ONA territory extent.

############################### 11. Build Resistance Surfaces.R #############################
Here, we construct the biophysical and probability of conflict resistance surfaces for the ONA territory extent.

############################### bioSocONA.ini #############################
This is an example of the .ini file input that we used to run the biophysical + probability of conflict model in omniscape





