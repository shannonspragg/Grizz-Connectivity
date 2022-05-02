# Grizz-Connectivity
The connectivity analyses for Ch.2 of Integrating social values for grizzly bears on the biophysical landscape. In this analysis, we produce a series of predictions based on the model results for our first analysis on British Columbia's Southern Interior Ecoprovince. We create a connectivity map to represent the predicted probability of bear conflict across the Okanagan Nation Alliance (ONA) historic territory. This analysis serves to produce a map showcasing the pathways of potential bear movement, and how they may differ when taking into account the way that social and conflict variables influence bears navigating this territory.

This repository includes the scripts that we used to prep our original data, resistance surfaces, predictor rasters, and format omniscape results. The order of scripts is as follows:

############################### Original Data Prep Ch.2.R #######################################
This script is where we download all of our data from googledrive, reproject our data, and join the BC and WA datasets together. The results of this script are a series of shapefiles with filtered and cropped data to be used in our raster manipulation in the next scripts.

############################### Prep Biophys Resistance Rasters.R ###############################
Here, we download the HII and elevation and topographic ruggedness data to compile into our biophysical resistance surface. We also format the grizz increase survey data to produce a biophysical + HII resistance surface and the grizz density data to create our source file. Both of these will be utalized for running in omniscape to produce our biophysical and combined biophysical connectivity maps

############################### Prep ONA Predictor Rasters.R ####################################
In this script, we prepare our predictor rasters for each variable. These will be used in a raster math calculation to produce our probability of predicted conflict raster for the ONA territory.

############################## Prep ONA P(Conflict).R ###########################################
This script is where we bring in our predictor rasters, scale them, multiply them by their corresponding regression coefficients, and add them together using raster math. The result of this is a probability of conflict raster for ONA based on our variables, which will be scaled and turned into a resistance layer.

############################## Probability of Conflict Resistance Surface.R #####################
This is the script where we convert our probability of conflict raster into a resistance surface for our omniscape run to produce our connectivity map based on the predicted probability of bear conflict across the ONA territory.

############################# Format ONA Connectivity Raster Maps.R #############################
In this final script, we bring in our omniscape connectivity outputs and build them into nice maps. The products of this script should include one biophysical connecitivity map, a biophysical + HII connectivity map, and then a probability of bear conflict connectivity map for the ONA territory.


