

############################################################
############################################################
### WRAPPER FILE FOR MARINE/TERRESTRIAL SDMS ###
############################################################
############################################################


### META ###
# by HHakkinen
# Start Date: 25/01/2021
# End Date: 08/10/2021
# As part of Institute of Zoology ZSL / University of Cambridge project
# "Terrestrial or marine species distribution model - Why not both? A case study with seabirds"
# 
# This file contains the key parameters and switch to run a marine/terrestrial SDM, it is the primary file of the repo
# it will call all other files via source()
#it:
# 1) LOAD LIBRARIES AND BASE DIRECTORIES (using CheckDependencies.R)
# 2) Ask you to set all the main parameters of the repo.
# 3) Build an index to join marine and terrestrial environmental layers, 
# and then combine them (with CalculateDistShore.R & CombineEnvData.R)
# 4) Run the combined SDM (after checking a few more parameters) with SeabirdSDM.R 

### /META ###




############################################
#### LOAD LIBRARIES AND BASE DIRECTORIES ###
############################################

#clear env
rm(list=ls())

#set current directory. Should be root of the repo folder
setwd("LOCATION_OF_REPO")

#check and load libraries
source("./Code/CheckDependencies.R")

#load functions needed
source("./Code/Dependencies/prepFunctions.R")
source("./Code/Dependencies/SDM_functionsPre.R")



######################################################################
##### SET PARAMETERS ########################
######################################################################

#build an index to help search neighbouring terrestrial and marine areas
#then combine layers into a combined raster stack

###SET VARIABLES

#the following are the key switches for this file

# Add marine data to terrestrial points?
RunAddMarine <- TRUE
# Add terrestrial data to marine points?
RunAddTerr <- TRUE
# Save raster output? (T/F)
output <- TRUE
# save plots of variables? (T/F)
plotVar <- TRUE

# what location would you like to save output (in relation to the working directory)
outpath <- "./IntermediateOutput/CombineEnvData/"


#which species to run? Should match the name of the occurrence input file ("sp_name")
sp_names<-c("Mbassanus","Farctica","Sdougallii")[3]
# Maximum distance to sample offshore marine conditions? (in meters), in same order as sp_names
#for seabirds we set this as mean maximum foraging distance
forDist <- c(200000, 40000, 16600)[3]


#set name and location of marine env layer
MarineEnvName<-"./InputData/EnvData/MarineEnvVariables_5m.tif"
#set names of the layers, to make display easier. This is not mandatory, you can use default names
AllMarLayerNames<-c("SST_WS","mean_sal", "max_chloro", "bathy", 
                 "ID", "distToLand", "direToLand", "nearestLandCell", 
                 "Amarinus")
#if you want to just use a subset of all layers (which I do cos I'm lazy) then set below
#all layers should be environmental layers, get rid of indices etc.
MarLayerNames<-c("SST_WS","mean_sal", "max_chloro", "bathy")



#set name and location of terrestrial env layer
TerrEnvName<-"./InputData/EnvData/TerrEnvVariables_5m.tif"
#set names of the layers, to make display easier. This is not mandatory, you can use default names
AllTerrLayerNames<-c("MeanTemp_WM", "Prec_BreedMonths", "Isol", 
                         "Area", "LandCover", "distToSea", "nearestSeaCell",
                         "NDVI_mean", "NDVI_min")
#if you want to just use a subset of all layers (which I do cos I'm lazy) then set below
#all layers should be environmental layers, get rid of indices etc.
TerrLayerNames<-c("MeanTemp_WM", "Prec_BreedMonths", "Isol", 
                     "Area","NDVI_mean", "NDVI_min")

#set study area
studyOutline<-"./InputData/StudyArea/OSPAR_LandSea.shp"



######################################################################
##### CalculateDistShore.R & CombineEnvData.R ########################
######################################################################

#set up the index for combining layers, will search and find nearest marine/terrestrial neighbors
#note: if you have a large study area this will take a long time to run (for the northern hemisphere it takes my setup 5 hours)
source("./Code/CalculateDistShore.R")

#once the index is built you can combine the layers, which is relatively quick
source("./Code/CombineEnvData.R")


#output for terr with marine points added saved in IntermediateOutput/CombineEnvData named TerrwithMarineVar_5min_"sp name".tif
#output for marine with terr points added saved in IntermediateOutput/CombineEnvData named MarwithTerrVar_5min.tif



###############################################
##### SeabirdSDM.R ########################
###############################################

#this section actually runs the SDM now that the data is set up

### SET PARAMETERS FOR SDM ###

#species to run SDM are assumed to be the same as in sp_names above, but can modify
sp_list<-sp_names
#sp_list<-c("Sdougallii")

#we assume you want to build a model with the same terrestrial and marine variables as above PLUS "distance from shore/land"
#however this can be adjusted manually here. All variables here will be included in the appropriate SDMs
#if you have high covariance then this is the place to remove some variables to reduce covariance
SDMenvMarine<-c(MarLayerNames, "distToLand")
SDMenvLand<-c(TerrLayerNames, "distToSea")

#optionally you can set neater label names for plots and model outputs
#there should be the same number of labels as there are variables in SDMenvMarine and SDMenvLand
terrLabels <- c("Mean Winter Temp", "Logged Breeding Precip", "Isolation (logged km)", 
                "Area", "NDVI (mean)", "NDVI (min)", 
                "Distance to Sea")
marLabels<-c("Sea surface temp (Winter/Spring)", "Mean Salinity", "Max Chlorophyll", 
             "Bathymetry", "Distance to Land")

#as a final note, it's worth checking if you need to log env variables


#select realm type, options are MarOnly, TerrOnly, MarwithTerr, TerrwithMar
realmlist=c("TerrOnly", "MarOnly", "TerrwithMar", "MarwithTerr")

#if the above modeltype is TerrwithMar, set terrmartype to mean or SD
terrmarlist<-c("mean")

#we clip marine and terrestrial values if there are too far from each other
#if manCoastDist is set to NA, SDM will use default foraging values to clip marine and terr layers
#if manCoastDist is set to 0 then it will ignore how far things are from the coast and use everything
#if manCoastDist is set to a positive number then that new threshold will be used
manCoastDist<-NA


#choose number of pseudoabsence draws
pseudodraws<-3
#choose how many evaluation runs (70/30 split), minimum of 2
evalruns<-3
#set number of pseudo-absences
nvalid<-1000

#define model list
#at the moment code will use up to GLM, GAM, ANN, RF, MAXENT
model_list<-c("GLM","GAM","ANN","RF","MAXENT.Phillips")


#run the code!
#this will load the env and occurrence data (which if the previous sections have run)
#check for covariance between env layers, it will prompt to stop and fix if desired
#after loading the data it will then run the main SDM functions
#output will be in 
source("./Code/SeabirdSDM.R")



  


