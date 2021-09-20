

############################################################
############################################################
### SEABIRD SDM PART 1 ###
############################################################
############################################################

### META ###
# by HHakkinen
# Start Date: 25/01/2021
# End Date:
# As part of Institute of Zoology ZSL / University of Cambridge project
# "Evidence-based Climate Change Adaptation Practices to safeguard vulnerable species: supporting conservation practitioners, donors and policy makers"
# 
# The following runs SDM models for 3 seabird species in the OSPAR region
# it runs several algorithms and makes a composite model
# outputs various plots and maps as an output

# runs 3 times, one terrestrial model, one marine model and one combined model

### /META ###




############################################################
### LOAD OCC AND ENV DATA ###
############################################################

#clear env
rm(list=ls())

start.t<-Sys.time()

library(biomod2);
library(raster);
library(RColorBrewer);
library(dismo);
library(rgdal)
library(sdmpredictors)
library(rasterVis)


setwd("C:/Users/Henry/Documents/Research/ZSL/Repos/SeabirdCCVA/IntermediateOutput/SDM")
source("../../code/Dependencies/SDM_functionsPre.R")
source("../../code/Dependencies/logSettings.R")

#load OSPAR map
ospar<-shapefile("C:/Users/Henry/Documents/Research/ZSL/ProcessedData/OSPAR_region/ospar_LandSea.shp")
os.ext<-extent(ospar)

#load simplified map for plotting purporse
landsh<-shapefile("C:/Users/Henry/Documents/Research/ZSL/ProcessedData/EEAcoastlines/SimpleLandOutline_wgs84.shp")

#set colour palette
myPal <- (RColorBrewer::brewer.pal('YlGn', n=9))
myTheme <- rasterTheme(region = myPal)

###SET PARAMETERS###

#choose species, options are Mbassanus, Farctica, Sdougallii
sp_list<-c("Sdougallii", "Mbassanus", "Farctica")
#sp_list<-c("Sdougallii")
#select realm type, options are MarOnly, TerrOnly, MarwithTerr, TerrwithMar
realmlist=c("TerrOnly", "MarOnly", "TerrwithMar", "MarwithTerr")
#realmlist<-c("MarwithTerr")
#if the above modeltype is TerrwithMar, set terrmartype to mean or SD
terrmarlist<-c("mean")


#we clip marine and terrestrial values if there are too far from each other
#if manCoastDist is set to NA, SDM will use default foraging values to clip marine and terr layers
#if manCoastDist is set to 0 then it will ignore how far things are from the coast and use everything
#if manCoastDist is set to a positive number then that new threshold will be used
manCoastDist<-NA


#choose number of pseudoabsence draws, minimum of 2
pseudodraws<-0
#choose how many evaluation runs (70/30 split), minimum of 2
evalruns<-5
#set number of pseudo-absences
nvalid<-10000

#define model list
#at the moment code will use up to GLM, GAM, ANN, RF, MAXENT
model_list<-c("GLM","GAM","ANN","RF","MAXENT.Phillips")
#model_list<-c("GLM","GAM","MAXENT.Phillips")





#set sdm parameter to supress warnings
options(sdmpredictors_datadir="C:/Users/Henry/Documents/Research/ZSL/Repos/SeabirdCCVA/IntermediateOutput/SDM")
i<-1; j<-1; k<-1

############################################################
### RUN MODELS ###
############################################################

start.now<-Sys.time()

for (i in 1:length(sp_list)){
  sp_name<-sp_list[i]

  
  for(j in 1:length(realmlist)){
    modeltype<-realmlist[j]

    #check for manual override to coastal clip, otherwise use default value
    if (!(is.na(manCoastDist))){
      if(manCoastDist<=0){coastClip<-NA}
      if(manCoastDist>0){coastClip<-manCoastDist}
    }else{
      #use defaults
      #set the coastal crop point, how far from the sea or land do we want calculations to include
      if (modeltype %in% c("TerrOnly", "TerrwithMar")){
        #if inland we accept up to the foraging distance
        if(sp_name == "Mbassanus"){coastClip <- 20000}
        if(sp_name == "Farctica"){coastClip <- 20000}
        if(sp_name == "Sdougallii"){coastClip <- 20000}
      }
      if (modeltype %in% c("MarOnly", "MarwithTerr")){
        #if at sea we accept up to the foraging distance
        if(sp_name == "Mbassanus"){coastClip <- 200000*1.2}
        if(sp_name == "Farctica"){coastClip <- 40000*1.2}
        if(sp_name == "Sdougallii"){coastClip <- 16600*1.2}
      }
    }
    

    for(k in 1:length(terrmarlist)){
      
      terrmartype<-terrmarlist[k]
      
      start.t<-Sys.time()
      

      #logManual(paste("Starting:", sp_name))
      #logManual(paste("Starting:", modeltype))
      print(sp_name)
      print(modeltype)

      #load environmental data
      #will return warning if there is too much covariation (>0.7 in a pearson correlation)
      envt.st<-readENVdata(sp_name, modeltype, coastClip)
      #logManual("Env read!")
      
      #can check covariance if you want
      #env_correlations <- pearson_correlation_matrix(envt.st)
      #p2 <- plot_correlation(env_correlations, prettynames)
      #p2
      #env_correlations
      
      #load occurrence data
      #note that this sets a variable nvalid which is used later
      points<-readOCCdata(sp_name, modeltype, envt.st, coastClip)
      #logManual("Occ read!")
      summary(points)
      
      #check how many points we have
      pcheck<-as.data.frame(extract(envt.st,points[,2:3]))
      pcheck$x<-points$X
      pcheck$y<-points$Y
      pcheck2<-pcheck[complete.cases(pcheck),]
      #plot(pcheck2$x, pcheck2$y,col="green",pch=3,cex=1.2)
      
      #low-res map for plotting purposes
      #newmap <- getMap(resolution = "low")  # different resolutions available
      #plot(newmap,add=T)
      
      npoints<-nrow(pcheck2)
      #print(npoints)
      
      
      
      #if nvalid is not set number of pseudoabsences set to number of valid points
      if(is.na(nvalid)){
        nps<-npoints
      }else{
        nps<-nvalid
      }
      
      #logManual(paste("Coast clip:",coastClip))
      #logManual(paste("pseudo-absenses:",nps))
      
      #run the actual model
      source("../../code/SDM_functions.R")
      end.t<-Sys.time()
      
      print(end.t-start.t)
      
    
      #logManual("SDM done!")
      #logBreak()

    }
    
  }
  
  
  
}

end.now<-Sys.time()
end.now-start.now


#at the very least it will run a model and give a biomod about

#what other sections would you like to get

getSensitivity<-TRUE
getImportance<-TRUE
getResponse<-TRUE
getPearson<-TRUE
getIndv<-TRUE
getEnsem<-TRUE






