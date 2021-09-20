
############################################################
############################################################
### PREPARING ENVIRONMENTAL VARIABLES FOR SEABIRD SDMS ###
############################################################
############################################################

### META ###
# by HHakkinen
# Start Date: 25/01/2021
# End Date:
# As part of Institute of Zoology ZSL / University of Cambridge project
# "Evidence-based Climate Change Adaptation Practices to safeguard vulnerable species: supporting conservation practitioners, donors and policy makers"
# 
# The following takes the raw environmental data from RawData (see DataSources.txt for more detail)
# and prepares them to be used in an SDM model
# Currently the data is at 5min resolution, but there are some notes on how to prepare 30s data

#The code should run as is, and the following variables need to be set
#   RunTerr  --  if set to TRUE, then section 1 will run and compile all terrestrial variables
#   RunMar   --  if set to TRUE, then section 2 will run and compile all marine variables
#   RunCombine - if set to TRUE, section 3 will run and sample from the marine stack and build  "nearest sea conditions" stack to add to terrestrial stack
#               NOTE: this will fail if the first two variables are not set to T
#   forDist  --  If RunCombine is set to T, then relevant foraging distance for the species of interest needs to be set (in meters)
#   output   --  If set to F all code will run but will not save output rasters
#            --  if set to T code will run and save output rasters for the above 3 stages
#   outpath  --  if output is set to T, then all output will save to outpath (preferably a folder)
#            --  if left blank will save to main working directory
#   plotVar  --  if set to TRUE, output plots will be made for all variables and saved to outpath/VarPlots

#Section 1 prepares the terrestrial environmental variables
#   1) crops the data to our OSPAR area of interest
#   2) processes them if necessary (add NA, rescale etc)
#   3) stacks them into a single aligned stack

#Section 2 does the same as Section 1 but for marine variables

#Section 3 combines the terrestrial and marines variables into a single stack
#   For each terrestrial gridcell, R will sample from all marine gridcells within forDist and calculate the
#   mean and SD of conditions at sea.

### /META ###



############################################################
### LOAD LIBRARIES AND SET VARIABLES ###
############################################################

#clear env
rm(list=ls())

#load libraries
library(sdmpredictors)
library(zoon)
library(rgdal)
library(rworldmap)
library(raster)
library(ggplot2)

#install (if necessary) and load OBIS package
#library(devtools)
#install_github("iobis/robis")
library(robis)

#set directory to directory where RawData is stored
setwd("C:/Users/Henry/Documents/Research/ZSL/RawData/")


#data_dir<-"C:/Users/Henry/Documents/Research/ZSL/RawData/sdmpredictors/"
#set file.path() to stop having to load every time
#options(sdmpredictors_datadir=data_dir)



### SET VARIABLES

# Compile terrestrial variables? (T/F)
RunTerr <- TRUE
# Compile marine variables? (T/F)
RunMar <- FALSE

# Add marine data to terrestrial points?
RunAddMarine <- TRUE
# Add terrestrial data to marine points?
RunAddTerr <- TRUE

#which species to run?
sp_names<-c("Mbassanus","Farctica","Sdougallii")[3]
# Maximum distance to sample offshore marine conditions? (in meters), in same order as sp_names
forDist <- c(200000, 40000, 16600)[3]

# Save raster output? (T/F)
output <- TRUE
# set location to save output (in relation to the working directory)
outpath <- "C:/Users/Henry/Documents/Research/ZSL/Repos/SeabirdCCVA/IntermediateOutput/PrepEnvData"
# save plots of variables? (T/F)
plotVar <- FALSE






#low-res map for plotting purposes
newmap <- getMap(resolution = "low")  # different resolutions available


#A useful summary of all variables available from BIO-ORACLE and MARSPEC
#Handled through sdmpredictors
# Inspect the available datasets and layers
datasets <- list_datasets(terrestrial = FALSE, marine = TRUE)
#View(datasets)
layers <- list_layers(datasets)
View(layers)
#write.csv(layers,"TerrLayerNames.csv")

terr_datasets <- list_datasets(terrestrial = T, marine = F)
#View(terr_datasets)
terr_layers <- list_layers(terr_datasets)
View(terr_layers)

#write.csv(layers,"MarineLayerNames.csv")



###############################################################
### SECTION 1: TERRESTRIAL VARIABLES ###
###############################################################

if (RunTerr){
  print("starting terrestrial variables")
  
  
  ###
  ### mean temperature of the warmest month 
  ###
  
  
  ### 30 SECOND (not currently used) ###
  #download monthly avg temp data
  #30sec northern hemisphere only
  #P_url<- "https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_30s_tavg.zip"
  #download.file(P_url, destfile=paste0(data_dir,"wc2.1_30s_tavg.zip"))
  #unzip("wc2.1_30s_tavg.zip")
  
  #setwd("C:/Users/Henry/Documents/Research/ZSL/RawData/WorldClim/wc2.1_30s_tavg")
  #file_list<-list.files(pattern="*.tif")
  
  #SST_raw<-stack(file_list)
  #north_hem<-extent(-180,180,0,90)
  #worldmax_raw<-crop(SST_raw, north_hem)
  
  
  ### 5 MINUTE ###
  savewd<-getwd()
  #these comments lines have been kept as a record of the download link, however only needs running once
  #P_url<- "https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_5m_tavg.zip"
  #download.file(P_url, destfile=paste0(data_dir,"wc2.1_5m_tavg.zip"))
  #unzip("wc2.1_5m_tavg.zip")
  
  setwd("./WorldClim/wc2.1_5m_tavg")
  file_list<-list.files(pattern="*.tif")
  worldmax_raw<-stack(file_list)
  
  #replace NA with -999 cos I hate warning messages
  r <- calc(worldmax_raw, function(x) { ifelse(is.na(x), -999, x) }) 
  
  #calculate the maximum for each raster cells, hence return mean temp of the warmest month
  MeanTemp_W<-calc(r, fun = max, na.rm = T)
  MeanTemp_W <- calc(MeanTemp_W, function(x) { ifelse(x==-999, NA, x) }) 
  
  #write raw data so we don't have to download it again
  if (output){
    writeRaster(MeanTemp_W, paste0(outpath,"/WorldClim_warmestMonthAvgTemp_5min.tif"), overwrite = TRUE)
  }
  print("DONE: mean temperature of the warmest month")
  
  #reset wd
  setwd(savewd)
  
  ###
  
  
  
  ###
  ###rainfall during the breeding season (March to August inclusive)
  ###

  ### 30 SECOND (not currently used) ###
  #setwd("C:/Users/Henry/Documents/Research/ZSL/RawData/WorldClim/")
  #P_url<- "https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_30s_prec.zip"
  #download.file(P_url, destfile=paste0(data_dir,"wc2.1_30s_prec.zip"))
  #unzip("wc2.1_30s_prec.zip")
  
  
  ### 5 MINUTE ###
  savewd<-getwd()
  P_url<- "https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_5m_prec.zip"
  #these commented lines have been kept as a record of the download link, however only needs running once
  #download.file(P_url, destfile=paste0(data_dir,"wc2.1_5m_prec.zip"))
  #unzip("wc2.1_5m_prec.zip")
  
  setwd("./WorldClim/wc2.1_5m_prec")
  #limit the months we take
  file_list<-list.files(pattern="(3|4|5|6|7|8).tif")
  prec_raw<-stack(file_list)
  
  Prec_BreedMonths<-calc(prec_raw, fun = sum, na.rm = T)
  
  if (output){
    writeRaster(Prec_BreedMonths, paste0(outpath,"/WorldClim_SummedPrecBreedMonths_5min.tif.tif"), overwrite = TRUE)
  }
  print("DONE: rainfall during the breeding season")
  
  setwd(savewd)
  ###
  
  
  
  
  ###
  ### Isolation (distance to larger island) and area
  ###
  
  
  ### Due to its complexity this is calculated in a CalculateIsolation.R, we just read in the result and attach it
  
  savewd<-getwd()
  setwd("../ProcessedData/EEAcoastlines")
  
  iso<-raster("IsolationRaster.tif")
  iarea<-raster("ShapeArea.tif")
  
  setwd(savewd)
  
  ###
  
  
  
  ###
  ### Distance to sea
  ###
  
  #set dir
  savewd<-getwd()
  setwd("../ProcessedData/CalculateDistShore")
  #This is calculated in CalculateDistShore.R, just load the output
  dland<-stack("NearestSea_Final.tif")
  
  dland<-subset(dland, c(3,5))
  names(dland)<-c("DistToSea", "nearestSeaCell")

  #library(rasterVis)
  #levelplot(dland$DistToSea/1000, margin=FALSE, at=seq(0, maxValue(dland$DistToSea)/1000, length=100),
            #colorkey=list(height=0.6), main='Distance to coast')
  
  
  setwd(savewd)
  ###
  
  
  ###
  ### Coastal type
  ###
  
  #previously prepared in process_coastalLandtype
  landcover<-brick("../ProcessedData/Copernicus_LandCover/Copernicus_LCraster_cleaned.tif")[[2]]
  names(landcover)<-c("Code")

  ###
  
  
  
  
  ###
  ### NDVI (mean and min)
  ###
  
  savewd<-getwd()
  setwd("../ProcessedData/MODIS_NDVI")
  
  NDVI_mean<-raster("NDVI_5minMEAN.tif")
  NDVI_min<-raster("NDVI_5minMIN.tif")
  
  setwd(savewd)
  ###
  
  
  
  
  ###
  ### Stack all the terr layers together
  ###
  
  #MeanTemp_W
  #Prec_BreedMonths
  #iso
  #iarea
  #dland
  #coastal type
  #NDVI_mean / NDVI_min
  
  #crop down to size
  ne.atlantic.ext <- extent(-100, 55, 30, 90)
  ospar<-shapefile("C:/Users/Henry/Documents/Research/ZSL/ProcessedData/OSPAR_region/ospar_LandSea.shp")
  os.ext<-extent(ospar)
  
  MT_crop <-crop(MeanTemp_W, os.ext)
  Prec_crop <-crop(Prec_BreedMonths, os.ext)
  iso_crop <-crop(iso, os.ext)
  area_crop <-crop(iarea, os.ext)
  dland_crop <-crop(dland, os.ext)
  lc_crop <-crop(landcover, os.ext)
  ndvimean_crop <-crop(NDVI_mean, os.ext)
  ndvimin_crop <-crop(NDVI_min, os.ext)
  
  #extend others so they have equal areas of blank space
  
  iso_crop<-extend(iso_crop, extent(MT_crop))
  area_crop<-extend(area_crop, extent(MT_crop))
  dland_crop<-extend(dland_crop, extent(MT_crop))
  lc_crop2<-extend(lc_crop, extent(MT_crop))
  ndvimean_crop2 <-extend(ndvimean_crop, extent(MT_crop))
  ndvimin_crop <-extend(ndvimin_crop, extent(MT_crop))
  
  TerrPre_crop<-stack(MT_crop, Prec_crop, iso_crop, 
                      area_crop, lc_crop, dland_crop, 
                      ndvimean_crop, ndvimin_crop)
  names(TerrPre_crop)<-c("MeanTemp_WM", "Prec_BreedMonths", "Isol", 
                         "Area", "LandCover", "distToSea", "nearestSeaCell",
                         "NDVI_mean", "NDVI_min")
  
  

  
  if (output){
    #write the raster to use later
    writeRaster(TerrPre_crop, paste0(outpath,"/TerrEnvVariables_5m.tif"), overwrite=T)
  }
  print("terrestrial variables done!")
}


###################################################
### MARINE PREDICTORS ###
##################################################


if (RunMar){
  print("processing marine variables")
  
  ###
  ### winter/spring (December to May inclusive) sea surface temperature adjacent to each focal gridsquare 
  ###
  
  # uses MARSPEC 5m data
  savewd<-getwd()
  setwd("./MARSPEC/MARSPEC_5m/Monthly_Variables_5m/Temperatures")
  
  SST_12 <- raster("sst12_5m/w001001.adf")
  SST_1 <- raster("sst01_5m/w001001.adf")
  SST_2 <- raster("sst02_5m/w001001.adf")
  SST_3 <- raster("sst03_5m/w001001.adf")
  SST_4 <- raster("sst04_5m/w001001.adf")
  SST_5 <- raster("sst05_5m/w001001.adf")
  
  SST_RAW <- stack(SST_12,SST_1,SST_2,SST_3,SST_4,SST_5)
  names(SST_RAW)<-c("meanSST_Dec","meanSST_Jan","meanSST_Feb","meanSST_Mar","meanSST_Apr","meanSST_May")
  
  #correct so it is on the actual scale
  SST_RAW<-SST_RAW/100
  SST_WS<-calc(SST_RAW, fun = mean, na.rm = T)
  
  setwd(savewd)
  
  ###
  #mean salinity at mean depth
  ###
  savewd<-getwd()
  setwd("./sdmpredictors/")
  mean_sal<-load_layers("BO2_salinitymean_bdmean")
  setwd(savewd)
  
  #surface chlorophyll concentration 
  #Chlorophyll concentration (maximum at min depth)
  savewd<-getwd()
  setwd("./sdmpredictors/")
  max_chloro<-load_layers("BO2_chlomax_bdmin")
  setwd(savewd)
  
  #standard deviation of SST as a proxy for oceanic fronts 
  
  ###
  #bathymetry
  ###
  savewd<-getwd()
  setwd("./sdmpredictors/")
  bathy<-load_layers("BO_bathymean")
  bathy[bathy$BO_bathymean>=0]<-NA
  setwd(savewd)
  
  ###
  #Distance to shore
  ###
  #setwd("./sdmpredictors/")
  #distShore<-load_layers("MS_biogeo05_dist_shore_5m")

  
  #replace with a new metric that we calculated
  nearLand<-stack("../ProcessedData/CalculateDistShore/NearestLand_Final.tif")[[2:5]]
  names(nearLand)<-c("ID", "distToLand", "direToLand", "nearestLandCell")
  nearLand
  plot(nearLand)
  
  
  ###
  #crop and stack
  ###
  
  #crop out antarctica so all extents match
  ne.atlantic.ext <- extent(-100, 55, 30, 90)
  ospar<-shapefile("C:/Users/Henry/Documents/Research/ZSL/ProcessedData/OSPAR_region/ospar_LandSea.shp")
  os.ext<-extent(ospar)
  
  
  SST_WS <- crop(SST_WS, os.ext) 
  mean_sal <- crop(mean_sal, os.ext) 
  max_chloro <- crop(max_chloro, os.ext) 
  bathy <- crop(bathy, os.ext) 
  distShore <- crop(nearLand, os.ext) 
  
  
  MarinePredictors <- stack(SST_WS, mean_sal, max_chloro, bathy, distShore)
  names(MarinePredictors)<-c("SST_WS","mean_sal", "max_chloro", "bathy", 
                             "ID", "distToLand", "direToLand", "nearestLandCell")
  
  
  
  
  ###############################
  ### DEPRECATED: PREY RANGES ###
  ##############################
  
  ###OPTION 1: GET OCCURRENCE DATA FROM OBIS (or GBIF or whatever)
  
  #Sand eel range
  #manual for ROBIS: https://obis.org/manual/accessr/
  #Amar <- occurrence("Ammodytes marinus")
  #View(Amar)
  
  #Amar$year <- as.numeric(format(as.Date(Amar$eventDate), "%Y"))
  #ggplot() +
  #geom_histogram(data = Amar, aes(x = year, fill = country), binwidth = 5)
  
  #map_leaflet(Amar)
  
  
  
  ###OPTION 2: GET estimated range from AquaMaps
  
  aqoccs<-read.csv("./AquaMaps/Ammodytes_marinus/Occ.csv")
  aqest<-read.csv("./AquaMaps/Ammodytes_marinus/EstimatedOcc.csv")
  
  #some plots to explore all the options
  # world_map <- map_data("world")
  # eur_map<-world_map[which(world_map$lat>40&world_map$lat<80 &
  #                            world_map$long>-50&world_map$long<60),]
  # 
  # # Ammodytes marinus OBIS occurrence map
  # ggplot(data=eur_map, aes(x = long, y = lat, group = group)) +
  #   geom_polygon(fill="lightgray", colour = "white")+
  #   geom_point(data=aqoccs, aes(Center.Long, Center.Lat), 
  #              inherit.aes = FALSE, size = 0.9)
  # 
  # 
  # # Ammodytes marinus aquamaps estimated map
  # #aqest$Overall.Probability[which(aqest$Overall.Probability < 0.8)]<-0
  # ggplot(data=eur_map, aes(x = long, y = lat, group = group)) +
  #   geom_polygon(fill="lightgray", colour = "white")+
  #   geom_point(data=aqest, aes(Center.Long, Center.Lat, color = Overall.Probability), 
  #              inherit.aes = FALSE, size = 0.9)+
  #   scale_color_gradient(low = "lightyellow", high = "red")
  
  
  #now map this to a raster
  aqest_cl<-aqest[,c(4,3,6)]
  aq_r<-rasterFromXYZ(aqest_cl)
  
  #disaggregate to same resolution
  aq_r2<-disaggregate(aq_r,fact=6)
  
  #extend to same extent
  aq_r<-extend(aq_r2, extent(MarinePredictors))
  aq_r<-crop(aq_r, extent(MarinePredictors))
  
  #add to raster stack
  MarinePredictors$Amarinus<-aq_r$Overall.Probability
  
  
  ###############################
  ### CROP AND SAVE ###
  ##############################
  #crop to N Atlantic to save space
  ne.atlantic.ext <- extent(-100, 55, 30, 90)
  ospar<-shapefile("C:/Users/Henry/Documents/Research/ZSL/ProcessedData/OSPAR_region/ospar_LandSea.shp")
  os.ext<-extent(ospar)
  
  MarinePredictors<-crop(MarinePredictors,os.ext)
  
  if (output){
    writeRaster(MarinePredictors, paste0(outpath,"/MarineEnvVariables_5m.tif"), overwrite=T)
  }

  print("marine variables done!")
}



#############################################################
### JOIN MARINE AND TERRESTRIAL LAYERS ###
#############################################################

if (RunAddMarine | RunAddTerr){
  #will only work if previous 2 sections have been run at some point to create output rasters

  ### LOAD FILES ###
  savewd<-getwd()
  setwd(outpath)
  
  #load prep functions
  source("../../Code/prepFunctions.R")
  
  #if we've already run the download/collation process then start here and load rasters
  MarinePredictors<-stack("MarineEnvVariables_5m.tif")
  names(MarinePredictors)<-c("SST_WS","mean_sal", "max_chloro", "bathy", 
                             "ID", "distToLand", "direToLand", "nearestLandCell", 
                             "Amarinus")
  TerrPredictors<-stack("TerrEnvVariables_5m.tif")
  names(TerrPredictors)<-c("MeanTemp_WM", "Prec_BreedMonths", "Isol", 
                           "Area", "LandCover", "distToSea", "nearestSeaCell",
                           "NDVI_mean", "NDVI_min")
  
  
  ospar<-shapefile("C:/Users/Henry/Documents/Research/ZSL/ProcessedData/OSPAR_region/ospar_LandSea.shp")
  
  
  ### CREATE INDEX ###
  #because I messed up and used a different extent to calculate distshore I need to re-work out the index
  #should probably clean this up at a later date
  MarineInd <- stack("C:/Users/Henry/Documents/Research/ZSL/RawData/MARSPEC/MARSPEC_5m/Monthly_Variables_5m/Temperatures/sst12_5m/w001001.adf")
  names(MarineInd)<-"SST_WS"
  # Crop raster to fit the North Atlantic
  ne.atlantic.ext <- extent(-100, 55, 30, 90)
  MarineInd1<-crop(MarineInd,ne.atlantic.ext)
  MarineInd1$Index<-1:length(MarineInd1)
  #now crop to the current extent but keep index
  MarineInd2<-crop(MarineInd1,ospar)
  #we now have two indexes, one for cells in ne.atlantic and one for OSPAR extent. Can now translate between them
  
  

  #for each terr cell we want to know what the surrounding marine conditions are like.
  #feed in the raster layer and the maximum distance we want to consider (which for now is mean foraging distance)
  TerrCrop<-crop(TerrPredictors, ospar)
  MarCrop<-crop(MarinePredictors, ospar)
  
  #add original index to allow conversion
  TerrCrop$origIndex<-MarineInd2$Index
  MarCrop$origIndex<-MarineInd2$Index

}

if(RunAddMarine){
  
  ###ASSIGN MARINE VALUES TO TERR POINTS ###
  
  #save memory, remove things not needed
  #rm(TerrPredictors, MarinePredictors, ospar)

  #load terrestrial points, find a unique list of points nearest the sea
  AllPoints<-as.data.frame(TerrCrop[[7]], na.rm=T)
  markeys<-unique(AllPoints$nearestSeaCell)

  #because we have cropped the map we remove keys we no longer need
  #convert from old index to new index
  indlevel<-(MarCrop$origIndex %in% markeys)
  indlevel$origIndex<-MarCrop$origIndex
  coastPoints<-as.data.frame(indlevel, na.rm=T)
  
  #coastInd is the index for the current area that we can use to work out where the nearest sea is
  coastInd<-which(coastPoints$layer==1)
  #can be used as a translation between the old and new index
  coastPoints<-coastPoints[which(coastPoints$layer==1),]
  coastPoints$newInd<-coastInd


  #coastInd represents the cell number in MarCrop that corresponds to the nearest cell number


  #the values in this field are the nearest marine cells to all points on land.
  #therefore these are the points we will take to work out the surrounding marine conditions
  #tpoint<-xyFromCell(TerrCrop, as.numeric(row.names(AllPoints)))
  tpoint<-xyFromCell(TerrCrop, coastInd)
  print(nrow(tpoint))
  
  #tpoint is now all coastal points we need to work out the foraging distance around
  plot(tpoint[,1], tpoint[,2], pch=".")
  
  #have to split this up because the computer freaks out if I don't
  startvals<-seq(1,nrow(tpoint), by=10000)
  endvals<-c(seq(10000,nrow(tpoint), by=10000),nrow(tpoint))

  
  #loop species and subsection, note this takes a long time! several hours
  for (i in (1:length(sp_names))){
    
    
    start.t<-Sys.time()
    
    sp_now<-sp_names[i]
    for_now<-forDist[i]
    
    
    for (q in 1:length(startvals)){
      print(q)
      cellmin=startvals[q]
      cellmax=endvals[q]
      
      #custom function to work through lat/lon points, buffer and calculate mean and SD marine conditions around it (dist set by for_now)
      medres<-getMarineVals(tpoint, for_now, cellmin=cellmin, cellmax=cellmax)
      #save intermediate values
      write.csv(medres[[1]], paste0("MarineLookup/MarineNearMean_",sp_now,"_",q,".csv"))
      write.csv(medres[[2]], paste0("MarineLookup/MarineNearSD_",sp_now,"_",q,".csv"))
      removeTmpFiles(h=0.5)
    }
    
    end.t<-Sys.time()
    print(paste(sp_now, ": "))
    print(end.t - start.t)
  }
  

  
  setwd(paste0(outpath,"./MarineLookup"))
  #after that's finished, stack up 
  for (i in (1:length(sp_names))){
    sp_now<-sp_names[i]
    print(sp_now)
    
    TerrCropTmp<-TerrCrop
    
    #read and stack intermediate values

    marineMeanVals<-read.csv(paste0("MarineNearMean_",sp_now,"_1.csv"))
    marineSDVals<-read.csv(paste0("MarineNearSD_",sp_now,"_1.csv"))
    
    for (q in 2:length(startvals)){
      marinemSub<-read.csv(paste0("MarineNearMean_",sp_now,"_",q,".csv"))
      marineMeanVals<-rbind(marineMeanVals,marinemSub)
      
      marinesdSub<-read.csv(paste0("MarineNearSD_",sp_now,"_",q,".csv"))
      marineSDVals<-rbind(marineSDVals,marinesdSub)
      
    }
  
    names(marineMeanVals)<-c("X","cell","MEAN_SST_WS","MEAN_mean_sal","MEAN_max_chloro","MEAN_bathy","MEAN_distShore","MEAN_Amarinus" , "origIndex")
    names(marineSDVals)<-c("X","cell","SD_SST_WS","SD_mean_sal","SD_max_chloro","SD_bathy","SD_distShore","SD_Amarinus", "origIndex"  )
    
    marineMeanVals<-subset(marineMeanVals,select=-c(X,cell, origIndex))
    marineSDVals<-subset(marineSDVals,select=-c(X,cell, origIndex))
    
    marineMeanVals$marIndex<-coastInd
    marineSDVals$marIndex<-coastInd
    
    
    #readd the original index so we can do lookups
    marineMeanVals$origInd<-coastPoints$origIndex
    marineSDVals$origInd<-coastPoints$origIndex
    

    #we now need to expand this out so it fits into the raster
    nearSeaDF<-as.data.frame(TerrCropTmp$nearestSeaCell)
    names(nearSeaDF)<-"origInd"
    nearSeaDF$sort<-1:nrow(nearSeaDF)


    #now add the near marine information to index we fed in
    nearSeaMean<-merge(nearSeaDF, marineMeanVals, by="origInd", all.x=T, sort=F)
    nearSeaSD<-merge(nearSeaDF, marineSDVals, by="origInd", all.x=T, sort=F)
    
    nearSeaMean<-nearSeaMean[order(nearSeaMean$sort),]
    nearSeaSD<-nearSeaSD[order(nearSeaSD$sort),]

    #now assign these to the original raster
    TerrCropTmp$MEAN_SST_WS <- nearSeaMean$MEAN_SST_WS
    TerrCropTmp$MEAN_mean_sal <- nearSeaMean$MEAN_mean_sal
    TerrCropTmp$MEAN_max_chloro <- nearSeaMean$MEAN_max_chloro
    TerrCropTmp$MEAN_bathy <- nearSeaMean$MEAN_bathy
    TerrCropTmp$MEAN_distShore <- nearSeaMean$MEAN_distShore
    TerrCropTmp$MEAN_Amarinus <- nearSeaMean$MEAN_Amarinus
    
    TerrCropTmp$SD_SST_WS <- nearSeaSD$SD_SST_WS
    TerrCropTmp$SD_mean_sal <- nearSeaSD$SD_mean_sal
    TerrCropTmp$SD_max_chloro <- nearSeaSD$SD_max_chloro
    TerrCropTmp$SD_bathy <- nearSeaSD$SD_bathy
    TerrCropTmp$SD_distShore <- nearSeaSD$SD_distShore
    TerrCropTmp$SD_Amarinus <- nearSeaSD$SD_Amarinus

    
    names(TerrCropTmp)
    
    #check what terrestrial points do not have valid marine points
    test1<-!is.na(TerrCropTmp$MeanTemp_WM) & is.na(TerrCropTmp$MEAN_SST_WS)
    #this should be as blank as possible, due due to map warping there will be some errors
    plot(test1)

    plot(TerrCropTmp$MEAN_SST_WS)
    plot(MarinePredictors$SST_WS,add=T)
    
    
    #remove any land that is not in OSPAR
    envcheck<-SpatialPoints(coordinates(TerrCropTmp))
    crs(envcheck)<-crs(ospar)
    
    ospres<-over(envcheck, ospar)
    unique(ospres$CNTR_NAME)
    outrows<-which(rowSums(is.na(ospres)) == ncol(ospres))
    TerrCropTmp[outrows]<-NA
    plot(TerrCropTmp$MeanTemp_WM)
    
    #cut out greenland
    t1<-TerrCropTmp
    t1[(coordinates(t1)[,1]<=-10 & coordinates(t1)[,2]>67) |
       (coordinates(t1)[,1]<=-32 & coordinates(t1)[,2]>59)]<-NA
    plot(t1)
     
    if(output){
      writeRaster(t1, paste0("../TerrwithMarineVar_5min_",sp_now,".tif"),overwrite=T)
      #writeRaster(MarCrop, "MarineVarCrop_5min.tif")
    }
  

  }
  
  setwd(savewd)
  print ("done adding marine values to terr points!")
  
  
}

  

###ASSIGN TERRESTRIAL VALUES TO MARINE POINTS ###

#this is a simpler version of what is done above
#we take the index of the nearest terrestrial point to each marine point
#and add the values of that terr cell to the marine cell

if (RunAddTerr){

  #get cell indexes for each cell
  indx<-as.data.frame((MarCrop[[8]]))
  indx$index<-1:nrow(indx)
  indx<-indx[!is.na(indx$nearestLandCell),]
  

  #look up in the terrestrial layer
  terrdf<-as.data.frame(TerrCrop)
  nearLanddf<-merge(indx, terrdf, by.x="nearestLandCell", by.y="origIndex", all.x=T, sort=F)
  nearLanddf<-nearLanddf[order(nearLanddf$index),]
  
  #set new layers to be blank by default
  MarCrop$NLand_MeanTempWM<-NA
  MarCrop$NLand_PrecBM<-NA
  MarCrop$NLand_Isol<-NA
  MarCrop$NLand_Area<-NA
  MarCrop$NLand_LandCover<-NA
  MarCrop$NLand_NDVImin<-NA
  MarCrop$NLand_NDVImean<-NA
  
  #move values over to marine layer
  MarCrop$NLand_MeanTempWM[indx$index]<-nearLanddf$MeanTemp_WM
  MarCrop$NLand_PrecBM[indx$index]<-nearLanddf$Prec_BreedMonths 
  MarCrop$NLand_Isol[indx$index]<-nearLanddf$Isol
  MarCrop$NLand_Area[indx$index]<-nearLanddf$Area 
  MarCrop$NLand_LandCover[indx$index]<-nearLanddf$LandCover 
  MarCrop$NLand_NDVImin[indx$index]<-nearLanddf$NDVI_min 
  MarCrop$NLand_NDVImean[indx$index]<-nearLanddf$NDVI_mean 
  
  #manual fix
  #Greenland did not have a defined area or isolation metric when we initially calculated them
  #this is now a problem as many marine points are near greenland

  #get all points near Greenland that we need to modify manually
  mcheck<-(coordinates(MarCrop)[,2]>49) * is.na(MarCrop$NLand_Isol) * !is.na(MarCrop$NLand_MeanTempWM)
  mdf<-as.data.frame(mcheck)
  mind<-which(mdf$layer==1)
  
  
  #optional check to make sure all the points we are talking about refer to greenland
  #plot(mcheck)
  #lookup<-MarCrop$nearestLandCell[mind]
  #TerrCrop$check<-NA
  #TerrCrop$check[which(getValues(TerrCrop$origIndex) %in% lookup)]<-1
  #plot(TerrCrop$check,col="Red")
  
  
  #adjust anything near greenland to refer to greenland
  #isolation is 0 as it's a large land mass
  MarCrop$NLand_Isol[mind]<-0
  #  #area estimate from Wiki: 2,166,086 km2. area in m2
  MarCrop$NLand_Area[mind]<-2166086000000
  # adjust land cover value
  MarCrop$NLand_LandCover[mind]<- 11
  
  #second fudge for azores
  mcheck<-(coordinates(MarCrop)[,2]<49 & coordinates(MarCrop)[,1]<=-5) * is.na(MarCrop$NLand_Area) * !is.na(MarCrop$NLand_MeanTempWM)
  mdf<-as.data.frame(mcheck)
  mind<-which(mdf$layer==1)
  
  #adjust anything near greenland to refer to greenland
  #isolation is 150,000m as median distance between islands
  MarCrop$NLand_Isol[mind]<-150000
  #  #area estimate from Wiki for larges island in azores: 760 km2. area in m2
  MarCrop$NLand_Area[mind]<-760000000
  # adjust land cover value
  MarCrop$NLand_LandCover[mind]<-12
  
  #double check we have values for marine points near greenland
  #MarCrop2<-MarCrop
  #MarCrop2$check<-NA
  #MarCrop2$check[!is.na(MarCrop2$NLand_MeanTempWM)&is.na(MarCrop2$NLand_Area)]<-1
  #plot(MarCrop2$check)
  #plot(ospar,add=T)

  
  #Fudge 3:
  #Note that in essence we are saying we cannot have a full data set for marine points that are nearest to land masses
  #outside of OSPAR region
  #set any remaining points in med to NA to remove from analysis
  MarCrop[which((coordinates(MarCrop)[,2]<48 & coordinates(MarCrop)[,1]>2) |
                    (coordinates(MarCrop)[,2]<42 & coordinates(MarCrop)[,1]>-4))]<-NA

  
  
  #save result
  if(output){
    writeRaster(MarCrop, paste0("./MarinewithTerrVar_5min.tif"),overwrite=T)
    #writeRaster(MarCrop, "MarineVarCrop_5min.tif")
  }
  print("Done with Marine (adding Terr)")
}
  



###optionally plot the variables###
if (plotVar){
  
  
  
  savewd<-getwd()
  setwd(paste0(outpath,"/PlotVars"))
  
  #set colour palette
  library(rasterVis)
  my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127"))
  

  #plot marine first as that is uniform across all species
  
  ### MARINE PLOTS
  
  if (RunMar){
    #if we've already run the download/collation process then start here and load rasters
    MarCrop<-stack("../MarineVarCrop_5min.tif")
    names(MarCrop)<-c("SST_WS","mean_sal", "max_chloro", "bathy", "distShore", "Amarinus")
    
    
    png(filename="Marine_SST_WS.png", width=8, height=8, units="in", res=300)
      plot(MarCrop$SST_WS,col=my.colors(1000),axes=FALSE, box=FALSE)
      title(cex.sub = 1.25, main = "Mean Winter/Spring Sea Surface Temperature (ºC)")
    dev.off()
    
    png(filename="Marine_mean_sal.png", width=8, height=8, units="in", res=300)
      plot(MarCrop$mean_sal,col=my.colors(1000),axes=FALSE, box=FALSE)
      title(cex.sub = 1.25, main = "Mean salinity at sea surface (PSS)")
    dev.off()
      
    png(filename="Marine_max_chloro.png", width=8, height=8, units="in", res=300)
      plot(MarCrop$max_chloro,col=my.colors(1000),axes=FALSE, box=FALSE)
      title(cex.sub = 1.25, main = "Concentration of photosynthetic pigment chlorophyll A (mg/mÅ)")
    dev.off()
      
    png(filename="Marine_bathy.png", width=8, height=8, units="in", res=300)   
      plot(MarCrop$bathy,col=my.colors(1000),axes=FALSE, box=FALSE)
      title(cex.sub = 1.25, main = "Bathymetry (m)")
    dev.off()
    
    png(filename="Marine_distShore.png", width=8, height=8, units="in", res=300)
      plot(MarCrop$distShore,col=my.colors(1000),axes=FALSE, box=FALSE)
      title(cex.sub = 1.25, main = "Distance to shore (km)")
    dev.off()
      
    png(filename="Marine_Amarinus.png", width=8, height=8, units="in", res=300)
      plot(MarCrop$Amarinus,col=my.colors(1000),axes=FALSE, box=FALSE)
      plot(newmap,add=T)
      title(cex.sub = 1.25, main = "Probability of occurrence of A. marinus")
    dev.off()
  }
  
  #if we've combined Marine values with terrestrial values
  if (RunAddTerr){

    #if we've already run the download/collation process then start here and load rasters
    MarCrop<-stack("../MarinewithTerrVar_5min.tif")
    names(MarCrop)<-c("SST_WS","mean_sal", "max_chloro", "bathy", 
                      "ID", "distShore", "direToLand", "nearestLandCell",
                      "Amarinus", "origIndex",
                      "NLand_MeanTempWM", "NLand_PrecBM", "NLand_Isol",
                      "NLand_Area","NLand_LandCover",
                      "NLand_NDVImin", "NLand_NDVImean")

    
    png(filename="Marine_DirecLand.png", width=8, height=8, units="in", res=300)
      plot(MarCrop$direToLand,col=my.colors(1000),axes=FALSE, box=FALSE)
      plot(newmap,add=T)
      title(cex.sub = 1.25, main = "Direction to land (m)")
    dev.off()
    
    png(filename="NLand_Temp.png", width=8, height=8, units="in", res=300)
      plot(MarCrop$NLand_MeanTempWM,col=my.colors(1000),axes=FALSE, box=FALSE)
      plot(newmap,add=T)
    title(cex.sub = 1.25, main = "Nearest Land: Mean Temperature of Warmest Month (ºC)")
    dev.off()
    
    png(filename="NLand_Prec.png", width=8, height=8, units="in", res=300)
      plot(MarCrop$NLand_PrecBM,col=my.colors(1000),axes=FALSE, box=FALSE)
      plot(newmap,add=T)
      title(cex.sub = 1.25, main = "Nearest Land: summed precipitation during breeding months (ml)")
    dev.off()
    
    png(filename="NLand_Isol.png", width=8, height=8, units="in", res=300)
      plot(MarCrop$NLand_Isol,col=my.colors(1000),axes=FALSE, box=FALSE)
      plot(newmap,add=T)
      title(cex.sub = 1.25, main = "Nearest Land: Isolation")
    dev.off()
    
    png(filename="NLand_Area.png", width=8, height=8, units="in", res=300)
      plot(log10(MarCrop$NLand_Area),col=my.colors(1000),axes=FALSE, box=FALSE)
      plot(newmap,add=T)
      title(cex.sub = 1.25, main = "Nearest Land: Area (logged m2)")
    dev.off()
    
    png(filename="NLand_LandCover.png", width=8, height=8, units="in", res=300)
      plot(log10(MarCrop$NLand_LandCover),col=my.colors(1000),axes=FALSE, box=FALSE)
      plot(newmap,add=T)
      title(cex.sub = 1.25, main = "Nearest Land: Land Cover")
    dev.off()
    
    png(filename="NLand_NDVImin.png", width=8, height=8, units="in", res=300)
      plot(MarCrop$NLand_NDVImin,col=my.colors(1000),axes=FALSE, box=FALSE)
      plot(newmap,add=T)
      title(cex.sub = 1.25, main = "Nearest Land: min NDVI")
    dev.off()
    
    png(filename="NLand_NDVImean.png", width=8, height=8, units="in", res=300)
      plot(MarCrop$NLand_NDVImean,col=my.colors(1000),axes=FALSE, box=FALSE)
      plot(newmap,add=T)
      title(cex.sub = 1.25, main = "Nearest Land: mean NDVI")
    dev.off()
    
  }

  
  #now loop through terrestrial points
  if (RunTerr){
    
    TerrPredictors<-stack("TerrEnvVariables_5m.tif")
    names(TerrPredictors)<-c("MeanTemp_WM", "Prec_BreedMonths", "Isol", 
                             "Area", "LandCover", "distToSea", "nearestSeaCell",
                             "NDVI_mean", "NDVI_min")

      ### TERR PLOTS
      #these are the same for all species, so just plot them once
        
    png(filename="Terr_MeanTemp_WM.png", width=8, height=8, units="in", res=300)
      plot(TerrPlot$MeanTemp_WM,col=my.colors(1000),axes=FALSE, box=FALSE)
      title(cex.sub = 1.25, sub = "Mean Temperature of Warmest Month (ºC)")
    dev.off()
    
    png(filename="Terr_Prec_BreedMonths.png", width=8, height=8, units="in", res=300)
      TerrPlot$Prec_BreedMonths[TerrPlot$Prec_BreedMonths==0]<-NA
      plot(TerrPlot$Prec_BreedMonths,col=my.colors(1000),axes=FALSE, box=FALSE)
      title(cex.sub = 1.25, sub = "Summed precipitation during breeding months (ml)")
    dev.off()
    
    png(filename="Terr_Isol.png", width=8, height=8, units="in", res=300)
      plot(log10(TerrPlot$Isol+1),col=my.colors(1000),axes=FALSE, box=FALSE)
      title(cex.sub = 1.25, sub = "Isolation (logged km to nearest larger island)")
    dev.off()
    
    png(filename="Terr_Area.png", width=8, height=8, units="in", res=300)
      plot(log10(TerrPlot$Area),col=my.colors(1000),axes=FALSE, box=FALSE)
      title(cex.sub = 1.25, sub = "Logged area of land (km^2)")
    dev.off()
    
    png(filename="Terr_distToSea.png", width=8, height=8, units="in", res=300)
      plot(TerrPlot$distToSea,col=my.colors(1000),axes=FALSE, box=FALSE)
      title(cex.sub = 1.25, sub = "Distance to sea (km)")
    dev.off()
    
    png(filename="Terr_NDVI_mean.png", width=8, height=8, units="in", res=300)
      plot(TerrPlot$NDVI_mean,col=my.colors(1000),axes=FALSE, box=FALSE)
      title(cex.sub = 1.25, sub = "NDVI (mean)")
    dev.off()
        
    png(filename="Terr_NDVI_min.png", width=8, height=8, units="in", res=300)
      plot(TerrPlot$NDVI_min,col=my.colors(1000),axes=FALSE, box=FALSE)
      title(cex.sub = 1.25, sub = "NDVI (min)")
    dev.off()
        
  }
      
      
      
  ### COMBINED PLOTS
  #my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127"))
  
  #these are all different for each species so we do them sepeately
  
  #if we've combined Marine values with terrestrial values
  if (RunAddMarine){
    
    for (q in 1:length(sp_names)){
      
      i<-sp_names[q]


      TerrPlot<-stack(paste0("./TerrandMarineVar_5min_", i, ".tif"))
      names(TerrPlot) <- c("MeanTemp_WM", "Prec_BreedMonths", "Isol",
                           "Area", "LandCover", "distToSea","nearestSeaCell",
                           "NDVI_mean", "NDVI_min",
                           "MEAN_SST_WS","MEAN_mean_sal","MEAN_max_chloro",
                           "MEAN_bathy","MEAN_distShore","MEAN_Amarinus",
                           "SD_SST_WS","SD_mean_sal","SD_max_chloro",
                           "SD_bathy","SD_distShore","SD_Amarinus" )
      
  
      png(filename=paste0("Terr_",i,"_SST_WS.png"), width=16, height=10, units="in", res=300)
        par(mfrow=c(1,2))
        plot(TerrPlot$MEAN_SST_WS,col=my.colors(1000),axes=FALSE, box=FALSE)
        title(cex.sub = 1.25, sub = "Mean: sea surface temperature during winter/spring (ºC)")
        plot(TerrPlot$SD_SST_WS,col=my.colors(1000),axes=FALSE, box=FALSE)
        title(cex.sub = 1.25, sub = "SD: sea surface temperature during winter/spring (ºC)")
      dev.off()
      
      png(filename=paste0("Terr_",i,"_mean_sal.png"), width=16, height=10, units="in", res=300)
        par(mfrow=c(1,2))
        plot(TerrPlot$MEAN_mean_sal,col=my.colors(1000),axes=FALSE, box=FALSE)
        title(cex.sub = 1.25, sub = "Mean: salinity at sea surface (PSS)")
        plot(TerrPlot$SD_mean_sal,col=my.colors(1000),axes=FALSE, box=FALSE)
        title(cex.sub = 1.25, sub = "SD: salinity at sea surface (PSS)")
      dev.off()
      
      png(filename=paste0("Terr_",i,"max_chloro.png"), width=16, height=10, units="in", res=300)
        par(mfrow=c(1,2))
        plot(TerrPlot$MEAN_max_chloro,col=my.colors(1000),axes=FALSE, box=FALSE)
        title(cex.sub = 1.25, sub = "Mean: concentration of photosynthetic pigment chlorophyll A (mg/mÅ)")
        plot(TerrPlot$SD_max_chloro,col=my.colors(1000),axes=FALSE, box=FALSE)
        title(cex.sub = 1.25, sub = "SD: concentration of photosynthetic pigment chlorophyll A (mg/mÅ)")
      dev.off()
      
      png(filename=paste0("Terr_",i,"_bathy.png"), width=16, height=10, units="in", res=300)
        par(mfrow=c(1,2))
        plot(TerrPlot$MEAN_bathy,col=my.colors(1000),axes=FALSE, box=FALSE)
        title(cex.sub = 1.25, sub = "Mean: bathymetry (m)")
        plot(TerrPlot$SD_bathy,col=my.colors(1000),axes=FALSE, box=FALSE)
        title(cex.sub = 1.25, sub = "SD: bathymetry (m)")
      dev.off()
      
      png(filename=paste0("Terr_",i,"_distShor.png"), width=16, height=10, units="in", res=300)
        par(mfrow=c(1,2))
        plot(TerrPlot$MEAN_distShore,col=my.colors(1000),axes=FALSE, box=FALSE)
        title(cex.sub = 1.25, sub = "Mean: distance to shore (km)")
        plot(TerrPlot$SD_distShore,col=my.colors(1000),axes=FALSE, box=FALSE)
        title(cex.sub = 1.25, sub = "SD: distance to shore (km)")
      dev.off()
      
      png(filename=paste0("Terr_",i,"_Amarinus.png"), width=16, height=10, units="in", res=300)
        par(mfrow=c(1,2))
        plot(TerrPlot$MEAN_Amarinus,col=my.colors(1000),axes=FALSE, box=FALSE)
        title(cex.sub = 1.25, sub = "Mean: distance to shore (km)")
        plot(TerrPlot$SD_MEAN_Amarinus,col=my.colors(1000),axes=FALSE, box=FALSE)
        title(cex.sub = 1.25, sub = "SD: distance to shore (km)")
      dev.off()
    
    }
  
  }
    
  print ("done plotting!")
    
    
}
  
  

#as a final stage we can choose whether we want to clip around the coasts












