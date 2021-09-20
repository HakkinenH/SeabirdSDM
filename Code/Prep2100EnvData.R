
#prep 2100 data

#clear env
rm(list=ls())
#set directory to directory where RawData is stored
setwd("C:/Users/Henry/Documents/Research/ZSL/RawData/")

library(rgdal)
library(rworldmap)
library(raster)
library(ggplot2)
library(sdmpredictors)
#library(tidyverse)

outpath <- "C:/Users/Henry/Documents/Research/ZSL/Repos/SeabirdCCVA/IntermediateOutput/Prep2100EnvData"

#specifically for puffins for now

#we need to prep



#the most important variables we found for puffins were 
  #distance to the sea
  #terrestrial temperature during the summer
  #marine temp during the winter
  #and salinity



#distance will be the same in the future so these can be loaded as they are


#This is calculated in CalculateDistShore.R, just load the output
dland<-stack("../ProcessedData/CalculateDistShore/NearestSea_Final.tif")

dland<-subset(dland, c(3,5))
names(dland)<-c("DistToSea", "nearestSeaCell")

library(rasterVis)
levelplot(dland$DistToSea/1000, margin=FALSE, at=seq(0, maxValue(dland$DistToSea)/1000, length=100),
colorkey=list(height=0.6), main='Distance to coast')



#terrestrial temperature 2100
#downloaded from worldclim
#dates 2080-2100 average maximum temperature of each month
#chose this GCM as it scores highly on https://gcmeval.met.no/, but should probably look at an ensemble
#5 min already
#RCP is medium-bad, so not conservative but not the worst case scenario

terrtemp2100_raw<-stack("WorldClim/wc2.1_5m_tmax_MRI-ESM2-0_ssp370_2081-2100/wc2.1_5m_tmax_MRI-ESM2-0_ssp370_2081-2100.tif")
names(terrtemp2100_raw)

#replace NA with -999 cos I hate warning messages
r <- calc(terrtemp2100_raw, function(x) { ifelse(is.na(x), -999, x) }) 

#calculate the maximum for each raster cells, hence return mean temp of the warmest month
MeanTemp_W<-calc(r, fun = max, na.rm = T)
MeanTemp_W <- calc(MeanTemp_W, function(x) { ifelse(x==-999, NA, x) }) 

#write raw data so we don't have to download it again
writeRaster(MeanTemp_W, paste0(outpath,"/WorldClim2100_warmestMonthAvgTemp_5min.tif"), overwrite = TRUE)

print("DONE: mean temperature of the warmest month")

##stack 


#crop down to size
ne.atlantic.ext <- extent(-100, 55, 30, 90)
ospar<-shapefile("C:/Users/Henry/Documents/Research/ZSL/ProcessedData/OSPAR_region/ospar_LandSea.shp")
os.ext<-extent(ospar)

MT_crop <-crop(MeanTemp_W, os.ext)
dland_crop <-crop(dland, os.ext)


#extend others so they have equal areas of blank space
dland_crop<-extend(dland_crop, extent(MT_crop))

TerrPre_crop<-stack(MT_crop, dland_crop)
names(TerrPre_crop)<-c("MeanTemp_WM","distToSea", "nearestSeaCell")


writeRaster(TerrPre_crop, paste0(outpath,"/TerrEnv2100Variables_5m.tif"), overwrite=T)



#now load the marine layers from BIO-ORACLE
# Inspect the available datasets and layers

#View(datasets)
layers <- list_layers(datasets)
View(layers)

# Future Representative Concentration Pathway (RCP) scenarios of interest
rcp = c("RCP60")

# Extract future data sets
datasets <- list_datasets(terrestrial = FALSE, marine = TRUE)
future = list_layers_future(datasets)
future<-subset(future, 
                grepl(paste(rcp, collapse = "|"), scenario) &
                  year == 2100 & 
                  version == 2.1)

# Inspect the available datasets and layers
View(future)



#mean temperature at the sea surface comes from BIO-ORACLE
sst2100<-load_layers("BO21_RCP60_2100_tempmean_ss")
#mean salinity at mean depth comes from BIO-ORACLE
sal2100<-load_layers("BO21_RCP60_2100_salinitymean_bdmean")



###
#crop and stack
###

#crop out antarctica so all extents match
ne.atlantic.ext <- extent(-100, 55, 30, 90)
ospar<-shapefile("C:/Users/Henry/Documents/Research/ZSL/ProcessedData/OSPAR_region/ospar_LandSea.shp")
os.ext<-extent(ospar)


SST_WS <- crop(sst2100, os.ext) 
mean_sal <- crop(sal2100, os.ext) 


MarinePredictors <- stack(SST_WS, mean_sal)
names(MarinePredictors)<-c("SST_WS","mean_sal")

writeRaster(MarinePredictors, paste0(outpath,"/MarineEnv2100Variables_5m.tif"), overwrite=T)


########JOIN MARINE DATA TO TERRESTRIAL

#now we have all the layers fed in we need to take an average from each terrestrial point


#join terrestrial layers together to create TerrPredictors

#join marine layers together to create MarinePredictors

#crop to OSPAR region


#use PrepEnvData as a basis to join the layers together



setwd(outpath)

#load prep functions
source("../../Code/prepFunctions.R")

#if we've already run the download/collation process then start here and load rasters
MarinePredictors<-stack("MarineEnv2100Variables_5m.tif")
names(MarinePredictors)<-c("SST_WS","mean_sal")

TerrPredictors<-stack("TerrEnv2100Variables_5m.tif")
names(TerrPredictors)<-c("MeanTemp_WM", "distToSea", "nearestSeaCell")


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



sp_names<-"Farctica"
forDist <- c(40000)

###ASSIGN MARINE VALUES TO TERR POINTS ###

#save memory, remove things not needed
#rm(TerrPredictors, MarinePredictors, ospar)

#load terrestrial points, find a unique list of points nearest the sea
AllPoints<-as.data.frame(TerrCrop[["nearestSeaCell"]], na.rm=T)
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
  
  names(marineMeanVals)<-c("X","cell","MEAN_SST_WS","MEAN_mean_sal","origIndex")
  names(marineSDVals)<-c("X","cell","SD_SST_WS","SD_mean_sal", "origIndex"  )
  
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

  
  TerrCropTmp$SD_SST_WS <- nearSeaSD$SD_SST_WS
  TerrCropTmp$SD_mean_sal <- nearSeaSD$SD_mean_sal

  
  
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
  plot(t1$MeanTemp_WM)
  

  writeRaster(t1, paste0("../TerrwithMarineVar2100_5min_",sp_now,".tif"),overwrite=T)
  #writeRaster(MarCrop, "MarineVarCrop_5min.tif")

  
  
}
names(t1)
setwd(savewd)
print ("done adding marine values to terr points!")








