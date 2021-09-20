

############################################################
############################################################
### CALCULATE DISTANCE TO SEA/SHORE AND PROPERTIES ###
############################################################
############################################################


### META ###
# by HHakkinen
# Start Date: 25/01/2021
# End Date:
# As part of Institute of Zoology ZSL / University of Cambridge project
# "Evidence-based Climate Change Adaptation Practices to safeguard vulnerable species: supporting conservation practitioners, donors and policy makers"
# 
# This file should be run before PrepEnvData.R
# As we wish to combine terrestrial and marine components for our SDM we want to know how far everything is from the sea
# and several aspects of the direction/distance/nearest cell

### /META ###




############################################################
### LOAD LIBRARIES AND LAYERS ###
############################################################

rm(list=ls())
#we see how far it is between a cell with terr values and a cell with marine values
#some might be 0 if coast is within a single cell
library(raster)



#load a terrestrial layer to use as the basis for land
setwd("C:/Users/Henry/Documents/Research/ZSL/RawData/")
file_list<-list.files(path= "./WorldClim/wc2.1_5m_tavg",pattern="*.tif")
TerrPre<-raster(paste0("./WorldClim/wc2.1_5m_tavg/",file_list[1]))
names(TerrPre)<-"MeanTemp_W"

#ne.atlantic.ext<-extent(15,25,68,71)

# Crop raster to fit the North Atlantic
ne.atlantic.ext <- extent(-100, 55, 30, 90)
Terr.crop <-crop(TerrPre, ne.atlantic.ext)



#load a marine layer to use as the basis for calculation
MarineInd <- stack("MARSPEC/MARSPEC_5m/Monthly_Variables_5m/Temperatures/sst12_5m/w001001.adf")
names(MarineInd)<-"SST_WS"
# Crop raster to fit the North Atlantic
ne.atlantic.ext <- extent(-100, 55, 30, 90)
Mar.crop<-crop(MarineInd,ne.atlantic.ext)


#in some places land and sea exist in the same cell
#take the conservative approach and if in doubt count it as a marine cell
#Terr.cropOR<-Terr.crop
#Terr.crop[!is.na(Terr.crop$MeanTemp_W)&!is.na(Mar.crop$SST_WS)]<-NA


Marshow<-Mar.crop
Marshow$SST_WS[!is.na(Terr.crop$MeanTemp_W)&!is.na(Marshow$SST_WS)]<-99999
plot(Marshow$SST_WS)
Marshow$Mar<-0
Marshow$Mar[!is.na(Marshow$SST_WS)]<-1
Marshow$Terr<-0
Marshow$Terr[!is.na(Terr.crop$MeanTemp_W)]<-2
Marshow$Comb<-Marshow$Mar+Marshow$Terr
plot(Marshow$Comb)


#check projections available
library(rgdal)
epsg <- make_EPSG()
i <- grep("Europe", epsg$note, ignore.case=TRUE)
epsg[i[1:8], ]



############################################################
### Reproject, calculate distance, direction etc. ###
############################################################

#set ID to keep track of warping
Mar.crop$ID<-1:length(Mar.crop$SST_WS)
Terr.crop$ID<-1:length(Terr.crop$MeanTemp_W)


#change projection so we get accurate distance and direction
newproj <- "+proj=lcc +lat_0=52 +lon_0=10 +lat_1=35 +lat_2=65 +x_0=4000000 +y_0=2800000 +ellps=GRS80 +units=m +no_defs +type=crs"
pr2 <- projectRaster(Mar.crop, crs=newproj, method="ngb")
landRep<- projectRaster(Terr.crop, crs=newproj, method="ngb")


#in some places land and sea exist in the same cell
#take the conservative approach and if in doubt count it as a marine cell
landRepOR<-landRep
landRep$MeanTemp_W[!is.na(landRep$MeanTemp_W)&!is.na(pr2$SST_WS)]<-NA


# Rasterize and set land pixels to NA
start.t<-Sys.time()
# Calculate distance to nearest non-NA pixel
#note the following operation takes 5.7 hours
d <- distance(pr2$SST_WS)
end.t<-Sys.time()
end.t - start.t


#calculate direction to nearest non-NA pixel
start.t<-Sys.time()
direct <- direction(pr2$SST_WS, from=FALSE)
end.t<-Sys.time()
end.t - start.t



#set land
landRep[!is.na(landRep)]<-1
# Optionally set non-land pixels to NA (otherwise values are "distance to non-land")
dland <- d*landRep[[1]]

#save output
setwd("C:/Users/Henry/Documents/Research/ZSL/ProcessedData/CalculateDistShore")
writeRaster(d, "DistancetoSeaTerr.tif", overwrite=T)

dirland <- direct*landRep[[1]]
#save output
setwd("C:/Users/Henry/Documents/Research/ZSL/ProcessedData/CalculateDistShore")
writeRaster(direct, "DirectiontoSeaTerr.tif", overwrite=T)






############################################################
### Sort out the result, make sure it's in the correct lat/long extent etc. ###
############################################################

#half way marker
#setwd("C:/Users/Henry/Documents/Research/ZSL/ProcessedData/CalculateDistShore")
#dland<-raster("DistancetoSeaTerr.tif")
#dirland<-raster("DirectiontoSeaTerr.tif")


# NA raster
rna <- is.na(pr2$SST_WS) # returns NA raster

# store coordinates in new raster: https://stackoverflow.com/a/35592230/3752258 
na.x <- init(rna, 'x')
na.y <- init(rna, 'y')

# calculate coordinates of the nearest Non-NA pixel
# assume that we have a orthogonal, projected CRS, so we can use (Pythagorean) calculations
co.x <- na.x + dland * sin(dirland)
co.y <- na.y + dland * cos(dirland)

# matrix with point coordinates of nearest non-NA pixel
co <- cbind(co.x[], co.y[]) 


# extract values of nearest non-NA cell with coordinates co
NAVals <- raster::extract(pr2$ID, co, method='simple') 

#do a quick test that we have a valid value for all marine cells
cotest<-co[which(!is.na(co[,1])&!is.na(co[,2])),]
NAtest <- raster::extract(pr2$SST_WS, cotest, method='simple') 
sum(is.na(NAtest))

IDvals <- rna # initiate new raster
IDvals[] <- NAVals # store values in raster


stackedDist<-stack(dland,dirland,IDvals,pr2$ID)
names(stackedDist)<-c("distToSea", "direToSea", "nearestSeaCell", "ID")
#set values to NA if outside of bounds
#stackedDist[is.na(stackedDist$ID)]<-NA


#part 2 if a terrestrial cell is shared with a marine cell, set distance and direction to 0
# set nearestSeaCell to self

stackedDist$distToSea[!is.na(landRepOR$MeanTemp_W)&!is.na(pr2$SST_WS)]<-0
stackedDist$direToSea[!is.na(landRepOR$MeanTemp_W)&!is.na(pr2$SST_WS)]<--1

stackedDist$nearestSeaCell[!is.na(landRepOR$MeanTemp_W)&!is.na(pr2$SST_WS)]<-stackedDist$ID[!is.na(landRepOR$MeanTemp_W)&!is.na(pr2$SST_WS)]


#reproject to original proj
stackedDistO<-projectRaster(stackedDist, to=Terr.crop, method="ngb")


Terr.crop$distToSea<-stackedDistO$distToSea
Terr.crop$direToSea<-stackedDistO$direToSea
Terr.crop$nearestSeaCell<-stackedDistO$nearestSeaCell
#Terr.crop$newID<-stackedDistO$ID


############################################################
### Check the result, save output. ###
############################################################

###occasionally because of warping we have cells with no valid values, fix with self
cellref<-as.data.frame((!is.na(Terr.crop$MeanTemp_W)&!is.na(Mar.crop$SST_WS) & is.na(Terr.crop$distToSea)))
cellref<-which(cellref==T)

Terr.crop$distToSea[cellref]<-0
Terr.crop$direToSea[cellref]<--1

Terr.crop$nearestSeaCell[cellref]<-Terr.crop$ID[cellref]


###for the few remaining cells they have lost their value due to map warping, manually find nearest sea cell
missT<-Terr.crop
missT$prob<-NA
missT$prob[!is.na(missT$MeanTemp_W)&is.na(missT$distToSea)]<-1
#if missT is blank then we good
missDF<-as.data.frame(missT$prob,na.rm=T)

missingCO<-as.data.frame(xyFromCell(test,as.numeric(row.names(missDF))))
missingCO$ID<-as.numeric(row.names(missDF))

mopdf<-nearestSea(missingCO[,1:2], Mar.crop, max_distance=100000)

#plot and check for errors
#plot(missT$MeanTemp_W)
#points(missingCO[,1], missingCO[,2], col="blue")
#arrows(missingCO[,1], missingCO[,2], mopdf[,1], mopdf[,2])

Terr.crop$distToSea[as.numeric(row.names(missDF))]<-mopdf[,4]
Terr.crop$nearestSeaCell[as.numeric(row.names(missDF))]<-mopdf[,6]



###final checks
test<-Terr.crop
test$prob<-NA
test$prob[!is.na(test$MeanTemp_W)&is.na(test$distToSea)]<-1
#if missT is blank then we good
testdf<-as.data.frame(test$prob,na.rm=T)

testCO<-as.data.frame(xyFromCell(test,as.numeric(row.names(testdf))))
testCO$ID<-as.numeric(row.names(testdf))

plot(test$MeanTemp_W)
points(testCO[,1], testCO[,2], col="blue")
#there's a few points around the edges of the map due to reprojections but these are not used so can be ignored


#if test is clear then just save it
Terr.crop$distToSea[is.na(Terr.crop$MeanTemp_W)]<-NA
Terr.crop$direToSea[is.na(Terr.crop$MeanTemp_W)]<-NA
Terr.crop$nearestSeaCell[is.na(Terr.crop$MeanTemp_W)]<-NA


###write out the result
setwd("C:/Users/Henry/Documents/Research/ZSL/ProcessedData/CalculateDistShore")
writeRaster(Terr.crop, "NearestSea_Final.tif",overwrite=T)



#can plot the coastline marine areas that are the basis for drawing marine data
AllPoints<-as.data.frame(Terr.crop$nearestSeaCell, na.rm=T)
markeys<-unique(AllPoints$nearestSeaCell)
indlevel<-(Mar.crop$ID %in% markeys)

coastPoints<-as.data.frame(indlevel, na.rm=T)
coastInd<-which(coastPoints$layer==1)

testr<-Terr.crop
testr$Valid<-NA
testr[coastInd]<-2

plot(testr$Valid,col="blue")
plot(Mar.crop$SST_WS,add=T)
#sum(!is.na(Mar.crop$SST_WS)[coastInd])

















