
#backup alternative methods to CalculateDistShore
#eventually retired because this method is so slow

############################################################
############################################################
### CALCULATE DISTANCE TO SHORE AND PROPERTIES ###
############################################################
############################################################


### META ###
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
library(sdmpredictors)
source("C:/Users/Henry/Documents/Research/ZSL/Repos/SeabirdCCVA/Code/prepFunctions.R")

setwd("C:/Users/Henry/Documents/Research/ZSL/RawData/")

#define NE atlantic extent
ne.atlantic.ext <- extent(-100, 55, 30, 90)
#ne.atlantic.ext<-extent(15,25,68,71)

ospar<-shapefile("C:/Users/Henry/Documents/Research/ZSL/ProcessedData/OSPAR_region/ospar_LandSea.shp")



#load a marine layer to use as the basis
#stack distance to shore, use as comparison point
distShore<-raster("sdmpredictors/MS_biogeo05_dist_shore_5m_lonlat.tif")
names(distShore)<-"distShore1"
# Crop raster to fit the North Atlantic
Mar.crop<-crop(distShore,ne.atlantic.ext)



#load a terrestrial layer to use as the basis for calculation
file_list<-list.files(path= "./WorldClim/wc2.1_5m_tavg",pattern="*.tif")
TerrPre<-raster(paste0("./WorldClim/wc2.1_5m_tavg/",file_list[1]))
names(TerrPre)<-"MeanTemp_W"

TerrC<-stack("C:/Users/Henry/Documents/Research/ZSL/Repos/SeabirdCCVA/IntermediateOutput/PrepEnvData/TerrEnvVariables_5m.tif")
names(TerrC)<-c("MeanTemp_WM", "Prec_BreedMonths", "Isol", 
                "Area", "LandCover", "distToSea", "nearestSeaCell",
                "NDVI_mean", "NDVI_min")

TerrC<-subset(TerrC, drop=T, c("MeanTemp_W", "Prec_BreedMonths", "Isol", 
                               "Area", "distToSea", "nearestSeaCell",
                               "NDVI_mean", "NDVI_min"))


# Crop raster to fit the North Atlantic
Terr.crop <-crop(TerrPre, ne.atlantic.ext)
Terrfull.crop <-crop(TerrC, ne.atlantic.ext)

#remove any cell that does not have a complete set of values.
TerrCheck<-as.data.frame(Terrfull.crop)
TerrCheck$invalid<-!complete.cases(TerrCheck)
Terrfull.crop$invalid<-TerrCheck$invalid

#extend to full size
Terrfull.ex<-extend(Terrfull.crop, extent(Terr.crop))[["invalid"]]

#turn anything outside of the ospar shape into NA to stop us overwriting useful things
Terrfull.exm<-mask(Terrfull.ex, ospar)
Terr.crop$invalid<-Terrfull.exm$invalid

plot(Terr.crop$invalid)

Terr.crop$MeanTemp_W[Terr.crop$invalid==1]<-NA
#drop the invalid layer
Terr.crop<-Terr.crop$MeanTemp_W

plot(Terr.crop)

#in some places land and sea exist in the same cell
Marshow<-Mar.crop
Marshow$Mar<-0
Marshow$Mar[!is.na(Marshow$distShore1)]<-1
Marshow$Terr<-0
Marshow$Terr[!is.na(Terr.crop$MeanTemp_W)]<-2
Marshow$Comb<-Marshow$Mar+Marshow$Terr
plot(Marshow$Comb)


#check projections available
library(rgdal)
epsg <- make_EPSG()
i <- grep("Europe", epsg$note, ignore.case=TRUE)
epsg[i[1:8], ]




#######TESTING SECTION





############################################################
### Reproject, calculate distance, direction etc. ###
############################################################

#set ID to keep track of warping
Mar.crop$ID<-1:length(Mar.crop$distShore1)
Terr.crop$ID<-1:length(Terr.crop$MeanTemp_W)


#change projection so we get accurate distance and direction
newproj <- "+proj=lcc +lat_0=52 +lon_0=10 +lat_1=35 +lat_2=65 +x_0=4000000 +y_0=2800000 +ellps=GRS80 +units=m +no_defs +type=crs"
seaRep <- projectRaster(Mar.crop, crs=newproj, method="ngb")
la2<- projectRaster(Terr.crop, crs=newproj, method="ngb")


#in some places land and sea exist in the same cell
#take the conservative approach and if in doubt count it as a terrestrial cell
seaRepOR<-seaRep
seaRep$distShore1[!is.na(seaRep$distShore1)&!is.na(la2$MeanTemp_W)]<-NA


# Rasterize and set land pixels to NA
start.t<-Sys.time()
# Calculate distance to nearest non-NA pixel
#note the following operation takes 5.7 hours
d <- distance(la2$MeanTemp_W)
end.t<-Sys.time()
end.t - start.t


#calculate direction to nearest non-NA pixel
start.t<-Sys.time()
direct <- direction(la2$MeanTemp_W, from=FALSE)
end.t<-Sys.time()
end.t - start.t



#set sea
seaRep[!is.na(seaRep)]<-1
# Optionally set non-land pixels to NA (otherwise values are "distance to non-land")
dsea <- d*seaRep[[1]]

#save output
setwd("C:/Users/Henry/Documents/Research/ZSL/ProcessedData/CalculateDistShore")
writeRaster(d, "DistancetoLandMar.tif", overwrite=T)

dirsea <- direct*seaRep[[1]]
#save output
setwd("C:/Users/Henry/Documents/Research/ZSL/ProcessedData/CalculateDistShore")
writeRaster(direct, "DirectiontoLandMar.tif", overwrite=T)




#d<-raster("C:/Users/Henry/Documents/Research/ZSL/ProcessedData/CalculateDistShore/DistancetoLandMar.tif")
#direct<-raster("C:/Users/Henry/Documents/Research/ZSL/ProcessedData/CalculateDistShore/DirectiontoLandMar.tif")





############################################################
### Sort out the result, make sure it's in the correct lat/long extent etc. ###
############################################################

#half way marker
#setwd("C:/Users/Henry/Documents/Research/ZSL/ProcessedData/CalculateDistShore")
#dland<-raster("DistancetoSeaTerr.tif")
#dirland<-raster("DirectiontoSeaTerr.tif")


# NA raster
rna <- is.na(la2$MeanTemp_W) # returns NA raster

# store coordinates in new raster: https://stackoverflow.com/a/35592230/3752258 
na.x <- init(rna, 'x')
na.y <- init(rna, 'y')

# calculate coordinates of the nearest Non-NA pixel
# assume that we have a orthogonal, projected CRS, so we can use (Pythagorean) calculations
co.x <- na.x + dsea * sin(dirsea)
co.y <- na.y + dsea * cos(dirsea)

# matrix with point coordinates of nearest non-NA pixel
co <- cbind(co.x[], co.y[]) 


# extract values of nearest non-NA cell with coordinates co
NAVals <- raster::extract(la2$ID, co, method='simple') 

#do a quick test that we have a valid value for all land cells
cotest<-co[which(!is.na(co[,1])&!is.na(co[,2])),]
NAtest <- raster::extract(la2$MeanTemp_W, cotest, method='simple') 
sum(is.na(NAtest))

IDvals <- rna # initiate new raster
IDvals[] <- NAVals # store values in raster


stackedDist<-stack(dsea,dirsea,IDvals,la2$ID)
names(stackedDist)<-c("distToLand", "direToLand", "nearestLandCell", "ID")
#set values to NA if outside of bounds
#stackedDist[is.na(stackedDist$ID)]<-NA


#part 2 if a terrestrial cell is shared with a marine cell, set distance and direction to 0
# set nearestSeaCell to self

stackedDist$distToLand[!is.na(seaRepOR$distShore1)&!is.na(la2$MeanTemp_W)]<-0
stackedDist$direToLand[!is.na(seaRepOR$distShore1)&!is.na(la2$MeanTemp_W)]<--1

stackedDist$nearestLandCell[!is.na(seaRepOR$distShore1)&!is.na(la2$MeanTemp_W)]<-stackedDist$ID[!is.na(seaRepOR$distShore1)&!is.na(la2$MeanTemp_W)]


#reproject to original proj
stackedDistO<-projectRaster(stackedDist, to=Mar.crop, method="ngb")


Mar.crop$distToLand<-stackedDistO$distToLand
Mar.crop$direToLand<-stackedDistO$direToLand
Mar.crop$nearestLandCell<-stackedDistO$nearestLandCell
#Mar.crop$newID<-stackedDistO$ID


############################################################
### Check the result, save output. ###
############################################################

###occasionally because of warping we have cells with no valid values, fix with self
cellref<-as.data.frame((!is.na(Mar.crop$distShore1)&!is.na(Terr.crop$MeanTemp_W) & is.na(Mar.crop$distToLand)))
cellref<-which(cellref==T)

Mar.crop$distToLand[cellref]<-0
Mar.crop$direToLand[cellref]<--1

Mar.crop$nearestLandCell[cellref]<-Mar.crop$ID[cellref]


###for the few remaining cells they have lost their value due to map warping, manually find nearest sea cell
missT<-Mar.crop
missT$prob<-NA
missT$prob[!is.na(missT$distShore1)&is.na(missT$distToLand)]<-1
#if missT is blank then we good
missDF<-as.data.frame(missT$prob,na.rm=T)

missingCO<-as.data.frame(xyFromCell(Mar.crop,as.numeric(row.names(missDF))))
missingCO$ID<-as.numeric(row.names(missDF))

mopdf<-nearestSea(missingCO[,1:2], Terr.crop, max_distance=100000)


#plot and check for errors
plot(Terr.crop$MeanTemp_W)
points(missingCO[,1], missingCO[,2], col="blue")
arrows(missingCO[,1], missingCO[,2], mopdf[,1], mopdf[,2])

Mar.crop$distToLand[as.numeric(row.names(missDF))]<-mopdf[,4]
Mar.crop$nearestLandCell[as.numeric(row.names(missDF))]<-mopdf[,6]



###final checks
test<-Mar.crop
test$prob<-NA
test$prob[!is.na(test$distShore1)&is.na(test$distToLand)]<-1
#if missT is blank then we good
testdf<-as.data.frame(test$prob,na.rm=T)

testCO<-as.data.frame(xyFromCell(test,as.numeric(row.names(testdf))))
testCO$ID<-as.numeric(row.names(testdf))

plot(test$distShore1)
points(testCO[,1], testCO[,2], col="blue")
#there's a few points around the edges of the map due to reprojections but these are not used so can be ignored

#see if original and new measure match
plot(test$distShore1,test$distToLand/1000)
abline(a=0,b=1)

#if test is clear then just save it
Mar.crop$distToLand[is.na(Mar.crop$distShore1)]<-NA
Mar.crop$direToLand[is.na(Mar.crop$distShore1)]<-NA
Mar.crop$nearestLandCell[is.na(Mar.crop$distShore1)]<-NA


###write out the result
setwd("C:/Users/Henry/Documents/Research/ZSL/ProcessedData/CalculateDistShore")
writeRaster(Mar.crop, "NearestLand_Final.tif",overwrite=T)

Mar.crop




















