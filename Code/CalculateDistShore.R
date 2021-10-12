

############################################################
############################################################
### CALCULATE DISTANCE TO SEA/SHORE AND PROPERTIES ###
############################################################
############################################################


### META ###
# by HHakkinen
# Start Date: 25/01/2021
# End Date: 08/10/2021
# As part of Institute of Zoology ZSL / University of Cambridge project
# "Terrestrial or marine species distribution model - Why not both? A case study with seabirds"
# 
# This file should be run before CombineEnvData.R, and after checking dependencies
# As we wish to combine terrestrial and marine components for our SDM we want to know how far everything is from the sea
# and several aspects of the direction/distance/nearest cell

### /META ###




############################################################
### LOAD LIBRARIES AND LAYERS ###
############################################################

MarinePredictors<-stack(MarineEnvName)
names(MarinePredictors)<-AllMarLayerNames

TerrPredictors<-stack(TerrEnvName)
names(TerrPredictors)<-AllTerrLayerNames

print("Finished loading Env layers")

ospar<-shapefile(studyOutline)


print("Finished loading study layer")


#set layers to use as basis for index. Should be representative of whole stack
Terr.crop<-TerrPredictors[[1]]
Mar.crop<-MarinePredictors[[1]]

#can cut down a lot more to make testing faster
#ospar<-crop(ospar, extent(-7,5,50,58))
#Mar.crop<-crop(Mar.crop, ospar)
#Terr.crop<-crop(Terr.crop, ospar)


#useful little section for finding where layers overlap
# Marshow<-searchL
# Marshow[!is.na(baseL)&!is.na(Marshow)]<-99999
# plot(Marshow$SST_WS)
# Marshow$Mar<-0
# Marshow$Mar[!is.na(Marshow$SST_WS)]<-1
# Marshow$Terr<-0
# Marshow$Terr[!is.na(baseL)]<-2
# Marshow$Comb<-Marshow$Mar+Marshow$Terr
# plot(Marshow$Comb)


#check projections available, should change if you're working on a part of the world that's not Europe
#library(rgdal)
#epsg <- make_EPSG()
#i <- grep("Europe", epsg$note, ignore.case=TRUE)
#epsg[i[1:8], ]



############################################################
### Reproject, calculate distance, direction etc. ###
############################################################

##if we need to add marine to terrestrial we need to find the nearest marine cell to every land cell

prolist<-c()

if(!RunAddMarine & !RunAddTerr){
  print("no options set to merge layers, set 'RunAddMarine' or 'RunAddTerr' to T")
}
#else we do the opposite
if(RunAddMarine){
  prolist<-c(prolist, "RunAddMarine")
}
if(RunAddTerr){
  prolist<-c(prolist, "RunAddTerr")
}

for(x1 in prolist){
  print(x1)
  if(x1=="RunAddMarine"){
    searchL<-Mar.crop
    baseL<-Terr.crop
  }
  if(x1=="RunAddTerr"){
    searchL<-Terr.crop
    baseL<-Mar.crop
  }
  
  

  #set ID to keep track of warping
  searchL$ID<-1:length(searchL)
  baseL$ID<-1:length(baseL)
  
  #set to generic name
  names(searchL)<-c("mar1","ID")
  names(baseL)<-c("terr1","ID")
  
  #change projection so we get accurate distance and direction
  newproj <- "+proj=lcc +lat_0=52 +lon_0=10 +lat_1=35 +lat_2=65 +x_0=4000000 +y_0=2800000 +ellps=GRS80 +units=m +no_defs +type=crs"
  pr2 <- projectRaster(searchL, crs=newproj, method="ngb")
  landRep<- projectRaster(baseL, crs=newproj, method="ngb")
  
  
  #in some places land and sea exist in the same cell
  #take the conservative approach and if in doubt count it as a marine cell
  landRepOR<-landRep
  landRep[[1]][!is.na(landRep$terr1)&!is.na(pr2$mar1)]<-NA
  
  
  print("Starting index building. Warning: this can take a long time for large study areas")
  
  # Rasterize and set land pixels to NA
  start.t<-Sys.time()
  # Calculate distance to nearest non-NA pixel
  #note the following operation takes 5.7 hours
  d <- distance(pr2$mar1)
  
  end.t<-Sys.time()
  end.t - start.t
  
  
  #calculate direction to nearest non-NA pixel
  start.t<-Sys.time()
  direct <- direction(pr2$mar1, from=FALSE)
  end.t<-Sys.time()
  end.t - start.t
  
  
  #set land
  landRep[!is.na(landRep)]<-1
  # Optionally set non-land pixels to NA (otherwise values are "distance to non-land")
  dland <- d*landRep[[1]]
  
  #save output
  if(x1=="RunAddMarine"){
    writeRaster(d, "./IntermediateOutput/CalculateDistShore/DistancetoSeaTerr.tif", overwrite=T)
  }
  if(x1=="RunAddTerr"){
    writeRaster(d, "./IntermediateOutput/CalculateDistShore/DistancetoLandMar.tif", overwrite=T)
  }
  
  dirland <- direct*landRep[[1]]
  #save output
  
  if(x1=="RunAddMarine"){
    writeRaster(direct, "./IntermediateOutput/CalculateDistShore/DirectiontoSeaTerr.tif", overwrite=T)
  }
  if(x1=="RunAddTerr"){
    writeRaster(d, "./IntermediateOutput/CalculateDistShore/DirectiontoLandMar.tif", overwrite=T)
  }
  
  print("COMPLETE: set up indices to compare land and sea")
  
  
  
  ############################################################
  ### Sort out the result, make sure it's in the correct lat/long extent etc. ###
  ############################################################
  
  #half way marker
  
  #dland<-raster("./ProcessedData/CalculateDistShore/DistancetoSeaTerr.tif")
  #dirland<-raster("./ProcessedData/CalculateDistShore/DirectiontoSeaTerr.tif")
  
  
  # NA raster
  rna <- is.na(pr2$mar1) # returns NA raster
  
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
  NAtest <- raster::extract(pr2$mar1, cotest, method='simple') 
  sum(is.na(NAtest))
  
  IDvals <- rna # initiate new raster
  IDvals[] <- NAVals # store values in raster
  
  
  stackedDist<-stack(dland,dirland,IDvals,pr2$ID)
  names(stackedDist)<-c("distToSea", "direToSea", "nearestSeaCell", "ID")
  #set values to NA if outside of bounds
  #stackedDist[is.na(stackedDist$ID)]<-NA
  
  
  #part 2 if a terrestrial cell is shared with a marine cell, set distance and direction to 0
  # set nearestSeaCell to self
  
  stackedDist$distToSea[!is.na(landRepOR$terr1)&!is.na(pr2$mar1)]<-0
  stackedDist$direToSea[!is.na(landRepOR$terr1)&!is.na(pr2$mar1)]<--1
  
  stackedDist$nearestSeaCell[!is.na(landRepOR$terr1)&!is.na(pr2$mar1)]<-stackedDist$ID[!is.na(landRepOR$terr1)&!is.na(pr2$mar1)]
  
  
  #reproject to original proj
  stackedDistO<-projectRaster(stackedDist, to=baseL, method="ngb")
  
  
  baseL$distToSea<-stackedDistO$distToSea
  baseL$direToSea<-stackedDistO$direToSea
  baseL$nearestSeaCell<-stackedDistO$nearestSeaCell
  #baseL$newID<-stackedDistO$ID
  
  
  ############################################################
  ### Check the result, save output. ###
  ############################################################
  
  ###occasionally because of warping we have cells with no valid values, fix with self
  cellref<-as.data.frame((!is.na(baseL$terr1)&!is.na(searchL$mar1) & is.na(baseL$distToSea)))
  cellref<-which(cellref==T)
  
  baseL$distToSea[cellref]<-0
  baseL$direToSea[cellref]<--1
  
  baseL$nearestSeaCell[cellref]<-baseL$ID[cellref]
  
  
  ###for the few remaining cells they have lost their value due to map warping, manually find nearest sea cell
  missT<-baseL
  missT$prob<-NA
  missT$prob[!is.na(missT$terr1)&is.na(missT$distToSea)]<-1
  #if missT is blank then we good
  missDF<-as.data.frame(missT$prob,na.rm=T)
  
  missingCO<-as.data.frame(xyFromCell(baseL,as.numeric(row.names(missDF))))
  missingCO$ID<-as.numeric(row.names(missDF))
  
  mopdf<-nearestSea(missingCO[,1:2], searchL, max_distance=100000)
  
  #plot and check for errors
  #plot(missT$terr1)
  #points(missingCO[,1], missingCO[,2], col="blue")
  #arrows(missingCO[,1], missingCO[,2], mopdf[,1], mopdf[,2])
  
  baseL$distToSea[as.numeric(row.names(missDF))]<-mopdf[,4]
  baseL$nearestSeaCell[as.numeric(row.names(missDF))]<-mopdf[,6]
  
  
  
  ###final checks
  test<-baseL
  test$prob<-NA
  test$prob[!is.na(test$terr1)&is.na(test$distToSea)]<-1
  #if test$prob is blank then we good
  testdf<-as.data.frame(test$prob,na.rm=T)
  
  testCO<-as.data.frame(xyFromCell(test,as.numeric(row.names(testdf))))
  testCO$ID<-as.numeric(row.names(testdf))
  

  png(filename=paste0("./IntermediateOutput/CalculateDistShore/ErrorPlot", x1,".png"), width=480, height=640)
  plot(test$terr1, main="Location of errors (blue points were not combined successfully)")
  points(testCO[,1], testCO[,2], col="blue")
  #there's a few points around the edges of the map due to reprojections but these are not used so can be ignored
  dev.off()
  
  #if test is clear then just save it
  baseL$distToSea[is.na(baseL$terr1)]<-NA
  baseL$direToSea[is.na(baseL$terr1)]<-NA
  baseL$nearestSeaCell[is.na(baseL$terr1)]<-NA
  
  
  ###write out the result
  if(x1=="RunAddMarine"){
    writeRaster(baseL, "./IntermediateOutput/CalculateDistShore/NearestSea_Final.tif",overwrite=T)
  }
  if(x1=="RunAddTerr"){
    writeRaster(baseL, "./IntermediateOutput/CalculateDistShore/NearestLand_Final.tif",overwrite=T)
  }
    
    
  print("Correction complete, final file has been written")
  
  
  
  #can plot the coastline marine areas that are the basis for drawing marine data
  # AllPoints<-as.data.frame(baseL$nearestSeaCell, na.rm=T)
  # markeys<-unique(AllPoints$nearestSeaCell)
  # indlevel<-(searchL$ID %in% markeys)
  # 
  # coastPoints<-as.data.frame(indlevel, na.rm=T)
  # coastInd<-which(coastPoints$layer==1)
  # 
  # testr<-baseL
  # testr$Valid<-NA
  # testr[coastInd]<-2
  # 
  # plot(testr$Valid,col="blue")
  # plot(searchL$mar1,add=T)
  #sum(!is.na(searchL$SST_WS)[coastInd])


}














