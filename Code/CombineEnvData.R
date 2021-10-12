
############################################################
############################################################
### PREPARING ENVIRONMENTAL VARIABLES FOR SEABIRD SDMS ###
############################################################
############################################################

### META ###
# by HHakkinen
# Start Date: 25/01/2021
# End Date: 08/10/2021
# As part of Institute of Zoology ZSL / University of Cambridge project
# "Terrestrial or marine species distribution model - Why not both? A case study with seabirds"
# 
# The following takes the raw marine and terrestrial raster stacks and combines them in various ways
# 
# and prepares them to be used in an SDM model
# Currently the data is at 5min resolution, but there are some notes on how to prepare 30s data
#
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

#these are now all set in the wrapper
#low-res map for plotting purposes
newmap <- getMap(resolution = "low")  # different resolutions available




#############################################################
### JOIN MARINE AND TERRESTRIAL LAYERS ###
#############################################################

if (RunAddMarine | RunAddTerr){
  #will only work if previous 2 sections have been run at some point to create output rasters

  ### LOAD FILES ###
  
  #if we've already run the download/collation process then start here and load rasters
  MarinePredictors<-stack(MarineEnvName)
  names(MarinePredictors)<-AllMarLayerNames

  TerrPredictors<-stack(TerrEnvName)
  names(TerrPredictors)<-AllTerrLayerNames
  
  
  
  MarinePredictors<-MarinePredictors[[MarLayerNames]]
  TerrPredictors<-TerrPredictors[[TerrLayerNames]]
  
  
  print("Finished loading Env layers")
  
  ospar<-shapefile(studyOutline)
  
  print("Finished loading study layer")
  

  
  
  ### CREATE INDEX ###
  
  TerrCrop<-TerrPredictors
  MarCrop <-MarinePredictors
  
  
  #set index to keep track after conversions
  TerrCrop$origIndex<-1:length(TerrCrop[[1]])
  MarCrop$origIndex<-1:length(MarCrop[[1]])
  
  
  #for each terr cell we want to know what the surrounding marine conditions are like.
  #feed in the raster layer and the maximum distance we want to consider (which for now is mean foraging distance)
  TerrCrop<-crop(TerrPredictors, ospar)
  MarCrop<-crop(MarinePredictors, ospar)
  
  #can cut down a lot more to make testing faster
  ospar<-crop(ospar, extent(-7,5,50,58))
  MarCrop<-crop(MarCrop, ospar)
  TerrCrop<-crop(TerrCrop, ospar)
  #set index to keep track after conversions
  TerrCrop$origIndex<-1:length(TerrCrop[[1]])
  MarCrop$origIndex<-1:length(MarCrop[[1]])
  
  
  #load results from CalculateDistShore and bolt on
  distRes<-stack("./IntermediateOutput/CalculateDistShore/NearestSea_Final.tif")
  names(distRes)<-c("terr1","ID", "distToSea", "direToSea", "nearestSeaCell")
  
  TerrCrop$ID<-distRes$ID
  TerrCrop$distToSea<-distRes$distToSea
  TerrCrop$direToSea<-distRes$direToSea
  TerrCrop$nearestSeaCell<-distRes$nearestSeaCell
  
  
  #load results from CalculateDistShore and bolt on
  distRes2<-stack("./IntermediateOutput/CalculateDistShore/NearestLand_Final.tif")
  names(distRes2)<-c("mar1","ID", "distToLand", "direToLand", "nearestLandCell")
  
  MarCrop$ID<-distRes2$ID
  MarCrop$distToLand<-distRes2$distToLand
  MarCrop$direToLand<-distRes2$direToLand
  MarCrop$nearestLandCell<-distRes2$nearestLandCell
  
  
  
}

if(RunAddMarine){
  
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
  
  #have to split this up if it's a large study area because the computer freaks out if I don't
  if(nrow(tpoint)>10000){
    startvals<-seq(1,nrow(tpoint), by=10000)
    endvals<-c(seq(10000,nrow(tpoint), by=10000),nrow(tpoint))
  }else{startvals<-1; endvals<-nrow(tpoint)}

  
  #loop species and subsection, note this takes a long time for large study areas! Can be several hours
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
      write.csv(medres[[1]], paste0("./IntermediateOutput/CombineEnvData/MarineLookup/MarineNearMean_",sp_now,"_",q,".csv"))
      write.csv(medres[[2]], paste0("./IntermediateOutput/CombineEnvData/MarineLookup/MarineNearSD_",sp_now,"_",q,".csv"))
      removeTmpFiles(h=0.5)
    }
    
    end.t<-Sys.time()
    print(paste("Adding marine to terr values for",sp_now, ": "))
    print(end.t - start.t)
  }
  
  #after that's finished, stack up the results
  for (i in (1:length(sp_names))){
    sp_now<-sp_names[i]
    print(sp_now)
    
    TerrCropTmp<-TerrCrop
    
    #read and stack intermediate values

    marineMeanVals<-read.csv(paste0("./IntermediateOutput/CombineEnvData/MarineLookup/MarineNearMean_",sp_now,"_1.csv"))
    marineSDVals<-read.csv(paste0("./IntermediateOutput/CombineEnvData/MarineLookup/MarineNearSD_",sp_now,"_1.csv"))
    
    #stacking only needed if we split values up
    if(length(startvals)>1){
      
      for (q in 2:length(startvals)){
        marinemSub<-read.csv(paste0("./IntermediateOutput/CombineEnvData/MarineLookup/MarineNearMean_",sp_now,"_",q,".csv"))
        marineMeanVals<-rbind(marineMeanVals,marinemSub)
        
        marinesdSub<-read.csv(paste0("./IntermediateOutput/CombineEnvData/MarineLookup/MarineNearSD_",sp_now,"_",q,".csv"))
        marineSDVals<-rbind(marineSDVals,marinesdSub)
        
      }
    }
  
    meannames<-c("X", "cell",paste0("MEAN_",MarLayerNames), "origIndex")
    sdnames<-c("X", "cell",paste0("SD_",MarLayerNames), "origIndex")
    
    names(marineMeanVals)<-meannames
    names(marineSDVals)<-sdnames
    
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
    for(n in 1:length(MarLayerNames)){
      TerrCropTmp$newMeanLayer<-NA
      TerrCropTmp$newMeanLayer<-nearSeaMean[,meannames[n+2]]
      names(TerrCropTmp)[length(names(TerrCropTmp))]<-meannames[n+2]
      
      TerrCropTmp$newSDLayer<-NA
      TerrCropTmp$newSDLayer<-nearSeaSD[,sdnames[n+2]]
      names(TerrCropTmp)[length(names(TerrCropTmp))]<-sdnames[n+2]
      
    }

    
    #check what terrestrial points do not have valid marine points
    #test1<-!is.na(TerrCropTmp[[1]]) & is.na(TerrCropTmp[[meannames[3]]])
    #this should be as blank as possible, due due to map warping there will be some errors
    #plot(test1)

    
    #Final checks and cleaning
    #remove any land that is not in OSPAR
    envcheck<-SpatialPoints(coordinates(TerrCropTmp))
    crs(envcheck)<-crs(ospar)
    
    ospres<-over(envcheck, ospar)
    unique(ospres$CNTR_NAME)
    outrows<-which(rowSums(is.na(ospres)) == ncol(ospres))
    TerrCropTmp[outrows]<-NA
    
    
    #cut out greenland
    t1<-TerrCropTmp
    t1[(coordinates(t1)[,1]<=-10 & coordinates(t1)[,2]>67) |
       (coordinates(t1)[,1]<=-32 & coordinates(t1)[,2]>59)]<-NA
    #plot(t1)
     
    if(output){
      writeRaster(t1, paste0("./IntermediateOutput/CombineEnvData/TerrwithMarineVar_5min_",sp_now,".tif"),overwrite=T)
    }
  

  }
  
  print ("done adding marine values to terr points!")
  
  
}

  

###ASSIGN TERRESTRIAL VALUES TO MARINE POINTS ###

#this is a simpler version of what is done above
#we take the index of the nearest terrestrial point to each marine point
#and add the values of that terr cell to the marine cell

if (RunAddTerr){
  MarCropTmp<-MarCrop
  
  #get cell indexes for each cell
  indx<-as.data.frame((MarCropTmp[["nearestLandCell"]]))
  indx$index<-1:nrow(indx)
  indx<-indx[!is.na(indx$nearestLandCell),]
  

  #look up in the terrestrial layer
  terrdf<-as.data.frame(TerrCrop)
  nearLanddf<-merge(indx, terrdf, by.x="nearestLandCell", by.y="origIndex", all.x=T, sort=F)
  nearLanddf<-nearLanddf[order(nearLanddf$index),]
  
  head(nearLanddf)
  
  #now assign these to the original raster
  for(n in 1:length(TerrLayerNames)){
    MarCropTmp$newLayer<-NA

    MarCropTmp$newLayer[indx$index]<-nearLanddf[,TerrLayerNames[n]]
    
    names(MarCropTmp)[length(names(MarCropTmp))]<-paste0("NLand_",TerrLayerNames[n])
  }
  
  

  
  ### MANUAL FIXING AREA
  #Sometimes the nearest land is outside of the data available
  #we recommend starting with a broad spatial area and then cropping down
  #however if that isn't possible (e.g. in our case we don't include Greenland but don't have data for some variables)
  #then manually set any variables here, not recommended unless necessary
  
  #specify which points to set manually
  #mcheck<-(coordinates(MarCropTmp)[,2]>49) * is.na(MarCropTmp$NLand_Isol) * !is.na(MarCropTmp$NLand_MeanTemp_WM)
  #mdf<-as.data.frame(mcheck)
  #mind<-which(mdf$layer==1)
  
  
  #plot are you are manually adjusting
  #plot(mcheck)
  #lookup<-MarCrop$nearestLandCell[mind]
  #TerrCrop$check<-NA
  #TerrCrop$check[which(getValues(TerrCrop$origIndex) %in% lookup)]<-1
  #plot(TerrCrop$check,col="Red")
  
  #specify values for you wish to set, adjust as necessary
  #adjust anything near greenland to refer to greenland
  #isolation is 0 as it's a large land mass
  #MarCropTmp$NLand_Isol[mind]<-0
  #  #area estimate from Wiki: 2,166,086 km2. area in m2
  #MarCropTmp$NLand_Area[mind]<-2166086000000
  # adjust land cover value
  #MarCropTmp$NLand_LandCover[mind]<- 11

  #double check if any points have escaped, remove any rogue pixels
  #Note that in essence we are saying we cannot have a full data set for marine points that are nearest to land masses
  #outside of OSPAR region
  #set any remaining points in med to NA to remove from analysis
  #MarCrop[which((coordinates(MarCrop)[,2]<48 & coordinates(MarCrop)[,1]>2) |
                    #(coordinates(MarCrop)[,2]<42 & coordinates(MarCrop)[,1]>-4))]<-NA

  
  
  #save result
  if(output){
    writeRaster(MarCropTmp, paste0("./IntermediateOutput/CombineEnvData/MarinewithTerrVar_5min.tif"),overwrite=T)
  }
  print("Done with Marine (adding Terr)")
}
  



###optionally plot the variables###
if (plotVar){
  
  
  #set colour palette
  my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127"))
  

  #plot marine first as that is uniform across all species
  
  ### MARINE PLOTS
  
  if (RunAddTerr){
    
    #if we've already run the download/collation process then start here and load rasters
    MarCrop<-stack("./IntermediateOutput/CombineEnvData/MarinewithTerrVar_5min.tif")
    names(MarCrop)<-c(MarLayerNames, 
                      "origIndex", "ID", "distToLand", "direToLand", "nearestLandCell", 
                      paste0("NLand_", TerrLayerNames))
    
    
    for (q in 1: length(MarLayerNames)){
      
      png(filename=paste0("./IntermediateOutput/CombineEnvData/EnvPlots/Marine_", MarLayerNames[q], ".png"), width=8, height=8, units="in", res=300)
        plot(MarCrop[[MarLayerNames[q]]],col=my.colors(1000),axes=FALSE, box=FALSE)
        title(cex.sub = 1.25, main = MarLayerNames[q])
      dev.off()
      
    }
    
    for (q in 1: length(TerrLayerNames)){
      
      png(filename=paste0("./IntermediateOutput/CombineEnvData/EnvPlots/MarinewTerr_", TerrLayerNames[q], ".png"), width=8, height=8, units="in", res=300)
      plot(MarCrop[[ paste0("NLand_", TerrLayerNames[q]) ]],col=my.colors(1000),axes=FALSE, box=FALSE)
      title(cex.sub = 1.25, main = paste0("Nearest Land: ",TerrLayerNames[q]))
      dev.off()
      
    }
    
  }
  

  
  #now loop through terrestrial points
  if (RunTerr){
    
    varList<-c()
    for(lim in MarLayerNames){
      varList<-c(varList, paste0("MEAN_", lim), paste0("SD_", lim))
    }
    
    #if we've already run the download/collation process then start here and load rasters
    TerrCrop<-stack(paste0("./IntermediateOutput/CombineEnvData/TerrwithMarineVar_5min_",sp_names[1],".tif"))
    names(TerrCrop)<-c(TerrLayerNames,
                      "origIndex", "ID", "distToSea", "direToSea", "nearestSeaCell", 
                      varList)

    
    
    for (q in 1: length(TerrLayerNames)){
      print(TerrLayerNames[q])
      png(filename=paste0("./IntermediateOutput/CombineEnvData/EnvPlots/Terr_", TerrLayerNames[q], ".png"), width=8, height=8, units="in", res=300)
      plot(TerrCrop[[TerrLayerNames[q]]],col=my.colors(1000),axes=FALSE, box=FALSE)
      title(cex.sub = 1.25, main = TerrLayerNames[q])
      dev.off()
      
    }
    
    #for the marine values, we have to do this species by species since they were all sampled in different ways
    for (x in 1:length(sp_names)){
      
      i<-sp_names[x]
      
      #if we've already run the download/collation process then start here and load rasters
      TerrCrop<-stack(paste0("./IntermediateOutput/CombineEnvData/TerrwithMarineVar_5min_",i,".tif"))
      names(TerrCrop)<-c(TerrLayerNames,
                         "origIndex", "ID", "distToSea", "direToSea", "nearestSeaCell", 
                         varList)
      
      
      for (q in 1: length(MarLayerNames)){
        
        png(filename=paste0("./IntermediateOutput/CombineEnvData/EnvPlots/TerrwMar_",i,"_", TerrLayerNames[q], ".png"), width=8, height=8, units="in", res=300)
        par(mfrow=c(1,2))
        
        plot(TerrCrop[[ paste0("MEAN_", MarLayerNames[q]) ]],col=my.colors(1000),axes=FALSE, box=FALSE)
        title(cex.sub = 1.25, main = paste0(i," mean values: ",MarLayerNames[q]))
        
        plot(TerrCrop[[ paste0("SD_", MarLayerNames[q]) ]],col=my.colors(1000),axes=FALSE, box=FALSE)
        title(cex.sub = 1.25, main = paste0(i," sd values: ",MarLayerNames[q]))
        
        dev.off()
        
      }
      
    }
    
    
  
  }
    
  print ("done plotting!")
    
    
}else{ print ("RunMar and RunTerr are set to FALSE. No processing was carried out.")}
  
  








