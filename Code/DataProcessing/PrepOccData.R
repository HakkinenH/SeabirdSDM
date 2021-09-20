
############################################################
############################################################
### PREPARING OCCURRENCE DATA FOR SEABIRD SDMS ###
############################################################
############################################################

### META ###
# by HHakkinen
# Start Date: 25/01/2021
# End Date:
# As part of Institute of Zoology ZSL / University of Cambridge project
# "Evidence-based Climate Change Adaptation Practices to safeguard vulnerable species: supporting conservation practitioners, donors and policy makers"
# 
# The following takes occurrence data for seabird breeding ranges
# and prepares them to be used in an SDM model
# Currently output is at 5min resolution

#the code takes the breeding range polygon from birdlife and converts it to point data

### /META ###


#clear env
rm(list=ls())

library(raster)
library(rgeos)
library(rgdal)
library(maptools)
library(rworldmap)


#low-res map for plotting purposes
newmap <- getMap(resolution = "low")  # different resolutions available


#load some functions to help out
source("C:/Users/Henry/Documents/Research/ZSL/Repos/SeabirdCCVA/Code/prepFunctions.R")


##################################################
#read in the species variables
#################################################
#read in the species polygon
setwd("C:/Users/Henry/Documents/Research/ZSL/ProcessedData/BirdLife-Ranges_IndFiles/BreedingLayers_HighRes")

#load species names
sp_names<-c("MbassanusBre","MbassanusBreRes","Farctica","Sdougallii")
#sp_names<-c("MbassanusBre")


#if needed load base layer
TerrCrop<-raster("C:/Users/Henry/Documents/Research/ZSL/Repos/SeabirdCCVA/IntermediateOutput/PrepEnvData/TerrwithMarineVar_5min_Mbassanus.tif")


#set extent of the map
ne.atlantic.ext <- extent(-100, 55, 30, 90)
ospar<-shapefile("C:/Users/Henry/Documents/Research/ZSL/ProcessedData/OSPAR_region/ospar_LandSea.shp")
os.ext<-extent(ospar)


outpath<-"C:/Users/Henry/Documents/Research/ZSL/Repos/SeabirdCCVA/IntermediateOutput/PrepOccData/"



#read in coastline
coastl<-shapefile("C:/Users/Henry/Documents/Research/ZSL/RawData/EEAcoastlines/EEA_Coastline_20170228.shp")
coastl2<-spTransform(coastl, CRS="+proj=longlat +datum=WGS84 +no_defs")

#plot(coastl2,axes=T)
#plot(poly,add=T,col="red")


#create blank raster with correct extent and resolution
rr <- raster(os.ext, res=0.0833333333333)


for (i in sp_names){
  print(i)
  
  
  #load our breeding range polygon
  #for Mbassanus we need breeding and resident range
  if (i =="MbassanusBre"){poly <- shapefile("Mbassanus_breedingLayer.shp")}
  if (i =="MbassanusBreRes"){poly <- shapefile("Mbassanus_BreedResidentLayer_cl.shp")}
  #for other species we just need breeding range
  if (i =="Farctica"){poly <- shapefile("Farctica_BreedingLayer.shp")}
  if (i =="Sdougallii"){poly <- shapefile("Sdougallii_BreedingLayer.shp")}
  
  #crop polygon to OSPAR region
  poly.crop <- crop(poly, os.ext)
  
  #sample from the polygon with the blank raster
  #sometimes produces non-fatal error, but Robert Hijmans says it's fine??
  #https://stackoverflow.com/questions/61598340/why-does-rastertopoints-generate-an-error-on-first-call-but-not-second
  rr2 <- rasterize(poly.crop, rr,getCover=T)
  #note that any colonies that are less than 100th of a cell (about 100m) will be lost
  rr2[rr2$layer>0]<-1
  #r<-rr2
  
  
  #for gannets add an extra check because we don't have a breeding range
  #we will take any marine area that is within foraging range of a terrestrial colony and say that is an occurrence
  if(i == "MbassanusBreRes"){
    envt.st<-stack(paste0("C:/Users/Henry/Documents/Research/ZSL/Repos/SeabirdCCVA/IntermediateOutput/PrepEnvData/TerrEnvVariables_5m.tif"))
    names(envt.st)<-c("MeanTemp_WM", "Prec_BreedMonths", "Isol", 
                      "Area", "LandCover", "distToSea", "nearestSeaCell",
                      "NDVI_mean", "NDVI_min")

    envt.st<-envt.st$MeanTemp_WM
    
    #get the land breeding area
    Terr.crop<-raster("C:/Users/Henry/Documents/Research/ZSL/Repos/SeabirdCCVA/IntermediateOutput/PrepOccData/MbassanusBre_BreedingRaster.tif")
    names(Terr.crop)<-"presLand"
    Terr.crop[Terr.crop$presLand==0]<-NA
    proj4string(Terr.crop)<-proj4string(envt.st)
    
    #get only the bits that also have a valid MeanTemp (otherwise it won't be used in the final SDM)
    #envt.st$pres<-resample(Terr.crop$presLand,envt.st, method="ngb")
    #Terr.crop<-!is.na(envt.st$MeanTemp_WM)&!is.na(envt.st$pres)
    #names(Terr.crop)<-"presLand"
    
    #range will act as the marine layer
    Mar.crop<-rr2
    names(Mar.crop)<-"presAll"
    Mar.crop[Mar.crop$presAll==0]<-NA
    proj4string(Mar.crop)<-proj4string(envt.st)

    #set ID to keep track of warping
    Mar.crop$ID<-1:length(Mar.crop$presAll)
    Terr.crop$ID<-1:length(Terr.crop$presLand)
    
    
    #change projection so we get accurate distance and direction
    newproj <- "+proj=lcc +lat_0=52 +lon_0=10 +lat_1=35 +lat_2=65 +x_0=4000000 +y_0=2800000 +ellps=GRS80 +units=m +no_defs +type=crs"
    seaRep <- projectRaster(Mar.crop, crs=newproj, method="ngb")
    la2<- projectRaster(Terr.crop, crs=newproj, method="ngb")
    
    
    #in some places land and sea exist in the same cell
    #take the conservative approach and if in doubt count it as a terrestrial cell
    seaRepOR<-seaRep
    seaRep[!is.na(seaRep$presAll)&!is.na(la2$presLand)]<-NA
    #seaRep now only has marine cells
    
    # Rasterize and set land pixels to NA
    start.t<-Sys.time()
    # Calculate distance to nearest non-NA pixel
    #note the following operation takes 5.7 hours
    d <- distance(la2$presLand)
    end.t<-Sys.time()
    end.t - start.t
    
    #set sea
    seaRep[!is.na(seaRep)]<-1
    # Optionally set non-land pixels so we only have results for the areas we want
    dsea <- d*seaRep[[1]]
    
    
    
    
    #reproject back into original projection and correct errors
    stackedDist<-projectRaster(dsea, to=Mar.crop, method="ngb")
    Mar.crop$distToColony<-stackedDist$layer
    
    #check we have a valid value for every cell
    #plot(!is.na(Mar.crop$presAll) & is.na(Mar.crop$distToColony) )
    
    
    ###for the few remaining cells they have lost their value due to map warping, manually find nearest sea cell
    ###and steal their value
    missT<-Mar.crop
    missT$prob<-NA
    missT$prob[!is.na(Mar.crop$presAll) & is.na(Mar.crop$distToColony)]<-1
    #if missT is blank then we good
    missDF<-as.data.frame(missT$prob,na.rm=T)
    
    missingCO<-as.data.frame(xyFromCell(Mar.crop,as.numeric(row.names(missDF))))
    missingCO$ID<-as.numeric(row.names(missDF))
    
    mopdf<-nearestSea(missingCO[,1:2], Mar.crop, max_distance=100000)

    
    #plot and check for errors
    plot(Mar.crop$presAll)
    points(missingCO[,1], missingCO[,2], col="blue")
    arrows(missingCO[,1], missingCO[,2], mopdf[,1], mopdf[,2])
    
    Mar.crop$distToColony[as.numeric(row.names(missDF))]<-mopdf[,7]

    #check we have a valid value for every cell (should be blank)
    #plot(!is.na(Mar.crop$presAll) & is.na(Mar.crop$distToColony) )
    
    
    #filter anything out that is more than foraging distance to colony
    Mar.crop$presAll[Mar.crop$distToColony>200000]<-NA
    #pass on the result raster to the end
    rr2<-Mar.crop$presAll
    
  }
  
  #get terrestrial base layer
  
  #get points at sea for gannets
  #work out how far the marine occurrence points are from a colony
  
  #crop marine distribution to only areas near colonies
  
  

  #save the intermediate raster

  writeRaster(rr2, paste0(outpath,i,"_BreedingRaster.tif"), overwrite=T)
  

  #convert raster to datapoints
  sp_pres<-as.data.frame(coordinates(rr2))
  sp_pres$presence<-values(rr2)
  
  sp_pres<-sp_pres[!is.na(sp_pres$presence),]
  
  #sp_pres<-sp_pres[sp_pres$presence==1,]
  
  

  
  # old code for creating pseudo-absences
  # sp_abs<-sp_df[which(is.na(sp_df$presence)),]
  # sp_abs$presence <- 0
  # sp_absP<-sp_abs[sample(nrow(sp_abs), nrow(sp_pres)), ]
  # 
  # 
  # sp_total<-rbind(sp_pres,sp_absP)
  
  
  
  #convert to SpatialPoints
  xy <- sp_pres[,c(1,2)]
  
  spdf <- SpatialPointsDataFrame(coords = xy, data = data.frame(sp_pres[,3]),
                                 proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
  names(spdf)<-"SP"
  
  
  
  #png(filename=paste0(outpath,i,"_OSPARbreedingOcc.png"), width=8, height=8, units="in", res=300)
    #plot(spdf,col=as.factor(spdf$SP),main=i)
    #plot(newmap,add=T)
   # plot(spdf,col="green",add=T)
  #dev.off()

  
  
  #write the spatial points out for later
  #these points are on land and at sea, but SDM algorithm sorts those out if needed
  writeOGR(obj=spdf, dsn=paste0(outpath,i,"BreedingSp.tif"), layer="SP", driver="ESRI Shapefile",overwrite_layer=T)
  
  
  
}




plot(spdf)






