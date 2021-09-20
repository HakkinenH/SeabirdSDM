
#take MODIS files (at 1km), stack them, find mean NDVI (Mar-Sept 2016-2020)
#reproject and rescale

#clear env
rm(list=ls())


library(MODISTools)
library(sf)
library(rworldmap)
library(raster)

setwd("C:/Users/Henry/Documents/Research/ZSL/RawData/MODIS_NDVI")

#set output path
outpath<-"C:/Users/Henry/Documents/Research/ZSL/ProcessedData/MODIS_NDVI/IndTiles/"



#low-res map for plotting purposes
newmap <- getMap(resolution = "low")  # different resolutions available


## PART 1 ## 

hdffiles<-list.files(pattern="*.hdf")
for (q in 1:length(hdffiles)){
  print(q)
  
  # Get a list of sds names
  sds <- get_subdatasets(hdffiles[q])
  
  #generate output path
  file1<-substr(hdffiles[q],1,nchar(hdffiles[q])-4)
  filename <- paste0("rasterNDVI/",file1,".tif")
  
  #choose layer we want, for us this is always layer 1 "1 km monthly NDVI"
  gdal_translate(sds[1], dst_dataset = filename)
  
}


### PART 2 ###
#to be run when the first part above is done

setwd("C:/Users/Henry/Documents/Research/ZSL/RawData/MODIS_NDVI/rasterNDVI/")

#get a list of all the new raster files
rasfiles<-list.files(path="./",pattern="*.tif")

#get a list of all the unique tiles

tilecodes<-substr(rasfiles, 29,34)
tilecodes<-unique(tilecodes)


#create dummy raster with the proper res, extent and origin
#r1<-raster(xmn=-100, xmx=50,ymn=35,ymx=90,
#           res=0.008333333, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#for each tile
for (i in 1:length(tilecodes)){
  
  #stack all these tiles, calculate the mean across these months
  print(paste(i, "out of", length(tilecodes)))
  
  #get current tile code
  curtile<-tilecodes[i]
  #find all the files with that code
  rastiles<-rasfiles[grep(curtile, rasfiles)]
  
  #stack those files
  stackTile<-stack(rastiles)
  #optionally plot to make sure it's the same place
  if (i %% 5 ==0){plot(stackTile,main=curtile)}
  
  
  #calculate mean of that tile
  meanTile<-calc(stackTile,fun=mean,na.rm=T)
  #plot(meanTile)

  #reproject from sinusoidal to lat/lon
  #make sure old is accurate
  proj4string(meanTile)<-"+proj=sinu +a=6371007.181 +b=6371007.181 +units=m"
  #set new
  sr <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0' 
  
  
  empt<-projectExtent(meanTile, crs=sr)
  res(empt)<-0.008333333
  origin(empt)<-c(0,0)
  
  meanTile_reproj <- projectRaster(from=meanTile, to=empt, method="bilinear")

  
  #convert scale to something more usable
  meanTile_reproj<-meanTile_reproj*0.0001*0.0001
  #plot(meanTile_reproj)
  #plot(newmap,add=T)
  
  #write out the final raster
  writeRaster(meanTile_reproj, paste0(outpath,curtile,".tif"), overwrite=T)
  
}


### PART 3 ###

#load the compiled tiles we have now

#join them all together (?merge?)

setwd("C:/Users/Henry/Documents/Research/ZSL/ProcessedData/MODIS_NDVI/IndTiles/")
tilefiles<-list.files(pattern="*.tif")

#save this mega-raster
base1<-raster(tilefiles[1])

#extend to full size
fullext<-extent(-180,180,20,90)
resRas<-extend(base1, fullext)


#I hate this method, but I couldn't get do.call merge to work for me
#it works ok for not that many files
for (n in 2:length(tilefiles)){

    print(n)
    #load latest raster
    newRas<-raster(tilefiles[n])
    
    #extend to full size
    
    #merge it onto the result raster
    resRas<-merge(resRas, newRas)
    
}
  

#check resRas
resRas
plot(resRas)

#save the result raster
writeRaster(resRas,"../NDVI_1kmcleaned.tif",overwrite=T)


  
## part 4 ##


#read in the result raster
setwd("C:/Users/Henry/Documents/Research/ZSL/ProcessedData/MODIS_NDVI/")
NDVI_full<-raster("NDVI_1kmcleaned.tif")



#load a reference raster with the correct spatial extent etc.
file_list<-list.files(path="C:/Users/Henry/Documents/Research/ZSL/RawData/WorldClim/wc2.1_5m_tavg",  pattern="*.tif")
worldmax_raw<-stack(file_list)

#replace NA with -999 cos I hate warning messages
r <- calc(worldmax_raw, function(x) { ifelse(is.na(x), -999, x) }) 

#calculate the maximum for each raster cells, hence return mean temp of the warmest month
MeanTemp_W<-calc(r, fun = max, na.rm = T)
MeanTemp_W <- calc(MeanTemp_W, function(x) { ifelse(x==-999, NA, x) }) 

#get extent from ospar
ospar<-shapefile("C:/Users/Henry/Documents/Research/ZSL/ProcessedData/OSPAR_region/ospar_LandSea.shp")
os.ext<-extent(ospar)


#os.ext<-extent(20,30,58,62)


#crop to extent
MeanTemp_crop<-crop(MeanTemp_W,os.ext)
NDVI_crop<-crop(NDVI_full,os.ext)


#aggregate from NDVI raster at our resolution (take mean and take min)

NDVI_mean<-aggregate(NDVI_crop,10,fun=mean,na.rm=T)
NDVI_min<-aggregate(NDVI_crop,10,fun=min,na.rm=T)

#resample from NDVI raster at our resolution (take mean and take min)
NDVI_meancl<-resample(NDVI_mean, MeanTemp_crop, method="bilinear")
NDVI_mincl<-resample(NDVI_min, MeanTemp_crop, method="bilinear")


#check which cells have data
plot(NDVI_meancl)

plot(MeanTemp_crop$layer)
plot(NDVI_mincl)
plot(!is.na(MeanTemp_crop$layer)&is.na(NDVI_meancl))
plot(is.na(MeanTemp_crop$layer)&!is.na(NDVI_meancl))

plot(!is.na(MeanTemp_crop$layer)&is.na(NDVI_mincl))
plot(is.na(MeanTemp_crop$layer)&!is.na(NDVI_mincl))



#save the final raster to be used later
writeRaster(NDVI_meancl,"NDVI_5minMEAN.tif",overwrite=T)
writeRaster(NDVI_mincl,"NDVI_5minMIN.tif",overwrite=T)

