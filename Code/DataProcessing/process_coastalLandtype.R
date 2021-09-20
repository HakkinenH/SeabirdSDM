
#Prep land cover data


#clear env
rm(list=ls())
library(raster)

start.t<-Sys.time()

#read in a reference terrestrial raster, 5 min
MeanTemp_W<-raster("C:/Users/Henry/Documents/Research/ZSL/Repos/SeabirdCCVA/IntermediateOutput/PrepEnvData/WorldClim_warmestMonthAvgTemp_5min.tif")

#read in OSPAR shape file for cropping
ospar<-shapefile("C:/Users/Henry/Documents/Research/ZSL/ProcessedData/OSPAR_region/ospar_LandSea.shp")
os.ext<-extent(ospar)


#read in the coastal zone shapefile
#coastz<-shapefile("C:/Users/Henry/Documents/Research/ZSL/ProcessedData/Copernicus_CoastalZones/CoastalZones_Clipped.shp")



#read in the land cover raster
#landc<-raster("C:/Users/Henry/Documents/Research/ZSL/RawData/Copernicus_LandCover/raster100m/DATA/U2018_CLC2018_V2020_20u1.tif")
landc<-raster("C:/Users/Henry/Documents/Research/ZSL/ProcessedData/Copernicus_LandCover/Copernicus_LCraster.tif")
#read in dictionary
landlook<-read.csv("C:/Users/Henry/Documents/Research/ZSL/ProcessedData/Copernicus_LandCover/CodeLookup.csv")


#crop to OSPAR size
MeanTemp_W<-crop(MeanTemp_W,os.ext)
landc<-crop(landc,os.ext)


#crop for testing
#test.ex<-extent(-7,-2,50,52)
#landc<-crop(landc, test.ex)
#MeanTemp_W<-crop(MeanTemp_W, test.ex)


#convert anything at sea or invalid to NA
landc <- reclassify(landc, data.frame(from=c(44,48,128), to=c(NA,NA,NA)))


#aggregate cells to get as close to matching resolution as possible (ideally this would be in one step but resample doesn't offer mode sampling)
landc2 <- aggregate(landc, fact = 65, fun = modal, na.rm = TRUE)

land_res<-resample(landc2,MeanTemp_W, method="ngb")

land_resback<-land_res

#assign codes
names(land_res)<-"Value"


#create some fields to add description etc.
land_res$LC.code<-NA
land_res$LC_desc<-NA


for (i in 1:nrow(landlook)){
  nowcode<-landlook[i,4]
  nowdes<-landlook[i,5]
  
  print(nowcode);print(nowdes)

  land_res$LC.code[land_res$Value==i]<-nowcode
  land_res$LC_desc[land_res$Value==i]<-nowdes
  
}

plot(land_res$LC.code)
land_res
writeRaster(land_res, "C:/Users/Henry/Documents/Research/ZSL/ProcessedData/Copernicus_LandCover/Copernicus_LCraster_cleaned.tif", overwrite=T)


#for each raster cell find out what the zone type is
#convert to correct resolution and projection


end.t<-Sys.time()

end.t-start.t



test1<-brick("C:/Users/Henry/Documents/Research/ZSL/ProcessedData/Copernicus_LandCover/Copernicus_LCraster_cleaned.tif")
test1

plot(test1$Copernicus_LCraster_cleaned.2)
unique(test1$Copernicus_LCraster_cleaned.2)
unique(land_res$Value)
