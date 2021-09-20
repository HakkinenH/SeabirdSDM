

library(raster)
#set directory to directory where RawData is stored
setwd("C:/Users/Henry/Documents/Research/ZSL/RawData/")
###############################
### PREY RANGES 1  ###
##############################


###OPTION 2: GET estimated range from AquaMaps

aqoccs<-read.csv("./OBIS/Ammodytes_marinus_data/bcec0ea7-a510-4d7f-8c2c-3668d459fa9d.csv")
aqest_cl<-aqoccs[,c("decimalLatitude","decimalLongitude", "scientificName")]
aqest_cl[,3]<-1
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
aq_r<-rasterFromXYZ(aqest_cl, res=0.0833333,digits=0.001)
aq_r

#disaggregate to same resolution
#aq_r2<-disaggregate(aq_r,fact=6)

#extend to same extent
#aq_r<-extend(aq_r2, extent(MarinePredictors))
#aq_r<-crop(aq_r, extent(MarinePredictors))

#add to raster stack

Amarinus<-aq_r
Amarinus


###############################
### CROP AND SAVE ###
##############################
#crop to N Atlantic to save space
ne.atlantic.ext <- extent(-100, 55, 30, 90)
ospar<-shapefile("C:/Users/Henry/Documents/Research/ZSL/ProcessedData/OSPAR_region/ospar_LandSea.shp")
os.ext<-extent(ospar)

Amarinus_crop<-crop(Amarinus,os.ext)

if (output){
  #writeRaster(Amarinus_crop, paste0(outpath,"/MarineEnvVariables_5m.tif"), overwrite=T)
}

print("marine variables done!")