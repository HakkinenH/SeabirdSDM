
#####################################################################
### DEPRECATED, NOT CURRENTLY USED###
####################################################################


rm(list=ls())

library(sp)
library(spdep)
library(raster)
library(rgeos)

start.t<-Sys.time()

coast<-shapefile("C:/Users/Henry/Documents/Research/ZSL/RawData/EEAcoastlines/EEA_Coastline_20170228.shp")

coast$ID<-1:length(coast)

spolys<-coast


# Centroid coordinates (not used but provided for example) 
coords <- coordinates(spolys)


# Create K Nearest Neighbor list
skNN.nb <- knn2nb(knearneigh(coordinates(spolys), longlat=F), 
                  row.names=spolys@data$ID)

# Calculate maximum distance for all linkages 
#maxDist <- max(unlist(nbdists(skNN.nb, coordinates(spolys), longlat=F)))
#if using generated distance then use maxDist^2


# Create spdep distance object
sDist <- dnearneigh(coordinates(spolys), 0, 1000, row.names=spolys@data$ID)

write.csv(faillist,"faillist.csv")
#summary(sDist, coordinates(spolys), longlat=F)

# Plot neighbor linkages                  
#plot(spolys, border="grey") 
#plot(sDist, coordinates(spolys), add=TRUE)  

# Create neighbor distance list 
dist.list <- nbdists(sDist, coordinates(spolys), longlat=F)



#random plots to check sense
plot_list<-sample(1:length(dist.list), 25, replace=F)
faillist<-c()

res<-c()

for (i in 1:length(dist.list)){
  
  dist_n<-dist.list[i]
  coast_curr<-spolys[-i,]
  
  #take the area using the index from sDist (all shapes within range) and see which are larger than focal island
  largeset<-spolys$Area[sDist[[i]]]>spolys$Area[i]
  #dist.list[[i]]<-dist.list[[i]][largeset]
  
  
  if(is.null(dist.list[[i]])){
    faillist<-c(faillist,i)
    res<-c(res,NA)
  }else{
       
    t1<-as.matrix(dist.list[[i]])
    
    t2<-cbind(t1,largeset,sDist[[i]])
    
    if (sum(t2[,2])>0){
      
      t3<-t2[t2[,2]==1,]
      
      if(is.null(nrow(t3))){
        mind1<-t3[1]
        minshape<-t3[3]
      }else{
        mind1<-min(t3[,1])
        minshape<-t3[which.min(t3[,1]),3]
      }
      
  
      if (i %in% plot_list){
        print(i)
        plot(spolys)
        plot(spolys[i,],col="blue",add=T)
        plot(coast_curr[minshape,],col="red",add=T)
        
      }
  
      res<-c(res,mind1)
    }else{res<-c(res,NA)}
  }
}
length(faillist)
spolys$Iso<-res


end.t<-Sys.time()
end.t - start.t

#1000 shapes: 1.862493 mins
#5000 shapes 16.43128 mins
#all shapes (70972) with 1000 buffer maxdist: 7.4 hours

#do some checks. Any mainland should be NA
#any island that is NA has no islands near it (we set maximum to 1000 km). Therefore set islands to the maximum (1,000,000m)


spolys$logIso<-log10(spolys$Iso)

setwd("C:/Users/Henry/Documents/Research/ZSL/RawData/EEAcoastlines/")
shapefile(spolys, filename="IsolationShape.shp",overwrite=T)


#test crop region
crop.ex<-extent(c(5000000,6000000,1200000,2000000))
spolys.crop<-crop(spolys,crop.ex)


spplot(spolys.crop, "logIso", main = "Island isolation")

failed<-spolys[is.na(spolys$Iso),]
spplot(failed, "ID")


spolys.crop<-spTransform(spolys.crop, CRS="+proj=longlat +datum=WGS84 +no_defs")


testerr<-raster("C:/Users/Henry/Documents/Research/ZSL/RawData/WorldClim/WorldClim_warmestMonthAvgTemp_5min.tif")

terrcr<-crop(testerr,extent(spolys.crop))
plot(terrcr,add=T,col="blue")

rlook<-as.data.frame(coordinates(testerr))
rlook$temp<-getValues(testerr)
rlook$index<-1:nrow(rlook)
rlook<-rlook[!is.na(rlook$temp),]

qpoin<-SpatialPoints(rlook[,1:2])
proj4string(qpoin)<-proj4string(spolys.crop)


lookupr<-over(qpoin,spolys.crop)[,c("Shape_Area","mainland","Iso")]
terrr<-cbind(rlook,lookupr)
head(terrr)

testerr$isolation<-NA
testerr$isolation[terrr$index]<-terrr[,"Iso"]



##end of the stuff that works
## the problem is that there are several raster cells that we don't have values for as they are not technically on an island
## ideally we need a buffer system, or to find the nearest island for an NA point


terrr2<-terrr[which(is.na(terrr$Iso)),]

qpoin2<-SpatialPoints(terrr2[,1:2])
proj4string(qpoin2)<-proj4string(spolys.crop)

disNA<-gDistance(qpoin2, spolys.crop,byid=TRUE)

plot(spolys.crop)
plot(qpoin2,add=T,col="red")
#find nearest island that already has a cell


head(terrr2)


plot(spolys.crop)

points(terrr$x,terrr$y,col="red")
points(terrr2$x,terrr2$y,col="green")

ext <- extent(20.33721, 28.69902, 34.8015, 38.72015)
rr <- raster(ext, res=0.0833333333333)
rr_cov <- rasterize(spolys.crop, rr,getCover=T)

rr_cov[rr_cov$layer==0]<-NA

rr2 <- rasterize(spolys.crop, rr, field="Iso")





plot(spolys.crop)
plot(rr_cov,add=T,col="red")
plot(rr2,add=T)








ext <- extent(-100, 45, 30.75, 90)
rr <- raster(ext, res=0.0833333333333)
proj4string(rr) <- proj4string(spolys)
values(rr)<-1

library(rgeos)
r <- rasterToPolygons(rr)
gi <- gIntersection(r, spolys.crop, byid = T)

plot(gi)

ind <- as.numeric(do.call(rbind, strsplit(names(gi), " "))[,1])   # getting intersected rr's id
r[] <- NA
r[ind] <- sapply(gi@polygons, function(x) slot(x, 'area'))  # a bit faster than gArea(gi, byid = T)

r[!is.na(r)]<-1


setwd("C:/Users/Henry/Documents/Research/ZSL/Repos/SeabirdCCVA/IntermediateOutput/GannetSDM")
writeRaster(r, "Farctica_BreedingRaster.tif")




# Minimum distance 
#dist.min <- lapply(dist.list, FUN=min)

# Distance coefficient of variation    
#dist.cv <- lapply(dist.list, FUN=function(x) { sd(x) / mean(x) } )



