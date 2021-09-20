

############################################################
############################################################
### CALCULATE ISOLATION OF ISLANDS IN EUROPE ###
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
# As a variable in our seabird SDMs we want to include the size and isolation of the area they are breeding
# Isolation in this case is defined as distance to a larger landmass
# This is a big operation so is run separately to the main PrepEnvData.R

### /META ###


############################################################
### LOAD LIBRARIES AND SET VARIABLES ###
############################################################

rm(list=ls())

setwd("C:/Users/Henry/Documents/Research/ZSL/ProcessedData")
library(ggplot2)
library(sf)

#set system clock start time
start.t<-Sys.time()

#read in the european coastline
coast<-st_read("C:/Users/Henry/Documents/Research/ZSL/RawData/EEAcoastlines/EEA_Coastline_20170228.shp")

#set a unique ID so we can keep track of the polygons even after subsetting
coast$ID<-1:nrow(coast)
#set base isolation layer to be filled in by the file
coast$Iso<-NA

#what's the maximum radius distance we are willing to search for another island (in m?)
crop.tol<-100000



############################################################
### CALCULATE ISOLATION ###
############################################################

#it's a for loop
#slow and inefficient but vector based analysis melts my computer
for (i in 1:nrow(coast)){

  #print every now and then so we know what is going on
  if(i%%50==0){print(i)}
    
  #take the current polygon
  nowsh<-coast[i,]
  #take all but the current polygon
  allsh<-coast[-i,]
  
  #if the polygon isn't a defined mainlands then:
  if (nowsh$mainland==0){
  
    sh.cen<-st_coordinates(st_centroid(nowsh))
    allex<-st_bbox(allsh)
    
    #set the total bounding box we are willing to search, based on crop.tol
    if (allex["xmin"]>sh.cen[1]-crop.tol){cr.xmin<-allex["xmin"]}else{cr.xmin<-sh.cen[1]-crop.tol}
    if (allex["xmax"]<sh.cen[1]+crop.tol){cr.xmax<-allex["xmax"]}else{cr.xmax<-sh.cen[1]+crop.tol}
    
    if (allex["ymin"]>sh.cen[2]-crop.tol){cr.ymin<-allex["ymin"]}else{cr.ymin<-sh.cen[2]-crop.tol}
    if (allex["ymax"]<sh.cen[2]+crop.tol){cr.ymax<-allex["ymax"]}else{cr.ymax<-sh.cen[2]+crop.tol}
    
    thecrop<-c(cr.xmin, cr.xmax, cr.ymin, cr.ymax)
    names(thecrop)<-c("xmin", "xmax", "ymin", "ymax")
    
    #nowsh.cr<-crop(nowsh,thecrop)
    allsh.cr<-st_crop(allsh,thecrop)
    
    #subselect to only polygon that are larger than the current polygon
    largesh.cr<-allsh.cr[allsh.cr$Area>nowsh$Area,]
    
    # if there are larger polygons in range
    if(nrow(allsh.cr)>0){
      
      # create an index of the nearest feature
      index <- st_nearest_feature(x = nowsh, y = largesh.cr)
      mainindex<-largesh.cr[index,]$ID
      
      largecen<-st_coordinates(st_centroid(largesh.cr[index,]))

      #build a plot if you want
      # ggplot() + geom_sf(data = allsh.cr, aes(fill = Shape_Area)) +
      #           geom_sf(data = largesh.cr, aes(fill = ID)) +
      #           geom_segment(aes(x = sh.cen[1], y = sh.cen[2], xend = largecen[1], yend = largecen[2]),
      #                        colour = "#EC7014", size=1,
      #                         arrow = arrow(length = unit(0.2, "cm")))
      # 
      # 
      
      
      # calculate distance between polygons
      poly_dist <- st_distance(x = nowsh, y= largesh.cr[index,], by_element = TRUE)
      
      # add the distance calculations to the fire polygons
      coast$Iso[i]<-poly_dist
      
      if(i%%50==0){print(poly_dist)}
      }else{
        #if allsh.cr has 0 rows it means there are no larger landmasses within the area of tolerance (1,000,000km)
        #set iso distance to max
        #print("no islands")
        if(i%%50==0){print("no islands")}
        coast$Iso[i]<-crop.tol
      }
  }
}


end.t<-Sys.time()
end.t - start.t

#why are there zeros
#checked this, because some are so close their geometries touch in our file

#why are some iso metrics over the tol?
#because of where their center lies

#with 100 features and crop.tol of 100,000 (100km): 5min
#with 100 features and crop.tol of 1,000,000 (1,000km): 8min


############################################################
### CLEAN-UP ###
############################################################

#Some values need to be manually set

#only 19 islands are more than 100km from a larger land mass
#set to maximum
coast$Iso[which(is.na(coast$Iso) & coast$mainland==0)]<-100000


#Mainland land masses (5)
#mainlands can be set to 0
coast$Iso[which(is.na(coast$Iso) & coast$mainland==1)]<-0


#write the final result
setwd("C:/Users/Henry/Documents/Research/ZSL/ProcessedData/EEAcoastlines/")
st_write(coast, dsn="IsolationShape.shp",overwrite=T)


testcrop<-st_crop(coast, xmin=3200000, xmax=3900000, ymin=4000000, ymax=4600000)
plot(coast["Iso"], axes=T)



############################################################
### CONVERT INTO RASTER ###
############################################################


library(raster)
testerr<-raster("C:/Users/Henry/Documents/Research/ZSL/RawData/WorldClim/WorldClim_warmestMonthAvgTemp_5min.tif")
spolys.crop<-st_read("./EEAcoastlines/IsolationShape.shp")

#buffer in case some points are slightly offshore
#spolys.buff6<-st_buffer(spolys.crop, dist=1000)
spolys.buff25<-st_buffer(spolys.crop, dist=10000)


spolys.crop<-st_transform(spolys.crop, crs="+proj=longlat +datum=WGS84 +no_defs")
#spolys.buff6<-st_transform(spolys.buff6, crs="+proj=longlat +datum=WGS84 +no_defs")
spolys.buff25<-st_transform(spolys.buff25, crs="+proj=longlat +datum=WGS84 +no_defs")



#spolys.crop2<-spolys.crop["Iso"]

#crop base raster layer to size of europe
terrcr<-crop(testerr,extent(spolys.crop))

#get df with coordinates of all land points
rlook<-as.data.frame(coordinates(terrcr))
rlook$temp<-getValues(terrcr)
rlook$index<-1:nrow(rlook)
rlook<-rlook[!is.na(rlook$temp),]

#convert to spatial points
qpoin<-st_as_sf(rlook[,1:3], coords = c("x", "y"))
st_crs(qpoin)<-"+proj=longlat +datum=WGS84 +no_defs"


#find where the newly made raster intersects with the island polygons
t1<-st_join(qpoin, spolys.crop, join = st_intersects)


#convert to df
t1_df<-as.data.frame(t1)
t2_df<-cbind(t1_df, rlook)

#now add isolation information to raster
terrcr$isolation<-NA
terrcr$isolation[t2_df$index]<-t2_df[,"Iso"]
writeRaster(terrcr$isolation, "IsolationRasterPre.tif", overwrite=T)

#now add area information to raster
terrcr$Shape_Area<-NA
terrcr$Shape_Area[t2_df$index]<-t2_df[,"Shape_Area"]
writeRaster(terrcr$Shape_Area, "ShapeAreaPre.tif", overwrite=T)




terrIll<-terrcr
terrIll$WorldClim_warmestMonthAvgTemp_5min[!is.na(terrIll$WorldClim_warmestMonthAvgTemp_5min)&!is.na(terrIll$isolation)]<-NA
plot(terrIll$WorldClim_warmestMonthAvgTemp_5min)


#PART 2:
#some points lie outside the coast polygon, we apply a buffer to find the values
#use all the blanks points and look up what the isolation values are in the buffered
isop<-st_join(qpoin, spolys.buff25, join = st_intersects)
isop_df<-as.data.frame(isop)
#create a df with coordinates values
isop_df<-cbind(isop_df,st_coordinates(isop))

#create a df with coordinates values and isolation values
landp_lookup<-isop_df[!is.na(isop_df$ID_1),]



#none contains cells that have a terr temp value, but not an isolation value, they need processing
none<-t2_df[is.na(t2_df$ID_1),]

dim(landp_lookup)
dim(none)


#for all in none see if they now have a value in t3_new (which we put a buffer on)
noans=0
oneans=0
mans=0


startt<-Sys.time()
for(i in 1:nrow(none)){

  #print occasionally so we know where the process is
  if(i%%1000 == 0){print(i)}
  rown<-none[i,]
  
  ansrow<-landp_lookup[which(landp_lookup$X == rown$x & landp_lookup$Y == rown$y),]
  
  #for the following we don't add area, I'm worried about adding incorrect tiny islands or merging landmasses
  #can't think of a good solution for this, if isolation is within 10km
  if(nrow(ansrow)==0){
    noans=noans+1
  }else if(nrow(ansrow)==1){
    none[i,"Shape_Leng"]<-ansrow[1,"Shape_Leng"]
    none[i,"Shape_Area"]<-ansrow[1,"Shape_Area"]
    none[i,"mainland"]<-ansrow[1,"mainland"]
    none[i,"ID_1"]<-ansrow[1,"ID_1"]
    none[i,"Iso"]<-ansrow[1,"Iso"]
    
  }else{
    #print(ansrow)
    #if multiple answers take the bigger island
    ansrow_2<-which.max(ansrow$Shape_Area)
    none[i,"Shape_Leng"]<-ansrow[ansrow_2,"Shape_Leng"]
    none[i,"Shape_Area"]<-ansrow[ansrow_2,"Shape_Area"]
    none[i,"mainland"]<-ansrow[ansrow_2,"mainland"]
    none[i,"ID_1"]<-ansrow[ansrow_2,"ID_1"]
    none[i,"Iso"]<-ansrow[ansrow_2,"Iso"]
  }
  
}
endt<-Sys.time()
endt-startt

newans<-none[!is.na(none$Iso),]

#with a buffer of 100, new rows=4576
#with a buffer 10,000 14247 new rows

plot(terrcr$WorldClim_warmestMonthAvgTemp_5min)
points(newans$x, newans$y)


terrcr<-stack("IsolationRasterPre.tif", "ShapeAreaPre.tif")
names(terrcr)<-c("isolation","Shape_Area")

#add the new values we got from the buffer
testerr2<-crop(testerr,extent(terrcr))
terrcr$tempMW<-testerr2$WorldClim_warmestMonthAvgTemp_5min

terrcr$isolation[newans$index]
terrcr$isolation[newans$index]<-newans[,"Iso"]
terrcr$isolation[newans$index]

getwd()
writeRaster(terrcr$isolation, "IsolationRaster.tif", overwrite=T)
writeRaster(terrcr$Shape_Area, "ShapeArea.tif", overwrite=T)

plot(!is.na(terrcr$tempMW)&is.na(terrcr$Shape_Area))

terrtest<-terrcr


terrtest$tempMW[!is.na(terrtest$tempMW)&!is.na(terrtest$isolation)]<-NA

plot(!is.na(terrtest$tempMW)&is.na(terrtest$Shape_Area))
plot(terrtest$tempMW)

#terrcr$isolation[is.na(terrcr$tempMW)&!is.na(terrcr$isolation)]
