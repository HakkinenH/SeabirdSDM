

setwd("C:/Users/Henry/Documents/Research/ZSL/Repos/SeabirdCCVA/IntermediateOutput/GannetSDM")
library(raster)
library(sdm)
library(rgdal)

predictors.crop<-stack("TerrSDMLayers.tif")
names(predictors.crop) <- c("MeanTemp_W" ,"Prec_BreedMonths","nearestBathy" ,"distToSea" ,"SST_WS","mean_sal","max_chloro", "distShore" )
plot(predictors.crop$mean_sal)

spdf<-readOGR("GannterBreedingSp.tif")
plot(spdf)
spdf
##################################################
#run the model
#################################################

d<-sdmData(Mbassanus~MeanTemp_W+SST_WS+Prec_BreedMonths, train=spdf, predictors=predictors.crop,
           bg=list(n=1000,method='gRandom',remove=TRUE))

library(rJava)

model <- sdm(Mbassanus~MeanTemp_W+SST_WS+Prec_BreedMonths, d, 
             methods=c("glm","brt","maxent","bioclim"),
             test.percent=30, replicate.method='bootstrapping')

m <- sdm(Mbassanus~MeanTemp_W+SST_WS+Prec_BreedMonths,data=d,methods=c('glm','gam','gbm','svm','rf'),
         replication='sub',test.percent=30,n=10)
m
names(p[[1]])
p<-predict(m, predictors.crop)
plot(p[[23]])

plot(m)

# Example 4: Spatial data:

file <- system.file("external/pa_spatial_points.shp", package="sdm") # path to a shapefile

# use a package like rgdal, or maptools, or shapefile function in package raster to read shapefile:
p <- shapefile(file)
class(p) # a "SpatialPointsDataFrame"


plot(p)

head(p) # it contains data for 3 species

# presence-absence plot for the first species (i.e., sp1)
plot(p[p@data$sp1 == 1,],col='blue',pch=16, main='Presence-Absence for sp1')

points(p[p@data$sp1 == 0,],col='red',pch=16)


# Let's read raster dataset containing predictor variables for this study area:

file <- system.file("external/predictors.grd", package="sdm") # path to a raster object

r <- brick(file)

r # a RasterBrick object including 2 rasters (covariates)

plot(r)

# now, we can use the species points and predictor rasters in sdmData function:
d <- sdmData(sp1+sp2+sp3~b15+NDVI,train=p,predictors = r)
p



