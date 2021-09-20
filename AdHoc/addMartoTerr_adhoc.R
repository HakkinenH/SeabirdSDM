#temp#

TerrPredictors<-TerrPre_crop
#read in the full raster
t1full<-stack("./TerrwithMarineVar_5min_Farctica.tif")

names(t1full) <- c("MeanTemp_WM", "Prec_BreedMonths", "Isol",
                    "Area", "LandCover", "distToSea","nearestSeaCell",
                    "NDVI_mean", "NDVI_min",
                    "MEAN_SST_WS","MEAN_mean_sal","MEAN_max_chloro",
                    "MEAN_bathy","MEAN_distShore","MEAN_Amarinus",
                    "SD_SST_WS","SD_mean_sal","SD_max_chloro",
                    "SD_bathy","SD_distShore","SD_Amarinus" )

#read in the terr raster
TerrPredictors$MEAN_SST_WS<-t1full$MEAN_SST_WS
TerrPredictors$MEAN_mean_sal<-t1full$MEAN_mean_sal
TerrPredictors$MEAN_max_chloro<-t1full$MEAN_max_chloro
TerrPredictors$MEAN_bathy<-t1full$MEAN_bathy
TerrPredictors$MEAN_distShore<-t1full$MEAN_distShore
TerrPredictors$MEAN_Amarinus<-t1full$MEAN_Amarinus

TerrPredictors$SD_SST_WS<-t1full$SD_SST_WS
TerrPredictors$SD_mean_sal<-t1full$SD_mean_sal
TerrPredictors$SD_max_chloro<-t1full$SD_max_chloro
TerrPredictors$SD_bathy<-t1full$SD_bathy
TerrPredictors$SD_distShore<-t1full$SD_distShore
TerrPredictors$SD_MEAN_Amarinus<-t1full$SD_Amarinus

writeRaster(TerrPredictors,"./TerrwithMarineVar_5min_FarcticaUP.tif",overwrite=T)
names(TerrPredictors)



#names of terrestrial only
names(TerrPre_crop)<-c("MeanTemp_WM", "Prec_BreedMonths", "Isol", 
                       "Area", "LandCover", "distToSea", "nearestSeaCell",
                       "NDVI_mean", "NDVI_min")


#names of marine only
names(MarinePredictors)<-c("SST_WS","mean_sal", "max_chloro", "bathy", 
                           "ID", "distToLand", "direToLand", "nearestLandCell")















