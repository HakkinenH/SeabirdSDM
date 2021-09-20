
#An experiment in defining a training area for SDM validation
#no longer used



setwd("C:/Users/Henry/Documents/Research/ZSL/Repos/SeabirdCCVA/IntermediateOutput/SDM")
#read in everything terr
terr.st<-stack(paste0("../PrepEnvData/TerrEnvVariables_5m.tif"))
names(terr.st) <- c("MeanTemp_WM", "Prec_BreedMonths", "Isol", 
                    "Area","LandCover", "distToSea","nearestSeaCell")

terr.st<-subset(terr.st, c("MeanTemp_WM","Prec_BreedMonths","Isol", "distToSea"))


#marine
mar.st<-stack(paste0("../PrepEnvData/MarinewithTerrVar_5min.tif"))
names(mar.st)<-c("SST_WS","mean_sal", "max_chloro", "bathy", 
                  "ID", "distShore", "direToLand", "nearestLandCell",
                  "Amarinus", "origIndex",
                  "NLand_MeanTempWM", "NLand_PrecBM", "NLand_Isol",
                  "NLand_Area","NLand_LandCover")

mar.st<-subset(mar.st, c("SST_WS","mean_sal","max_chloro", "distShore"))


#stack everything together
all.st<-stack(terr.st,mar.st)

plot(all.st$MeanTemp_WM)
plot(all.st$SST_WS,add=T)


sp_name<-"Farctica"
#load Puffin range
spdf<-readOGR(paste0("../PrepOccData/",sp_name,"BreedingSp.tif"))
points<-as.data.frame(coordinates(spdf))
points$species<-sp_name
points<-points[,c(3,1,2)]


#Get data points
points <- cbind(points, rep.int(1, length(nrow(points)))); #Adds another column indicating these are presence points
colnames(points) <- c("Species", "X", "Y", "Response");

points(points[,"X"],points[,"Y"],col="blue")

#define training area
trai.ext<-extent(-17,4,47,63)

trai.env<-crop(all.st, trai.ext)

plot(trai.env$MeanTemp_WM)
plot(trai.env$SST_WS,add=T)
points(points[,"X"],points[,"Y"],col="blue")


#define projection area
proj.ext<-extent(3,40,55,75)
proj.env<-crop(all.st, proj.ext)

plot(proj.env$MeanTemp_WM)
plot(proj.env$SST_WS,add=T)
points(points[,"X"],points[,"Y"],col="blue")

names(proj.env)
setwd("C:/Users/Henry/Documents/Research/ZSL/Repos/SeabirdCCVA/IntermediateOutput/defineTrainingArea")

writeRaster(trai.env, paste0("./Training/",sp_name, "_training.tif"), overwrite=T)
writeRaster(proj.env, paste0("./Projection/",sp_name, "_projection.tif"), overwrite=T)
write.csv(points, paste0("./Occurrence/",sp_name,"occ.csv"),row.names=F)

#names c(""MeanTemp_WM", "Prec_BreedMonths", "Isol", "distToSea", "SST_WS", "mean_sal", "max_chloro", "distShore" )



