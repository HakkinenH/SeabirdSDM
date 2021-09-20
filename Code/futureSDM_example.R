
##PROJECT FORWARDS
#clear env
rm(list=ls())

start.t<-Sys.time()

setwd("C:/Users/Henry/Documents/Research/ZSL/Repos/SeabirdCCVA/IntermediateOutput/SDM2100")

source("../../code/SDM_functions2100Pre.R")

library(biomod2);
library(raster);
library(RColorBrewer);
library(dismo);
library(rgdal)
library(sdmpredictors)
library(rasterVis)

#load OSPAR map
ospar<-shapefile("C:/Users/Henry/Documents/Research/ZSL/ProcessedData/OSPAR_region/ospar_LandSea.shp")
os.ext<-extent(ospar)

#load simplified map for plotting purporse
landsh<-shapefile("C:/Users/Henry/Documents/Research/ZSL/ProcessedData/EEAcoastlines/SimpleLandOutline_wgs84.shp")

#set colour palette
myPal <- (RColorBrewer::brewer.pal('YlGn', n=9))
myTheme <- rasterTheme(region = myPal)


#we clip marine and terrestrial values if there are too far from each other
#if manCoastDist is set to NA, SDM will use default foraging values to clip marine and terr layers
#if manCoastDist is set to 0 then it will ignore how far things are from the coast and use everything
#if manCoastDist is set to a positive number then that new threshold will be used
manCoastDist<-NA


#choose number of pseudoabsence draws, minimum of 2
pseudodraws<-2
#choose how many evaluation runs (70/30 split), minimum of 2
evalruns<-2
#set number of pseudo-absences
nvalid<-10000

#define model list
#at the moment code will use up to GLM, GAM, ANN, RF, MAXENT
#model_list<-c("GLM","GAM","ANN","RF","MAXENT.Phillips")
model_list<-c("GLM","GAM","MAXENT.Phillips")



############################################################
### RUN MODELS ###
############################################################

start.now<-Sys.time()

sp_name<-"Sdougallii"
modeltype<-"TerrwithMar"
terrmartype<-"mean"
    

#check for manual override to coastal clip, otherwise use default value
if (!(is.na(manCoastDist))){
  if(manCoastDist<=0){coastClip<-NA}
  if(manCoastDist>0){coastClip<-manCoastDist}
}else{
  #use defaults
  #set the coastal crop point, how far from the sea or land do we want calculations to include
  if (modeltype %in% c("TerrOnly", "TerrwithMar")){
    #if inland we accept up to the foraging distance
    if(sp_name == "Mbassanus"){coastClip <- 20000}
    if(sp_name == "Farctica"){coastClip <- 20000}
    if(sp_name == "Sdougallii"){coastClip <- 20000}
  }
  if (modeltype %in% c("MarOnly", "MarwithTerr")){
    #if at sea we accept up to the foraging distance
    if(sp_name == "Mbassanus"){coastClip <- 200000*1.2}
    if(sp_name == "Farctica"){coastClip <- 40000*1.2}
    if(sp_name == "Sdougallii"){coastClip <- 16600*1.2}
  }
}
    
    

start.t<-Sys.time()


#logManual(paste("Starting:", sp_name))
#logManual(paste("Starting:", modeltype))
print(sp_name)
print(modeltype)

#load environmental data
#will return warning if there is too much covariation (>0.7 in a pearson correlation)
envt.st<-readENVdata(sp_name, modeltype, coastClip)
envt.st2100<-readENVdata2100(sp_name, modeltype, coastClip)



#logManual("Env read!")

#can check covariance if you want
#env_correlations <- pearson_correlation_matrix(envt.st)
#p2 <- plot_correlation(env_correlations, prettynames)
#p2
#env_correlations

#load occurrence data
#note that this sets a variable nvalid which is used later
points<-readOCCdata2100(sp_name, modeltype, envt.st, coastClip)
#logManual("Occ read!")


#check how many points we have
pcheck<-as.data.frame(extract(envt.st,points[,2:3]))
pcheck$x<-points$X
pcheck$y<-points$Y
pcheck2<-pcheck[complete.cases(pcheck),]
#plot(pcheck2$x, pcheck2$y,col="green",pch=3,cex=1.2)

#low-res map for plotting purposes
#newmap <- getMap(resolution = "low")  # different resolutions available
#plot(newmap,add=T)

npoints<-nrow(pcheck2)
#print(npoints)

#if nvalid is not set number of pseudoabsences set to number of valid points
if(is.na(nvalid)){
  nps<-npoints
}else{
  nps<-nvalid
}

#logManual(paste("Coast clip:",coastClip))
#logManual(paste("pseudo-absenses:",nps))

#run the actual model
source("../../code/SDM_functions.R")
end.t<-Sys.time()

print(end.t-start.t)



#take our current data and project forwards

#get 2100 data
#the most important variables we found for puffins were 
  #distance to the sea
  #terrestrial temperature during the summer
  #marine temp during the winter
  #and salinity




##FUTURE SECTION


#Projecting your model to the future
pipomodProj2100 <- BIOMOD_Projection(modeling.output = PIPO.mod,
                                      new.env = stack(envt.st2100),
                                      proj.name = 'In2100',
                                      selected.models = grep('_Full_', get_built_models(
                                       PIPO.mod), value=TRUE),
                                      compress = 'gzip',
                                      clamping.mask = T,
                                      output.format = '.grd',
                                      do.stack=T
);

mod_proj2100 <- get_predictions(pipomodProj2100);
result2100 <- calc(mod_proj2100,fun = median); #Choose whatever descriptive statistic you'd like
plot(result2100);
writeRaster(result2100, filename = "Farctica2100", format = "ascii", overwrite = T);


#Projecting the ensemble model in 2100
myBiomodProj2100Ensemble <- BIOMOD_EnsembleForecasting(BiomodFullEM,
                                                       projection.output = pipomodProj2100,
                                                       selected.models = 'all',
                                                       compress = 'gzip'
);

mod_proj2100Ensemble <- get_predictions(myBiomodProj2100Ensemble);

ensembleResult2100 <- mod_proj2100Ensemble[[1]] #This is the weighted mean model ensemble
plot(ensembleResult2100);
writeRaster(ensembleResult2100, filename = "Farctica2100Ensemble", format = "ascii", overwrite = T);



##Calculate MESS for 2100
mess2100 <- mess(envt.st2100, rasterToPoints(envt.st)[,-1:-2],-9999);
writeRaster(mess2100$rmess, filename = "Farctica2100MESS", format = "ascii", overwrite = T)


#here is the present day ensemble
ensembleResultCURR <- get_predictions(ensem)[[2]];
plot(ensembleResultCURR)

#and here is the ensemble suitability for 2100
plot(ensembleResult2100)


diff<- ensembleResult2100 - ensembleResultCURR



#neat plot


#setExt<-as.list(extent(Mbassanus.TerrOnlyEns))
setExt<-c(-30,51,36,85)
names(setExt)<-c("xmin","xmax","ymin","ymax")

#set the settings
myPal <- (RColorBrewer::brewer.pal('RdYlBu', n=9))
myTheme <- rasterTheme(region = myPal)

blTheme <- rasterTheme(region = "grey")


#set up a blank europe map
eur<-layer(sp.polygons(landsh, col=alpha("grey",0.3))) 

M3<-levelplot(diff/1000, 
              margin=FALSE, 
              #at=seq(-1, 1, length=11),
              colorkey=T,
              #xlab=NULL,
              #ylab=NULL,
              #scales=list( 
              #alternating=1, 
              #tck=c(1,0), 
              #x=list(col="white"),
              #y=list(col="white")) ,
              #colorkey=list(height=0.9), 
              main=list('ChangeInSuit', cex=1.5),
              par.settings = myTheme)

M3<-M3 + eur
M3
png(filename=paste0("./SuitabilityMaps/FarcticaChangeinSuit2100.png"), width=4, height=5, units="in", res=300)
M3
dev.off()


threslist<-c(500,600,750)
i<-600

for (i in threslist){
  print(i)
  
  m4<-levelplot(ensembleResultCURR>i, 
                margin=FALSE, 
                main=list(paste0('CurSuitThres',i), cex=1.5),
                par.settings = myTheme)
  m4<-m4+eur
  
  png(filename=paste0("./SuitabilityMaps/FarcticaPresSuitTHRESH",i,".png"), width=4, height=5, units="in", res=300)
  m4
  dev.off()
  
  
  m5<-levelplot(ensembleResult2100>i, 
                margin=FALSE, 
                main=list(paste0('FutureSuitThres',i), cex=1.5),
                par.settings = myTheme)
  m5<-m5+eur
  
  png(filename=paste0("./SuitabilityMaps/Farctica2100SuitTHRESH",i,".png"), width=4, height=5, units="in", res=300)
  m5
  dev.off()
  
  
  diffscore<-(ensembleResult2100>i) - (ensembleResultCURR>i)
  
  m6<-levelplot(diffscore, 
                margin=FALSE, 
                main=list(paste0('ChangeInSuitThres',i), cex=1.5),
                par.settings = myTheme)
  m6<-m6+eur
  
  png(filename=paste0("./SuitabilityMaps/FarcticaChangeinSuitTHRESH",i,".png"), width=4, height=5, units="in", res=300)
  m6
  dev.off()
}
  

changeBinary<-(ensembleResult2100>i) - (ensembleResultCURR>i)
#remove anything that was never suitable

changeBinary[( (ensembleResult2100>i) == 0 & (ensembleResultCURR>i) ==0)]<-NA

#set the settings
myPal2 <- (RColorBrewer::brewer.pal('RdYlBu', n=3))
myPal2<-c("#FF0000", "#0000FF", "#00FF00")
myTheme2 <- rasterTheme(region = myPal2)


m7<-levelplot(changeBinary, 
              margin=FALSE, 
              main=list(paste0('ChangeInSuitBinary600',i), cex=1.5),
              par.settings = myTheme2)
m7<-m7+eur

png(filename=paste0("./SuitabilityMaps/FarcticaChangeinSuitBinaryTHRESH",i,".png"), width=4, height=5, units="in", res=300)
m7
dev.off()

changedf<-as.data.frame(changeBinary$layer)
changedf<-na.omit(changedf)
table(changedf)



#take points, which are suitable now, which will be in future?


envScores<-extract(envt.st2100, points[,c("X","Y")])


currScores<-extract(ensembleResultCURR, points[,c("X","Y")])
length(currScores)
sum(currScores>750)


futScores<-extract(ensembleResult2100, points[,c("X","Y")])
length(futScores)
sum(futScores>750)





