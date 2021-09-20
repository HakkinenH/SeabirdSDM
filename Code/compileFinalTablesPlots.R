
#TAKE RESPONSE FROM SDM MODELS, NEATEN AND GENERATE PLOTS AND TABLES#



#clear env
rm(list=ls())

start.t<-Sys.time()

library(biomod2);
library(raster);
library(RColorBrewer);
library(dismo);
library(rgdal)
library(sdmpredictors)
library(rasterVis)
library(scales)
library(gridExtra)

setwd("C:/Users/Henry/Documents/Research/ZSL/Repos/SeabirdCCVA/IntermediateOutput/SDM")
source("../../code/SDM_functionsPre.R")
source("../../code/logSettings.R")

#load OSPAR map
ospar<-shapefile("C:/Users/Henry/Documents/Research/ZSL/ProcessedData/OSPAR_region/ospar_LandSea.shp")
os.ext<-extent(ospar)

###SET PARAMETERS###

#set output folder name
nowT<-as.Date(Sys.time(),format='%y%m%d')
outpath<-paste0("C:/Users/Henry/Documents/Research/ZSL/Repos/SeabirdCCVA/FinalOutput/SDMresults-",nowT)


#choose species, options are Mbassanus, Farctica, Sdougallii
spall<-c("Mbassanus", "Farctica", "Sdougallii")

#select realm type, options are MarOnly, TerrOnly, MarwithTerr, TerrwithMar
realmlist=c("TerrOnly", "MarOnly", "TerrwithMar", "MarwithTerr")

#if the above modeltype is TerrwithMar, set terrmartype to mean or SD
terrmarlist<-c("mean")


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


#set sdm parameter to supress warnings
options(sdmpredictors_datadir="C:/Users/Henry/Documents/Research/ZSL/Repos/SeabirdCCVA/IntermediateOutput/SDM")



#check if the output folder exists, if not create it
if (!file.exists(outpath)){
  dir.create(file.path(outpath))
}


#assemble the final table
resTab<-data.frame("Species"=as.character(),
                   "Approach"=as.character(),
                   "TSS"=as.character(),
                   "ROC"=as.character(),
                   "SPEARMAN"=as.character(),
                   "SENSITIVITY"=as.character(),
                   "SPECIFICITY"=as.character(),     
                   "Accuracy"=as.character() )


for (i in 1:length(spall)){
  sp_name<-spall[i]
  print(sp_name)
  
  for(j in 1:length(realmlist)){
    modeltype<-realmlist[j]
    print(modeltype)
    
    #Get all validation metrics
    resrow<-read.csv(paste0("OutputTables/",sp_name,"/",modeltype,"/MEANSD_STATS.csv"))
    
    #get spearmans between validation and full dataset
    nowSP<-read.csv(paste0("OutputTables/",sp_name,"/",modeltype,"/spearman.csv"))
    resSP<-paste0(round(nowSP[nowSP$run=="SpearMean",3],4)," (",round(nowSP[nowSP$run=="SpearSD",3],4),")")
    
    #join together
    resrow$SPEARMAN<-resSP
    #rearrange
    resrow<-resrow[,c("SPECIES", 
                      "MODEL.TYPE",
                      "TSS",
                      "ROC",
                      "SPEARMAN",
                      "SENSITIVITY",
                      "SPECIFICITY",
                      "ACCURACY")]
    

    
    #pretty up species names
    if(sp_name=="Farctica"){resrow[,"SPECIES"]<-"F. arctica"
    } else if (sp_name=="Mbassanus") { resrow[,"SPECIES"]<-"M. bassanus"
    } else if (sp_name=="Sdougallii") { resrow[,"SPECIES"]<-"S. dougallii"
    } else { resrow[,"SPECIES"]<-sp_name}
    
    #pretty up model type
    if(modeltype=="MarwithTerr") { resrow[,"MODEL.TYPE"]<-"Mar w/Terr"
    } else if(modeltype=="TerrwithMar") { resrow[,"MODEL.TYPE"]<-"Terr w/Mar"
    } else{ resrow[,"MODEL.TYPE"]<-modeltype }
    
    #add to the overall table
    resTab[nrow(resTab)+1,]<-resrow
    
  }
}

resTab
write.csv(resTab,paste0(outpath,"/Table1.csv"))


#select the best model for each species
bestMods<-resTab[c("Species", "Approach")]
bestMods$TSS<-substr(resTab$TSS, 1, 6)


# check bestMods carefully, are you happy to use the best models in here? 
#Is there enough gap to make it worth it?

bestMods
conPass<-askYesNo("Are you happy to use the best models in here?")
conPass<-TRUE

if (conPass){
  print("DONE")
}



###plot the terrestrial part


for (i in 1:length(spall)){
  sp<-spall[i]
  print(sp)
 
  


  
  # LOAD TERRESTRIAL MODELS # 
  load(paste0("rstates/",sp, "_TerrOnly.RData"))
  #get the ensemble TerrOnly model for the species in question
  assign(paste0(sp,".TerrOnlyEnsMod"), ensem)
  #find index of weighted mean, which is what we want to plot
  modind<-which(ensem@models.projected== "Weighted_Mean")
  #we take weighted mean raster to plot
  assign(paste0(sp,".TerrOnlyEns"), ensem@proj@val[[modind]])
  
  #Do the same for uncertainty which we want for the Supp. Material
  modind<-which(ensem@models.projected== "Uncertainty")
  assign(paste0(sp,".TerrOnlyUncertainty"), ensem@proj@val[[modind]])

  #while we're here load the var.imp table
  vardf$sp<-sp
  vardf$modeltype<-"TerrOnly"
  assign(paste0(sp, "TerrOnly.vardf"), vardf)
  
  

  load(paste0("rstates/",sp, "_TerrwithMar.RData"))
  assign(paste0(sp,".TerrwMarEnsMod"), ensem)
  #find index of weighted mean, which is what we want to plot
  modind<-which(ensem@models.projected== "Weighted_Mean")
  #we take weighted mean raster to plot
  assign(paste0(sp,".TerrwMarEns"), ensem@proj@val[[modind]])
  
  #Do the same for uncertainty which we want for the Supp. Material
  modind<-which(ensem@models.projected== "Uncertainty")
  assign(paste0(sp,".TerrwMarUncertainty"), ensem@proj@val[[modind]])
  
  #while we're here load the var.imp table
  vardf$sp<-sp
  vardf$modeltype<-"TerrwMar"
  assign(paste0(sp, "TerrwMar.vardf"), vardf)
  
  
  
  # LOAD ENV AND OCC DATA FOR EACH SPECIES #
  #load env data for terrestrial
  envt.st<-readENVdata(sp, "TerrOnly", coastClip)
  #load occurrence data for each species
  points<-readOCCdata(sp, "TerrOnly", envt.st, coastClip)
  
  #check how many points we have
  pcheck<-as.data.frame(extract(envt.st,points[,2:3]))
  pcheck$x<-points$X
  pcheck$y<-points$Y
  assign(paste0(sp,".Terrpoints"), pcheck[complete.cases(pcheck),])
  
  
  
  
  # LOAD MARINE MODELS # 
  load(paste0("rstates/",sp, "_MarOnly.RData"))
  #get the ensemble MarOnly model for the species in question
  assign(paste0(sp,".MarOnlyEnsMod"), ensem)
  #find index of weighted mean, which is what we want to plot
  modind<-which(ensem@models.projected== "Weighted_Mean")
  #we take weighted mean raster to plot
  assign(paste0(sp,".MarOnlyEns"), ensem@proj@val[[modind]])
  
  #Do the same for uncertainty which we want for the Supp. Material
  modind<-which(ensem@models.projected== "Uncertainty")
  assign(paste0(sp,".MarOnlyUncertainty"), ensem@proj@val[[modind]])
  
  #while we're here load the var.imp table
  vardf$sp<-sp
  vardf$modeltype<-"MarOnly"
  assign(paste0(sp, "MarOnly.vardf"), vardf)
  
  
  load(paste0("rstates/",sp, "_MarwithTerr.RData"))
  assign(paste0(sp,".MarwTerrEnsMod"), ensem)
  #find index of weighted mean, which is what we want to plot
  modind<-which(ensem@models.projected== "Weighted_Mean")
  #we take weighted mean raster to plot
  assign(paste0(sp,".MarwTerrEns"), ensem@proj@val[[modind]])
  
  #Do the same for uncertainty which we want for the Supp. Material
  modind<-which(ensem@models.projected== "Uncertainty")
  assign(paste0(sp,".MarwTerrUncertainty"), ensem@proj@val[[modind]])
  
  #while we're here load the var.imp table
  vardf$sp<-sp
  vardf$modeltype<-"MarwTerr"
  assign(paste0(sp, "MarwTerr.vardf"), vardf)
  
  
  # LOAD ENV AND OCC DATA FOR EACH SPECIES (marine) #
  envt.st<-readENVdata(sp, "MarOnly", coastClip)
  #load occurrence data for each species
  points<-readOCCdata(sp, "MarOnly", envt.st, coastClip)
  
  #check how many points we have
  pcheck<-as.data.frame(extract(envt.st,points[,2:3]))
  pcheck$x<-points$X
  pcheck$y<-points$Y
  assign(paste0(sp,".Marpoints"), pcheck[complete.cases(pcheck),])
  
  

  
}

#########################
##### PLOT SDM MAPS ####
##########################

#we now have loaded a lot of indiv models

#6 occurrence data sets
#Mbassanus.Terrpoints
#Mbassanus.Marpoints
#Farctica.Terrpoints
#Farctica.Marpoints
#Sdougallii.Terrpoints
#Sdougallii.Marpoints


#6 terrestrial models
#Mbassanus.TerrOnlyEns
#Mbassanus.TerrwMarEns
#Farctica.TerrOnlyEns
#Farctica.TerrwMarEns
#Sdougallii.TerrOnlyEns
#Sdougallii.TerrwMarEns


#6 marine models
#Mbassanus.MarOnlyEns
#Mbassanus.MarwTerrEns
#Farctica.MarOnlyEns
#Farctica.MarwTerrEns
#Sdougallii.MarOnlyEns
#Sdougallii.MarwTerrEns


#setExt<-as.list(extent(Mbassanus.TerrOnlyEns))
setExt<-c(-30,51,36,85)
names(setExt)<-c("xmin","xmax","ymin","ymax")

#set the settings
myPal <- (RColorBrewer::brewer.pal('YlGn', n=9))
myTheme <- rasterTheme(region = myPal)

blTheme <- rasterTheme(region = "grey")


#set up a blank europe map
eur<-layer(sp.polygons(landsh, col=alpha("grey",0.3))) 



#now we can plot them

Mbassanus.Terrpoints2<-SpatialPoints(Mbassanus.Terrpoints[,c("x","y")], proj4string=crs(Mbassanus.TerrOnlyEns))
Farctica.Terrpoints2<-SpatialPoints(Farctica.Terrpoints[,c("x","y")], proj4string=crs(Mbassanus.TerrOnlyEns))
Sdougallii.Terrpoints2<-SpatialPoints(Sdougallii.Terrpoints[,c("x","y")], proj4string=crs(Mbassanus.TerrOnlyEns))

#do some cropping
Mbassanus.TerrOnlyEns<-crop(Mbassanus.TerrOnlyEns, extent(-30,51,36,85))
Mbassanus.TerrwMarEns<-crop(Mbassanus.TerrwMarEns, extent(-30,51,36,85))

Farctica.TerrOnlyEns<-crop(Farctica.TerrOnlyEns, extent(-30,51,36,85))
Farctica.TerrwMarEns<-crop(Farctica.TerrwMarEns, extent(-30,51,36,85))

Sdougallii.TerrOnlyEns<-crop(Sdougallii.TerrOnlyEns, extent(-30,51,36,85))
Sdougallii.TerrwMarEns<-crop(Sdougallii.TerrwMarEns, extent(-30,51,36,85))


#make a massive legend for high res
legPlot<-levelplot(Mbassanus.TerrOnlyEns/1000, 
              margin=F, 
              at=seq(0, 1, length=11),
              #colorkey=FALSE,
              xlab=NULL,
              ylab=NULL,
              colorkey=list(space="bottom"), 
              #main=list('Legend', cex=1.5),
              par.settings=myTheme)

png(filename=paste0(outpath,"/Fig2and3/LegendPlot.png"), width=4, height=5, units="in", res=500)
legPlot
dev.off()


#ROW 1: M bassanus 

#col 1: occurrence
M1<-levelplot(Mbassanus.TerrOnlyEns/1000, 
              margin=FALSE, 
              #at=seq(0, 1, length=11),
              colorkey=FALSE,
              xlab=NULL,
              ylab=NULL,
              scales=list( 
                alternating=1, 
                tck=c(1,0), 
                x=list(col="white"),
                y=list(col="black")) ,
              #colorkey=list(height=0.6), 
              main=list('Occurrence', cex=1.5),
              par.settings=blTheme)


mpn<-layer( sp.points( Mbassanus.Terrpoints2,
     col="Green",
     pch=3,
     cex=1) )

M1<-M1+eur+mpn



#col 2: TerrOnly Mbassanus.TerrOnlyEns

M2<-levelplot(Mbassanus.TerrOnlyEns/1000, 
                margin=FALSE, 
                at=seq(0, 1, length=11),
                colorkey=FALSE,
                xlab=NULL,
                ylab=NULL,
                scales=list( 
                  alternating=1, 
                  tck=c(1,0), 
                  x=list(col="white"),
                  y=list(col="white")) ,
                #colorkey=list(height=0.6), 
                main=list('TerrOnly', cex=1.5),
                par.settings = myTheme)

M2 <- M2 + eur



#col 3: TerrwMarine Mbassanus.TerrwMarEns

M3<-levelplot(Mbassanus.TerrwMarEns/1000, 
                margin=FALSE, 
                at=seq(0, 1, length=11),
                colorkey=FALSE,
                xlab=NULL,
                ylab=NULL,
                scales=list( 
                  alternating=1, 
                  tck=c(1,0), 
                  x=list(col="white"),
                  y=list(col="white")) ,
                #colorkey=list(height=0.9), 
                main=list('Terr w/Mar', cex=1.5),
                par.settings = myTheme)

M3<-M3 + eur



#9 part panel (Terrestrial)
png(filename=paste0(outpath,"/Fig2and3/Fig1Mbaocc.png"), width=4, height=5, units="in", res=300)
M1
dev.off()

png(filename=paste0(outpath,"/Fig2and3/Fig1MbbTerrOnly.png"), width=4, height=5, units="in", res=300)
M2
dev.off()

png(filename=paste0(outpath,"/Fig2and3/Fig1MbcTerrwMar.png"), width=4, height=5, units="in", res=300)
M3
dev.off()



#now do the same for F. arctica

#col 1: occurrence
M1<-levelplot(Farctica.TerrOnlyEns/1000, 
              margin=FALSE, 
              #at=seq(0, 1, length=11),
              colorkey=FALSE,
              xlab=NULL,
              ylab=NULL,
              scales=list( 
                alternating=1, 
                tck=c(1,0), 
                x=list(col="white"),
                y=list(col="black")) ,
              #colorkey=list(height=0.6), 
              #main=list('Occurrence', cex=1.5),
              par.settings=blTheme)


mpn<-layer( sp.points( Farctica.Terrpoints2,
                       col="Green",
                       pch=3,
                       cex=0.1) )

M1<-M1+eur+mpn



#col 2: TerrOnly Mbassanus.TerrOnlyEns

M2<-levelplot(Farctica.TerrOnlyEns/1000, 
              margin=FALSE, 
              at=seq(0, 1, length=11),
              colorkey=FALSE,
              xlab=NULL,
              ylab=NULL,
              scales=list( 
                alternating=1, 
                tck=c(1,0), 
                x=list(col="white"),
                y=list(col="white")) ,
              #colorkey=list(height=0.6), 
              #main=list('TerrOnly', cex=1.5),
              par.settings = myTheme)

M2 <- M2 + eur



#col 3: TerrwMarine Mbassanus.TerrwMarEns

M3<-levelplot(Farctica.TerrwMarEns/1000, 
              margin=FALSE, 
              at=seq(0, 1, length=11),
              colorkey=FALSE,
              xlab=NULL,
              ylab=NULL,
              scales=list( 
                alternating=1, 
                tck=c(1,0), 
                x=list(col="white"),
                y=list(col="white")) ,
              #colorkey=list(height=0.9), 
              #main=list('Terr w/Mar', cex=1.5),
              par.settings = myTheme)

M3<-M3 + eur



#9 part panel (Terrestrial)
png(filename=paste0(outpath,"/Fig2and3/Fig1Faaocc.png"), width=4, height=5, units="in", res=300)
M1
dev.off()

png(filename=paste0(outpath,"/Fig2and3/Fig1FabTerrOnly.png"), width=4, height=5, units="in", res=300)
M2
dev.off()

png(filename=paste0(outpath,"/Fig2and3/Fig1FacTerrwMar.png"), width=4, height=5, units="in", res=300)
M3
dev.off()






#now do the same for S. dougallii

#col 1: occurrence
M1<-levelplot(Sdougallii.TerrOnlyEns/1000, 
              margin=FALSE, 
              #at=seq(0, 1, length=11),
              colorkey=FALSE,
              xlab=NULL,
              ylab=NULL,
              scales=list( 
                alternating=1, 
                tck=c(1,0), 
                x=list(col="black"),
                y=list(col="black")) ,
              #colorkey=list(height=0.6), 
              #main=list('Occurrence', cex=1.5),
              par.settings=blTheme)


mpn<-layer( sp.points( Sdougallii.Terrpoints2,
                       col="Green",
                       pch=3,
                       cex=0.5) )

M1<-M1+eur+mpn



#col 2: TerrOnly Mbassanus.TerrOnlyEns

M2<-levelplot(Sdougallii.TerrOnlyEns/1000, 
              margin=FALSE, 
              at=seq(0, 1, length=11),
              colorkey=FALSE,
              xlab=NULL,
              ylab=NULL,
              scales=list( 
                alternating=1, 
                tck=c(1,0), 
                x=list(col="black"),
                y=list(col="white")) ,
              #colorkey=list(height=0.6), 
              #main=list('TerrOnly', cex=1.5),
              par.settings = myTheme)

M2 <- M2 + eur



#col 3: TerrwMarine Mbassanus.TerrwMarEns

M3<-levelplot(Sdougallii.TerrwMarEns/1000, 
              margin=FALSE, 
              at=seq(0, 1, length=11),
              colorkey=FALSE,
              xlab=NULL,
              ylab=NULL,
              scales=list( 
                alternating=1, 
                tck=c(1,0), 
                x=list(col="black"),
                y=list(col="white")) ,
              #colorkey=list(height=0.9), 
              #main=list('Terr w/Mar', cex=1.5),
              par.settings = myTheme)

M3<-M3 + eur



#9 part panel (Terrestrial)
png(filename=paste0(outpath,"/Fig2and3/Fig1Sdaocc.png"), width=4, height=5, units="in", res=300)
M1
dev.off()

png(filename=paste0(outpath,"/Fig2and3/Fig1SdbTerrOnly.png"), width=4, height=5, units="in", res=300)
M2
dev.off()

png(filename=paste0(outpath,"/Fig2and3/Fig1SdcTerrwMar.png"), width=4, height=5, units="in", res=300)
M3
dev.off()







###NOW DO THE MARINE PART


#now we can plot them

Mbassanus.Marpoints2<-SpatialPoints(Mbassanus.Marpoints[,c("x","y")], proj4string=crs(Mbassanus.MarOnlyEns))
Farctica.Marpoints2<-SpatialPoints(Farctica.Marpoints[,c("x","y")], proj4string=crs(Mbassanus.MarOnlyEns))
Sdougallii.Marpoints2<-SpatialPoints(Sdougallii.Marpoints[,c("x","y")], proj4string=crs(Mbassanus.MarOnlyEns))


#do some cropping
Mbassanus.MarOnlyEns<-crop(Mbassanus.MarOnlyEns, extent(-30,51,36,85))
Mbassanus.MarwTerrEns<-crop(Mbassanus.MarwTerrEns, extent(-30,51,36,85))

Farctica.MarOnlyEns<-crop(Farctica.MarOnlyEns, extent(-30,51,36,85))
Farctica.MarwTerrEns<-crop(Farctica.MarwTerrEns, extent(-30,51,36,85))

Sdougallii.MarOnlyEns<-crop(Sdougallii.MarOnlyEns, extent(-30,51,36,85))
Sdougallii.MarwTerrEns<-crop(Sdougallii.MarwTerrEns, extent(-30,51,36,85))



#ROW 1: M bassanus 

#col 1: occurrence
M1<-levelplot(Mbassanus.MarOnlyEns/1000, 
              margin=FALSE, 
              #at=seq(0, 1, length=11),
              colorkey=FALSE,
              xlab=NULL,
              ylab=NULL,
              scales=list( 
                alternating=1, 
                tck=c(1,0), 
                x=list(col="white"),
                y=list(col="black")) ,
              #colorkey=list(height=0.6), 
              main=list('Occurrence', cex=1.5),
              par.settings=blTheme)


mpn<-layer( sp.points( Mbassanus.Marpoints2,
                       col="Green",
                       pch=".",
                       cex=1) )

M1<-M1+eur+mpn



#col 2: MarOnly Mbassanus.MarOnlyEns

M2<-levelplot(Mbassanus.MarOnlyEns/1000, 
              margin=FALSE, 
              at=seq(0, 1, length=11),
              colorkey=FALSE,
              xlab=NULL,
              ylab=NULL,
              scales=list( 
                alternating=1, 
                tck=c(1,0), 
                x=list(col="white"),
                y=list(col="white")) ,
              #colorkey=list(height=0.6), 
              main=list('MarOnly', cex=1.5),
              par.settings = myTheme)

M2 <- M2 + eur



#col 3: MarwTerr Mbassanus.TerrwMarEns

M3<-levelplot(Mbassanus.MarwTerrEns/1000, 
              margin=FALSE, 
              at=seq(0, 1, length=11),
              colorkey=FALSE,
              xlab=NULL,
              ylab=NULL,
              scales=list( 
                alternating=1, 
                tck=c(1,0), 
                x=list(col="white"),
                y=list(col="white")) ,
              #colorkey=list(height=0.9), 
              main=list('Mar w/Terr', cex=1.5),
              par.settings = myTheme)

M3<-M3 + eur



#9 part panel (Terrestrial)
png(filename=paste0(outpath,"/Fig2and3/Fig2Mbaocc.png"), width=4, height=5, units="in", res=300)
M1
dev.off()

png(filename=paste0(outpath,"/Fig2and3/Fig2MbbMarOnly.png"), width=4, height=5, units="in", res=300)
M2
dev.off()

png(filename=paste0(outpath,"/Fig2and3/Fig2MbcMarwMar.png"), width=4, height=5, units="in", res=300)
M3
dev.off()



#now do the same for F. arctica

#col 1: occurrence
M1<-levelplot(Farctica.MarOnlyEns/1000, 
              margin=FALSE, 
              #at=seq(0, 1, length=11),
              colorkey=FALSE,
              xlab=NULL,
              ylab=NULL,
              scales=list( 
                alternating=1, 
                tck=c(1,0), 
                x=list(col="white"),
                y=list(col="black")) ,
              #colorkey=list(height=0.6), 
              #main=list('Occurrence', cex=1.5),
              par.settings=blTheme)


mpn<-layer( sp.points( Farctica.Marpoints2,
                       col="Green",
                       pch=3,
                       cex=0.1) )

M1<-M1+eur+mpn



#col 2: TerrOnly Mbassanus.TerrOnlyEns

M2<-levelplot(Farctica.MarOnlyEns/1000, 
              margin=FALSE, 
              at=seq(0, 1, length=11),
              colorkey=FALSE,
              xlab=NULL,
              ylab=NULL,
              scales=list( 
                alternating=1, 
                tck=c(1,0), 
                x=list(col="white"),
                y=list(col="white")) ,
              #colorkey=list(height=0.6), 
              #main=list('MarOnly', cex=1.5),
              par.settings = myTheme)

M2 <- M2 + eur



#col 3: MarwTerr Farctica.MarwTerrEns

M3<-levelplot(Farctica.MarwTerrEns/1000, 
              margin=FALSE, 
              at=seq(0, 1, length=11),
              colorkey=FALSE,
              xlab=NULL,
              ylab=NULL,
              scales=list( 
                alternating=1, 
                tck=c(1,0), 
                x=list(col="white"),
                y=list(col="white")) ,
              #colorkey=list(height=0.9), 
              #main=list('Terr w/Mar', cex=1.5),
              par.settings = myTheme)

M3<-M3 + eur



#9 part panel (Terrestrial)
png(filename=paste0(outpath,"/Fig2and3/Fig2Faaocc.png"), width=4, height=5, units="in", res=300)
M1
dev.off()

png(filename=paste0(outpath,"/Fig2and3/Fig2FabMarOnly.png"), width=4, height=5, units="in", res=300)
M2
dev.off()

png(filename=paste0(outpath,"/Fig2and3/Fig2FacMarwTerr.png"), width=4, height=5, units="in", res=300)
M3
dev.off()






#now do the same for S. dougallii

#col 1: occurrence
M1<-levelplot(Sdougallii.MarOnlyEns/1000, 
              margin=FALSE, 
              #at=seq(0, 1, length=11),
              colorkey=FALSE,
              xlab=NULL,
              ylab=NULL,
              scales=list( 
                alternating=1, 
                tck=c(1,0), 
                x=list(col="black"),
                y=list(col="black")) ,
              #colorkey=list(height=0.6), 
              #main=list('Occurrence', cex=1.5),
              par.settings=blTheme)


mpn<-layer( sp.points( Sdougallii.Marpoints2,
                       col="Green",
                       pch=3,
                       cex=0.5) )

M1<-M1+eur+mpn



#col 2: TerrOnly Mbassanus.TerrOnlyEns

M2<-levelplot(Sdougallii.MarOnlyEns/1000, 
              margin=FALSE, 
              at=seq(0, 1, length=11),
              colorkey=FALSE,
              xlab=NULL,
              ylab=NULL,
              scales=list( 
                alternating=1, 
                tck=c(1,0), 
                x=list(col="black"),
                y=list(col="white")) ,
              #colorkey=list(height=0.6), 
              #main=list('MarOnly', cex=1.5),
              par.settings = myTheme)

M2 <- M2 + eur



#col 3: TerrwMarine Mbassanus.TerrwMarEns

M3<-levelplot(Sdougallii.MarwTerrEns/1000, 
              margin=FALSE, 
              at=seq(0, 1, length=11),
              colorkey=FALSE,
              xlab=NULL,
              ylab=NULL,
              scales=list( 
                alternating=1, 
                tck=c(1,0), 
                x=list(col="black"),
                y=list(col="white")) ,
              #colorkey=list(height=0.9), 
              #main=list('Mar w/Terr', cex=1.5),
              par.settings = myTheme)

M3<-M3 + eur



#9 part panel (Marine)
png(filename=paste0(outpath,"/Fig2and3/Fig2Sdaocc.png"), width=4, height=5, units="in", res=300)
M1
dev.off()

png(filename=paste0(outpath,"/Fig2and3/Fig2SdbMarOnly.png"), width=4, height=5, units="in", res=300)
M2
dev.off()

png(filename=paste0(outpath,"/Fig2and3/Fig2SdcMarwTerr.png"), width=4, height=5, units="in", res=300)
M3
dev.off()




#########################
##### PLOT VAR IMP ####
##########################

#we have 12 tables

vardf.table<-c(
  "FarcticaTerrOnly.vardf" ,
  "FarcticaTerrwMar.vardf" ,
  "FarcticaMarOnly.vardf" ,
  "FarcticaMarwTerr.vardf" ,
  
  "MbassanusTerrOnly.vardf" ,
  "MbassanusTerrwMar.vardf" ,
  "MbassanusMarOnly.vardf" ,
  "MbassanusMarwTerr.vardf" ,

  "SdougalliiTerrOnly.vardf" ,
  "SdougalliiTerrwMar.vardf" ,
  "SdougalliiMarOnly.vardf" ,
  "SdougalliiMarwTerr.vardf"
)

#define a table of colours, one for each variable
terrPal <- (RColorBrewer::brewer.pal('YlGn', n=5))

marPal <- (RColorBrewer::brewer.pal('PuBu', n=5))

terrcoldf<-as.data.frame(para = c("MeanTemp_WM"))

terrcoldf<-data.frame(MeanTemp_WM  = terrPal[1], 
                  Prec_BreedMonths=terrPal[2],
                  Isol = terrPal[3],
                  distToSea = terrPal[4],
                  NDVI_min = terrPal[5],
                  
                  NLand_MeanTempWM = terrPal[1],
                  NLand_PrecBM = terrPal[2],
                  NLand_Isol = terrPal[3],
                  NLand_NDVImin = terrPal[5])
#changed my mind, transform it
terrcoldf<-as.data.frame(t(terrcoldf))
colnames(terrcoldf)<-"col"
terrcoldf$para<-rownames(terrcoldf)

marcoldf<-data.frame(SST_WS  =marPal[1], 
                     mean_sal = marPal[2],
                     max_chloro  = marPal[3],
                     distShore = marPal[4],
                     
                     MEAN_SST_WS = marPal[1],
                     MEAN_mean_sal = marPal[2],
                     MEAN_max_chloro = marPal[3],
                     SD_bathy = marPal[5])
#changed my mind, transform it
marcoldf<-as.data.frame(t(marcoldf))
colnames(marcoldf)<-"col"
marcoldf$para<-rownames(marcoldf)

colpaldf<-rbind(terrcoldf, marcoldf)


terrpretty<-data.frame(MeanTemp_WM="Temp of warmest month", 
                 Prec_BreedMonths="Spr/Sum precip", 
                Isol="Isolation",
                distToSea="Distance to sea",
                NDVI_min = "Minimum NDVI",
                NLand_PrecBM = "Spr/Sum precip on nearest land",
                NLand_Isol = "Isolation of nearest land",
                NLand_NDVImin = "NDVI on nearest land" 
)

#terrpretty my mind, transform it
terrpretty<-as.data.frame(t(terrpretty))
colnames(terrpretty)<-"pretty"
terrpretty$para<-rownames(terrpretty)
terrpretty$realm<-1

marpretty <- data.frame(SST_WS="SST (Winter/Spring)", 
                     mean_sal="Mean Salinity", 
                     max_chloro="Max. chlorophyll",
                     distShore="Distance to shore",
                     MEAN_SST_WS = "Mean Win/Spr SST", MEAN_mean_sal = "Mean salinity",
                     MEAN_max_chloro = "Mean chlorophyll", SD_bathy = "Bathymetry var" )
marpretty<-as.data.frame(t(marpretty))
colnames(marpretty)<-"pretty"
marpretty$para<-rownames(marpretty)
marpretty$realm<-2

prettydf<-rbind(terrpretty, marpretty)

#create table to save results
fulltab<-data.frame(para=character(), varpl=numeric(), upper=numeric(), lower=numeric(), pretty=character())


for (i in vardf.table){
  print(i)
  
  curvar<-get(i)
  
  spnow<-curvar$sp[1]
  modnow<-curvar$modeltype[1]

  
  #ignore last three columns as they are labels
  
  
  varpl<-apply(curvar[,1:(ncol(curvar)-3)], 2, mean, na.rm=T)
  varsd<-apply(curvar[,1:(ncol(curvar)-3)], 2, sd, na.rm=T)
  
  
  resdf<-as.data.frame(varpl)
  resdf$para<-rownames(resdf)
  
  resdf$upper<-resdf$varpl + varsd
  resdf$lower<-resdf$varpl - varsd
  resdf$lower[resdf$lower<0]<-0
  resdf$upper[resdf$upper>1]<-1
  
  rescom1<-merge(resdf, colpaldf,by="para", all.x=T)
  rescom<-merge(rescom1, prettydf,by="para", all.x=T)
  
  #sort in order of importance which varies
  
  if (modnow %in% c("TerrOnly", "TerrwMar")){
    #put terr first
    rescom <- rescom [order(rescom[, "realm"], -rescom[, "varpl"]), ]
  }else{ 
    #put marine first
    rescom <- rescom [order(-rescom[, "realm"], -rescom[, "varpl"]), ]
  }
  
  
  
  png(filename=paste0(outpath,"/Fig4/",spnow,modnow,"_VarImp.png"), width=13, height=5, units="in", res=300)
  
  barCenters<- barplot(height=rescom$varpl, beside=T,
                       xlab="Variable",
                       ylab = "Variable Importance",
                       ylim=c(0,max(rescom$upper)),
                       #ylim=c(0,1.0),
                       col=rescom$col,
                       names.arg = rescom$pretty,
                       cex.names=0.5)
  #legend=prettynames)
  
  segments(barCenters, rescom$lower, barCenters,
           rescom$upper, lwd = 1.5)
  arrows(barCenters, rescom$lower, barCenters,
         rescom$upper, lwd = 1.5, angle = 90,
         code = 3, length = 0.05)
  
  dev.off()
  
  #save the output for the supplementary as well
  nowrow<-rescom
  nowrow$col<-NULL;nowrow$realm<-NULL
  nowrow$model<-i
  
  fulltab<-rbind(fulltab,nowrow)
  
}



##### SUPPLEMENTARY MATERIAL #####

#set the settings
myPal <- (RColorBrewer::brewer.pal('Blues', n=9))
myTheme <- rasterTheme(region = myPal)

blTheme <- rasterTheme(region = "grey")


#set up a blank europe map
eur<-layer(sp.polygons(landsh, col=alpha("grey",0.3))) 


#plot uncertainty

#list of plots
uncerList<-c("Mbassanus.TerrOnlyUncertainty", "Mbassanus.TerrwMarUncertainty", "Mbassanus.MarOnlyUncertainty", "Mbassanus.MarwTerrUncertainty",
             "Farctica.TerrOnlyUncertainty", "Farctica.TerrwMarUncertainty", "Farctica.MarOnlyUncertainty", "Farctica.MarwTerrUncertainty", 
             "Sdougallii.TerrOnlyUncertainty", "Sdougallii.TerrwMarUncertainty", "Sdougallii.MarOnlyUncertainty", "Sdougallii.MarwTerrUncertainty")



for (x in uncerList){
  
  print(x)
  xplot<-get(x)
  xplot<-crop(xplot, extent(-30,51,36,82))
  print(cellStats(xplot,max))
  
  supp1<-levelplot(xplot/1000, 
                  margin=FALSE, 
                  at=seq(0, 1, length=11),
                  colorkey=FALSE,
                  xlab=NULL,
                  ylab=NULL,
                  scales=list( 
                    alternating=1, 
                    tck=c(1,0), 
                    x=list(col="black"),
                    y=list(col="black")) ,
                  #colorkey=list(height=0.9), 
                  #main=list('Mar w/Terr', cex=1.5),
                  par.settings = myTheme)
  
  supp1<-supp1 + eur
  
  png(filename=paste0(outpath,"/SuppFig2and3/",x,".png"), width=5, height=5, units="in", res=300)
  print(supp1)
  dev.off()

  
}


legPlot<-levelplot(Mbassanus.TerrOnlyUncertainty/1000, 
                   margin=F, 
                   at=seq(0, 1, length=11),
                   #colorkey=FALSE,
                   xlab=NULL,
                   ylab=NULL,
                   colorkey=list(space="left"), 
                   #main=list('Legend', cex=1.5),
                   par.settings=myTheme)

png(filename=paste0(outpath,"/SuppFig2and3/LegendPlot.png"), width=5, height=5, units="in", res=500)
  legPlot
dev.off()






##get full model results and make a neat table




#make a neater table with variable imporance for all variable


neatvar<-fulltab

neatvar$sp<-NA
neatvar$modelname<-NA

neatvar$sp[grepl("Farctica", neatvar$model)]<-"F. arctica"
neatvar$sp[grepl("Mbassanus", neatvar$model)]<-"M. bassanus"
neatvar$sp[grepl("Sdougallii", neatvar$model)]<-"S. dougallii"

neatvar$modelname[grepl("TerrOnly", neatvar$model)]<-"Terr Only"
neatvar$modelname[grepl("TerrwMar", neatvar$model)]<-"Terr w/Marine"
neatvar$modelname[grepl("MarOnly", neatvar$model)]<-"Marine Only"
neatvar$modelname[grepl("MarwTerr", neatvar$model)]<-"Marine w/Terr"

neatvar$para<-NULL; neatvar$model<-NULL
neatvar<-neatvar[,c("sp", "modelname", "pretty", "varpl","upper", "lower")]


neatvar$varpl<-round(neatvar$varpl, digits=2)
neatvar$upper<-round(neatvar$upper, digits=2)
neatvar$lower<-round(neatvar$lower, digits=2)

write.csv(neatvar,paste0(outpath,"/VarImpFull.csv"))






