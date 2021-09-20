##Exploratory file used to open and map species ranges.


rm(list=ls())
#open birdlife seabird data and explore

library(rgdal)
library(sp)
library(raster)
library(rworldmap)

library(openxlsx)

setwd("C:/Users/Henry/Documents/Research/ZSL/Repos/SeabirdCCVA/Code")


#set input folder
input<-"../../../RawData/ProcessedData/Birdlife-RangeDatabase_SeabirdOnly/Simplified/"

#set output folder
output<-"../FinalOutput/BirdLifePlotRanges/"


#low-res map for plotting purposes
newmap <- getMap(resolution = "low")  # different resolutions available

#load the birdlife seabird data
BLdb<-shapefile(paste0(input,"BLdb_seabirdRoughClean.shp"))
names(BLdb)

#read in species list for extra info
sp_infodf<-read.xlsx("../../../RawData/Seabird-SpeciesList_MariaDiasDec2020/Seabirds_NW_Europe.xlsx")

#get species list
sp_list<-unique(BLdb$binomial)

colRamp<-c("#fde725","#5dc962","#20908d","#3a528b","#440154")
leg.txt <- c("Resident","Breeding","Non-breeding","Passage","Uncertain")




for (i in 1:length(sp_list)){
  
  print(sp_list[i])
  sp1<-BLdb[BLdb$binomial==sp_list[i],]
  
  sp_info <- sp_infodf[sp_infodf$Scientific.name == sp_list[i],]
  
  sp1$col<-NA
  sp1$col[sp1$seasonal==1]<-colRamp[1]
  sp1$col[sp1$seasonal==2]<-colRamp[2]
  sp1$col[sp1$seasonal==3]<-colRamp[3]
  sp1$col[sp1$seasonal==4]<-colRamp[4]
  sp1$col[sp1$seasonal==5]<-colRamp[5]

  
  mainLabel <- paste0(sp_list[i], " (", sp_info$English.name,")")
  if( sp_info$Breeder ==1 ){brStatus <- "Breeds in NW Europe"}else{brStatus <- "Does not breed in NW Europe"}
  

  png(filename=paste0(output,sp_info$HH_BirdType,"/",sp_list[i],".png"),
      width=2000, height=2000,units="px", pointsize=12, res=300)
  plot(newmap, xlim=c(-25,45), ylim=c(30,90),asp=1,main=mainLabel)
  title(main = paste0(sp_info$Global.IUCN.Red.List.Category, " | ", sp_info$HH_BirdType, " | ", brStatus),  line = -0.8, cex.main = 0.9)

  plot(sp1,col=sp1$col, border=sp1$col, add=T)
  plot(newmap,asp=1,add=T)
  legend("bottomright", leg.txt, pch = 15, col = colRamp, cex = 0.75)
  
  dev.off()

}



