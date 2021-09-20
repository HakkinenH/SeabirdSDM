



readENVdata<-function(sp_name, modeltype, coastClip=NA){

  

    
    #### NOTE BECAUSE I DIDN'T RUN PREPENVDATA ALL THE WAY FROM THE TOP, 
    #there is now manual fudge here where I add land cover manually
    #this can be corrected by running prepenvdata from the top!
    
    #Get environmental variables
    envt.st<-stack(paste0("../PrepEnvData/TerrwithMarineVar_5min_",sp_name,".tif"))
    
    names(envt.st) <- c("MeanTemp_WM", "Prec_BreedMonths", "Isol",
                        "Area", "LandCover", "distToSea","nearestSeaCell",
                        "NDVI_mean", "NDVI_min",
                        "MEAN_SST_WS","MEAN_mean_sal","MEAN_max_chloro",
                        "MEAN_bathy","MEAN_distShore","MEAN_Amarinus",
                        "SD_SST_WS","SD_mean_sal","SD_max_chloro",
                        "SD_bathy","SD_distShore","SD_Amarinus" )
    
    #manually add landcover (for now)
    #backup<-stack(paste0("../PrepEnvData/TerrEnvVariables_5m.tif"))
    #names(backup) <- c("MeanTemp_WM", "Prec_BreedMonths", "Isol", 
    #                    "Area","LandCover", "distToSea","nearestSeaCell")
    
    #envt.st$LandCover<-backup$LandCover
    
    #ospar<-shapefile("C:/Users/Henry/Documents/Research/ZSL/ProcessedData/OSPAR_region/ospar_LandSea.shp")
    
    
    #a few manual checks on values
    envt.st$Prec_BreedMonths[envt.st$Prec_BreedMonths==0]<-NA
    envt.st$distToSea[envt.st$distToSea<0]<-0
    
    #log for normality
    envt.st$Prec_BreedMonths<-log10(envt.st$Prec_BreedMonths)
    #envt.st$distToSea<-log10(envt.st$distToSea+1)
    envt.st$Isol<-log10(envt.st$Isol+0.1)
    envt.st$Area<-log10(envt.st$Area)
    
    
    
    if (terrmartype=="mean"){
      
      #MeanTemp and NDVI_mean are very correlated
      #have to drop either SD_bathy or MEAN_bathy as they are so correlated
      #have to drop either Area or Isol as they are very correlated (drop Area)
      envt.st<-subset(envt.st, c("MeanTemp_WM", 
                                 "distToSea",
                                 "MEAN_SST_WS","MEAN_mean_sal"))
      
      layercodes <<- c("MeanTemp_WM", 
                       "distToSea",
                       "MEAN_SST_WS","MEAN_mean_sal" 
                       )
      
      prettynames <<- list(MeanTemp_W="Mean Winter Temp",
                           distToSea="Dist to Sea (logged km)",
                           MEAN_SST_WS = "Mean Winter SST", MEAN_mean_sal = "Mean salinity")
    }
    

    
  
  
  
  #some final cropping and cleaning
  
  #remove any points further from the sea than distance defined coastClip
  if(!is.na(coastClip)){
    envt.st[envt.st$distToSea>=coastClip]<-NA
    
  }
  
  
  #as a final step mask any coastal points not in the OSPAR region
  envt.st<-mask(envt.st, ospar)
  
  
  #get rid of med
  envt.st2<-envt.st
  envt.st2[which((coordinates(envt.st2)[,2]<47.5 & coordinates(envt.st2)[,1]>1) |
                   (coordinates(envt.st2)[,2]<42.1 & coordinates(envt.st2)[,1]>-5))]<-NA
  
  envt.st3<-envt.st2
  #get rid of any lingering greenland points
  envt.st3[which((coordinates(envt.st3)[,2]>67 & coordinates(envt.st3)[,1]<=-10) |
                   (coordinates(envt.st3)[,2]>59 & coordinates(envt.st3)[,1]<=-30))]<-NA
  
  #check plot
  #plot(envt.st3$MeanTemp_WM)
  #the bits we've just removed if you want to see
  #plot(is.na(envt.st3$MeanTemp_WM) & !is.na(envt.st$MeanTemp_WM))
  
  envt.st<-envt.st3
  #names are lost, reinsert
  names(envt.st)<-layercodes
  #plot(envt.st)

  
  

  
  
  
  #plot(envt.st)
  ###Check for correlations between variables###
  options(sdmpredictors_datadir="C:/Users/Henry/Documents/Research/ZSL/Repos/SeabirdCCVA/IntermediateOutput/SDM")
  env_correlations <- pearson_correlation_matrix(envt.st)
  p2 <- plot_correlation(env_correlations, prettynames)
  p2
  #as a general rule we don't want any correlations over 0.7
  #env_correlations
  #(env_correlations>=0.7 | env_correlations<=-0.7)
  if(sum(env_correlations>=0.7 | env_correlations<=-0.7)>length(names(envt.st))){
    env_correlations>=0.7 | env_correlations<=-0.7
    print(env_correlations)
    warning("WARNING: one or more variables correlate more than 0.7")
  }
  
  return(envt.st)
  
}







readENVdata2100<-function(sp_name, modeltype, coastClip=NA){
  

    #select what variables you want the model to work with

    if (modeltype=="TerrwithMar"){
      
      #### NOTE BECAUSE I DIDN'T RUN PREPENVDATA ALL THE WAY FROM THE TOP, 
      #there is now manual fudge here where I add land cover manually
      #this can be corrected by running prepenvdata from the top!
      
      #Get environmental variables
      envt.st<-stack(paste0("../Prep2100EnvData/TerrwithMarineVar2100_5min_",sp_name,".tif"))
      
      names(envt.st) <- c("MeanTemp_WM",
                          "distToSea","nearestSeaCell", "origIndex",
                          "MEAN_SST_WS","MEAN_mean_sal",
                          "SD_SST_WS", "SD_mean_sal")

      #manually add landcover (for now)
      #backup<-stack(paste0("../PrepEnvData/TerrEnvVariables_5m.tif"))
      #names(backup) <- c("MeanTemp_WM", "Prec_BreedMonths", "Isol", 
      #                    "Area","LandCover", "distToSea","nearestSeaCell")
      
      #envt.st$LandCover<-backup$LandCover
      
      #ospar<-shapefile("C:/Users/Henry/Documents/Research/ZSL/ProcessedData/OSPAR_region/ospar_LandSea.shp")
      
      
      #a few manual checks on values

      envt.st$distToSea[envt.st$distToSea<0]<-0
      

      
      
      
      if (terrmartype=="mean"){
        
        #MeanTemp and NDVI_mean are very correlated
        #have to drop either SD_bathy or MEAN_bathy as they are so correlated
        #have to drop either Area or Isol as they are very correlated (drop Area)
        envt.st<-subset(envt.st, c("MeanTemp_WM",
                                   "distToSea",
                                   "MEAN_SST_WS","MEAN_mean_sal"))
        
        layercodes <<- c("MeanTemp_WM",
                         "distToSea",
                         "MEAN_SST_WS","MEAN_mean_sal")
        
        prettynames <<- list(MeanTemp_W="Mean Winter Temp", 
                             distToSea="Dist to Sea (logged km)",
                             MEAN_SST_WS = "Mean Winter SST", 
                             MEAN_mean_sal = "Mean salinity")
      }
      
      

      
    
    
    
    
    #some final cropping and cleaning
    
    #remove any points further from the sea than distance defined coastClip
    if(!is.na(coastClip)){
      envt.st[envt.st$distToSea>=coastClip]<-NA
      
    }
    
    
    #as a final step mask any coastal points not in the OSPAR region
    envt.st<-mask(envt.st, ospar)
    
    
    #get rid of med
    envt.st2<-envt.st
    envt.st2[which((coordinates(envt.st2)[,2]<47.5 & coordinates(envt.st2)[,1]>1) |
                     (coordinates(envt.st2)[,2]<42.1 & coordinates(envt.st2)[,1]>-5))]<-NA
    
    envt.st3<-envt.st2
    #get rid of any lingering greenland points
    envt.st3[which((coordinates(envt.st3)[,2]>67 & coordinates(envt.st3)[,1]<=-10) |
                     (coordinates(envt.st3)[,2]>59 & coordinates(envt.st3)[,1]<=-30))]<-NA
    
    #check plot
    #plot(envt.st3$MeanTemp_WM)
    #the bits we've just removed if you want to see
    #plot(is.na(envt.st3$MeanTemp_WM) & !is.na(envt.st$MeanTemp_WM))
    
    envt.st<-envt.st3
    #names are lost, reinsert
    names(envt.st)<-layercodes
    #plot(envt.st)
  }
  
  
  
  
  
  
  #plot(envt.st)
  ###Check for correlations between variables###
  options(sdmpredictors_datadir="C:/Users/Henry/Documents/Research/ZSL/Repos/SeabirdCCVA/IntermediateOutput/SDM")
  env_correlations <- pearson_correlation_matrix(envt.st)
  p2 <- plot_correlation(env_correlations, prettynames)
  p2
  #as a general rule we don't want any correlations over 0.7
  #env_correlations
  #(env_correlations>=0.7 | env_correlations<=-0.7)
  if(sum(env_correlations>=0.7 | env_correlations<=-0.7)>length(names(envt.st))){
    env_correlations>=0.7 | env_correlations<=-0.7
    print(env_correlations)
    warning("WARNING: one or more variables correlate more than 0.7")
  }
  
  return(envt.st)
  
}





readOCCdata2100<-function(sp_name, modeltype, envt.st, coastClip=NA){
  
  #Read in occ
  #special case for Mbassanus
  if (sp_name == "Mbassanus"){
    
    if(modeltype %in% c("TerrOnly","TerrwithMar")){
      spdf<-readOGR(paste0("../PrepOccData/",sp_name,"BreBreedingSp.tif"))
    }else{
      spdf<-readOGR(paste0("../PrepOccData/",sp_name,"BreResBreedingSp.tif"))
    }
    
  }else{
    spdf<-readOGR(paste0("../PrepOccData/",sp_name,"BreedingSp.tif"))
  }
  
  
  points<-as.data.frame(coordinates(spdf))
  points$species<-sp_name
  points<-points[,c(3,1,2)]
  
  
  #Get data points
  points <- cbind(points, rep.int(1, length(nrow(points)))); #Adds another column indicating these are presence points
  colnames(points) <- c("Species", "X", "Y", "Response");
  
  
  #get count of total points
  #get count of points that only exist on land/sea (as relevant)
  #get number of points that lie within our coastal boundary
  
  #ranges may need to be clipped by foraging distance, these are set
  if(sp_name=="Farctica"){maxfor<-40000}
  if(sp_name=="Mbassanus"){maxfor<-200000}
  if(sp_name=="Sdougallii"){maxfor<-16600}
  
  rasvals<-extract(envt.st, points[,2:3])
  #points_valid are points that have all relevant information and can be used in model (Terr/Marine)
  points_valid<-points[complete.cases(rasvals),]
  
  if (!is.na(coastClip)){
    #points_range are points that are valid AND are within range of the coast line (as defined by coastClip)
    if(modeltype %in% c("TerrOnly", "TerrwithMar")){
      points_range<-points[complete.cases(rasvals) & rasvals[,"distToSea"] <= coastClip,]
    }
    if(modeltype %in% c("MarOnly", "MarwithTerr")){
      points_range<-points[complete.cases(rasvals) & rasvals[,"distShore"] <= coastClip,]
    }
    
  }else{
    points_range<-points[complete.cases(rasvals),]
  }
  
  
  #for puffins and terns we have a defined marine range so can leave as is
  #for gannets there is no distinction between breeding and resident so we clip to max breeding range
  # if(sp_name == "Mbassanus" & modeltype %in% c("MarOnly", "MarwithTerr")){
  #   if (!is.na(coastClip)){
  #     points_range<-points[complete.cases(rasvals) & 
  #                          rasvals[,"distShore"] <= coastClip &
  #                          rasvals[,"distShore"] < 200000,]
  #   }else{
  #     points_range<-points[complete.cases(rasvals) & 
  #                            rasvals[,"distShore"] < 200000,]
  #   }
  #}
  
  print(paste("Total points:", nrow(points)))
  #print(paste0("Valid points (",modeltype,"): ", nrow(points)-())
  print(paste0("Valid points within range of shore (",modeltype,"): ", nrow(points_range)))
  
  
  #save a occurrence plot
  library(rworldmap)
  newmap <- getMap(resolution = "low") 
  
  png(filename=paste0(sp_name,"_rangemap.png"), width=8, height=8, units="in", res=300)
  plot(points[,2:3],col="blue", pch=3, cex=0.1, 
       ylab="", xlab=paste0("Blue: Invalid  Green: valid and in range (km)"))
  plot(newmap,add=T)
  
  #add valid points that are also in range
  points(points_range[,2:3],col="Green", pch=3, cex=0.1)
  
  title(cex.sub = 1.25, main = paste(sp_name, "Occurrence map"))
  dev.off()
  
  
  #nvalid<<-nrow(rasvals_all)
  return(points_range)
}



buildGreenlandIndex<-function(){
  
  #construct an index for the NE atlantic and find the indecies for Greenland
  
  searchpath<-"C:/Users/Henry/Documents/Research/ZSL/RawData/WorldClim/wc2.1_5m_tavg"
  file_list<-list.files(path= searchpath,pattern="*.tif")
  TerrPre<-raster(paste0(searchpath,"/",file_list[1]))
  names(TerrPre)<-"MeanTemp_W"
  
  ne.atlantic.ext <- extent(-100, 55, 30, 90)
  
  Terr.crop <-crop(TerrPre, ne.atlantic.ext)
  Terr.crop$ID<-1:length(Terr.crop$MeanTemp_W)
  
  Terrdf_full<-as.data.frame(Terr.crop)
  Terrdf_full$x<-coordinates(Terr.crop)[,1]
  Terrdf_full$y<-coordinates(Terr.crop)[,2]
  
  #choose only land cellse
  Terrdf_l<-Terrdf_full[!is.na(Terrdf_full$MeanTemp_W),]
  
  #crop to boundaries of greenland
  Terrdf<-Terrdf_l[which( (Terrdf_l$x <= -7 & Terrdf_l$y>75) |
                            (Terrdf_l$x <= -13 & Terrdf_l$y>67.5) | 
                            (Terrdf_l$x <= -28 & Terrdf_l$y>59) ), ]
  
  plot(Terrdf_l$x,Terrdf_l$y,cex=0.1)
  points(Terrdf$x, Terrdf$y, cex=0.2, col="green")
  
  #strictly this isn't just greenland and includes bits of North America
  #doesn't matter though
  write.csv(Terrdf$ID, "GreenlandIndices.csv")
  print("Done!")
}

