

#read in env data, add labels and check for covariance
readENVdata<-function(sp_name, modeltype, coastClip=NA){
  
  ##read in terrestrial points
  if(modeltype %in% c("TerrOnly","TerrwithMar")){
    
    
    #select what variables you want the model to work with
    if (modeltype=="TerrOnly"){
      varList<-c()
      for(lim in MarLayerNames){
        varList<-c(varList, paste0("MEAN_", lim), paste0("SD_", lim))
      }
      
      envt.st<-stack(paste0("./IntermediateOutput/CombineEnvData/TerrwithMarineVar_5min_",sp_list[1],".tif"))
      names(envt.st)<-c(TerrLayerNames,
                        "origIndex", "ID", "distToSea", "direToSea", "nearestSeaCell", 
                        varList)
      
      #cut down to just terrestrial layers
      envt.st<-envt.st[[SDMenvLand]]
      
      #a few manual checks on values if required
      #envt.st$Prec_BreedMonths[envt.st$Prec_BreedMonths==0]<-NA
      #envt.st$distToSea[envt.st$distToSea<0]<-0
      
      #log for normality if required
      #envt.st$Prec_BreedMonths<-log10(envt.st$Prec_BreedMonths)
      #envt.st$distToSea<-log10(envt.st$distToSea+1)
      #envt.st$Isol<-log10(envt.st$Isol+0.1)
      #envt.st$Area<-log10(envt.st$Area)
      
      

      #set codes
      layercodes <<- c(SDMenvLand)
      
      #construct frame of label names
      e1 <- new.env()
      for(gg in 1:length(SDMenvLand)){ e1[[SDMenvLand[gg]]] <-  terrLabels[gg]}
      prettynames<<-as.list(e1)
      
      
    }
    
    if (modeltype=="TerrwithMar"){
      
      #load all env variables
      varList<-c()
      for(lim in MarLayerNames){
        varList<-c(varList, paste0("MEAN_", lim), paste0("SD_", lim))
      }
      
      envt.st<-stack(paste0("./IntermediateOutput/CombineEnvData/TerrwithMarineVar_5min_",sp_list[1],".tif"))
      names(envt.st)<-c(TerrLayerNames,
                        "origIndex", "ID", "distToSea", "direToSea", "nearestSeaCell", 
                        varList)
      
      
      #a few manual checks on values
      #envt.st$Prec_BreedMonths[envt.st$Prec_BreedMonths==0]<-NA
      #envt.st$distToSea[envt.st$distToSea<0]<-0
      
      #log for normality
      #envt.st$Prec_BreedMonths<-log10(envt.st$Prec_BreedMonths)
      #envt.st$distToSea<-log10(envt.st$distToSea+1)
      #envt.st$Isol<-log10(envt.st$Isol+0.1)
      #envt.st$Area<-log10(envt.st$Area)
      
      
      #sort out mean of parameters surrounding marine area and add to env stack
      if (terrmartype=="mean"){
        
        SDMarsubset<-SDMenvMarine[SDMenvMarine!="distToLand"]
        #MeanTemp and NDVI_mean are very correlated
        #have to drop either SD_bathy or MEAN_bathy as they are so correlated
        #have to drop either Area or Isol as they are very correlated (drop Area)
        envt.st<-subset(envt.st, c(SDMenvLand, paste0("MEAN_", SDMarsubset)))
        
        
        
        #set codes
        layercodes <<- c(SDMenvLand, paste0("MEAN_",SDMarsubset))
        
        #construct frame of label names
        e1 <- new.env()
        for(gg in 1:length(SDMenvLand)){ 
          e1[[SDMenvLand[gg]]] <-  terrLabels[gg]
        }
        for(gg in 1:length(SDMarsubset)){ 
          e1[[paste0("MEAN_",SDMarsubset[gg])]] <-  paste0("Mean of ", marLabels[gg])
        }
        
        prettynames<<-as.list(e1)
      }
      
      #sort out SD of parameters of surrounding marine area and add to env stack
      if (terrmartype=="sd"){
        
        #we don't care about distance from land, so get rid of it
        SDMarsubset<-SDMenvMarine[SDMenvMarine!="distToLand"]
        envt.st<-subset(envt.st, c(SDMenvLand, paste0("SD_", SDMarsubset)))
        

        #set codes
        layercodes <<- c(SDMenvLand, paste0("SD_",SDMarsubset))
        
        #construct frame of label names
        e1 <- new.env()
        for(gg in 1:length(SDMenvLand)){ 
          e1[[SDMenvLand[gg]]] <-  terrLabels[gg]
        }
        for(gg in 1:length(SDMarsubset)){ 
          e1[[paste0("SD_",SDMarsubset[gg])]] <-  paste0("Variance of ", marLabels[gg])
        }
        
        prettynames<<-as.list(e1)
        
      }
      
      #any log adjustments needed
      #envt.st$SD_bathy<-log10(envt.st$SD_bathy+0.01)
      
    }
    
    
    #some final cropping and cleaning
    
    #remove any points further from the sea than distance defined coastClip
    if(!is.na(coastClip)){
      envt.st[envt.st$distToSea>=coastClip]<-NA
      
    }
    
    
    #as a final step mask any coastal points not in the OSPAR region
    envt.st<-mask(envt.st, ospar)
    
    
    #names are lost, reinsert
    names(envt.st)<-layercodes
    #plot(envt.st)
  }
  
  
  
  #now the marine stuff
  if(modeltype %in% c("MarOnly","MarwithTerr")){
  
    
    envt.st<-stack(paste0("./IntermediateOutput/CombineEnvData/MarinewithTerrVar_5min.tif"))
    names(envt.st)<-c(MarLayerNames,
                      "origIndex", "ID", "distToLand", "direToLand", "nearestLandCell", 
                      paste0("NLand_", TerrLayerNames))
    

    
    #keep nearest land cell for later use
    indLayer<-envt.st$nearestLandCell
    
    #log if needed
    #envt.st$max_chloro<-log10(envt.st$max_chloro)
    #envt.st$NLand_Isol<-log10(envt.st$NLand_Isol+0.1)
    #envt.st$NLand_Area<-log10(envt.st$NLand_Area)
    


    if(modeltype=="MarOnly"){
      #cut down to just marine layers
      envt.st<-envt.st[[SDMenvMarine]]
      
      layercodes <<- c(SDMenvMarine)
      #construct frame of label names
      e1 <- new.env()
      for(gg in 1:length(SDMenvMarine)){ e1[[SDMenvMarine[gg]]] <-  marLabels[gg]}
      prettynames<<-as.list(e1)
    }
    
    if(modeltype=="MarwithTerr"){
      #have to remove area of nearest land as it's massively correlated with isolation
      #have to remove temp of nearest land as it's massivly correlation with SST
      #have to remove bathymetry as it is strongly correlated to max_chloro
      #NDVI mean and min are highly correlated, remove mean
      
      #we don't care about distance from sea, so get rid of it
      SDLandsubset<-SDMenvLand[SDMenvLand!="distToSea"]
      envt.st<-subset(envt.st, c(SDMenvMarine, paste0("NLand_", SDLandsubset)))
      
      layercodes <<- c(SDMenvMarine, paste0("NLand_", SDLandsubset))

      
      #construct frame of label names
      e1 <- new.env()
      for(gg in 1:length(SDMenvMarine)){ 
        e1[[SDMenvMarine[gg]]] <-  marLabels[gg]
      }
      for(gg in 1:length(SDLandsubset)){ 
        e1[[paste0("NL_",SDLandsubset[gg])]] <-  paste0(terrLabels[gg], " of nearest land")
      }
      
      prettynames<<-as.list(e1)

    }
    
    
    #some final cropping and cleaning
    
    #envt.orig<<-envt.st
    
    #remove any points further from the sea than distance defined coastClip
    if(!is.na(coastClip)){
      envt.st[envt.st$distToLand>=coastClip]<-NA
    }
    
    
    #as a final step mask any coastal points not in the OSPAR region
    envt.st<-mask(envt.st, ospar)
    
    
    #get rid of lingering med points
    #envt.st2<-envt.st
    #envt.st2[which(coordinates(envt.st2)[,2]<40 & coordinates(envt.st2)[,1]>-2)]<-NA
    

    #names are lost, reinsert
    names(envt.st)<-layercodes
    #hist(envt.st)
    
  }
  
  
  
  
  
  
  
  
  #plot(envt.st)
  ###Check for correlations between variables###
  options(sdmpredictors_datadir="./IntermediateOutput/SeabirdSDM")
  env_correlations <- pearson_correlation_matrix(envt.st)
  p2 <- plot_correlation(env_correlations, prettynames)
  p2
  #as a general rule we don't want any correlations over 0.7
  #env_correlations
  #(env_correlations>=0.7 | env_correlations<=-0.7)
  if(sum(env_correlations>=0.7 | env_correlations<=-0.7)>length(names(envt.st))){
    env_correlations>=0.7 | env_correlations<=-0.7
    print(env_correlations)
    warning(paste("WARNING: In", sp_name, modeltype, "one or more variables correlate more than 0.7"))
    
    ans<-readline(prompt="HIGH COVARIANCE! Hit 'n' to stop the script, hit any other key to continue: \n")
    
    if (ans=='n' | ans =='N'){ stop("SCRIPT STOPPED TO FIX COVARIANCE") }
    
  }
  
  
  print("Environmental data loaded!")
  return(envt.st)
  
}


readOCCdata<-function(sp_name, modeltype, envt.st, coastClip=NA){
  
  #Read in occ
  spdf<-readOGR(paste0("./InputData/OccData/",sp_name))
  
  
  points<-as.data.frame(coordinates(spdf))
  points$species<-sp_name
  points$Response<-spdf$SP
  #plot(points$coords.x1,points$coords.x2,col=as.factor(points$Response))
  points<-points[,c(3,1,2,4)]
  
  
  #Get data points
  #points <- cbind(points, rep.int(1, length(nrow(points)))); #Adds another column indicating these are presence points
  colnames(points) <- c("Species", "X", "Y", "Response");
  
  #this can be changed but we set it so we only have presences
  points<-points[points$Response==1,]
  
  #get count of total points
  #get count of points that only exist on land/sea (as relevant)
  #get number of points that lie within our coastal boundary
  
  #ranges may need to be clipped by foraging distance, these are set
  maxfor<-forDist[which(sp_names==sp_name)]

  
  rasvals<-extract(envt.st, points[,2:3])
  #points_valid are points that have all relevant information and can be used in model (Terr/Marine)
  points_valid<-points[complete.cases(rasvals),]
  
  
  if (!is.na(coastClip)){
    #points_range are points that are valid AND are within range of the coast line (as defined by coastClip)
    if(modeltype %in% c("TerrOnly", "TerrwithMar")){
      points_range<-points[complete.cases(rasvals) & rasvals[,"distToSea"] <= coastClip,]
    }
    if(modeltype %in% c("MarOnly", "MarwithTerr")){
      points_range<-points[complete.cases(rasvals) & rasvals[,"distToLand"] <= coastClip,]
    }
    
  }else{
    points_range<-points[complete.cases(rasvals),]
  }
  

  print(paste("Total points:", nrow(points)))
  #print(paste0("Valid points (",modeltype,"): ", nrow(points)-())
  print(paste0("Valid points within range of shore (",modeltype,"): ", nrow(points_range)))
  
  
  #save a occurrence plot
  newmap <- getMap(resolution = "low") 
  
  png(filename=paste0("./IntermediateOutput/SeabirdSDM/OccMap/",sp_name,"_rangemap.png"), width=8, height=8, units="in", res=300)

  #add valid points that are also in range
  plot(points_range[,2:3],col="blue", pch=3, cex=0.1)
  plot(newmap,add=T)
  
  title(cex.sub = 1.25, main = paste(sp_name, "Occurrence map"))
  
  dev.off()
  
  print("occurrence data loaded!")
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

