

############################################################
############################################################
### SEABIRD SDM PART 1 ###
############################################################
############################################################

### META ###
# by HHakkinen
# Start Date: 25/01/2021
# End Date: 08/10/2021
# As part of Institute of Zoology ZSL / University of Cambridge project
# "Terrestrial or marine species distribution model - Why not both? A case study with seabirds"
# 
# The following runs SDM models for seabird species in a defined study region (currently OSPAR)
# it runs several algorithms and makes a composite model
# outputs various plots and maps as an output

# parameters for the model are set in the wrapper file, but it should be run after CombineEnvData
#output is saved in IntermediateData/SeabirdSDM

### /META ###




############################################################
### LOAD BASE FILES ###
############################################################



#load OSPAR map
ospar<-shapefile(studyOutline)
os.ext<-extent(ospar)

#load simplified map for plotting purporse
landsh<-shapefile("./InputData/StudyArea/SimpleLandOutline_wgs84.shp")

#set colour palette
myPal <- (RColorBrewer::brewer.pal('YlGn', n=9))
myTheme <- rasterTheme(region = myPal)


#set sdm parameter to supress warnings
options(sdmpredictors_datadir="./IntermediateOutput/SDM")
i<-1; j<-1; k<-1


############################################################
### RUN MODELS ###
############################################################



for (i in 1:length(sp_list)){
  sp_name<-sp_list[i]

  
  for(j in 1:length(realmlist)){
    modeltype<-realmlist[j]

    #check for manual override to coastal clip, otherwise use default value
    if (!(is.na(manCoastDist))){
      if(manCoastDist<=0){coastClip<-NA}
      if(manCoastDist>0){coastClip<-manCoastDist}
    }else{
      #use defaults
      #set the coastal crop point, how far from the sea or land do we want calculations to include
      if (modeltype %in% c("TerrOnly", "TerrwithMar")){
        #if inland we accept up to 20km
        coastClip <- 20000
      }
      if (modeltype %in% c("MarOnly", "MarwithTerr")){
        #if at sea we accept up to the foraging distance
        coastClip <- forDist[i]*1.2
      }
    }
    

    for(k in 1:length(terrmarlist)){
      
      terrmartype<-terrmarlist[k]
      
      start.t<-Sys.time()
      

      print(sp_name)
      print(modeltype)

      #load environmental data
      #will return warning if there is too much covariation (>0.7 in a pearson correlation)
      envt.st<-readENVdata(sp_name, modeltype, coastClip)
      
      #can check covariance if you want
      #env_correlations <- pearson_correlation_matrix(envt.st)
      #p2 <- plot_correlation(env_correlations, prettynames)
      #p2
      #env_correlations
      
      #load occurrence data
      #note that this sets a variable nvalid which is used later
      points<-readOCCdata(sp_name, modeltype, envt.st, coastClip)
      summary(points)
      
      #check how many points we have
      pcheck<-as.data.frame(extract(envt.st,points[,2:3]))
      pcheck$x<-points$X
      pcheck$y<-points$Y
      pcheck2<-pcheck[complete.cases(pcheck),]
      #plot(pcheck2$x, pcheck2$y,col="green",pch=3,cex=1.2)
      npoints<-nrow(pcheck2)
      
      
      #if nvalid is not set number of pseudoabsences set to number of valid points
      if(is.na(nvalid)){
        nps<-npoints
      }else{
        nps<-nvalid
      }

      
      #run the actual model, calls code and functions from SDM file
      source("./Code/Dependencies/SDM_functions.R")
      end.t<-Sys.time()
      
      print(end.t-start.t)
      cat("")


    }
    
  }
  
  
  
}











