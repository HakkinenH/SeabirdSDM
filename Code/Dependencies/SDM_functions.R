
############################################################
############################################################
### SEABIRD SDM FUNCTIONS ###
############################################################
############################################################

### META ###
# by HHakkinen
# Start Date: 25/01/2021
# End Date: 08/10/2021
# As part of Institute of Zoology ZSL / University of Cambridge project
# "Terrestrial or marine species distribution model - Why not both? A case study with seabirds"
# 
# Called from SeabirdSDM.R. Not currently in the form of proper functions to make debugging easier. 
# After loading occ data and env data (in SDM_functionsPre.R), checking formatting and covariance this file is run
#there are several sections, broadly this file has:
# 1) FORM DATA INTO BIOMOD SET: merges data in preparation for BIOMOD
# 2) RUN SDM MODEL: run individual models (algorithms, pseudo-absence sets and validation sets)
# 3) Get TSS scores: get scores and ratings from individual models, saved for inspection
# 4) VARIABLE IMPORTANCE: estimate variable importance using pearson scores, saves output for inspection
# 5) plot response curves: as it sounds, plots of how env variables predict prob of occurrence
# 6) ENSEMBLE MODEL: screens (based on threshold) and combines weighted mean ensemble
# 7) check pearson scores: if using pseudo-absences the code will check the degree of match between the two types of model (validation and full)
#
#output is all saved intermediateData/SeabirdsSDM

### /META ###

#seabirdSDM<-function(sp_name, modeltype, points, envt.st, model_list, pseudodraws, evalruns, terrmartype=NULL){
  
  ############################################################
  ### FORM DATA INTO BIOMOD SET ###
  ############################################################
  #Setting up data file for Biomod2
  
  #convert landcover to cat if present
  if("LandCover" %in% names(envt.st)){
    envt.st$LandCover<-as.factor(envt.st$LandCover)
  }
  

  envt.st<-stack(envt.st)
  
  
  bmData <- BIOMOD_FormatingData(resp.var = points[,4],
                                 resp.xy = points[,2:3], 
                                 resp.name = as.character(points[1,1]),
                                 expl.var = envt.st,
                                 PA.nb.rep=pseudodraws, #if using pseudo absences use pseudodraws
                                 PA.nb.absences = nps, #if using pseudo absences use nps
                                 PA.strategy = 'random',
                                 na.rm=T
  
                                 );
  

  
  #check for spatial autocorrelation
  #library(gstat)
  #c.variog <- variogram(SP ~1, data=spdf, xlab= "Distance (m)", ylab="Semivariance")
  #plot(c.variog)
  
  
  #ps<-bmData@data.species
  #ps[is.na(ps)]<-0
  
  #x11()
  #plot to show where pseudo-absences are
  #plot(envt.st$MeanTemp_WM,col="grey")
  #points(bmData@coord, col=as.factor(ps),pch=3, cex=0.1)
  
  
  #Setting up Maxent run
  myBiomodOption <- Print_Default_ModelingOptions();
  myBiomodOption@MAXENT.Phillips$path_to_maxent.jar = paste(system.file(package="dismo"), "/java", sep='');
  myBiomodOption@MAXENT.Phillips$memory_allocated = 2048; #Allocates 2048 MB/2 GB of memory to modeling
  myBiomodOption@MAXENT.Phillips$maximumiterations = 50000;
  myBiomodOption@MAXENT.Phillips$threshold = F;
  myBiomodOption@MAXENT.Phillips$hinge = F;
  myBiomodOption@MAXENT.Phillips$visible = F;
  myBiomodOption@MAXENT.Phillips$beta_lqp = .95;
  
  
  myBiomodOption@GAM$control$maxit = 1000
  myBiomodOption@ANN$maxit = 1000
  
  
  ############################################################
  ### RUN SDM MODEL ###
  ############################################################
  
  #library(maxent)
  #BIOMOD_ModelingOptions()
  #NbPseudoAbsences * NbRunEval + 1 models will be created
  PIPO.mod <-BIOMOD_Modeling(data = bmData,models =model_list,
                             SaveObj = TRUE,
                             models.options = myBiomodOption,
                             DataSplit = 70, 
                             NbRunEval=evalruns,
                             do.full.models = T,
                             # common practice to validate!
                             models.eval.meth = c('TSS', 'ROC', 'ACCURACY'),
                             VarImport = 1)
  curID<-PIPO.mod@modeling.id
  
  #load model if needed and explore in detail
  #load("Mbassanus/models/1614879030/Mbassanus_PA1_Full_GLM")

  #glmres<-get_formal_model(Mbassanus_PA1_Full_GLM)
  #gamres<-get_formal_model(Mbassanus_PA1_Full_GAM)
  #rfres<-get_formal_model (Mbassanus_PA1_RUN1_RF)
  #annres<-get_formal_model (Mbassanus_PA1_RUN1_ANN)
  #maxres<-get_formal_model (Mbassanus_PA1_RUN1_MAXENT.Phillips)
  
  #summary(glmres)
  #summary(gamres)
  #summary(rfres)
  #summary(annres)
  #summary(maxres)
  
  
  
  
  
  
  ############################################################
  ### Get TSS (and other validation) scores ###
  ############################################################
  
  fulltss <-get_evaluations(PIPO.mod)
  #dimensions are climate variables, model type, run, pseudo-absence
  
  #can look at all metric results
  fulltss["TSS","Testing.data",,,]
  fulltss["ACCURACY","Testing.data",,,]
  fulltss["ROC","Testing.data",,,]
  
  #check output folders exist, otherwise create them
  if(!dir.exists(paste0("./IntermediateOutput/SeabirdSDM/OutputTables/",sp_name,"/"))){
    dir.create(paste0("./IntermediateOutput/SeabirdSDM/OutputTables/",sp_name))
  }
  if(!dir.exists(paste0("./IntermediateOutput/SeabirdSDM/OutputTables/",sp_name,"/",modeltype))){
    dir.create(paste0("./IntermediateOutput/SeabirdSDM/OutputTables/",sp_name, "/", modeltype))
  }

  if(!dir.exists(paste0("./IntermediateOutput/SeabirdSDM/OutputPlots/",sp_name,"/"))){
    dir.create(paste0("./IntermediateOutput/SeabirdSDM/OutputPlots/",sp_name))
  }
  if(!dir.exists(paste0("./IntermediateOutput/SeabirdSDM/OutputPlots/",sp_name,"/",modeltype))){
    dir.create(paste0("./IntermediateOutput/SeabirdSDM/OutputPlots/",sp_name,"/",modeltype))
  }
  
  #do this two different ways
  #1) get all validation models across all pseudo-absence sets and validation runs. Calc aver and SD
  #2) create an ensemble model containing each model type and validation set, but separated by pseudo-absence set. Calc aver and SD
  TSStab<-t(as.data.frame(fulltss["TSS","Testing.data",,1:evalruns,]))
  write.csv(TSStab,paste0("./IntermediateOutput/SeabirdSDM/OutputTables/",sp_name,"/",modeltype,"/FullTSS.csv"))
  ROCtab<-t(as.data.frame(fulltss["ROC","Testing.data",,1:evalruns,]))
  write.csv(ROCtab,paste0("./IntermediateOutput/SeabirdSDM/OutputTables/",sp_name,"/",modeltype,"/FullROC.csv"))
  ACCtab<-t(as.data.frame(fulltss["ACCURACY","Testing.data",,1:evalruns,]))
  write.csv(ACCtab,paste0("./IntermediateOutput/SeabirdSDM/OutputTables/",sp_name,"/",modeltype,"/FullACC.csv"))
  TSSsensit<-t(as.data.frame(fulltss["TSS","Sensitivity",,1:evalruns,]))
  write.csv(TSSsensit,paste0("./IntermediateOutput/SeabirdSDM/OutputTables/",sp_name,"/",modeltype,"/FullSENSI.csv"))
  TSSspeci<-t(as.data.frame(fulltss["TSS","Specificity",,1:evalruns,]))
  write.csv(TSSspeci,paste0("./IntermediateOutput/SeabirdSDM/OutputTables/",sp_name,"/",modeltype,"/FullSPECI.csv"))
  
  
  TSSmean<-round(mean(TSStab, na.rm=T), digits=4)
  ROCmean<-round(mean(ROCtab, na.rm=T), digits=4)
  ACCmean<-round(mean(ACCtab, na.rm=T), digits=4)
  
  SENSImean<-round(mean(TSSsensit, na.rm=T)/100, digits=4)
  SPECImean<-round(mean(TSSspeci, na.rm=T)/100, digits=4)
  
  TSSsd<-round(sd(TSStab, na.rm=T), digits=4)
  ROCsd<-round(sd(ROCtab, na.rm=T), digits=4)
  ACCsd<-round(sd(ACCtab, na.rm=T), digits=4)
  
  SENSIsd<-round(sd(TSSsensit, na.rm=T)/100, digits=4)
  SPECIsd<-round(sd(TSSspeci, na.rm=T)/100, digits=4)
  
  resRow<-as.data.frame(t(c(sp_name, 
            modeltype, 
            paste0(TSSmean, " (", TSSsd, ")"),
            paste0(ROCmean, " (", ROCsd, ")"),
            paste0(SENSImean, " (", SENSIsd, ")"),
            paste0(SPECImean, " (", SPECIsd, ")"),
            paste0(ACCmean, " (", ACCsd, ")")
            )))
  colnames(resRow)<-c("SPECIES", "MODEL TYPE","TSS", "ROC", "SENSITIVITY", "SPECIFICITY", "ACCURACY")
  write.csv(resRow,
            paste0("./IntermediateOutput/SeabirdSDM/OutputTables/",sp_name,"/",modeltype,"/MEANSD_STATS.csv"))
  
  

  
  ############################################################
  ### VARIABLE IMPORTANCE ###
  ############################################################
  
  #get for full models, work it out across all SDM models and pseudo-absence sets
  
  var.imp <-drop(get_variables_importance(PIPO.mod))
  
  #dimensions change depending on PSA or not, so swap between subsets
  if(pseudodraws>0){var.imp <- var.imp[,,"Full",]
    }else{  var.imp <- var.imp[,,"Full"]}

  vardf<-as.data.frame(apply(var.imp, 1, cbind))
  vardf$Run<-colnames(var.imp)
  
  write.csv(vardf,paste0("./IntermediateOutput/SeabirdSDM/OutputTables/",sp_name,"/",modeltype,"/VarImp.csv"))
  
  
  #calculate mean and SD for each column (arbitrary number of columns, last one is label and ignored)
  varpl<-c()
  varsd<-c()
  for ( q in 1:(ncol(vardf)-1)){
    varpl<-c(varpl, round(mean(vardf[,q], na.rm=T),digits=3))
    varsd<-c(varsd, round(sd(vardf[,q], na.rm=T),digits=3))
  }
  
  
  
  png(filename=paste0("./IntermediateOutput/SeabirdSDM/OutputPlots/",sp_name,"/",modeltype,"/VarImp.png"), width=24, height=8, units="in", res=300)
  barCenters<- barplot(height=varpl, beside=T,
                       xlab="Variable",
                       ylab = "Variable Importance",
                       ylim=c(min(varpl-varsd),max(varpl+varsd)),
                       col=brewer.pal(10, "Spectral"),
                       names.arg = prettynames)
  #legend=prettynames)
  
  segments(barCenters, varpl - varsd, barCenters,
           varpl + varsd, lwd = 1.5)
  arrows(barCenters, varpl - varsd, barCenters,
         varpl + varsd, lwd = 1.5, angle = 90,
         code = 3, length = 0.05)
  
  dev.off()
  
  
  
  ##########################################
  ### plot response curves ###
  #########################################
  
  #One response curve per model
  
  myBiomodProj <-BIOMOD_Projection(modeling.output = PIPO.mod,
                                   new.env =stack(envt.st),
                                   # modern environment
                                   proj.name ='currentALL',
                                   selected.models ='all',
                                   binary.meth ='TSS',
                                   compress ='xz',
                                   clamping.mask = F,
                                   output.format ='.grd')
  
  myBiomodProj.plot <- myBiomodProj
  
  
  for(i in model_list){
    
    print(i)
    png(filename=paste0("./IntermediateOutput/SeabirdSDM/OutputPlots/",sp_name,"/",modeltype,"/ResCurves",i,".png"), width=16, height=8, units="in", res=300)
    response.plot2(models = BIOMOD_LoadModels(PIPO.mod, models=i),
                   Data = get_formal_data(PIPO.mod,'expl.var'),
                   show.variables= get_formal_data(PIPO.mod,'expl.var.names'),
                   do.bivariate = FALSE,
                   fixed.var.metric = 'median',
                   col = brewer.pal(10, "Spectral"),
                   legend = TRUE,
                   data_species = get_formal_data(PIPO.mod,'resp.var') )
    dev.off()
    
  }
  
  
  
  #############################
  ###ENSEMBLE MODEL ###
  #############################
  
  #this is the one that will actually be used in the paper
  # Take mean, median etc ensemble model built on Full data models
  #models are selected by TSS (threshold = 0.6)
  myBiomodEM <-BIOMOD_EnsembleModeling(modeling.output = PIPO.mod,
                                       chosen.models =grep('_Full_', get_built_models(
                                         PIPO.mod), value=TRUE),
                                       em.by ='all',
                                       eval.metric =c('TSS'),
                                       eval.metric.quality.threshold =c(0.6),
                                       prob.mean = F,
                                       prob.mean.weight = T,
                                       prob.cv = T,
                                       prob.ci = F,
                                       prob.median = F,
                                       committee.averaging=F)
  

  
  #Individual models projections on current environmental conditions
  myBiomodProjection <- BIOMOD_Projection( modeling.output = PIPO.mod,
                                           new.env = envt.st,
                                           proj.name = paste0('currentFULL',modeltype),
                                           selected.models = grep('_Full_', get_built_models(
                                             PIPO.mod), value=TRUE),
                                           compress = FALSE,
                                           build.clamping.mask = FALSE)
  
  ensem<-BIOMOD_EnsembleForecasting(myBiomodEM, projection.output=myBiomodProjection)

  
  #first model is means
  #second is coefficent variation across predictions (sd/mean)
  #third is confidence interval upper (98 percentile)
  #fourth is confidence interval around mean (2 percentile)
  #fifth is median
  #sixth is committee Averaging
  #seventh is weighted mean (by TSS)
  ensem@models.projected<-c("Uncertainty","Weighted_Mean")
  
  #extract stack for plotting
  #be careful if you change the ensembling method as indexing may change
  
  ensemst<-ensem@proj@val[[2]]
  ensemun<-ensem@proj@val[[1]]
  

  #ensemst<-crop(ensemst, extent(-30,51,36,85))
  #ensemun<-crop(ensemun, extent(-30,51,36,85))
  
  plwm<-levelplot(ensemst/1000, 
            margin=FALSE, 
            at=seq(0, 1, length=11),
            colorkey=list(height=0.6), 
            main='Weighted_Mean Suitability',
            par.settings = myTheme)
  
  plun<-levelplot(ensemun/1000, 
                  margin=FALSE, 
                  at=seq(0, 1, length=11),
                  colorkey=list(height=0.6), 
                  main='Uncertainty',
                  par.settings = myTheme)
  

  
  png(filename=paste0("./IntermediateOutput/SeabirdSDM/OutputPlots/",sp_name,"/",modeltype,"/Finalensemble.png"), width=8, height=8, units="in", res=600)
  print ( plwm +
    layer(sp.polygons(landsh, col=alpha("grey",0.3))) )
  dev.off()
    
  
  png(filename=paste0("./IntermediateOutput/SeabirdSDM/OutputPlots/",sp_name,"/",modeltype,"/Finalensemble_uncertainty.png"), width=8, height=8, units="in", res=600)
  print ( plun +
    layer(sp.polygons(landsh, col=alpha("grey",0.3))) )
  dev.off()
  
  
  
  
  ############################################################
  ### Pearson correlation between Full and Validation set ###
  ############################################################
  #(also method 2 for above)
  
  #construct a TSS table with an ensemble for each pseudo-absence dataset
  TSS_table<-data.frame(run=character(),TSSvalue=numeric())
  Spear_table<-data.frame(run=character(), SpearRho=numeric())
  
  
  #for each pseudo-absence set make an ensemble model of all validation sets and all model types
  #for each pseudo-absence set make an ensemble model of the full datasets and all model types
  if(pseudodraws>0){
    for (i in 1:pseudodraws){
    print(paste("pseudo:", i))
    #validation model ensemble
    BiomodValEM <-BIOMOD_EnsembleModeling(modeling.output = PIPO.mod,
                                          chosen.models =grep(paste0("PA",i,"_RUN"), 
                                                              get_built_models(PIPO.mod), 
                                                              value=TRUE),
                                          em.by ='all',
                                          eval.metric =c('TSS'),
                                          eval.metric.quality.threshold =c(0.6),
                                          prob.mean = F,
                                          prob.mean.weight = T,
                                          prob.cv = F,
                                          prob.ci = F,
                                          prob.median = F,
                                          committee.averaging=F)
    vald.eval<-get_evaluations(BiomodValEM)

    
    TSS_row<-c(paste0("Valid_PA",i), vald.eval[[1]]["TSS",1])
    TSS_table[nrow(TSS_table)+1,]<-TSS_row
    
    
    #get validation projection
    BiomodValProj <-BIOMOD_Projection(modeling.output = PIPO.mod,
                                      new.env =stack(envt.st),
                                      # modern environment
                                      proj.name ='currentPARUN',
                                      selected.models =grep(paste0("PA",i,"_RUN"), 
                                                            get_built_models(PIPO.mod), 
                                                            value=TRUE),
                                      #binary.meth ='TSS',
                                      compress ='xz',
                                      clamping.mask = F,
                                      output.format ='.grd')
    

    #do ensemble forecast
    ensemVal<-BIOMOD_EnsembleForecasting(BiomodValEM, projection.output=BiomodValProj)
    
    r1 <- getValues(ensemVal@proj@val[[1]])
    
    #full model ensemble
    BiomodFullEM <-BIOMOD_EnsembleModeling(modeling.output = PIPO.mod,
                                           chosen.models =grep(paste0("PA",i,"_Full"), 
                                                               get_built_models(PIPO.mod), 
                                                               value=TRUE),
                                           em.by ='all',
                                           eval.metric =c('TSS'),
                                           eval.metric.quality.threshold =c(0.6),
                                           prob.mean = F,
                                           prob.mean.weight = T,
                                           prob.cv = F,
                                           prob.ci = F,
                                           prob.median = F,
                                           committee.averaging=F)
    
    
    vald.eval<-get_evaluations(myBiomodEM)
    
    
    TSS_row<-c(paste0("Full_PA",i), vald.eval[[1]]["TSS",1])
    TSS_table[nrow(TSS_table)+1,]<-TSS_row
    
    
    BiomodFullProj <-BIOMOD_Projection(modeling.output = PIPO.mod,
                                       new.env =stack(envt.st),
                                       # modern environment
                                       proj.name ='currentPAFULL',
                                       selected.models =grep(paste0("PA", i, "_Full"), 
                                                             get_built_models(PIPO.mod), 
                                                             value=TRUE),
                                       #binary.meth ='TSS',
                                       compress ='xz',
                                       clamping.mask = F,
                                       output.format ='.grd')
    
    
    
    ensemFull<-BIOMOD_EnsembleForecasting(BiomodFullEM, projection.output=BiomodFullProj)
    

    r2 <- getValues(ensemFull@proj@val[[1]])

    
    
    rc <- as.numeric(cor(r1, r2,  method = "spearman", use = "complete.obs"))
    
    Spear_table[nrow(Spear_table)+1,]<-c(paste0("PSAB_",i),as.numeric(rc))
    
    
    } 
  }else{
      print("no pseudo")
      #validation model ensemble
      BiomodValEM <-BIOMOD_EnsembleModeling(modeling.output = PIPO.mod,
                                            chosen.models =grep(paste0("RUN"), 
                                                                get_built_models(PIPO.mod), 
                                                                value=TRUE),
                                            em.by ='all',
                                            eval.metric =c('TSS'),
                                            eval.metric.quality.threshold =c(0.6),
                                            prob.mean = F,
                                            prob.mean.weight = T,
                                            prob.cv = F,
                                            prob.ci = F,
                                            prob.median = F,
                                            committee.averaging=F)
      vald.eval<-get_evaluations(BiomodValEM)
      
      
      TSS_row<-c(paste0("no_PA"), vald.eval[[1]]["TSS",1])
      TSS_table[nrow(TSS_table)+1,]<-TSS_row
      
      
      #get validation projection
      BiomodValProj <-BIOMOD_Projection(modeling.output = PIPO.mod,
                                        new.env =stack(envt.st),
                                        # modern environment
                                        proj.name ='currentRUN',
                                        selected.models =grep(paste0("RUN"), 
                                                              get_built_models(PIPO.mod), 
                                                              value=TRUE),
                                        #binary.meth ='TSS',
                                        compress ='xz',
                                        clamping.mask = F,
                                        output.format ='.grd')
      
      
      #do ensemble forecast
      ensemVal<-BIOMOD_EnsembleForecasting(BiomodValEM, projection.output=BiomodValProj)
      
      r1 <- getValues(ensemVal@proj@val[[1]])
      
      #full model ensemble
      BiomodFullEM <-BIOMOD_EnsembleModeling(modeling.output = PIPO.mod,
                                             chosen.models =grep(paste0("_Full"), 
                                                                 get_built_models(PIPO.mod), 
                                                                 value=TRUE),
                                             em.by ='all',
                                             eval.metric =c('TSS'),
                                             eval.metric.quality.threshold =c(0.6),
                                             prob.mean = F,
                                             prob.mean.weight = T,
                                             prob.cv = F,
                                             prob.ci = F,
                                             prob.median = F,
                                             committee.averaging=F)
      
      
      vald.eval<-get_evaluations(myBiomodEM)
      
      
      TSS_row<-c(paste0("Full_PA",i), vald.eval[[1]]["TSS",1])
      TSS_table[nrow(TSS_table)+1,]<-TSS_row
      
      
      BiomodFullProj <-BIOMOD_Projection(modeling.output = PIPO.mod,
                                         new.env =stack(envt.st),
                                         # modern environment
                                         proj.name ='currentPAFULL',
                                         selected.models =grep(paste0("_Full"), 
                                                               get_built_models(PIPO.mod), 
                                                               value=TRUE),
                                         #binary.meth ='TSS',
                                         compress ='xz',
                                         clamping.mask = F,
                                         output.format ='.grd')
      
      
      
      ensemFull<-BIOMOD_EnsembleForecasting(BiomodFullEM, projection.output=BiomodFullProj)
      
      
      r2 <- getValues(ensemFull@proj@val[[1]])
      
      
      
      rc <- as.numeric(cor(r1, r2,  method = "spearman", use = "complete.obs"))
      
      Spear_table[nrow(Spear_table)+1,]<-c(paste0("PSAB_none"),as.numeric(rc))
      
      
    }  
    
    
  valres<-as.numeric(TSS_table[grep("Valid", TSS_table$run),2])
  TSS_mean<-c("ValMean",round(mean(valres, na.rm=T),digits=4))
  TSS_sd<-c("ValSD",round(sd(valres, na.rm=T),digits=4))
  
  TSS_df<-TSS_table
  TSS_df[nrow(TSS_df)+1,]<-TSS_mean
  TSS_df[nrow(TSS_df)+1,]<-TSS_sd
  
  write.csv(TSS_df,paste0("./IntermediateOutput/SeabirdSDM/OutputTables/",sp_name,"/",modeltype,"/ensTSS.csv"))
  
  spres<-as.numeric(Spear_table[,2])
  Spear_table[nrow(Spear_table)+1,]<-c("SpearMean",mean(spres, na.rm=T))
  Spear_table[nrow(Spear_table)+1,]<-c("SpearSD",sd(spres, na.rm=T))
  
  write.csv(Spear_table,paste0("./IntermediateOutput/SeabirdSDM/OutputTables/",sp_name,"/",modeltype,"/spearman.csv"))
  
  
  
  
  
  #################################################
  ### MODEL EACH INDIVIDUAL MODEL (deprecated)###
  #################################################
  # 
  # for(i in model_list){
  #   print(i)
  #   
  #   #plot mean model of all algorithms for all models
  #   myBiomodProj <-BIOMOD_Projection(modeling.output = PIPO.mod,
  #                                    new.env =stack(envt.st),
  #                                    # modern environment
  #                                    proj.name ='current',
  #                                    selected.models =grep(i, get_built_models(
  #                                      PIPO.mod), value=TRUE),
  #                                    #binary.meth ='TSS',
  #                                    compress ='xz',
  #                                    clamping.mask = F,
  #                                    output.format ='.grd')
  #   
  #   myBiomodProj.plot <- myBiomodProj
  #   #myBiomodProj.plot@models.projected <-model_list
  #   
  #   png(filename=paste0("OutputPlots/",sp_name,"/",modeltype,"/allproj",i,".png"), width=20, height=16, units="in", res=300)
  #   plot(myBiomodProj.plot)
  #   dev.off()
  #   
  #   
  #   #plot the ensemble model and look at all the averaging methods
  #   myBiomodEM <-BIOMOD_EnsembleModeling(modeling.output = PIPO.mod,
  #                                        chosen.models =grep(i, get_built_models(
  #                                          PIPO.mod), value=TRUE),
  #                                        em.by ='all',
  #                                        eval.metric =c('TSS'),
  #                                        eval.metric.quality.threshold =c(0.6),
  #                                        prob.mean = T,
  #                                        prob.mean.weight = T,
  #                                        prob.cv = T,
  #                                        prob.ci = T,
  #                                        prob.median = T,
  #                                        committee.averaging=T)
  #   get_evaluations(myBiomodEM)
  #   
  #   ensem<-BIOMOD_EnsembleForecasting(myBiomodEM, projection.output=myBiomodProj)
  #   #first model is means
  #   #second is coefficent variation across predictions (sd/mean)
  #   #third is confidence interval upper (98 percentile)
  #   #fourth is confidence interval around mean (2 percentile)
  #   #fifth is median
  #   #sixth is committee Averaging
  #   #seventh is weighted mean (by TSS)
  #   ensem@models.projected<-c("Mean","CV_uncertainty",
  #                             "CI_98","CI_2",
  #                             "Median","Committee_Averaging",
  #                             "Weighted_Mean")
  #   
  #   png(filename=paste0("OutputPlots/",sp_name,"/",modeltype,"/ensemble_",i,".png"), width=16, height=8, units="in", res=600)
  #   plot(ensem)
  #   dev.off()
  # }
  # 
  
  #summary(get_formal_model(Mbassanus_PA1_Full_GAM))
  #summary(get_formal_model(Mbassanus_PA2_Full_GAM))
  
  
  save.image(file=paste0("./IntermediateOutput/SeabirdSDM/rstates/",sp_name,"_",modeltype, ".RData"))
  
  
#}



