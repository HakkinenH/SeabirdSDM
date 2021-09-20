

#clear env
rm(list=ls())

library(biomod2);
library(raster);
library(RColorBrewer);
library(dismo);
library(rgdal)
library(sdmpredictors)


setwd("C:/Users/Henry/Documents/Research/ZSL/Repos/SeabirdCCVA/IntermediateOutput/SDM")


spdf<-readOGR("../PrepOccData/MbassanusBreedingSp.tif")

#spdf<-readOGR("Farctica_BreedingRaster.tif")



points<-as.data.frame(coordinates(spdf))

points$species<-"Mbassanus"
points<-points[,c(3,1,2)]


#Get data points
#points <- read.csv(file = "GadMa.csv", header = T);
points <- cbind(points, rep.int(1, length(nrow(points)))); #Adds another column indicating these are presence points
colnames(points) <- c("Species", "X", "Y", "Response");



#Get environmental variables
#setwd("~/AnalysisFolder/Ms/GadMa/");
#envtList <- list.files(pattern = ".asc");
#envt.st <- stack(envtList);

envt.st<-stack("../PrepEnvData/TerrandMarineVar_5min_Mbassanus.tif")
names(envt.st) <- c("MeanTemp_WM", "Prec_BreedMonths", "Isol", "Area", "distToSea","nearestSeaCell", "origIndex",
                    "MEAN_SST_WS","MEAN_mean_sal","MEAN_max_chloro","MEAN_bathy","MEAN_distShore","MEAN_Amarinus",
                    "SD_SST_WS","SD_mean_sal","SD_max_chloro","SD_bathy","SD_distShore","SD_MEAN_Amarinus" )

#a few manual checks on values
envt.st$Prec_BreedMonths[envt.st$Prec_BreedMonths==0]<-NA
envt.st$distToSea[envt.st$distToSea<0]<-0

#log for normality
envt.st$Prec_BreedMonths<-log10(envt.st$Prec_BreedMonths)
envt.st$distToSea<-log10(envt.st$distToSea+1)
envt.st$Isol<-log10(envt.st$Isol+0.1)
envt.st$Area<-log10(envt.st$Area)





#### TERRESTRIAL SDM
envt.st<-subset(envt.st, c("MeanTemp_WM","Prec_BreedMonths","Isol", "Area", "distToSea"))

layercodes <- c("MeanTemp_WM","Prec_BreedMonths","Isol", "Area", "distToSea")

prettynames <- list(MeanTemp_W="Mean Winter Temp", Prec_BreedMonths="Logged Breeding Precip", 
                    Isol="Isolation (logged km)", Area="Island Area (logged km)", distToSea="Dist to Sea (logged km)")

plot(envt.st)

#### TERRESTRIAL AND MARINE SDM
#choose variables you want right now
envt.st<-subset(envt.st, c("MeanTemp_WM","Prec_BreedMonths","Isol", "Area", "distToSea",
                           "MEAN_SST_WS","MEAN_mean_sal","MEAN_max_chloro","MEAN_bathy"))

layercodes <- c("MeanTemp_WM","Prec_BreedMonths","Isol", "Area", "distToSea",
                "MEAN_SST_WS","MEAN_mean_sal","MEAN_max_chloro","MEAN_bathy")

prettynames <- list(MeanTemp_W="Mean Winter Temp", Prec_BreedMonths="Logged Breeding Precip", 
                    Isol="Isolation (logged km)", Area="Island Area (logged km)", distToSea="Dist to Sea (logged km)",
                    MEAN_SST_WS="Mean SST",MEAN_mean_sal="Mean Salinity",MEAN_max_chloro="Mean Chlorophyll",MEAN_bathy="Mean Bathymetry")

plot(envt.st)

#### TERRESTRIAL but with food availability
envt.st<-subset(envt.st, c("MeanTemp_WM","Prec_BreedMonths","Isol", "Area", "distToSea", "MEAN_Amarinus"))

layercodes <- c("MeanTemp_WM","Prec_BreedMonths","Isol", "Area", "distToSea", "A. marinus availability")

prettynames <- list(MeanTemp_W="Mean Winter Temp", Prec_BreedMonths="Logged Breeding Precip", 
                    Isol="Isolation (logged km)", Area="Island Area (logged km)", distToSea="Dist to Sea (logged km)",
                    "A. marinus availability")

plot(envt.st)


#####NULL MODEL

newvals<-sampleRandom(envt.st$MEAN_SST_WS, length(envt.st$MEAN_SST_WS) ,na.rm=T, replace=T)
envt.st$MEAN_SST_WS[!is.na(envt.st$MEAN_SST_WS)]<-newvals

envt.st<-stack(envt.st$MEAN_SST_WS)
plot(envt.st)

env_correlations <- pearson_correlation_matrix(envt.st)
p2 <- plot_correlation(env_correlations, prettynames)
p2



#Get projection variables
#setwd("~/AnalysisFolder/EnvironmentalData/Future/rcp8_5/2100/")
#projectionList <- list.files(pattern = ".asc");
#proj.st <- stack(projectionList);


#Setting up data file for Biomod2
bmData <- BIOMOD_FormatingData(resp.var = points[,4],
                               resp.xy = points[,2:3], 
                               resp.name = as.character(points[1,1]),
                               expl.var = envt.st,
                               PA.nb.rep=1
);


#Setting up Maxent run
myBiomodOption <- Print_Default_ModelingOptions();
myBiomodOption@MAXENT.Phillips$path_to_maxent.jar = paste(system.file(package="dismo"), "/java", sep='');
myBiomodOption@MAXENT.Phillips$memory_allocated = 2048; #Allocates 2048 MB/2 GB of memory to modeling
myBiomodOption@MAXENT.Phillips$maximumiterations = 20000;
myBiomodOption@MAXENT.Phillips$threshold = F;
myBiomodOption@MAXENT.Phillips$hinge = F;
myBiomodOption@MAXENT.Phillips$visible = F;
myBiomodOption@MAXENT.Phillips$beta_lqp = .95;



#Running Maxent, make sure Java is installed
setwd("./SDMoutput/")
myMaxentModel <- BIOMOD_Modeling(data=bmData,
                                 models=c('MAXENT.Phillips'),
                                 models.options=myBiomodOption,
                                 NbRunEval=10,
                                 do.full.models = F,
                                 DataSplit=80,
                                 models.eval.meth = c('KAPPA','TSS','ROC'),
                                 SaveObj = T
);

#Ensemble of all models--combines model runs using a user-selected evaluation metric
myMaxentEnsemble <- BIOMOD_EnsembleModeling( modeling.output = myMaxentModel,
                                             chosen.models = 'all',
                                             em.by = 'all',
                                             eval.metric = c('TSS'),
                                             eval.metric.quality.threshold = NULL,
                                             models.eval.meth = c('TSS','ROC','KAPPA'),
                                             prob.median = TRUE )

#Projecting your model to the present
myBiomodProjPres <- BIOMOD_Projection(modeling.output = myMaxentModel,
                                      new.env = envt.st,
                                      proj.name = 'Present',
                                      selected.models = 'all',
                                      compress = 'gzip',
                                      clamping.mask = T,
                                      output.format = '.grd',
                                      do.stack=T
);

mod_projPres <- get_predictions(myBiomodProjPres);
presentResult <- calc(mod_projPres,fun = median); #Choose whatever descriptive statistic you'd like
plot(presentResult);
writeRaster(presentResult, filename = "MbassanusPresent", format = "ascii", overwrite = T);

#Projecting the ensemble model in the present
myBiomodProjPresEnsemble <- BIOMOD_EnsembleForecasting(myMaxentEnsemble,
                                                       projection.output = myBiomodProjPres,
                                                       selected.models = 'all',
                                                       compress = 'gzip'
);
mod_projPresEnsemble <- get_predictions(myBiomodProjPresEnsemble);
presentEnsembleResult <- mod_projPresEnsemble[[2]] #This is the median model ensemble
plot(presentEnsembleResult);
writeRaster(presentEnsembleResult, filename = "MbassanusPresentEnsemble", format = "ascii", overwrite = T);



#Evaluating models
## Variable response curves
response.plot2(models = BIOMOD_LoadModels(myMaxentModel, models='MAXENT.Phillips'),
               Data = get_formal_data(myMaxentModel,'expl.var'),
               show.variables= get_formal_data(myMaxentModel,'expl.var.names'),
               do.bivariate = FALSE,
               fixed.var.metric = 'median',
               col = brewer.pal(10, "Spectral"),
               legend = TRUE,
               data_species = get_formal_data(myMaxentModel,'resp.var')
);

##Varible contributions; only for Maxent, not possible for other models
forSetup <- read.csv(file = paste("./Mbassanus/models/1614792299/Mbassanus_PA1_RUN1_MAXENT.Phillips_outputs/maxentResults.csv", sep = ""), header = T)#Choose the appropriate model folder with the seed of the analysis you want
variableContributions <- matrix(data = NA, nrow = length(forSetup[, grep('.contribution', names(forSetup))]), ncol = 10);
rownames(variableContributions) <- names(forSetup[, grep('.contribution', names(forSetup))])
colnames(variableContributions) <- c("Run1", "Run2", "Run3", "Run4", "Run5", "Run6", "Run7", "Run8", "Run9", "Run10")
variablePermutationImportance <- matrix(data = NA, nrow = length(forSetup[, grep('.permutation.importance', names(forSetup))]), ncol = 10);
colnames(variablePermutationImportance) <- c("Run1", "Run2", "Run3", "Run4", "Run5", "Run6", "Run7", "Run8", "Run9", "Run10")
count <- 1;
while (count <= 10){
  temporary <- read.csv(file = paste("./Mbassanus/models/1614792299/Mbassanus_PA1_RUN", count, "_MAXENT.Phillips_outputs/maxentResults.csv", sep = ""), header = T);
  variableContributions[,count] <- unlist(unname(temporary[, grep('.contribution', names(temporary))]))
  variablePermutationImportance[,count] <- unlist(unname(temporary[, grep('.permutation.importance', names(temporary))]))
  count <- count + 1;
}
write.csv(variableContributions, "VariableContributions.csv", quote = F);
write.csv(variablePermutationImportance, "VariablePermutationImportance.csv", quote = F);


##Create dataset for evaluation
###"Cutoff" gives threshold to optimize evaluation metric, "Sensitivity" and "Specificity" are based on this threshold
myMaxentModelEval <- get_evaluations(myMaxentModel, as.data.frame = F); 
write.csv(myMaxentModelEval["TSS",,,,],"TSSEvaluation.csv", quote = F);#Results for True Skill Score
write.csv(myMaxentModelEval["ROC",,,,],"TSSEvaluation.csv", quote = F);#Results for Area Under the Curve
write.csv(myMaxentModelEval["Kappa",,,,],"TSSEvaluation.csv", quote = F) #Results for Cohen's Kappa




##FUTURE SECTION


#Projecting your model to the future
myBiomodProj2100 <- BIOMOD_Projection(modeling.output = myMaxentModel,
                                      new.env = proj.st,
                                      proj.name = 'In2100',
                                      selected.models = 'all',
                                      compress = 'gzip',
                                      clamping.mask = T,
                                      output.format = '.grd',
                                      do.stack=T
);

mod_proj2100 <- get_predictions(myBiomodProj2100);
result2100 <- calc(mod_proj2100,fun = median); #Choose whatever descriptive statistic you'd like
plot(result2100);
writeRaster(result2100, filename = "GadusMacrocephalus2100", format = "ascii", overwrite = T);


#Projecting the ensemble model in 2100
myBiomodProj2100Ensemble <- BIOMOD_EnsembleForecasting(myMaxentEnsemble,
                                                       projection.output = myBiomodProj2100,
                                                       selected.models = 'all',
                                                       compress = 'gzip'
);
mod_proj2100Ensemble <- get_predictions(myBiomodProj2100Ensemble);
ensembleResult2100 <- mod_proj2100Ensemble[[2]] #This is the median model ensemble
plot(ensembleResult2100);
writeRaster(ensembleResult2100, filename = "GadusMacrocephalus2100Ensemble", format = "ascii", overwrite = T);

##Calculate MESS for 2100
mess2100 <- mess(proj.st, rasterToPoints(envt.st)[,-1:-2],-9999);
writeRaster(mess2100, filename = "GadusMacrocephalus2100MESS", format = "ascii", overwrite = T)











#alt section
library(maxent)
BIOMOD_ModelingOptions()
PIPO.mod <-BIOMOD_Modeling(data = bmData,models =c('GLM','GAM','ANN','RF','MAXENT.Phillips'),
                           SaveObj = TRUE,
                           models.options = myBiomodOption,
                           DataSplit = 80, 
                           do.full.models = F,
                           # common practice to validate!
                           models.eval.meth = c('KAPPA','TSS','ROC'),
                           VarImport = 1)
PIPO.mod.eval <-get_evaluations(PIPO.mod)

PIPO.mod.eval["TSS","Testing.data",,,]
PIPO.mod.eval["KAPPA","Testing.data",,,]
PIPO.mod.eval["ROC","Testing.data",,,]

var.imp <-drop(get_variables_importance(PIPO.mod))
var.impterr<-var.imp[c("MeanTemp_WM","Prec_BreedMonths","Isol","Area","distToSea"),]
var.impmar<-var.imp[c("MEAN_SST_WS","MEAN_mean_sal","MEAN_max_chloro","MEAN_bathy"),]

barplot(height =t(var.impterr),beside = TRUE,
        horiz = TRUE,xlab = "Variable Importance",
        legend =c("GLM", "GAM", "ANN", "RF", "MAXENT"))
barplot(height =t(var.impmar),beside = TRUE,
        horiz = TRUE,xlab = "Variable Importance",
        legend =c("GLM", "GAM", "ANN", "RF", "MAXENT"))


barplot(height =t(var.imp),beside = TRUE,
        horiz = TRUE,xlab = "Variable Importance",
        legend =c("GLM", "GAM", "ANN", "RF", "MAXENT"))


myBiomodEM <-BIOMOD_EnsembleModeling(modeling.output = PIPO.mod,
                                     chosen.models ='all',
                                     em.by ='all',
                                     eval.metric =c('TSS'),
                                     eval.metric.quality.threshold =c(0.6),
                                     prob.mean = TRUE,
                                     prob.mean.weight = FALSE,
                                     prob.cv = FALSE,
                                     prob.ci = FALSE,
                                     prob.median = F)
myBiomodEM
get_evaluations(myBiomodEM)

#mod_projPresEnsemble <- get_predictions(myBiomodEM);
#presentEnsembleResult <- mod_projPresEnsemble[[2]] #This is the median model ensemble
#plot(presentEnsembleResult);


myBiomodProj <-BIOMOD_Projection(modeling.output = PIPO.mod,
                                 new.env =stack(envt.st),
                                 # modern environment
                                 proj.name ='current',
                                 selected.models ='all',
                                 binary.meth ='TSS',
                                 compress ='xz',
                                 clamping.mask = F,
                                 output.format ='.grd')

myBiomodProj.plot <- myBiomodProj
myBiomodProj.plot@models.projected <-c("GLM","GAM","ANN","RF","MAXENT.Phillips")
plot(myBiomodProj.plot)


response.plot2(models = BIOMOD_LoadModels(PIPO.mod, c('GLM','GAM','ANN','RF','MAXENT.Phillips')),
               Data = get_formal_data(PIPO.mod,'expl.var'),
               show.variables= get_formal_data(PIPO.mod,'expl.var.names'),
               do.bivariate = FALSE,
               fixed.var.metric = 'median',
               col = brewer.pal(10, "Spectral"),
               legend = TRUE,
               data_species = get_formal_data(PIPO.mod,'resp.var') )
