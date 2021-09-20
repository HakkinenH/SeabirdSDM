
#non-functioning code
#wanted to look at Frans work

#run the Frans et al SDM
#code from https://besjournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1111%2F2041-210X.12847&file=mee312847-sup-0002-AppendixS1.pdf



#set maxent

system.file('java', package='dismo')

options(java.home='C:/Users/Henry/Documents/R/win-library/4.0/dismo/java')


myBiomodOption <- Print_Default_ModelingOptions();
myBiomodOption@MAXENT.Phillips$path_to_maxent.jar = paste(system.file(package="dismo"), "/java", sep='')



library("raster")# raster data
library("rgdal")# input/output; projections; reading ASCII files
library("dismo")# species distribution modeling package
library("maptools")# mapping
library("classInt")# changing color intervals on a plot
library("gridExtra")# grid graphics
library("grid")# creating graphical objects
library("lattice")# required for grid graphics
library("ggplot2")# making complex plots from complex tables
library("RColorBrewer")# colors for graphics
library("plyr")# sort data
library("reshape")# sort data
library("reshape2")# sort data
library("rasterVis")# plotting rasters
library("igraph")# clumping contiguous pixels
library("rgeos")# polygon and point conversions

#Root directory
dir <-c('C:/Users/Henry/Documents/Research/ZSL/Repos/SeabirdCCVA/IntermediateOutput/FransSDM/Testing/')

# Occurrence data
nzsl_occur <-"C:/Users/Henry/Documents/Research/ZSL/Repos/SeabirdCCVA/IntermediateOutput/defineTrainingArea/Occurrence/"

# Environmental variables
train_var <-c("C:/Users/Henry/Documents/Research/ZSL/Repos/SeabirdCCVA/IntermediateOutput/defineTrainingArea/Training/") #variables are in ASCII format
proj_var <-("C:/Users/Henry/Documents/Research/ZSL/Repos/SeabirdCCVA/IntermediateOutput/defineTrainingArea/Projection/") #variables are in ASCII format


dir.create(c(paste0(train_var,'maxent.cache')),recursive=TRUE)
dir.create(c(paste0(proj_var,'maxent.cache')),recursive=TRUE)


train_cache <-c(paste0(train_var,'maxent.cache\\')) #folder for maxent cache files
proj_cache <-c(paste0(proj_var,'maxent.cache\\')) #folder for maxent cache files


#Maxent.jar file
maxent_dir <-c(paste0(dir,'maxent'))


# Folders for per-state Maxent outputs
# (Here, we model 3 states, but a 4th--full--model, S4, is also done for comparison)
for(state in 1:4){
  dir.create(paste0(dir,'m_output\\',state,'\\images'),
             recursive=TRUE)
  }

#Results of all states together (tables and figures)
dir.create(paste0(dir,'m_output\\results'),recursive=TRUE)
all_out <-paste0(dir,'m_output\\results\\')

# State-by-state projections
dir.create(paste0(dir,'m_output\\projections'),recursive=TRUE)
proj_out <-paste0(dir,'m_output\\projections\\')


# Mean outputs (this is to show an example when the mean across states is taken)
dir.create(paste0(dir,'mean_output'),recursive=TRUE)
mean_out <-paste0(dir,'mean_output\\')


# Multi-state SDM outputs
dir.create(paste0(dir,'mssdm_output\\threshold_layers'),recursive=TRUE)
dir.create(paste0(dir,'mssdm_output\\images'),recursive=TRUE)
mssdm_out <-paste0(dir,'mssdm_output\\')# general output folder
thresh_out <-paste0(mssdm_out,'threshold_layers\\')# threshold layers saved herems
sdm_images <-paste0(mssdm_out,'images\\')# images saved here



#read in occ
nzsl <-read.csv(paste0(nzsl_occur,"Farcticaocc.csv"),header=T)[,1:3]

names(nzsl)<-c("SPECIES", "LONGITUDE", "LATITUDE")
all_points <- nzsl[,c("SPECIES","LONGITUDE","LATITUDE")]


# Save as a CSV
write.csv(all_points,file=c(paste0(nzsl_occur,"NZSL_All.csv")), row.names = FALSE)
all_points <-read.csv(paste0(nzsl_occur,"NZSL_All.csv"),header=T)


# Check the first few rows
head(all_points)

# Rename "species" to "state," rename "NZ_Sea_Lion" to "state_4"
# (needed for for-loop to be done later)
all_points$SPECIES <-rep("state_4",length(all_points$SPECIES))
all_points$SPECIES <-factor(all_points$SPECIES)
all_points <-rename(all_points,c("SPECIES"="STATE"))

# Remove duplicate records
# (a default action in Maxent, but here, we choose to do it ahead of time)
all_points <- all_points[c("STATE","LONGITUDE","LATITUDE")]
all_points <- all_points[!duplicated(
                        all_points[,c("STATE","LONGITUDE","LATITUDE")]),]




