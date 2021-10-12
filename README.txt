

##############################################
### SDM readme ###
##############################################



### META ###
	HHakkinen
	Start Date: 01/02/2021
	End Date: 20/09/2021
	As part of Institute of Zoology ZSL project
	"Terrestrial or marine species distribution model - Why not both? A case study with seabirds"

We assess building terrestrial, marine and combined species distribution models (SDMs) for seabirds in Europe.
The following is a description of the files and folders required to run the models and generate outputs

### ###


### FOLDER STRUCTURE ###

	# AdHoc: Contains exploratory/experimental code that has not been fully integrated into the workflow. There is no guarantee this will run correctly after pulling repo (or at least without some adjustment)

	# Code: The main repo folder containing all major code and functions. Described in more detail below
		#Dependencies: Underlying custom functions used in multiple scripts
		
	#InputData: 
		#EnvData: environmental data in a raster stack
		#OccData: Species occurrence data shapefiles or spatial point dataframe
		#StudyArea:  a shapefile of the study area extent
	
	# FinalOutput: Final figures and tables produced by main SDM. Used to assess final results or produce final figures/tables for inclusion in paper

	# IntermediateOutput: For a variety of reasons we might want to produce output that is not needed in the final publication, e.g. diagnostics or subsidiery analysis. All such output is stored here. Subfolders are named according to the R script that generates the output.

### ### 


### INPUT ###

	In order to run an SDM, you need a defined area of study, occurrence data and environmental data
	
	Since this data does not belong to the authors it is not included in the repository
	However we specify how to acquire the data and any processing required in "DataSources.txt"
	It is assumed by the code the data will be stored in the folder "InputData". These directories can be adjusted in "1-Wrapper.R".

	In order to run an SDM you need to have the following files compiled. This can be done with the code provided or can be done manually outside of the repo.
	
	
	
	### ENV DATA
		#MARINE
		A raster stack of marine variables. This should be aligned and cropped to (near) the study area. Name is assumed to be "MarineEnvVariables_5m.tif" but this can be changed in "Directories.R"
		
		#TERRESTRIAL
		A raster stack of terrestrial variables. This should be aligned and cropped to (near) the study area. Name is assumed to be "TerrEnvVariables_5m.tif" but this can be changed in "Directories.R"
		
	### OCCURRENCE DATA
	This should be prepared occurrence data, in the form of a spatial points dataframe or shapefile. I store these in folders named after the species name, but can be adjusted
	
	### STUDY AREA
	This is provided in "InputData/StudyArea" as "ospar_LandSea.shp". For how this was made see "DataSources.txt". can be replaced with whatever area you prefer.
	

### ###


### CODE ###
All code required to run the model is in the "code" folder

If all data is present, then only two files need to be opened and adjusted
	-"1-Wrapper.R" should be customised to whatever is required. By default internal references should work for anyone, but external links (in particular links to downloaded data not included in the repo) should be checked.  This calls on all the other scripts, and can be opened and run stage by stage or all at once. Set the species and types of model desired and run.
	- "SeabirdSDM.R" The default settings in the wrapper file should allow this to be run without changes. However there are numerous custom settings that you may wish to change. In which case consult this file.
	
"SeabirdSDM.R" assumes:
	-the repo is present and up to date
	-required packages are installed
	-File paths are correct in "1-Wrapper"
	-OSPAR shapefile is present (to set limits of study area)
	-Occurrence data is present as a shapefile. By default this is expected to be named the species name in question but this can be changed in the wrapper file
	-Environmental data is present as a rasterStack. By default this is expected to be named e.g. MarineEnvVariables_5m.tif but this can be changed in the wrapper file
If the above 


There are also several other code files which do not to be adjusted. They are all called from 1-Wrapper.R
	-CheckDependencies.R: does a quick check the required libraries and folders are in place
	-CalculateDistShore.R: one of the key parts about combinging terrestrial and marine data is we need to work out which area of the sea is closest to a given terrestrial grid-cell and vice versa. This file contains the code to do this. See the file notes for more detail
	-CombineEnvData.R: after the previous file has worked out distances and indexes, this file actually combines information from terrestrial and marine raster stacks into combined stacks.

In the "Dependencies" there are a number of function files which are used throughout
	-prepFunctions.R: various functions to combine terrestrial and marine data
	-SDM_functions.R: the actual function and code used to run an ensemble SDM model
	-SDM_functionsPre.R: code and functions to prepare and check data prior to running an SDM model


### ###


### OUTPUT ###

For individual steps various diagnositic plots and output is stored in the "IntermediateOutput" folder
All final SDM output will be in the "FinalOutput" folder


### ### 

