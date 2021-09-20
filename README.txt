

##############################################
### SDM readme ###
##############################################



### META ###
	HHakkinen
	Start Date: 01/02/2021
	End Date: 20/09/2021
	As part of Institute of Zoology ZSL project
	"Evidence-based Climate Change Adaptation Practices to safeguard vulnerable species: supporting conservation practitioners, donors and policy makers"


We assess building terrestrial, marine and combined species distribution models (SDMs) for seabirds in Europe.
The following is a description of the files and folders required to run the models and generate outputs

### ###


### FOLDER STRUCTURE ###

	# AdHoc: Contains exploratory/experimental code that has not been fully integrated into the workflow. There is no guarantee this will run correctly after pulling repo (or at least without some adjustment)

	# Code: The main repo folder containing all major code and functions. Described in more detail below
		#DataProcessing: Subfolder for code that processes data (e.g. occurrence and environmental) into the necessary format for models
		#Dependencies: Underlying custom functions used in multiple scripts
		
	# FinalOutput: Final figures and tables produced by main SDM. Used to assess final results or produce final figures/tables for inclusion in paper

	# IntermediateOutput: For a variety of reasons we might want to produce output that is not needed in the final publication, e.g. diagnostics or subsidiery analysis. All such output is stored here. Subfolders are named according to the R script that generates the output.

### ### 


### INPUT ###

	There are a variety of data required to run an SDM, in particular a defined area of study, occurrence data and environmental data
	Since this data does not belong to the authors it is not included in the repository
	However we specify how to acquire the data and any processing required in "DataSources.txt"
	It is assumed by the code the data will be stored in folders called "RawData" (for raw, unprocessed data) or "ProcessedData" (for filtered, processed or otherwise altered data). These directories can be adjusted in "SetDirectories.R".

	In order to run an SDM you need to have the following files compiled. This can be done with the files included in "DataProcessing" or can be done manually outside of the repo.
	
	

### ###


### CODE ###
All code required to run the model is in the "code" folder

If all data is present, then only two files need to be opened and adjusted
"SetDirectories.R" should be customised to whatever is required. By default internal references should work for anyone, but external links (in particular links to downloaded data not included in the repo) should be checked.
"SeabirdSDM.R" This calls on all the other scripts, and can be opened and run stage by stage or all at once. Set the species and types of model desired and run.

"SeabirdSDM.R" assumes:
	-the repo is present and up to date
	-required packages are installed
	-File paths are correct in "SetDirectories.R"
	-OSPAR shapefile is present (to set limits of study area)
	-Occurrence data is present as a .tif. By default this is expected to be named "" but this can be changed in "SetDirectories"
	-Environmental data is present as a rasterStack. By default this is expected to be named "" but this can be changed in "SetDirectories"
If the above 



### ###


### OUTPUT ###

All final output will be in the "FinalOutput" folder




### ### 


For information on raw data see "DataSources.txt"

The "SetDirectories" R file can be customised to the required file path, but my system is based on the following:
In the RawData folder is the original data exactly as downloaded
In the Processed Data is any cleaned data processed by code in the "DataProcessing" folder


Any output made by this or any other code in the repo is stored in the "IntermediateOutput" folder


