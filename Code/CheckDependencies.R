
#check if the following libraries are installed or not

require(sdmpredictors)
require(rgdal)
require(rworldmap)
require(raster)
require(ggplot2)
require(rasterVis)
require(biomod2)
require(RColorBrewer)
require(dismo)



#check that required files and folders are present
if(!dir.exists("./IntermediateOutput/CombineEnvData/EnvPlots/")){
  dir.create("./IntermediateOutput/CombineEnvData/EnvPlots/")
}
if(!dir.exists("./IntermediateOutput/CombineEnvData/MarineLookup/")){
  dir.create("./IntermediateOutput/CombineEnvData/MarineLookup/")
}


if(!dir.exists("./IntermediateOutput/SeabirdSDM/OccMap/")){
  dir.create("./IntermediateOutput/SeabirdSDM/OccMap/")
}
if(!dir.exists("./IntermediateOutput/SeabirdSDM/OutputPlots/")){
  dir.create("./IntermediateOutput/SeabirdSDM/OutputPlots/")
}
if(!dir.exists("./IntermediateOutput/SeabirdSDM/OutputTables/")){
  dir.create("./IntermediateOutput/SeabirdSDM/OutputTables/")
}
if(!dir.exists("./IntermediateOutput/SeabirdSDM/rstates/")){
  dir.create("./IntermediateOutput/SeabirdSDM/rstates/")
}

