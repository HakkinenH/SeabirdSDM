
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

if(!dir.exists(paste0("./IntermediateOutput/SeabirdSDM/OutputTables/",sp_name,"/"))){
  dir.create(paste0("./IntermediateOutput/SeabirdSDM/OutputTables/",sp_name))
}