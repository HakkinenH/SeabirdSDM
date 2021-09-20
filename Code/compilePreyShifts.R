



library(raster)

library(ncdf4)
library(anytime)


setwd("C:/Users/Henry/Documents/Research/ZSL/RawData/AquaMaps")

###read in range data from COPERNICUS


#choose species
sp_name<-"Sprattus_sprattus"


#step 1: read in present day suitability data

#work out file name

occfile<-list.files(paste0(sp_name,"_present/"))

if (sum(grepl("*.nc", occfile)) >0 ){
  #check if there's an nc file to use
  
}else{
  #otherwise it's a csv
  
  spf<-read.csv(paste0("./",sp_name,"_present/EstimatedOcc.csv"))
  spf<-spf[,c("Center.Long", "Center.Lat", "Overall.Probability")]
  

  spra<-rasterFromXYZ(spf)
  

}




#step 2: read in 2050(?) suitability data
occ50file<-list.files(paste0(sp_name,"_2050/"))

if (sum(grepl("*.nc", occ50file)) >0 ){
  #check if there's an nc file to use
  
}else{
  #otherwise it's a csv
  
  spf50<-read.csv(paste0("./",sp_name,"_2050/EstimatedOcc.csv"))
  spf50<-spf50[,c("Center.Long", "Center.Lat", "Overall.Probability")]
  
  spra50<-rasterFromXYZ(spf50)
  plot(spra50-spra)
}






#step 3: read in comparison data from IUCN, do a quick comparison


#step 4: assess where suitability changes significantly
#what is a "significant" change in suitability? If suitability is currently over 0.5, and will drop by >0.1 in 2050



setwd("C:/Users/Henry/Documents/Research/ZSL/RawData/")


newmap <- getMap(resolution = "low") 

filen<-paste0("Copernicus_FishRanges/SS_DBEM_POLCOMS_fish_abundance-abundance-rcp45-msy06-herring-v0.1.nc")

nc_data <- nc_open(filen)

print(nc_data)


lon <- ncvar_get(nc_data, "longitude")
lat <- ncvar_get(nc_data, "latitude", verbose = F)
t <- ncvar_get(nc_data, "time")
tunits <- ncatt_get(nc_data, "time", "units")




abun.array <- ncvar_get(nc_data, "abundance") # store the data in a 3-dimensional array
fillvalue <- ncatt_get(nc_data, "abundance", "_FillValue")


nc_close(nc_data) 



abun.array[abun.array == fillvalue$value] <- NA

#convert unix epoch date (days since 1st Jan 1970) to a more useable format
tcon<-as.Date(as.POSIXct(t*24*60*60, origin = "1970-01-01", tz="UTC"))


abun.slice <- abun.array[, , which(tcon=="2098-01-01")] 

dim(abun.slice)

r <- raster(t(abun.slice), 
            xmn=min(lon), 
            xmx=max(lon), 
            ymn=min(lat), 
            ymx=max(lat), 
            crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

r <- flip(r, direction='y')

plot(log10(r))
plot(newmap,add=T)
plot(log10(r),add=T)
