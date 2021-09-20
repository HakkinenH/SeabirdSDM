

##############################################
### SDM readme ###
##############################################



### META ###
	HHakkinen
	Start Date: 01/02/2021
	End Date:
	As part of Institute of Zoology ZSL project
	"Evidence-based Climate Change Adaptation Practices to safeguard vulnerable species: supporting conservation practitioners, donors and policy makers"


The following is a description of the multi-realm SDM built for seabirds

### ###


For information on raw data see "DataSources.txt"

In the RawData folder is the original data exactly as downloaded
In the Processed Data is any cleaned data processed by code in the "DataProcessing" folder

Any output made by this or any other code in the repo is stored in the "IntermediateOutput" folder



#links
https://esnielsen.github.io/post/coastal-species-distribution-models/
https://www.frontiersin.org/articles/10.3389/fmars.2017.00421/full
https://link.springer.com/article/10.1007/s00338-020-01937-3
https://git.earthdata.nasa.gov/projects/LPDUR/repos/daac_data_download_r/browse/DAACDataDownload.R
https://www.sciencedirect.com/science/article/pii/S2351989415000311
average NVDI dataset




### DEFINED AREA OF INTEREST ###
	The area defined is based on the OSPAR regions, but with some modifications

	### OSPAR Regions
		We limit our area of interest to the OSPAR regions
		This information was downloaded from
		https://odims.ospar.org/layers/geonode:ospar_regions_2017_01_002
		Downloaded 5th Jan 2021

		A useful note: the NE vector lies on the x=51 vector, the NW vector lies on the x=-44 vector
		We modify this to include the Baltic (incorporating the coastlines of Poland, Sweden, Finland and the Baltic states)
		and to exclude Greenland, the Azores, Madeira and the Canaries.

		Some additional useful shapes to help make our map:


	### Europe coastline
		Detailed polyline of Europe's coastline. Useful for plotting and cropping
		Downloaded from https://www.eea.europa.eu/data-and-maps/data/eea-coastline-for-analysis-2
		6th Jan 2021


	### European countries 
		Shapefile of all European coastlines at a high resolution. Useful for plotting and cropping
		Downloaded (at 1:1m) from https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/countries
		6th Jan 2021

		For some useful notes on the content
		https://gisco-services.ec.europa.eu/distribution/v2/countries/countries-2020-files.html
		Our work uses content from the CNTR_RG_01M_2020_4326.shp folder


	### Europe's sea regions and subregions
		Shape of europe's marine region, with subregions
		Downloaded from https://www.eea.europa.eu/data-and-maps/data/europe-seas
		6th Jan 2021

	### Russia provinces
		To add only the coastal provinces of russia we need a shapefile of their outline
		Downloaded from https://gadm.org/download_country_v3.html
		21st Jan 2021
	
	### Global coastline
		To calculate the isolation of various islands
		Downloaded from https://www.ngdc.noaa.gov/mgg/shorelines/
		27th Jan 2021


	### The final outline shape of the OSPAR region was made with "Code/DataProcessing/ospar_regions_2017_01_002/CreateCustomOSPAR.qgis"
		See "BuildingAreaofInterest.txt" in the same folder for instructions on how to do this
		Processed OSPAR region shape ready to be used is in ProcessedData/OSPAR_region

### ###





### BIRDLIFE RANGES ###

	The ESRI geodatabase from BirdLife (provided via FTP request, see emails) is contained in a geodatabase
	And is unchanged raw data. It is however massive, because it is ALL species.
	Downloaded: 19th Dec 2020

	The data has been filtered down to the above seabird species list

	In addition I have filtered by the following fields:

	#Presence
		Keep extant (1) range information only
		Exclude probably extant (defunct anyway), possibly extant, possibly extinct, probably extinct, extince, uncertain

	#Origin
		Keep Native, Reintroduced, Introduced, origin uncertain, assisted colonisation (1,2,3,5,6)
		Exclude Vagrant

	#Season
		Keep resident, breeding season, non-breeding season, seasonal occurrence uncertain (1,2,3,5)
		exclude passage

	#Compiler
		For some reason the compiler can be split for a species, these are merged to make it simpler.
		
	### Filters and settings are in Code/DataProcessing/Birdlife-RangeDatabase_Dec2020
	
	We convert this to a spatially disaggregated lat/lon file using PrepOcc.R. It takes a polygon and spits out a spatial point dataframe.
	In addition, for species that do not have a clear marine breeding marine range, we use foraging distance to "crop" the total range and ignore points far out to sea that would otherwise be included.
	

### ###




	### Current
		Terrestrial: WorldClim (V2.1) data https://www.worldclim.org/data/worldclim21.html
					 WorldClim has a number of variables at various resolutions, and has had recent updates
					 Downloaded from biogeo.ucdavis.edu V2.1 repo 26th Jan 2021
					 
			Currently the main variables are Mean temperature of the warmest month, precipitation during the breeding months, isolation of landmass, area of landmass, distance to sea, mean/min NDVI
		
		Marine: Bio-Oracle data https://www.bio-oracle.org/downloads-to-email.php
				Downloaded 26th Jan 2021
		
				for monthly values I use: http://www.marspec.org/
				Downloaded 25th Jan 2021
				
				Marine values are trickier as there is LOADS of raw data available and trawling through it to get a usable dataset is tricky.
				In addition most of these are at 5min resolution (close to source resolution), not many are interpolated to 30s
				In the absence of a clear 30s dataset I'm tempted to stick to 5min as high-res information is less important at sea, can resample if needed
				
			Currently the main marine variables are sea surface temperature during the winter/spring, maximum chlorophyll concentration (as sea surface), bathymetry, distance to land
				
		
		Note that for some pieces of work I use SDMpredictors which is just a wrapper to download the above sources
		Anything downloaded through the package is placed in RawData/sdmpredictors rather than any of the above
		Other raw data is in a folder named after the source (as above)
		
		Note that marine layers are derived from raw data, a comparison of raw data sources can be found here: https://climatedataguide.ucar.edu/climate-data/sst-data-sets-overview-comparison-table
		https://www.nodc.noaa.gov/OC5/indprod.html
		https://www.nodc.noaa.gov/cgi-bin/OC5/woa18/woa18.pl

	### Future
	
		The difficulty here is not all the above variables easily translate into the future
		they are frequently not available.
		
		RCP scenario:
			We are aiming for a "middle-of-the-road" scenario. In CMIP6 this is SSP3-7.0. There isn't an exact equivalent in CMIP5 but the closest is RCP60
		
		
		WorldClim (bioclim) data https://www.worldclim.org/data/cmip6/cmip6climate.html
		
		v2.1 5min MRI-ESM2 SSP370 2081-2100
		
		
		Marine: Bio-Oracle data https://www.bio-oracle.org/downloads-to-email.php
		2100 v2.1 RCP60
		Ensemble model based on CSM4, HadGEM2-ES, MIROC
		
		
	

### ###


### NDVI ###
	A lot of options
	In the end I downloaded MODIS NDVI (as it seems complete and relatively easy to use)
	https://modis.gsfc.nasa.gov/data/dataprod/mod13.php
	MOD13C2  (V0.006)
	1km Res, 2016-2020 (Mar-Sept) resolution, many layers with raw data.
	
	Trying to download takes you to either a server (which is a pain to use)
	or to the NASA EarthData download portal. However this portal does not actually let you download, it just gives you server links
	
	These links can then be fed to an API, e.g. cmd/python/R. I used R (DownloadMODIS_NDVI.R)
		DOwnloaded 15th March 2021

### ###
