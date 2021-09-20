#MARINE SPEED TEST

# install if needed
#devtools::install_github("lifewatch/marinespeed")

library(marinespeed)

# set a data directory, preferably something different from tempdir to avoid 
# unnecessary downloads for every R session
options(marinespeed_datadir = tempdir())
# list all species
species <- list_species()

species[order(species$species),]
"Morus bassanus" %in% species$species


dim(species)
knitr::kable(species[1:5,], row.names = FALSE)




install.packages("sdmpredictors")
# or for the latest dev version
devtools::install_github("lifewatch/sdmpredictors")

#set file.path() to stop having to load every time

library(sdmpredictors)
library(zoon)

# Inspect the available datasets and layers
datasets <- list_datasets(terrestrial = FALSE, marine = TRUE)
View(datasets)
layers <- list_layers(datasets)
View(layers)

terr_datasets <- list_datasets(terrestrial = T, marine = F)
View(terr_datasets)
terr_layers <- list_layers(terr_datasets)
View(terr_layers)


# Load equal area rasters and crop with the extent of the Baltic Sea
layercodes <- c("MS_biogeo05_dist_shore_5m", "MS_bathy_5m", 
                "BO_sstrange", "BO_sstmean", "BO_salinity")
env <- load_layers(layercodes, equalarea = TRUE)
australia <- raster::crop(env, extent(106e5,154e5, -52e5, -13e5))
plot(australia)

# Compare statistics between the original and the Australian bathymetry
View(rbind(layer_stats("MS_bathy_5m"),
           calculate_statistics("Bathymetry Australia", 
                                raster(australia, layer = 2))))

# Compare correlations between predictors, globally and for Australia
prettynames <- list(BO_salinity="Salinity", BO_sstmean="SST (mean)", 
                    BO_sstrange="SST (range)", MS_bathy_5m="Bathymetry",
                    MS_biogeo05_dist_shore_5m = "Shore distance")
p1 <- plot_correlation(layers_correlation(layercodes), prettynames)
australian_correlations <- pearson_correlation_matrix(australia)
p2 <- plot_correlation(australian_correlations, prettynames)
cowplot::plot_grid(p1, p2, labels=c("A", "B"), ncol = 2, nrow = 1)
print(correlation_groups(australian_correlations))

# Fetch occurrences and prepare for ZOON
occ <- marinespeed::get_occurrences("Dictyota diemensis")
points <- SpatialPoints(occ[,c("longitude", "latitude")],
                        lonlatproj)
points <- spTransform(points, equalareaproj)
occfile <- tempfile(fileext = ".csv")
write.csv(cbind(coordinates(points), value=1), occfile)

?workflow
# Create SDM with ZOON
workflow(
  occurrence = LocalOccurrenceData(
    occfile, occurrenceType="presence",
    columns = c("longitude", "latitude", "value")), 
  covariate = LocalRaster(stack(australia)),
  process = OneHundredBackground(seed = 42),
  model = LogisticRegression,
  output = PrintMap)


# Layer citations
print(layer_citations(layercodes))

LocalOccurrenceData(
  occfile, occurrenceType="presence",
  columns = c("longitude", "latitude", "value"))

