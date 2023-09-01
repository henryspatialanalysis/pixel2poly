## #######################################################################################
## 
## BUILD AGGREGATION TABLES USING PIXEL2POLY PACKAGE FUNCTIONS
## 
## #######################################################################################

library(terra); library(data.table)
devtools::load_all()

# Some test data from the `terra` package
test_vec <- terra::vect(system.file("ex/lux.shp", package="terra"))
# Margin of error - when we suspect pixel fractions should actually sum to 1
moe <- 1e-4

## Build ID raster and aggregation table
id_raster <- build_id_raster(polygons = test_vec)
agg_table <- build_aggregation_table(
  polygons = test_vec,
  id_raster = id_raster,
  polygon_id_field = 'ID_2'
)
