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

# Build ID raster and aggregation table
id_raster <- pixel2poly::build_id_raster(polygons = test_vec)
agg_table <- pixel2poly::build_aggregation_table(
  polygons = test_vec,
  id_raster = id_raster,
  polygon_id_field = 'ID_2'
)

## Create some test data to aggregate --------------------------------------------------->

set.seed(12345)

# Single layer rasters
pop_raster <- data_raster <- id_raster
terra::values(data_raster) <- rnorm(n = prod(dim(data_raster)))
terra::values(pop_raster) <- runif(n = prod(dim(id_raster)), min = 10, max = 1e3)

# Multi layer rasters
z_dim <- 2001:2010
data_raster_stack <- lapply(z_dim, function(x){
  rr <- id_raster
  terra::values(rr) <- rnorm(prod(dim(id_raster)))
  return(rr)
}) |> terra::rast()
pop_raster_stack <- lapply(z_dim, function(x){
  rr <- id_raster
  terra::values(rr) <- runif(n = prod(dim(id_raster)), min = 10, max = 1e3)
  return(rr)
}) |> terra::rast()

# Single year of draws
n_draws <- 100
n_px <- sum(!is.na(terra::values(id_raster)))
draws_matrix_single_year <- matrix(rnorm(n_draws * n_px), nrow = n_px, ncol = n_draws) |>
  as.data.table()

# Multiple years of draws
draws_matrix_multi_year <- lapply(z_dim, function(x) draws_matrix_single_year) |>
  data.table::rbindlist()

# Test inserting the draws matrix into the ID raster
draws_rast <- pixel2poly::values_to_raster(
  x = draws_matrix_single_year,
  id_raster = id_raster
)


## Test raster to polygon aggregation: Single year of data ------------------------------>

# Test 1: Mean
data_raster_agg_1 <- pixel2poly::aggregate_raster_to_polygons(
  data_raster = data_raster,
  aggregation_table = agg_table,
  aggregation_cols = c('polygon_id', 'ID_1', 'ID_2', 'NAME_1', 'NAME_2'),
  aggregated_field = 'agg_data',
  method = 'mean'
)
# Test 2: Weighted mean
data_raster_agg_2 <- pixel2poly::aggregate_raster_to_polygons(
  data_raster = data_raster,
  aggregation_table = agg_table,
  aggregation_cols = c('polygon_id', 'ID_1', 'ID_2', 'NAME_1', 'NAME_2'),
  method = 'weighted.mean',
  weighting_raster = pop_raster
)
# Test 3: Sum
data_raster_agg_3 <- pixel2poly::aggregate_raster_to_polygons(
  data_raster = data_raster,
  aggregation_table = agg_table,
  aggregation_cols = c('polygon_id', 'ID_1', 'ID_2', 'NAME_1', 'NAME_2'),
  method = 'sum'
)
# Test 4: Weighted sum
data_raster_agg_4 <- pixel2poly::aggregate_raster_to_polygons(
  data_raster = data_raster,
  aggregation_table = agg_table,
  aggregation_cols = c('polygon_id', 'ID_1', 'ID_2', 'NAME_1', 'NAME_2'),
  method = 'weighted.sum',
  weighting_raster = pop_raster
)


## Test raster to polygon aggregation: Multiple years of data --------------------------->

# Test 1: Mean
multiyear_agg_1 <- pixel2poly::aggregate_raster_to_polygons(
  data_raster = data_raster_stack,
  aggregation_table = agg_table,
  aggregation_cols = c('polygon_id', 'ID_1', 'ID_2', 'NAME_1', 'NAME_2'),
  method = 'mean',
  z_dimension = z_dim,
  z_dimension_name = 'year'
)
# Test 2: Weighted mean
multiyear_agg_2 <- pixel2poly::aggregate_raster_to_polygons(
  data_raster = data_raster_stack,
  aggregation_table = agg_table,
  aggregation_cols = c('polygon_id', 'ID_1', 'ID_2', 'NAME_1', 'NAME_2'),
  method = 'weighted.mean',
  z_dimension = z_dim,
  z_dimension_name = 'year',
  weighting_raster = pop_raster_stack
)
# Test 3: Sum
multiyear_agg_3 <- pixel2poly::aggregate_raster_to_polygons(
  data_raster = data_raster_stack,
  aggregation_table = agg_table,
  aggregation_cols = c('polygon_id', 'ID_1', 'ID_2', 'NAME_1', 'NAME_2'),
  method = 'sum',
  z_dimension = z_dim,
  z_dimension_name = 'year'
)
# Test 4: Weighted sum
multiyear_agg_4 <- pixel2poly::aggregate_raster_to_polygons(
  data_raster = data_raster_stack,
  aggregation_table = agg_table,
  aggregation_cols = c('polygon_id', 'ID_1', 'ID_2', 'NAME_1', 'NAME_2'),
  method = 'weighted.sum',
  z_dimension = z_dim,
  z_dimension_name = 'year',
  weighting_raster = pop_raster_stack
)
# Test 5: Weighted mean, one year of weighting data
multiyear_agg_5 <- pixel2poly::aggregate_raster_to_polygons(
  data_raster = data_raster_stack,
  aggregation_table = agg_table,
  aggregation_cols = c('polygon_id', 'ID_1', 'ID_2', 'NAME_1', 'NAME_2'),
  method = 'weighted.mean',
  z_dimension = z_dim,
  z_dimension_name = 'year',
  weighting_raster = pop_raster
)


## Test draws matrix to polygon aggregation --------------------------------------------->

# Test 1: Mean
draws_matrix_agg_1 <- pixel2poly::aggregate_draws_to_polygons(
  draws_matrix = draws_matrix_single_year,
  aggregation_table = agg_table,
  aggregation_cols = c('polygon_id', 'ID_1', 'ID_2', 'NAME_1', 'NAME_2'),
  method = 'mean'
)
# Test 2: Weighted mean
draws_matrix_agg_2 <- pixel2poly::aggregate_draws_to_polygons(
  draws_matrix = draws_matrix_single_year,
  aggregation_table = agg_table,
  aggregation_cols = c('polygon_id', 'ID_1', 'ID_2', 'NAME_1', 'NAME_2'),
  method = 'weighted.mean',
  weighting_raster = pop_raster
)
# Test 3: Sum
draws_matrix_agg_3 <- pixel2poly::aggregate_draws_to_polygons(
  draws_matrix = draws_matrix_single_year,
  aggregation_table = agg_table,
  aggregation_cols = c('polygon_id', 'ID_1', 'ID_2', 'NAME_1', 'NAME_2'),
  method = 'sum'
)
# Test 4: Weighted sum
draws_matrix_agg_4 <- pixel2poly::aggregate_draws_to_polygons(
  draws_matrix = draws_matrix_single_year,
  aggregation_table = agg_table,
  aggregation_cols = c('polygon_id', 'ID_1', 'ID_2', 'NAME_1', 'NAME_2'),
  method = 'weighted.sum',
  weighting_raster = pop_raster
)

## Test draws matrix to polygon aggregation: Multiple years of draws -------------------->

# Test 1: Mean
draws_multi_year_agg_1 <- pixel2poly::aggregate_draws_to_polygons(
  draws_matrix = draws_matrix_multi_year,
  aggregation_table = agg_table,
  aggregation_cols = c('polygon_id', 'ID_1', 'ID_2', 'NAME_1', 'NAME_2'),
  method = 'mean',
  z_dimension = z_dim,
  z_dimension_name = 'year',
)
# Test 2: Weighted mean
draws_multi_year_agg_2 <- pixel2poly::aggregate_draws_to_polygons(
  draws_matrix = draws_matrix_multi_year,
  aggregation_table = agg_table,
  aggregation_cols = c('polygon_id', 'ID_1', 'ID_2', 'NAME_1', 'NAME_2'),
  method = 'weighted.mean',
  z_dimension = z_dim,
  z_dimension_name = 'year',
  weighting_raster = pop_raster_stack
)
# Test 3: Sum
draws_multi_year_agg_3 <- pixel2poly::aggregate_draws_to_polygons(
  draws_matrix = draws_matrix_multi_year,
  aggregation_table = agg_table,
  aggregation_cols = c('polygon_id', 'ID_1', 'ID_2', 'NAME_1', 'NAME_2'),
  method = 'sum',
  z_dimension = z_dim,
  z_dimension_name = 'year',
)
# Test 4: Weighted sum
draws_multi_year_agg_4 <- pixel2poly::aggregate_draws_to_polygons(
  draws_matrix = draws_matrix_multi_year,
  aggregation_table = agg_table,
  aggregation_cols = c('polygon_id', 'ID_1', 'ID_2', 'NAME_1', 'NAME_2'),
  method = 'weighted.sum',
  z_dimension = z_dim,
  z_dimension_name = 'year',
  weighting_raster = pop_raster_stack
)
# Test 5: Weighted mean, one year of weighting data
draws_multi_year_agg_5 <- pixel2poly::aggregate_draws_to_polygons(
  draws_matrix = draws_matrix_multi_year,
  aggregation_table = agg_table,
  aggregation_cols = c('polygon_id', 'ID_1', 'ID_2', 'NAME_1', 'NAME_2'),
  method = 'weighted.mean',
  z_dimension = z_dim,
  z_dimension_name = 'year',
  weighting_raster = pop_raster
)
