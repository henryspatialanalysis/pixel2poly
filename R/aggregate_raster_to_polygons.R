#' Aggregate a raster to polygons: validation
#' 
#' @description Data validation for aggregate_raster_to_polygons
#' 
#' @seealso aggregate_raster_to_polygons
#' 
#' @param data_raster SpatRaster containing data to be aggregated to polygons.
#' @param aggregation_table Aggregation table linking pixels to polygon identifiers,
#'   created using `build_aggregation_table()`
#' @param aggregation_cols (character vector, default 'polygon_id') Polygon identifiers
#'   to use for aggregation.
#' @param method (character, default 'mean') Aggregation method: one of 'mean', 'sum',
#'   'weighted.mean', or 'weighted.sum'.
#' @param aggregated_field (character, default 'data') Name of the aggregated raster
#'   values in the output table.
#' @param z_dimension (vector, default NULL) If passing a `data_raster` with multiple
#'   layers, how should each layer be identified?
#' @param z_dimension_name (default 'z') The field name for the "z" dimension
#'   corresponding to each layer of the `data_raster`.
#' @param weighting_raster (spatRaster, default NULL) The relative weighting of each whole
#'   pixel to the overall polygon value, for example, if calculating a population-weighted
#'   mean.
#' @param na.rm (bool, default TRUE) How to handle NA pixels in `data_raster` and
#'   `weighting_raster`.
#' 
#' @return Errors if checks fail; silently passes if checks pass
#' 
#' @importFrom assertthat assert_that has_name noNA
#' @importFrom terra compareGeom ncell nlyr
aggregate_raster_to_polygons_validation <- function(
  data_raster, aggregation_table, aggregation_cols, method, aggregated_field, z_dimension,
  z_dimension_name, weighting_raster, na.rm
){
  # Checks on the aggregation table
  assertthat::assert_that(inherits(aggregation_table, 'data.table'))
  assertthat::assert_that(assertthat::has_name(aggregation_table, aggregation_cols))
  # Checks on the data raster
  assertthat::assert_that(inherits(data_raster, 'SpatRaster'))
  assertthat::assert_that(assertthat::noNA(aggregation_table$pixel_id))
  max_pixel_id <- aggregation_table[, max(pixel_id)]
  assertthat::assert_that(
    (terra::ncell(data_raster)) >= max_pixel_id,
    msg = 'The data raster has dimensions larger than expected by the aggregation table.'
  )
  # Checks on the aggregation method
  valid_methods <- c('mean', 'weighted.mean', 'sum', 'weighted.sum')
  assertthat::assert_that(length(method) == 1L)
  assertthat::assert_that(method %in% valid_methods)
  # Checks on the aggregated field name
  assertthat::assert_that(inherits(aggregated_field, 'character'))
  assertthat::assert_that(length(aggregated_field) == 1L)
  # Checks on the Z dimension
  if(!is.null(z_dimension)){
    assertthat::assert_that(length(z_dimension) == terra::nlyr(data_raster))
  }
  # Check for duplicates in the aggregated table fields
  aggregated_all_fields <- c(aggregation_cols, aggregated_field)
  if(!is.null(z_dimension) | (terra::nlyr(data_raster) > 1L)){
    assertthat::assert_that(!is.null(z_dimension_name))
    aggregated_all_fields <- c(aggregated_all_fields, z_dimension_name)
  }
  duplicates <- aggregated_all_fields[duplicated(aggregated_all_fields)] |> unique()
  if(length(duplicates) > 0L) stop(
    "Fields ", paste(duplicates, collapse = ', '), " would appear multiple times in the ",
    "aggregated table."
  )
  # Checks on the weighting raster, if needed
  if(grepl('^weighted', method)){
    assertthat::assert_that(inherits(weighting_raster, 'SpatRaster'))
    assertthat::assert_that(terra::compareGeom(x = data_raster, y = weighting_raster))
    data_raster_n_layers <- terra::nlyr(data_raster)
    weighting_raster_n_layers <- terra::nlyr(weighting_raster)
    # The weighting raster will usually have the same number of layers as the weighting
    #  raster. A weighting raster with one layer will pass checks but yield a warning
    if((data_raster_n_layers > 1L) & (weighting_raster_n_layers == 1L)){
      warning(
        "Data raster has multiple layers but weighting raster has only one layer. The ",
        "same weighting raster will be used for each z dimension."
      )
    } else {
      assertthat::assert_that(data_raster_n_layers == weighting_raster_n_layers)
    }
  }
  # Checks on na.rm
  assertthat::assert_that(inherits(na.rm, 'logical'))
  assertthat::assert_that(length(na.rm) == 1L)
  invisible(NULL)
}


#' Aggregate a raster to polygons
#' 
#' @description Aggregate raster values to polygons using an aggregation table
#' 
#' @details This is a more efficient and feature-rich alternative to terra's zonal
#'   statistics functions. Features include:
#'   - Always fractionally aggregate, weighting by area of the pixel in a polygon
#'   - Optionally weight by both area fraction and a weighting raster (e.g. population)
#'   - Means or sums of raster values across polygons
#'   - Optionally aggregate multiple years of raster data at once
#' 
#' @param data_raster SpatRaster containing data to be aggregated to polygons.
#' @param aggregation_table Aggregation table linking pixels to polygon identifiers,
#'   created using `build_aggregation_table()`
#' @param aggregation_cols (character vector, default 'polygon_id') Polygon identifiers
#'   to use for aggregation. Must be field names within `aggregation_table`.
#' @param method (character, default 'mean') Aggregation method: one of 'mean', 'sum',
#'   'weighted.mean', or 'weighted.sum'. The latter two methods require a
#'   `weighting_raster.`
#' @param aggregated_field (character, default 'data') Name of the aggregated raster
#'   values in the output table.
#' @param z_dimension (vector, default NULL) If passing a `data_raster` with multiple
#'   layers, how should each layer be identified? Should be a vector with length equal to
#'   the number of layers in `data_raster`. If left as `NULL`, the default, and
#'   `data_raster` has 1 layer, no z dimension will be added. If left as `NULL` and
#'   `data_raster` has more than 1 layer, will default to (1, 2, ..., N layers).
#' @param z_dimension_name (default 'z') The field name for the "z" dimension
#'   corresponding to each layer of the `data_raster`. This field is only added if
#'   `z_dimension` is passed or if `data_raster` has more than one layer.
#' @param weighting_raster (spatRaster, default NULL) The relative weighting of each whole
#'   pixel to the overall polygon value, for example, if calculating a population-weighted
#'   mean. Required for methods 'weighted.mean' and 'weighted.sum', ignored for the other
#'   methods.
#' @param na.rm (bool, default TRUE) How to handle NA pixels in `data_raster` and
#'   `weighting_raster`. If set to TRUE but ALL pixels in a polygon are NA, will still
#'   return an NA value for the polygon.
#' 
#' @return data.table containing polygon identifiers, (optionally) layer identifiers in
#'   the `z_dimension_name` column, and data values aggregated by polygon.
#' 
#' @seealso build_aggregation_table
#' 
#' @importFrom terra nlyr ncell values
#' @importFrom stats weighted.mean
#' @import data.table
#' @export
aggregate_raster_to_polygons <- function(
  data_raster, aggregation_table, aggregation_cols = 'polygon_id', method = 'mean',
  aggregated_field = 'data', z_dimension = NULL, z_dimension_name = 'z',
  weighting_raster = NULL, na.rm = TRUE
){
  ## Data preparation
  # Validate function inputs
  aggregate_raster_to_polygons_validation(
    data_raster = data_raster,
    aggregation_table = aggregation_table,
    aggregation_cols = aggregation_cols,
    method = method,
    aggregated_field = aggregated_field,
    z_dimension = z_dimension,
    z_dimension_name = z_dimension_name,
    weighting_raster = weighting_raster,
    na.rm = na.rm
  )
  # Determine whether Z dimension will be reported
  num_z <- terra::nlyr(data_raster)
  report_z <- ((num_z > 1L) | !is.null(z_dimension))
  # Fill Z dimension, if needed
  if(is.null(z_dimension)) z_dimension <- seq_len(num_z)

  ## Create a table indexing pixels to their ID
  px_table <- data.table::data.table(val__ = terra::values(data_raster, mat = F))
  px_table$pixel_id <- rep(seq_len(terra::ncell(data_raster)), times = num_z)
  px_table$z__ <- rep(z_dimension, each = terra::ncell(data_raster))
  # Optionally add weights
  need_weights <- grepl('^weighted\\.', method)
  weights_num_z <- terra::nlyr(weighting_raster)
  if(need_weights){
    if(num_z == weights_num_z){
      px_table$w__ <- terra::values(weighting_raster, mat = F)
    } else if(weights_num_z == 1){
      px_table$w__ <- rep(terra::values(weighting_raster, mat = F), times = num_z)
    } else {
      stop("ISSUE: Weighting raster has a different number of layers from data raster.")
    }
  } else {
    px_table$w__ <- 1L
  }

  ## Merge pixel table with aggregation table
  disaggregated_table <- merge(
    x = px_table,
    y = aggregation_table,
    by = 'pixel_id',
    allow.cartesian = T
  )

  ## Aggregation varies depending on method
  if(method %in% c('mean', 'weighted.mean')){
    prepped_table <- disaggregated_table[
      , .(val__ = weighted.mean(val__, w = area_fraction * w__, na.rm = na.rm)),
      by = c(aggregation_cols, 'z__')
    ]
  } else if(method %in% c('sum', 'weighted.sum')){
    prepped_table <- disaggregated_table[
      , .(val__ = sum(val__ * area_fraction * w__, na.rm = na.rm)),
      by = c(aggregation_cols, 'z__')
    ]
  } else {
    stop("Method ", method, " not yet available as an aggregation function.")
  }

  ## Format for return
  if(report_z){
    if(z_dimension_name != 'z__') setnames(prepped_table, 'z__', z_dimension_name)
    setorderv(prepped_table, c(z_dimension_name, aggregation_cols))
  } else {
    prepped_table[, z__ := NULL ]
    setorderv(prepped_table, aggregation_cols)
  }
  if(aggregated_field != 'val__') setnames(prepped_table, 'val__', aggregated_field)

  # Return
  return(prepped_table)
}
