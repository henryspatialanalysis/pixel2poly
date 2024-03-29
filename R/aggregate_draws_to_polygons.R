#' Aggregate grid cell draws to polygons: validation
#' 
#' @description Data validation for aggregate_draws_to_polygons
#' 
#' @seealso aggregate_draws_to_polygons
#' 
#' @param draws_matrix matrix, array, data.frame, corresponding to grid cell draws that
#'   will be aggregated to polygons.
#' @param aggregation_table Aggregation table linking pixels to polygon identifiers,
#'   created using `build_aggregation_table()`
#' @param aggregation_cols (character vector, default 'polygon_id') Polygon identifiers
#'   to use for aggregation.
#' @param method (character, default 'mean') Aggregation method: one of 'mean', 'sum',
#'   'weighted.mean', or 'weighted.sum'.
#' @param z_dimension (vector, default NULL) If passing a `draws_matrix` with multiple
#'   sets of estimates, how should each layer be identified?
#' @param z_dimension_name (default 'z') The field name for the "z" dimension
#'   corresponding to each set of estimates contained in `draws_matrix`.
#' @param weighting_raster (spatRaster, default NULL) The relative weighting of each whole
#'   pixel to the overall polygon value, for example, if calculating a population-weighted
#'   mean.
#' @param na.rm (bool, default TRUE) How to handle NA values in `draws_matrix` and
#'   `weighting_raster`.
#' 
#' @return Errors if checks fail; silently passes if checks pass
#' 
#' @importFrom assertthat assert_that has_name noNA
#' @importFrom terra compareGeom ncell nlyr
aggregate_draws_to_polygons_validation <- function(
  draws_matrix, aggregation_table, aggregation_cols, method, z_dimension,
  z_dimension_name, weighting_raster, na.rm
){
  # Checks on the aggregation table
  assertthat::assert_that(inherits(aggregation_table, 'data.table'))
  assertthat::assert_that(assertthat::has_name(aggregation_table, aggregation_cols))
  assertthat::assert_that(assertthat::noNA(aggregation_table$masked_pixel_id))
  non_na_px <- max(aggregation_table$masked_pixel_id)
  # Checks on the draws matrix
  assertthat::assert_that(
    inherits(draws_matrix, 'matrix') | inherits(draws_matrix, 'data.frame')
  )
  assertthat::assert_that(
    nrow(draws_matrix) %% non_na_px == 0,
    msg = paste(
      "Number of rows in the draws matrix must be exactly divisible by the number of ",
      "non-NA pixels in the ID raster, that is, max(aggregation_table$masked_pixel_id)."
    )
  )
  num_z <- nrow(draws_matrix) / non_na_px
  # Checks on the aggregation method
  valid_methods <- c('mean', 'weighted.mean', 'sum', 'weighted.sum')
  assertthat::assert_that(length(method) == 1L)
  assertthat::assert_that(method %in% valid_methods)
  # Checks on the Z dimension
  if(!is.null(z_dimension)){
    assertthat::assert_that(length(z_dimension) == num_z)
  }
  # Check for duplicates in the aggregated table fields
  num_draws <- ncol(draws_matrix)
  draw_cols <- paste0('draw_', seq_len(num_draws))
  aggregated_all_fields <- c(aggregation_cols, draw_cols)
  if(!is.null(z_dimension) | (num_z > 1L)){
    assertthat::assert_that(length(z_dimension_name) == 1L)
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
    weighting_raster_n_layers <- terra::nlyr(weighting_raster)
    # The number of layers in the weighting raster will usually equal the number of Z
    #  dimensions. A weighting raster with one layer will pass checks but yield a warning
    if((num_z > 1L) & (weighting_raster_n_layers == 1L)){
      warning(
        "Data raster has multiple layers but weighting raster has only one layer. The ",
        "same weighting raster will be used for each z dimension."
      )
    } else {
      assertthat::assert_that(num_z == weighting_raster_n_layers)
    }
  }
  # Checks on na.rm
  assertthat::assert_that(inherits(na.rm, 'logical'))
  assertthat::assert_that(length(na.rm) == 1L)
  invisible(NULL)
}


#' Aggregate grid cell draws to polygons
#' 
#' @description Aggregate grid cell draws to polygons using an aggregation table
#' 
#' @details This is a more efficient and feature-rich alternative to terra's zonal
#'   statistics functions. Features include:
#'   - Always fractionally aggregate, weighting by area of the pixel in a polygon
#'   - Optionally weight by both area fraction and a weighting raster (e.g. population)
#'   - Means or sums of raster values across polygons
#'   - Optionally aggregate multiple years of raster data at once
#' 
#' @param draws_matrix matrix, array, data.frame, corresponding to grid cell draws that
#'   will be aggregated to polygons:
#'   - Each row represents a non-NA grid cell in the ID raster. If the matrix contains
#'     multiple years of estimates, the matrix is ordered by year, then by
#'     masked_pixel_id. For example, if there are 200 non-NA pixels in the ID raster and
#'     five years of draws, then the matrix contains 1000 rows: row 200 corresponds to
#'     (year 1, masked_pixel_id 200), row 201 corresponds to (year 2, masked_pixel_id 1),
#'     and so on.
#'   - Each column represents a draw. There should be no non-draw columns (such as ID
#'     fields) in the `draws_matrix`.
#' @param aggregation_table Aggregation table linking pixels to polygon identifiers,
#'   created using `build_aggregation_table()`
#' @param aggregation_cols (character vector, default 'polygon_id') Polygon identifiers
#'   to use for aggregation. Must be field names within `aggregation_table`.
#' @param method (character, default 'mean') Aggregation method: one of 'mean', 'sum',
#'   'weighted.mean', or 'weighted.sum'. The latter two methods require a
#'   `weighting_raster.`
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
#' @importFrom stats na.omit weighted.mean
#' @import data.table
#' @export
aggregate_draws_to_polygons <- function(
  draws_matrix, aggregation_table, aggregation_cols = 'polygon_id', method = 'mean',
  z_dimension = NULL, z_dimension_name = 'z', weighting_raster = NULL, na.rm = TRUE
){
  ## Data preparation
  # Validate function inputs
  aggregate_draws_to_polygons_validation(
    draws_matrix = draws_matrix,
    aggregation_table = aggregation_table,
    aggregation_cols = aggregation_cols,
    method = method,
    z_dimension = z_dimension,
    z_dimension_name = z_dimension_name,
    weighting_raster = weighting_raster,
    na.rm = na.rm
  )
  # Determine whether Z dimension will be reported
  num_non_na_pixels <- max(aggregation_table$masked_pixel_id)
  num_z <- nrow(draws_matrix) / num_non_na_pixels
  report_z <- ((num_z > 1L) | !is.null(z_dimension))
  # Fill Z dimension, if needed
  if(is.null(z_dimension)) z_dimension <- seq_len(num_z)

  ## Create a table indexing draws to a masked pixel ID
  draws_table <- as.data.table(draws_matrix)
  num_draws <- ncol(draws_table)
  draw_cols <- paste0('draw_', seq_len(num_draws))
  colnames(draws_table) <- draw_cols
  # Add masked pixel ID
  draws_table$masked_pixel_id <- rep(seq_len(num_non_na_pixels), times = num_z)
  draws_table$z__ <- rep(z_dimension, each = num_non_na_pixels)
  # Optionally add weights
  need_weights <- grepl('^weighted\\.', method)
  if(need_weights){
    weights_num_z <- terra::nlyr(weighting_raster)
    weights_ncell <- terra::ncell(weighting_raster)
    id_to_masked_id <- unique(aggregation_table[, .(pixel_id, masked_pixel_id)])
    weights_table <- (
      data.table(
        w__ = terra::values(weighting_raster, mat = F),
        pixel_id = rep(seq_len(weights_ncell), times = weights_num_z),
        z__ = rep(z_dimension[seq_len(weights_num_z)], each = weights_ncell)
      )
      [id_to_masked_id, masked_pixel_id := i.masked_pixel_id, on = 'pixel_id']
      [!is.na(masked_pixel_id), ]
    )
    if(weights_num_z == num_z){
      draws_table[weights_table, w__ := i.w__, on = c('masked_pixel_id', 'z__')]
    } else if(weights_num_z == 1){
      draws_table[weights_table, w__ := i.w__, on = 'masked_pixel_id']
    } else {
      stop("ISSUE: Weighting raster has a different number of layers from data raster.")
    }
  } else {
    draws_table$w__ <- 1L
  }

  ## Merge pixel table with aggregation table
  disaggregated_table <- merge(
    x = draws_table,
    y = aggregation_table,
    by = 'masked_pixel_id',
    allow.cartesian = T
  )

  ## Aggregation varies depending on method
  if(method %in% c('mean', 'weighted.mean')){
    prepped_table <- disaggregated_table[
      , lapply(.SD, weighted.mean, w = area_fraction * w__, na.rm = na.rm),
      .SDcols = draw_cols,
      by = c(aggregation_cols, 'z__')
    ]
  } else if(method %in% c('sum', 'weighted.sum')){
    prepped_table <- disaggregated_table[
      , lapply(.SD, function(x) sum(x * area_fraction * w__, na.rm = na.rm)),
      .SDcols = draw_cols,
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

  # Return
  return(prepped_table)
}
