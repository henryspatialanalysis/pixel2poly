#' Calculate fractional pixels in a polygon
#' 
#' @description Calculate the fraction of each pixel's area that falls within a single
#'   polygon
#' 
#' @details This is a helper function called by `build_aggregation_table()`.
#' 
#' @param polygon terra SpatVector object of length 1. The polygon to calculate fractional
#'   areas across.
#' @param id_raster terra SpatRaster object. ID raster created for the set of all polygons
#'   to be considered, created by `build_id_raster()`.
#' @param polygon_id (optional). ID for this polygon. Must have length 1.
#' 
#' @return data.table containing two or three columns:
#'   * pixel_id: unique pixel ID from the ID raster
#'   * area_fraction: fraction of the pixel area falling within this polygon
#'   * polygon_id (optional): If `polygon_id` was defined, it is added to the table
#' 
#' @importFrom assertthat assert_that is.scalar
#' @importFrom data.table data.table
#' @importFrom terra same.crs extract
#' @export
calculate_pixel_fractions_single_polygon <- function(polygon, id_raster, polygon_id = NULL){
  # Input data checks
  assertthat::assert_that(inherits(polygon, 'SpatVector'))
  assertthat::assert_that(inherits(id_raster, 'SpatRaster'))
  assertthat::assert_that(terra::same.crs(id_raster, polygon))
  assertthat::assert_that(nrow(polygon) == 1)
  if(!is.null(polygon_id)){
    assertthat::assert_that(assertthat::is.scalar(polygon_id))
  }

  # Use terra::extract to get pixel area fractions falling within the polygon
  # Drop the first column, which gives the polygon row ID
  extract_wide <- terra::extract(
    x = id_raster,
    y = polygon,
    fun = 'table',
    exact = TRUE,
    ID = FALSE
  )
  # Reshape long
  extract_long <- data.table::data.table(
    pixel_id = colnames(extract_wide) |> as.integer(),
    area_fraction = unlist(extract_wide[1, ])
  )
  # Optionally add the polygon ID to the table
  if(!is.null(polygon_id)) extract_long$polygon_id <- polygon_id

  return(extract_long)
}
