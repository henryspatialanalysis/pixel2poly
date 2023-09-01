#' Build ID raster
#' 
#' @description Build an ID raster matching the extent of a vector dataset
#' 
#' @details The ID raster will be used to build the aggregation table. Each pixel has a
#'   unique integer value from 1 to the number of pixels in the ID raster.
#' 
#' @param polygons terra SpatVector object. The polygons to be aggregated to
#' @param template_raster (optional) terra SpatRaster object. The template raster should
#'   contain and have the same CRS as the polygons. If template raster is `NULL`, the
#'   default, uses the default world template raster from `make_world_template_raster()`.
#' 
#' @return ID raster. A terra SpatRaster object that minimally encloses the polygons
#' 
#' @importFrom terra same.crs crop values
#' @importFrom assertthat assert_that
#' @export
build_id_raster <- function(polygons, template_raster = NULL){
  # Get default template raster, if needed
  if(is.null(template_raster)) template_raster <- make_world_template_raster()

  # Input data checks
  assertthat::assert_that(inherits(template_raster, 'SpatRaster'))
  assertthat::assert_that(inherits(polygons, 'SpatVector'))
  assertthat::assert_that(terra::same.crs(template_raster, polygons))

  # Set ID raster extent
  id_raster <- terra::crop(x = template_raster, y = polygons, snap = 'out')

  # Fill values of the ID raster
  terra::values(id_raster) <- seq_len(prod(dim(id_raster)))

  return(id_raster)
}
