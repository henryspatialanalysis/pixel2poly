#' Make world template raster
#' 
#' @description Create a template raster for the world with approximately 5x5km resolution
#'   at the equator, matching many common raster covariates for health.
#' 
#' @details The raster has the following specifications:
#'   * 4320 rows, 8640 columns
#'   * Resolution: 0.04166667 decimal degrees, approx. 5km at the equator
#'   * CRS: WGS 1984 unprojected latitude/longitude, `terra::crs('EPSG:4326')`
#'   * Values: All NA. Used exclusively for creating a shapefile-specific ID raster
#' 
#' @returns terra SpatRaster matching the details above
#' 
#' @importFrom terra rast crs
#' @export
make_world_template_raster <- function(){
  world_template_raster <- terra::rast(
    nrows = 4320,
    ncols = 8640,
    crs = terra::crs("EPSG:4326")
  )
  return(world_template_raster)
}
