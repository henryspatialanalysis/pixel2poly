#' Build aggregation table
#' 
#' @description Build a table to quickly aggregate from pixels to polygons
#' 
#' @param polygons terra SpatVector object. Should contain a unique ID field.
#' @param id_raster terra SpatRaster object. ID raster created by `build_id_raster()` for
#'   the polygons object. Should have the same CRS as `polygons` and completely cover it.
#' @param polygon_id_field (character) Unique identifier field in `polygons`.
#' @param moe (numeric, default 1e-4) Margin of error for pixel area fractions summing to
#'   approximately 1. If the sum of all pixel fractions across polygons is within
#'   [1 - moe, 1 + moe], the function will adjust the sum to exactly 1. Set to zero for
#'   no margin of error adjustment.
#' 
#' @return data.table containing three columns:
#'   * pixel_id: unique pixel ID from the ID raster
#'   * area_fraction: fraction of the pixel area falling within this polygon
#'   * Polygon identifiers taken from polygon_id_field
#' 
#' @seealso calculate_pixel_fractions_single_polygon()
#' 
#' @importFrom assertthat assert_that has_name noNA
#' @importFrom terra same.crs
#' @importFrom glue glue
#' @import data.table
#' @export
build_aggregation_table <- function(polygons, id_raster, polygon_id_field, moe = 1e-4){
  # Input data checks
  assertthat::assert_that(inherits(polygons, 'SpatVector'))
  assertthat::assert_that(inherits(id_raster, 'SpatRaster'))
  assertthat::assert_that(terra::same.crs(id_raster, polygons))
  assertthat::assert_that(nrow(polygons) >= 1)
  # Checks on the polygon ID field
  assertthat::assert_that(assertthat::has_name(polygons, polygon_id_field))
  poly_ids <- polygons[[polygon_id_field]][, 1]
  assertthat::assert_that(assertthat::noNA(poly_ids))
  assertthat::assert_that(
    sum(duplicated(poly_ids)) == 0L,
    msg = glue::glue("Polygon ID field {polygon_id_field} contains duplicates.")
  )

  agg_table <- lapply(poly_ids, function(poly_id){
    calculate_pixel_fractions_single_polygon(
      polygon = polygons[poly_ids == poly_id, ],
      id_raster = id_raster,
      polygon_id = poly_id
    )
  }) |> data.table::rbindlist()

  # Correct for small calculation errors in pixel sums
  if(moe > 0L){
    close_to_1 <- (agg_table
      [, .(area_fraction =sum(area_fraction)), by = pixel_id]
      [(area_fraction >= (1 - moe)) & (area_fraction <= (1 + moe)), pixel_id]
    )
    agg_table[
      pixel_id %in% close_to_1,
      area_fraction := area_fraction / sum(area_fraction),
      by = pixel_id
    ]
  }

  # Reorder the aggregation table
  agg_table <- (agg_table
    [, .(polygon_id, pixel_id, area_fraction)]
    [order(polygon_id, pixel_id)]
  )

  # Return the aggregation table
  return(agg_table)
}
