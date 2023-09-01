library(terra); library(data.table)

## Get pixel fractions contained within a particular polygon
get_pixel_fractions <- function(id_raster, polygon, polygon_id = NULL){
  extract_wide <- terra::extract(x = id_raster, y = polygon, fun = 'table', exact = T)[, -1]
  extract_long <- data.frame(
    pixel_id = colnames(extract_wide) |> as.integer(),
    area_fraction = unlist(extract_wide[1, ])
  )
  if(!is.null(polygon_id)) extract_long$polygon_id <- polygon_id
  return(extract_long)
}


# Some test data from the `terra` package
test_vec <- terra::vect(system.file("ex/lux.shp", package="terra"))
test_rast <- terra::rast(system.file("ex/elev.tif", package="terra"))
# Margin of error - when we suspect pixel fractions should actually sum to 1
moe <- 1e-4

# Create ID raster
id_rast <- test_rast
id_rast[seq_len(prod(dim(id_rast)))] <- seq_len(prod(dim(id_rast)))

# Create aggregation table
agg_table <- seq_len(nrow(test_vec)) |>
  lapply(
    function(ii) get_pixel_fractions(
      id_raster = id_rast, polygon = test_vec[ii, ], polygon_id = ii
    )
  ) |>
  lapply(data.table::as.data.table) |>
  data.table::rbindlist()

# Correct for small calculation errors in pixel sums
close_to_1 <- (agg_table
  [, .(area_fraction = sum(area_fraction)), by = pixel_id]
  [(area_fraction > 1 - moe) & (area_fraction < 1 + moe), pixel_id]
)
agg_table[
  pixel_id %in% close_to_1,
  area_fraction := area_fraction / sum(area_fraction),
  by = pixel_id
]

# Check that aggregation yields similar results
weighted_agg_table <- (
  merge(
    x = data.table(elevation = values(test_rast)[, 1], pixel_id = values(id_rast)[, 1]),
    y = agg_table,
    by = 'pixel_id',
    allow.cartesian = T
  )
  [, .(elevation = weighted.mean(elevation, w = area_fraction, na.rm = T)), by = polygon_id]
  [order(polygon_id)]
)
weighted_zonal <- terra::zonal(
  x = test_rast, z = test_vec, weighted = T, exact = T, na.rm=T
)
weighted_agg_table$zonal_compare <- weighted_zonal$elevation
weighted_agg_table[, pct_diff := abs(elevation - zonal_compare) / elevation]
