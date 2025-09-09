#' Point in Polygon Test for Generative Art
#'
#' Simple function to test whether points are inside a polygon using Cartesian coordinates.
#' Boundary points are considered inside the polygon.
#'
#' @param points A tibble/data.frame with x and y coordinates
#' @param x_col Name of x coordinate column (default: "x")
#' @param y_col Name of y coordinate column (default: "y")
#' @param polygon A tibble/data.frame with x and y coordinates defining polygon vertices
#' @param poly_x_col Name of polygon x coordinate column (default: "x")
#' @param poly_y_col Name of polygon y coordinate column (default: "y")
#'
#' @return Logical vector: TRUE = inside polygon, FALSE = outside polygon
#'
#' @examples
#' # Create test data
#' points <- tibble(x = c(0.5, 1.5, 2.5), y = c(0.5, 1.5, 2.5))
#' polygon <- tibble(x = c(0, 2, 2, 0, 0), y = c(0, 0, 2, 2, 0))
#'
#' # Test points
#' inside <- point_in_polygon(points, polygon = polygon)
#' print(inside)

point_in_polygon <- function(points,
                             x_col = "x",
                             y_col = "y",
                             polygon,
                             poly_x_col = "x",
                             poly_y_col = "y"
                             ) {

  # Input validation
  if (!all(c(x_col, y_col) %in% names(points))) {
    stop("Point coordinate columns not found in points data")
  }
  if (!all(c(poly_x_col, poly_y_col) %in% names(polygon))) {
    stop("Polygon coordinate columns not found in polygon data")
  }

  # Handle CRS specification
    # No CRS - pure Cartesian coordinates
    points_sf <- st_as_sf(points, coords = c(x_col, y_col))

    poly_coords <- polygon[, c(poly_x_col, poly_y_col)]
    if (!identical(poly_coords[1, ], poly_coords[nrow(poly_coords), ])) {
      poly_coords <- rbind(poly_coords, poly_coords[1, ])
    }

    polygon_sf <- st_polygon(list(as.matrix(poly_coords))) %>%
      st_sfc() %>%
      st_sf()

    # Use both st_within and st_intersects to distinguish boundary points
    within <- st_within(points_sf, polygon_sf)
    intersects <- st_intersects(points_sf, polygon_sf)

    within_bool <- lengths(within) > 0
    intersects_bool <- lengths(intersects) > 0

    # 0 = outside, 1 = inside, 2 = on boundary
    result <- ifelse(within_bool, 1,
                     ifelse(intersects_bool, 2, 0))

  return(result)
}
