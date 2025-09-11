#' Point in Polygon Test for Generative Art
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Tests whether points are inside a polygon using Cartesian coordinates.
#' Distinguishes between points that are inside, outside, or on the polygon boundary.
#'
#' @param point_x Numeric vector of x coordinates for points to test
#' @param point_y Numeric vector of y coordinates for points to test
#' @param poly_x Numeric vector of x coordinates defining polygon vertices
#' @param poly_y Numeric vector of y coordinates defining polygon vertices
#'
#' @return Integer vector: 0 = outside polygon, 1 = inside polygon -OR- on polygon boundary
#'
#' @details
#' The function uses the sf package's geometry operations
#' with Cartesian coordinates. If the provided polygon's first and last vertices
#' are different, it is not considered to be a closed polygon. If this occurs,
#' it will be automatically closed. All input vectors must be numeric, and
#' point_x/point_y must have matching lengths, as must poly_x/poly_y.
#'
#' @export
#'
#' @examplesIf rlang::is_installed(c("dplyr", "ggplot2"))
#' @examples
#' # point_in_polygon can be used with basic vectors#
#' # X points we want to check the position of relevant to the polygon
#' point_x <- c(0.5, 1.5, 2.5, 1.0, 3.0)
#' # Y points we want to check the position of relevant to the polygon
#' point_y <- c(0.5, 1.5, 2.5, 0.0, 1.0)
#' # X points of the polygon we want to test against
#' poly_x <- c(0, 2, 2, 0, 0)
#' # Y points of the polygon we want to test against
#' poly_y <- c(0, 0, 2, 2, 0)
#'
#' # 0 = outside polygon, 1 = inside polygon or on boundary
#' point_in_polygon(point_x, point_y, poly_x, poly_y)
#'
#' # point_in_polygon can also be used within a data frame#
#' library(dplyr)
#'
#' test_data <- data.frame(px = c(0.5, 1.5, 2.5), py = c(0.5, 1.5, 2.5))
#' polygon_x <- c(0, 2, 2, 0, 0)
#' polygon_y <- c(0, 0, 2, 2, 0)
#'
#' test_data |>
#'   mutate(logic = point_in_polygon(px, py, polygon_x, polygon_y))
#'
#' # You can also see the results of the function visualized on a ggplot#
#' library(ggplot2)
#'
#' # Create test points and polygon for visualization
#' df_points_prep <-
#'   tibble(
#'     x = c(0.5, 1.5, 2.5, 1.0, 0.0, 3.0),
#'     y = c(0.5, 1.5, 2.5, 0.0, 1.0, 1.0)
#'   )
#' df_polygon <-
#'   tibble(
#'     x = c(0, 2, 2, 0, 0),
#'     y = c(0, 0, 2, 2, 0)
#'   )
#'
#' # Test the points and add labels for the plot
#' df_points <-
#'   df_points_prep |>
#'   mutate(
#'     position = point_in_polygon(x, y, df_polygon$x, df_polygon$y),
#'     position_string = case_match(position,
#'                                  0 ~ "Outside",
#'                                  1 ~ "Inside/On Boundary"
#'                                  ) |> factor(),
#'     color = case_match(position,
#'                        0 ~ "#e31a1c",
#'                        1 ~ "#33a02c"
#'                        )
#'   )
#'
#' # Pull out the colors for the plot points
#' unique_df <- unique(df_points[c("position_string", "color")])
#' vec_colors <- setNames(unique_df$color, unique_df$position_string)
#'
#' # Plot it
#' ggplot() +
#'   geom_polygon(data = df_polygon, aes(x = x, y = y),
#'                fill = "#104d70", alpha = 0.3, color = "black", linewidth = 1) +
#'   geom_point(data = df_points, aes(x = x, y = y, color = position_string), size = 4) +
#'   scale_color_manual(values = vec_colors) +
#'   labs(color = "Position:") +
#'   coord_equal()
#'
point_in_polygon <-
  function(point_x = NULL, point_y = NULL, poly_x = NULL, poly_y = NULL) {

    #==========================================================================#
    # Input Checks--------------------------------------------------------------
    #==========================================================================#
    ## Check that all inputs are provided---------------------------------------
    # Throw an error if any are missing, otherwise continue#
    ### point_x-----------------------------------------------------------------
    is.var.present(point_x)
    ### point_y-----------------------------------------------------------------
    is.var.present(point_y)
    ### poly_x------------------------------------------------------------------
    is.var.present(poly_x)
    ### poly_y------------------------------------------------------------------
    is.var.present(poly_y)

    ## Check that all inputs are the expected length----------------------------
    ### point_x-----------------------------------------------------------------
    check.length(point_x, expected_length = 1, expected_op = ">=")
    ### point_y-----------------------------------------------------------------
    check.length(point_y, expected_length = 1, expected_op = ">=")
    ### poly_x------------------------------------------------------------------
    check.length(poly_x, expected_length = 3, expected_op = ">=")
    ### poly_x------------------------------------------------------------------
    check.length(poly_y, expected_length = 3, expected_op = ">=")

    ## Check that point and poly lengths match----------------------------------
    ### points------------------------------------------------------------------
    flag_point_lengths <- length(point_x) != length(point_y)

    if(flag_point_lengths){
      c(
        "x" = paste("The length of {.var point_x}", error("must be equal"), "to the length of {.var point_y}"),
        "!" = paste("You've supplied:\n", callout("{.var point_x} Length: {length(point_x)} and {.var point_y} Length: {length(point_y)}")),
        "i" = paste(status("Check the ", "{.var point_x} and {.var point_y}"), "inputs.")
      ) |>
        cli::cli_abort()
    }

    ### polys-------------------------------------------------------------------
    flag_poly_lengths <- length(poly_x) != length(poly_y)

    if(flag_poly_lengths){
      c(
        "x" = paste("The length of {.var poly_x}", error("must be equal"), "to the length of {.var poly_y}"),
        "!" = paste("You've supplied:\n", callout("{.var poly_x} Length: {length(poly_x)} and {.var poly_y} Length: {length(poly_y)}")),
        "i" = paste(status("Check the ", "{.var poly_x} and {.var poly_y}"), "inputs.")
      ) |>
        cli::cli_abort()
    }

    ## Check that all inputs are numeric----------------------------------------
    ### point_x-----------------------------------------------------------------
    check.class(point_x, expected_class = "numeric")
    ### point_y-----------------------------------------------------------------
    check.class(point_y, expected_class = "numeric")
    ### poly_x------------------------------------------------------------------
    check.class(poly_x, expected_class = "numeric")
    ### poly_y------------------------------------------------------------------
    check.class(poly_y, expected_class = "numeric")

    #==========================================================================#
    # Spatial Work--------------------------------------------------------------
    #==========================================================================#
    # Convert points to an sf object#
    df_points <- tibble(x = point_x, y = point_y)
    df_points_sf <- sf::st_as_sf(df_points, coords = c("x", "y"))

    df_poly <- tibble(x = poly_x, y = poly_y)

    # Ensure polygon is closed
    first_coord <- df_poly[1, ]
    last_coord <- df_poly[nrow(df_poly), ]
    flag_poly_coords <- !identical(first_coord, last_coord)

    if (flag_poly_coords) {
      df_poly <- rbind(df_poly, first_coord)
    }

    df_polygon_sf <- sf::st_polygon(list(as.matrix(df_poly))) %>%
      sf::st_sfc() %>%
      sf::st_sf()

    # Test spatial relationships
    df_within_results <- sf::st_within(df_points_sf, df_polygon_sf)
    df_intersects_results <- sf::st_intersects(df_points_sf, df_polygon_sf)

    flag_within <- lengths(df_within_results) > 0
    flag_intersects <- lengths(df_intersects_results) > 0

    # Create result vector
    vec_result <- integer(length(flag_within))
    vec_result[!flag_within & !flag_intersects] <- 0L
    vec_result[flag_within & flag_intersects] <- 1L
    vec_result[!flag_within & flag_intersects] <- 2L
    vec_return <- ifelse(vec_result %in% c(1L, 2L), 1L, 0L)

    # spit it out
    return(vec_return)
  }

