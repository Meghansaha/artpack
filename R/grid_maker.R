#' Create Data for A Custom-built Square Grid
#' @description
#'Creates a dataframe of `x` and `y` points to visualize a square grid based on given `x` and `y` limits.
#'Providing a color palette and fill style are optional.
#'
#' @param xlim A numeric vector with two X limits. A minimum and maximum limit for the X axis.
#' @param ylim A numeric vector with two Y limits. A minimum and maximum limit for the Y axis.
#' @param size A numeric input. The size of the grid. How many shapes will appear in a single row or column.
#' @param pal Optional. A character vector of color codes to be applied to the grid.
#' @param fill_style Optional. A character input. "range" or "random". Determines how the color palette is mapped.
#'
#' @return a tibble
#'
#' @importFrom purrr pmap
#' @importFrom tibble tibble
#' @importFrom grDevices colorRampPalette
#' @export
#'
#' @examples
#'grid_data <- grid_maker(xlim = c(0,1),
#'                        ylim = c(0,1),
#'                        size = 2,
#'                        pal = c("red", "black", "purple"))
#'
#' ggplot2::ggplot()+
#'   ggplot2::geom_polygon(data = grid_data,
#'                         ggplot2::aes(x,y, group = group),
#'                         fill = grid_data$fill)+
#'   ggplot2::coord_equal()
#'
grid_maker <- function(xlim, ylim, size, pal = NULL, fill_style = "range"){

  #Creating group names for each individual square#
  group_names = paste0("square_", rep(1:(size * size), each = 5))

  #Calculating X and Y points manually based on the x and y limits#
  x_points = seq(xlim[1], xlim[2], length.out = size + 1)
  y_points = seq(ylim[1], ylim[2], length.out = size + 1)

  #Calculating appropriate transformations to create the grid#
  point_x_indexes = rep(c(1,2,2,1,1) + rep(0:(size-1), each = 5), times = size)
  point_y_indexes = rep(c(1,1,2,2,1), times = size) + rep(0:(size-1), each = 5*size)

  #Applying the transformations#
  x_points_grid = x_points[point_x_indexes]
  y_points_grid = y_points[point_y_indexes]

  #Calculating the appropriate mapping for the fills#

  #No palette entered#
  if(is.null(pal)){
    #Compiling all options#
    grid_comps = list(x_points_grid, y_points_grid, group_names)

    grid <- purrr::pmap_df(grid_comps, ~ tibble::tibble(x = ..1,
                                                        y = ..2,
                                                        group = ..3))

    return(grid)

    #If palette entered#
  } else{

    fill = switch(fill_style,
                  "range" = rep(colorRampPalette(pal)(size *size), each = 5),
                  "random" = rep(sample(colorRampPalette(pal)(size *size)), each = 5)
    )

    grid_comps = list(x_points_grid, y_points_grid, fill, group_names)

    grid = purrr::pmap_df(grid_comps, ~ tibble::tibble(x = ..1,
                                                       y = ..2,
                                                       fill = ..3,
                                                       group = ..4))

    return(grid)
  }

}
