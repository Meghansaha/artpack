#' Rotator
#' Rotates the points in a given data frame by a given angle based on a designated anchor point.
#'
#' @param data A data frame or tibble with at least `x` and `y` variables
#' @param angle The angle (in degrees) the points in `data` will be rotated around it's anchor
#' @param anchor The anchor point for the rotation. Default is "center"
#'
#' @return #A data frame
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @examples
#' original_square <- data.frame(x = c(0,3,3,0,0),
#' y = c(0,0,3,3,0))

#' rotated_square <- rotator(data = original_square, angle = 45, anchor = "center")
#'
#' ggplot2::ggplot()+
#'   ggplot2::geom_path(data = original_square, ggplot2::aes(x,y), color = "red")+
#'   ggplot2::geom_polygon(data = rotated_square, ggplot2::aes(x,y), fill = "purple")+
#'   ggplot2::coord_equal()
#'
rotator <- function(data = NULL, angle = 5, anchor = "center"){

  x = NULL
  y = NULL
  x2 = NULL
  y2 = NULL

  rad <- (angle * pi)/180

  if(!is.data.frame(data)){
    stop("`data` should be of type data frame or a tibble, not ",class(data))
  }

  anchor = switch(anchor,
                  "center" = list("x" = (min(data$x)+max(data$x))/2,
                                  "y" = (min(data$y)+max(data$y))/2),
                  "bottom" = list("x" = (min(data$x)+max(data$x))/2,
                                  "y" = min(data$y)),
                  "top" = list("x" = (min(data$x)+max(data$x))/2,
                               "y" = max(data$y)),
                  "left" = list("x" = min(data$x),
                                "y" = (min(data$y)+max(data$y))/2),
                  "right" = list("x" = max(data$x),
                                 "y" = (min(data$y)+max(data$y))/2),
                  stop(paste0("`",anchor,"`"," is not a valid anchor option. Valid options include: 'center', 'bottom', 'top', 'left', and 'right'")))


  rotated_shape <- data |>
    mutate(x2 = (x - anchor$x)*cos(rad) - (y - anchor$y)*sin(rad) + anchor$x,
           y2 = (x - anchor$x)*sin(rad) + (y - anchor$y)*cos(rad) + anchor$y,
           x = x2,
           y = y2) |>
    select(-c(x2,y2))

  rotated_shape

}
