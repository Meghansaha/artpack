#' Rotate Points in a Data Frame Based on an Anchor Point
#' @description
#'Rotates the `x` and `y` points in a given data frame by a given angle based on a designated anchor point.
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
#' @importFrom cli cli_abort
#'
#' @examples
#' library(ggplot2)
#' original_square <- data.frame(x = c(0,3,3,0,0),
#'                               y = c(0,0,3,3,0))

#' rotated_square <- rotator(data = original_square,
#'                           angle = 45,
#'                           anchor = "center")
#'
#' ggplot()+
#'   geom_path(data = original_square,
#'                      aes(x,y),
#'                      color = "red")+
#'   geom_polygon(data = rotated_square,
#'                         aes(x,y),
#'                         fill = "purple")+
#'   coord_equal()
#'
rotator <- function(data, x, y, angle = 5, anchor = "center"){

  # Data is present
  if(missing(data)){
    cli::cli_abort(c("x" = paste("{.var data} is",error("missing")),
                     "!" = paste("{.var data} should be a", status("dataframe"), "or" ,status("tibble"))))
  }

  # Pulling existing column names
  x_name <- data |> dplyr::select({{x}}) |> names()
  y_name <- data |> dplyr::select({{y}}) |> names()
  x <- dplyr::pull(.data = data, {{x}})
  y <- dplyr::pull(.data = data, {{y}})



  rad <- (angle * pi)/180

  if(!is.data.frame(data)){
    cli::cli_abort(c("x" = "{.var data} should be of type data frame or a tibble, not {.cls {class(data)}} "))
  }

  anchor = switch(anchor,
                  "center" = list("x" = (min({{ x }})+max({{ x }}))/2,
                                  "y" = (min({{ y }})+max({{ y }}))/2),
                  "bottom" = list("x" = (min({{ x }})+max({{ x }}))/2,
                                  "y" = min({{ y }})),
                  "top" = list("x" = (min({{ x }})+max({{ x }}))/2,
                               "y" = max({{ y }})),
                  "left" = list("x" = min({{ x }}),
                                "y" = (min({{ y }})+max({{ y }}))/2),
                  "right" = list("x" = max({{ x }}),
                                 "y" = (min({{ y }})+max({{ y }}))/2),
                  cli::cli_abort(c("x" = "{.var anchor} is not a valid anchor option.", "Valid options include: 'center', 'bottom', 'top', 'left', and 'right'"))
                  )


  data_rotated <- data |>
    dplyr::mutate(x2 = ({{x}} - anchor$x)*cos(rad) - ({{y}} - anchor$y)*sin(rad) + anchor$x,
           y2 = ({{x}} - anchor$x)*sin(rad) + ({{y}} - anchor$y)*cos(rad) + anchor$y,
           x = x2,
           y = y2) |>
    dplyr::select(x,y) |>
    dplyr::rename(rlang::`!!`(x_name)  := x,
           rlang::`!!`(y_name)  := y)

  df_out <- data_rotated |>
    cbind(data |>
            dplyr::select(c(-{{x}},{{y}}))
          )

  return(df_out)

}
