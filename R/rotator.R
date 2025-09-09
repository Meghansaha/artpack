#' Rotate Points in a Data Frame Based on an Anchor Point
#' @description
#' Rotates the `x` and `y` points in a given data frame by a given angle based on a designated anchor point.
#'
#' @param data A data frame or tibble with at least `x` and `y` variables
#' @param x A numeric variable in `data`. The variable intended to be plotted on the x axis in a `ggplot`.
#' @param y A numeric variable in `data`. The variable intended to be plotted on the y axis in a `ggplot`.
#' @param angle The angle (in degrees) the points in `data` will be rotated around it's anchor
#' @param anchor The anchor point for the rotation. Default is "center". Options include:"center", "bottom", "top", "left", and "right"
#' @param drop Logical `TRUE` or `FALSE` that determines if all other variables that are not being rotated are removed from the final output. Default is `FALSE`.
#'
#' @return A data frame
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr pull
#' @importFrom cli cli_abort
#' @importFrom purrr map_chr
#' @importFrom knitr combine_words
#'
#' @examplesIf rlang::is_installed("ggplot2")
#' @examples
#' library(ggplot2)
#' original_square <- data.frame(
#'   x = c(0, 3, 3, 0, 0),
#'   y = c(0, 0, 3, 3, 0)
#' )

#' rotated_square <- rotator(data = original_square,
#'                           x = x,
#'                           y = y,
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
#'
rotator <- function(data, x, y, angle = 5, anchor = "center", drop = FALSE) {
  # ===========================================================================#
  # Global Variable Handling----------------------------------------------------
  # ===========================================================================#
  # Establish these for use later in the workflow
  `:=` <- NULL

  # ===========================================================================#
  # Input Checks----------------------------------------------------------------
  # ===========================================================================#
  ## Check that all inputs are provided-----------------------------------------
  ### data----------------------------------------------------------------------
  is.var.present(data)

  ## Pulling column names-------------------------------------------------------
  x_name <- data |>
    dplyr::select({{ x }}) |>
    names()

  y_name <- data |>
    dplyr::select({{ y }}) |>
    names()

  # Pulling actual column data--------------------------------------------------
  x <- data |>
    dplyr::select({{ x }}) |>
    dplyr::pull()

  y <- data |>
    dplyr::select({{ y }}) |>
    dplyr::pull()

  ### angle---------------------------------------------------------------------
  is.var.present(angle)
  ### anchor--------------------------------------------------------------------
  is.var.present(anchor)
  ### drop----------------------------------------------------------------------
  is.var.present(drop)

  ## Check that all inputs are of expected class--------------------------------
  ### data----------------------------------------------------------------------
  check.class(data, expected_class = "data.frame")
  ### x-------------------------------------------------------------------------
  check.class(x, expected_class = "numeric")
  ### y-------------------------------------------------------------------------
  check.class(y, expected_class = "numeric")
  ### anchor--------------------------------------------------------------------
  # It can be a string#
  anchor_string <- check.class(anchor, expected_class = "character", required = FALSE)
  # Or a numeric vector#
  anchor_numeric <- check.class(anchor, expected_class = "numeric", required = FALSE)
  # Flag and throw an error if it's neither#
  anchor_class_check <- anchor_string | anchor_numeric

  if(anchor_class_check == FALSE){

    actual_anchor_class <- class(anchor)
    c(
      "x" = paste("{.var anchor} must be of class", error("<character> -OR- <numeric>")),
      "!" = paste("The input you've supplied, {.var anchor}, is of class", callout("<{actual_anchor_class}>")),
      "i" = paste(status("Check the ", "{.var anchor}"), "input.")
    ) |>
      cli::cli_abort()
  }
  ### angle---------------------------------------------------------------------
  check.class(angle, expected_class = "numeric")
  ### drop----------------------------------------------------------------------
  check.class(drop, expected_class = "logical")

  ## Check that all applicable inputs are of expected length--------------------
  ### data----------------------------------------------------------------------
  check.length(data, expected_length = 1, expected_op = ">=")
  ### x-------------------------------------------------------------------------
  data_rows <- nrow(data)
  check.length(x, expected_length = data_rows)
  ### y-------------------------------------------------------------------------
  check.length(y, expected_length = data_rows)
  ### angle---------------------------------------------------------------------
  check.length(angle, expected_length = 1)
  ### anchor--------------------------------------------------------------------
  if(anchor_numeric){
    check.length(anchor, expected_length = 2)
  }

  if(anchor_string){
    check.length(anchor, expected_length = 1)
  }
  ### drop----------------------------------------------------------------------
  check.length(drop, expected_length = 1)

  ## Check that all applicable inputs are of expected values--------------------
  ### anchor--------------------------------------------------------------------
  if(anchor_string){
    is.expected.value(anchor, expected_values = c("center", "bottom", "top", "left", "right"))
  }

  # ===========================================================================#
  # FX Helpers and defaults----------------------------------------------------
  # ===========================================================================#
  # Degree Conversion
  rad <- (angle * pi) / 180
  # anon vars
  x2 <- NULL
  y2 <- NULL

  # ===========================================================================#
  # Anchor Handling------------------------------------------------------------
  # ===========================================================================#
  # Setting the anchor if string...
  if(anchor_string){
    anchor <-
      switch(anchor,
             "center" = list(
               "x" = (min({{ x }}) + max({{ x }})) / 2,
               "y" = (min({{ y }}) + max({{ y }})) / 2
             ),
             "bottom" = list(
               "x" = (min({{ x }}) + max({{ x }})) / 2,
               "y" = min({{ y }})
             ),
             "top" = list(
               "x" = (min({{ x }}) + max({{ x }})) / 2,
               "y" = max({{ y }})
             ),
             "left" = list(
               "x" = min({{ x }}),
               "y" = (min({{ y }}) + max({{ y }})) / 2
             ),
             "right" = list(
               "x" = max({{ x }}),
               "y" = (min({{ y }}) + max({{ y }})) / 2
             )
      )
  } else{
    # ...Or numeric#
    anchor <- list(
      "x" = anchor[1],
      "y" = anchor[2]
    )
  }

  # ===========================================================================#
  # Rotation Work---------------------------------------------------------------
  # ===========================================================================#

  # Rotating the x and y variables
  data_rotated <-
    data |>
    dplyr::mutate(
      x2 = ({{ x }} - anchor$x) * cos(rad) - ({{ y }} - anchor$y) * sin(rad) + anchor$x,
      y2 = ({{ x }} - anchor$x) * sin(rad) + ({{ y }} - anchor$y) * cos(rad) + anchor$y,
      x = x2,
      y = y2
    ) |>
    dplyr::select(x, y) |>
    dplyr::rename(
      rlang::`!!`(x_name) := x,
      rlang::`!!`(y_name) := y
    )

  # If drop is TRUE - just return the rotated values w/o other vars...
  if (drop) {
    return(data_rotated)
  } else {
    # ...Or else bind the other variables back into the output
    # Preserving any other variables in the original data frame
    og_cols <- data |>
      dplyr::select(-dplyr::all_of(c(x_name, y_name)))

    # Binding the rotated variables and any other original variables
    df_out <- data_rotated |>
      cbind(og_cols)

    return(df_out)
  }
}
