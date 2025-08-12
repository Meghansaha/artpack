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
  # Logic Checks---------------------------------------------------------------
  # ===========================================================================#
  # Data is present
  if (missing(data)) {
    c(
      "x" = paste("{.var data} is", error("missing")),
      "!" = paste("{.var data} should be a", status("dataframe"), "or", status("tibble"))
    ) |>
      cli::cli_abort()
  }
  # Data is a dataframe
  if (!is.data.frame(data)) {
    c(
      "x" = paste("{.var data} is", error("{.cls {typeof(data)}}")),
      "!" = paste("{.var data} should be a", status("dataframe"), "or", status("tibble"))
    ) |>
      cli::cli_abort()
  }

  # x and y variables are present
  vars_df <-
    c(
      "x" = missing(x),
      "y" = missing(y)
    )

  var_check <-
    any(vars_df)


  if (var_check) {
    var_missing <-
      names(vars_df[vars_df == TRUE]) |>
      purrr::map_chr(
        ~ paste0("{.var ", .x, "}")
      ) |>
      knitr::combine_words()

    c(
      "x" = paste("{sum(vars_df)}", "variable{?s}", ifelse(sum(vars_df) == 1, "is", "are"), error("missing:")),
      "!" = paste(var_missing, "should be {.cls numeric}", ifelse(sum(vars_df) == 1, "variable", "variables"), "in a ", status("dataframe"), "or", status("tibble"))
    ) |>
      cli::cli_abort()
  }

  # Pulling existing column names
  x_name <- data |>
    dplyr::select({{ x }}) |>
    names()
  y_name <- data |>
    dplyr::select({{ y }}) |>
    names()

  # Pulling x and y columns for manipulation work
  x <- data |>
    dplyr::select({{ x }}) |>
    dplyr::pull()
  y <- data |>
    dplyr::select({{ y }}) |>
    dplyr::pull()

  # x variable is of type "numeric"
  x_var_check <- !is.numeric(x)

  if (x_var_check) {
    c(
      "x" = paste("The {.var x} variable, {x_name}, is of type", error("{typeof(x)}")),
      "!" = paste("The {.var x} variable, should be", status("{.cls numeric}"))
    ) |>
      cli::cli_abort()
  }

  # x variable is of type "numeric"
  y_var_check <- !is.numeric(y)

  if (y_var_check) {
    c(
      "x" = paste("The {.var y} variable, {y_name}, is of type", error("{typeof(y)}")),
      "!" = paste("The {.var y} variable should be", status("{.cls numeric}"))
    ) |>
      cli::cli_abort()
  }

  # angle is of type "numeric"
  angle_check <- !is.numeric(angle)

  if (angle_check) {
    c(
      "x" = paste("The {.var angle} input, {.var {angle}}, is of type", error("{typeof(angle)}")),
      "!" = paste("The {.var angle} input should be", status("{.cls numeric}"))
    ) |>
      cli::cli_abort()
  }

  # anchor is an known option
  anchor_check <- !anchor %in% c(
    "center",
    "bottom",
    "top",
    "left",
    "right"
  )

  if (anchor_check) {
    c(
      "x" = paste("{.var {anchor}} is", error("not a valid anchor option")),
      "i" = paste(status("Valid"), 'options include:"center", "bottom", "top", "left", and "right"')
    ) |>
      cli::cli_abort()
  }

  # Drop is logical
  drop_check <- !is.logical(drop)

  if (drop_check) {
    c(
      "x" = paste("The {.var drop} value you've supplied: {.var {drop}}, is of type", error("{typeof(drop)}")),
      "i" = paste("The {.var drop} input must be a logical", status("TRUE"), "or", status("FALSE"), "value")
    ) |>
      cli::cli_abort()
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

  # Setting the anchor
  anchor <- switch(anchor,
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

  # ===========================================================================#
  # Rotation Work--------------------------------------------------------------
  # ===========================================================================#

  # Rotating the x and y variables
  data_rotated <- data |>
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
