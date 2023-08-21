#' Data Generation for 2D Sine and Cosine Waves
#'
#' @description
#' A tool for making data frames filled with data that displays sine or cosine waves when graphed.
#'
#'
#' The `geom_path` and `geom_polygon` geoms are recommended with this data for use in `ggplot2` for generative art.
#'
#' @param start Numeric value. The starting point of the wave on the coordinate system. By default refers to the x-axis. Will refer to the y-axis if `orientation` is set to `vertical`. Must be of length 1.
#' @param end Numeric value. The ending point of the wave on the coordinate system. By default refers to the x-axis. Will refer to the y-axis if `orientation` is set to `vertical` Must be of length 1.
#' @param size Numeric value. The height or width of the wave. Orientation is set to `horizontal` by default, thus size will affect height by default. When orientation is set to `vertical`, size controls the width of the wave. Must be a positive numeric value. Must be of length 1.
#' @param type String value. "sin" or "cos" for sine or cosine waves. `sin` is default. Must be of length 1.
#' @param orientation String value. Default is `horizontal` which will draw the wave from left to right (x-axis) on the coordinate system. `vertical` will draw the wave from bottom to top (y-axis) on the coordinate system. Must be of length 1.
#' @param freq Numeric value. Default is 3 cycles per second. This affects how many "peaks" are created in the wave. Must be a positive numeric value. Must be of length 1.
#' @param n_points Numeric value. Default is 500. This determines how many points each half of the wave will have. This option can come in handy when using jitter options or other texture/illusion methods. Must be of length 1.
#' @param color Optional String Value. A 6 digit hexadecimal webcolor code, or `R` `colors()` color string for the border color of the wave. Must be of length 1.
#' @param fill Optional String Value. A 6 digit hexadecimal webcolor code, or `R` `colors()` color string for the fill color of the wave. Must be of length 1.
#' @param group_var Logic value. `TRUE` or `FALSE`. Default is `FALSE`. If `TRUE`, Adds a group variable to the data frame. Useful for iterative work to make multiple waves in a single data frame.
#' @param dampen Optional. A factor in which to dampen the wave (make "flatter"). Must be of length 1.
#' @param amplify Optional. A factor in which to amplify the wave (make "sharper"). Must be of length 1.
#'
#' @return A Tibble
#' @export
#' @importFrom dplyr tibble
#' @importFrom dplyr mutate
#' @importFrom grDevices colors
#'
#' @examples
#' library(ggplot2)
#' wave_df <- wave_data(
#'   start = 0, end = 10,
#'   fill = "purple",
#'   color = "green"
#' )
#'
#' wave_df |>
#'   ggplot(aes(x, y)) +
#'   theme_void() +
#'   geom_polygon(
#'     fill = wave_df$fill,
#'     color = wave_df$color,
#'     linewidth = 3
#'   ) +
#'   coord_equal()
#'
wave_data <- function(start,
                      end,
                      size = 1,
                      type = "sin",
                      orientation = "horizontal",
                      freq = 3,
                      n_points = 500,
                      color = NULL,
                      fill = NULL,
                      group_var = FALSE,
                      dampen = NULL,
                      amplify = NULL) {
  #===========================================================================#
  # Input/Logic Checks---------------------------------------------------------
  #===========================================================================#

  # Check for required inputs
  required_args <- c(
    "start" = missing(start),
    "end" = missing(end)
  )

  missing_args <- which(required_args) |>
    names() |>
    knitr::combine_words(before = "`", after = "`")

  if (!rlang::is_empty(missing_args)) {
    c(
      paste("{missing_args}", ifelse(length(which(required_args)) > 1, "are", "is"), error("missing")),
      "x" = paste("{missing_args}", ifelse(length(which(required_args)) > 1, "are", "is"), status("required"), "and should be a numeric value with a length of 1"),
      "i" = paste("Check the {missing_args}", ifelse(length(which(required_args)) > 1, "variables", "variable"))
    ) |>
      cli::cli_abort()
  }

  # Check for equal lengths of all inputs
  arg_lengths <-
    c(
      "start" = length(start) != 1,
      "end" = length(end) != 1,
      "size" = length(size) != 1,
      "type" = length(type) != 1,
      "orientation" = length(orientation) != 1,
      "n_points" = length(n_points) != 1,
      "freq" = length(freq) != 1,
      "color" = length(color) != 1 & !is.null(color),
      "fill" = length(fill) != 1 & !is.null(fill),
      "group_var" = length(group_var) != 1,
      "dampen" = length(dampen) != 1 & !is.null(dampen),
      "amplify" = length(amplify) != 1 & !is.null(amplify)
    )

  # If any arguments are flagged above, they are invalid
  arg_check <- any(arg_lengths)

  # Argument length checks
  if (arg_check) {
    invalid_args <- names(arg_lengths[which(arg_lengths)])
    invalid_lengths <- purrr::map(invalid_args, ~ paste0("{length({", .x, "})}")) |> purrr::list_c()
    invalid_args <- paste0("{.var ", names(arg_lengths[which(arg_lengths)]), "}")

    c(
      paste("All arguments must have a", callout("length of 1")),
      "x" = knitr::combine_words(purrr::map2(invalid_args, invalid_lengths, ~ paste(.x, "has a length of", error(.y)))),
      "i" = paste("Check the", knitr::combine_words(invalid_args), ifelse(length(invalid_args) > 1, "variables", "variable"))
    ) |>
      cli_abort()
  }

  # Check that numeric inputs are numeric
  numeric_args <-
    c(
      "start" = !is.numeric(start),
      "end" = !is.numeric(end),
      "size" = !is.numeric(size),
      "freq" = !is.numeric(freq),
      "n_points" = !is.numeric(n_points),
      "dampen" = !is.numeric(dampen) & !is.null(dampen),
      "amplify" = !is.numeric(amplify) & !is.null(amplify)
    )

  # If any arguments are flagged above, they are invalid
  numeric_check <- any(numeric_args)

  # Argument numeric checks
  if (numeric_check) {
    invalid_args <- names(numeric_args[which(numeric_args)])
    invalid_classes <- purrr::map(invalid_args, ~ paste0("{.cls {class({", .x, "})}}")) |> purrr::list_c()
    invalid_args <- paste0("{.var ", names(numeric_args[which(numeric_args)]), "}")

    c(
      paste("The", knitr::combine_words(invalid_args), ifelse(length(invalid_args) > 1, "arguments", "argument"), "must have a class of", callout("<numeric>")),
      "x" = knitr::combine_words(purrr::map2(invalid_args, invalid_classes, ~ paste(.x, "has a class of", error(.y)))),
      "i" = paste("Check the", knitr::combine_words(invalid_args), ifelse(length(invalid_args) > 1, "variables", "variable"))
    ) |>
      cli::cli_abort()
  }


  # Check that applicable numeric inputs are valid
  valid_num_args <-
    c(
      "size" = size <= 0,
      "freq" = freq <= 0,
      "n_points" = n_points <= 0,
      "dampen" = dampen <= 0 & !is.null(dampen),
      "amplify" = amplify <= 0 & !is.null(amplify)
    )

  # If any arguments are flagged above, they are invalid
  valid_num_check <- any(valid_num_args)

  # Argument numeric checks
  if (valid_num_check) {
    invalid_args <- names(valid_num_args[which(valid_num_args)])
    invalid_nums <- purrr::map(invalid_args, ~ paste0("{", .x, "}")) |> purrr::list_c()
    invalid_args <- paste0("{.var ", names(valid_num_args[which(valid_num_args)]), "}")

    c(
      paste("The", knitr::combine_words(invalid_args), ifelse(length(invalid_args) > 1, "arguments", "argument"), "must be", callout("greater than zero")),
      "x" = knitr::combine_words(purrr::map2(invalid_args, invalid_nums, ~ paste(.x, "has a value of ", error(.y)))),
      "i" = paste("Check the", knitr::combine_words(invalid_args), ifelse(length(invalid_args) > 1, "variables", "variable"))
    ) |>
      cli::cli_abort()
  }

  # Check that applicable character inputs are characters
  char_args <- c(
    "type" = !is.character(type),
    "orientation" = !is.character(orientation),
    "color" = !is.character(color) & !is.null(color),
    "fill" = !is.character(fill) & !is.null(fill)
  )

  # If any arguments are flagged above, they are invalid
  char_check <- any(char_args)

  # Argument char checks
  if (char_check) {
    invalid_args <- names(char_args[which(char_args)])
    invalid_classes <- purrr::map(invalid_args, ~ paste0("{.cls {class({", .x, "})}}")) |> purrr::list_c()
    invalid_args <- paste0("{.var ", names(char_args[which(char_args)]), "}")

    c(
      paste("The", knitr::combine_words(invalid_args), ifelse(length(invalid_args) > 1, "arguments", "argument"), "must have a class of", callout("<character>")),
      "x" = knitr::combine_words(purrr::map2(invalid_args, invalid_classes, ~ paste(.x, "has a class of", error(.y)))),
      "i" = paste("Check the", knitr::combine_words(invalid_args), ifelse(length(invalid_args) > 1, "variables", "variable"))
    ) |>
      cli::cli_abort()
  }

  # String Preset Catches#
  type_check <- !type %in% c("sin", "cos")

  if (type_check) {
    c(
      paste("{.var type} must be a string value of", callout('"sin"'), "or", callout('"cos"')),
      "x" = paste("{.var type} is of value:", error({
        type
      })),
      "i" = "Check the {.var type} variable"
    ) |>
      cli::cli_abort()
  }

  ori_check <- !orientation %in% c("horiz", "horizontal", "vert", "vertical")

  if (ori_check) {
    c(
      paste("{.var orientation} must be a string value of", callout('"horizontal"'), "or", callout('"vertical"')),
      "x" = paste("{.var orientation} is of value:", error({
        orientation
      })),
      "i" = "Check the {.var orientation} variable"
    ) |>
      cli::cli_abort()
  }

  # Checking for valid colors
  if (!is.null(color)) {
    color_check <- is.color(color)

    if (!color_check) {
      c(
        paste("{.var color} is", error("invalid")),
        "x" = paste("{.var color} must be a valid:", status("`r` color from `colors()`"), "or a valid 6 digit", status("hexadecimal webcolor")),
        "i" = paste("{.var {color}} is an", callout("invalid color"))
      ) |>
        cli::cli_abort()
    }
  }

  if (!is.null(fill)) {
    fill_check <- is.color(fill)

    if (!fill_check) {
      c(
        paste("{.var fill} is", error("invalid")),
        "x" = paste("{.var fill} must be a valid:", status("`r` color from `colors()`"), "or a valid 6 digit", status("hexadecimal webcolor")),
        "i" = paste("{.var {fill}} is an", callout("invalid color"))
      ) |>
        cli::cli_abort()
    }
  }


  # Logic Catches#
  if (!is.logical(group_var)) {
    c(
      paste("{.var group_var} is", error("invalid")),
      "x" = paste("{.var group_var} must be of class", status("<logical>")),
      "i" = paste("{.var group_var} is of class", callout("{.cls {class({group_var})}}"))
    ) |>
      cli::cli_abort()
  }

  #=========================================================================#
  # Data Generation----------------------------------------------------------
  #=========================================================================#

  freq <- (2 * pi) * freq


  wave <- switch(type,
    "sin" = sin(seq(0, freq, length = n_points)),
    "cos" = cos(seq(0, freq, length = n_points))
  )

  if (!is.null(dampen)) {
    wave <- wave / dampen
  }

  if (!is.null(amplify)) {
    wave <- wave * amplify
  }

  path <- seq(start, end, length = n_points)

  if (orientation %in% c("vert", "vertical")) {
    wave_df <- tibble(
      x = c(wave, wave[n_points], rev(wave + size), wave[1]),
      y = c(path, path[n_points], rev(path), path[1])
    )
  } else {
    wave_df <- tibble(
      x = c(path, path[n_points], rev(path), path[1]),
      y = c(wave, wave[n_points], rev(wave + size), wave[1])
    )
  }

  if (!is.null(color)) {
    wave_df <- wave_df |>
      mutate(color = color)
  }

  if (!is.null(fill)) {
    wave_df <- wave_df |>
      mutate(fill = fill)
  }

  if (group_var) {
    wave_df <- wave_df |>
      mutate(group = "wave_")
  }


  return(wave_df)
}
