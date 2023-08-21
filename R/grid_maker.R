#' Data Generation for A Custom-built Square Grid
#' @description
#' Creates a dataframe of `x` and `y` points to visualize a square grid based on given `x` and `y` limits.
#' Providing a color palette and fill style are optional.
#'
#' @param xlim A numeric vector with two X limits. A minimum and maximum limit for the X axis. Must be a length of 2.
#' @param ylim A numeric vector with two Y limits. A minimum and maximum limit for the Y axis. Must be a length of 2.
#' @param size A numeric input. The size of the grid. How many shapes will appear in a single row or column. Must be a length of 1, greater than 0, and less than or equal to the max `xlim` and max `ylim`.
#' @param fill_pal Optional. A character vector of 6 digit hexadecimal webcolor code, or `R` `colors()` color strings to be applied to fill the grid.
#' @param fill_style Optional. A character input. "range" or "random". Determines how the fill color palette is mapped.
#' @param color_pal Optional. A character vector of 6 digit hexadecimal webcolor code, or `R` `colors()` color strings to be applied to borders of the grid.
#' @param color_style Optional. A character input. "range" or "random". Determines how the border color palette is mapped.

#' @return a tibble
#'
#' @importFrom purrr pmap
#' @importFrom purrr map2
#' @importFrom purrr map
#' @importFrom purrr list_rbind
#' @importFrom purrr list_c
#' @importFrom tibble tibble
#' @importFrom knitr combine_words
#' @importFrom grDevices colorRampPalette
#' @importFrom rlang is_empty
#' @importFrom cli cli_abort
#' @export
#'
#' @examples
#' # Creating data for a grid:
#'
#' library(ggplot2)
# grid_data <- grid_maker(
#   xlim = c(0, 1),
#   ylim = c(0, 1),
#   size = 10,
#   fill_pal = c("turquoise", "black", "purple"),
#   color_pal = c("black","limegreen")
# )
#
# ggplot() +
#   geom_polygon(
#     data = grid_data,
#     aes(x, y, group = group),
#     fill = grid_data$fill,
#     color = grid_data$color
#   ) +
#   coord_equal()
#'
grid_maker <- function(xlim, ylim, size,
                       fill_pal = NULL, fill_style = "range",
                       color_pal = NULL, color_style = "range") {
  # ===========================================================================#
  # Input Checks---------------------------------------------------------------
  # ===========================================================================#
  # Check for required inputs
  required_args <- c(
    "xlim" = missing(xlim),
    "ylim" = missing(ylim),
    "size" = missing(size)
  )

  missing_args <- which(required_args) |>
    names() |>
    knitr::combine_words(before = "`", after = "`")

  if (!rlang::is_empty(missing_args)) {
    c(
      paste("{missing_args}", ifelse(length(which(required_args)) > 1, "are", "is"), error("missing")),
      "x" = paste("{missing_args}", ifelse(length(which(required_args)) > 1, "are", "is"), status("required"), "and should be a numeric value with a length of 2"),
      "i" = paste("Check the {missing_args}", ifelse(length(which(required_args)) > 1, "variables", "variable"))
    ) |>
      cli::cli_abort()
  }


  # Check for equal lengths of all inputs
  arg_lengths <-
    c(
      "xlim" = length(xlim) != 2,
      "ylim" = length(ylim) != 2,
      "size" = length(size) != 1,
      "fill_style" = length(fill_style) != 1 & !is.null(fill_pal),
      "color_style" = length(color_style) != 1 & !is.null(color_pal)
    )

  # If any arguments are flagged above, they are invalid
  arg_check <- any(arg_lengths)

  # Argument length checks
  if (arg_check) {
    invalid_args <- names(arg_lengths[which(arg_lengths)])
    correct_lengths <- c("xlim" = 2, "ylim" = 2, "size" = 1)
    correct_lengths <- correct_lengths[which(arg_lengths)]
    invalid_lengths <- purrr::map(invalid_args, ~ paste0("{length({", .x, "})}")) |> purrr::list_c()
    invalid_args <- paste0("{.var ", names(arg_lengths[which(arg_lengths)]), "}")

    c(
      paste(knitr::combine_words(purrr::map2(invalid_args, correct_lengths, ~ paste(.x, "must have a", callout(paste("length of", .y)))))),
      "x" = knitr::combine_words(purrr::map2(invalid_args, invalid_lengths, ~ paste(.x, "has a length of", error(.y)))),
      "i" = paste("Check the", knitr::combine_words(invalid_args), ifelse(length(invalid_args) > 1, "variables", "variable"))
    ) |>
      cli::cli_abort()
  }


  # Check that numeric inputs are numeric
  numeric_args <-
    c(
      "xlim" = !is.numeric(xlim),
      "ylim" = !is.numeric(ylim),
      "size" = !is.numeric(size)
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


  # Check for valid size
  size_check <- size <= 0

  if (size_check) {
    c(
      paste("{.var size} must be", callout("greater than 0")),
      "x" = paste("{.var size} is", error({
        size
      })),
      "i" = "Check the {.var size} variable"
    ) |>
      cli::cli_abort()
  }

  size_int_check <- size %% 1 != 0

  if (size_int_check) {
    c(
      paste("{.var size} must be", callout("a whole number"), "with", callout("no decimals")),
      "x" = paste("{.var size} is", error({
        size
      })),
      "i" = "Check the {.var size} variable"
    ) |>
      cli::cli_abort()
  }

  size_lim_check <- size > max(xlim) | size > max(ylim)

  if (size_lim_check) {
    c(
      paste("{.var size} must be", callout("less than or equal to the max limits for x and y")),
      "x" = paste("{.var size} is", error({size})),
      "i" = paste("max xlim is", status({max(xlim)})),
      "i" = paste("max ylim is", status({max(ylim)})),
      "i" = "Check the {.var size} variable"
    ) |>
      cli::cli_abort()
  }

  # Check that applicable character inputs are characters
  char_args <- c(
    "fill_pal" = !is.character(fill_pal) & !is.null(fill_pal),
    "fill_style" = !is.character(fill_style) & !is.null(fill_pal),
    "color_pal" = !is.character(color_pal) & !is.null(color_pal),
    "color_style" = !is.character(color_style) & !is.null(color_pal)
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

  # Checking for valid colors
  # Fill pal
  if (!is.null(fill_pal)) {
    color_check <- any(is.color(c(fill_pal)) == FALSE)

    if (color_check) {
      invalid_cols <- names(which(is.color(c(fill_pal)) == FALSE))

      c(
        paste("{.var fill_pal} contains", error("invalid colors")),
        "x" = paste("{.var fill_pal} must be a vector of valid:", status("`r` color from `colors()`"), "or valid 6 digit", status("hexadecimal webcolors")),
        "i" = paste("{.var {invalid_cols}}", ifelse(length(invalid_cols) > 1, paste("are", callout("invalid colors")), paste("is an", callout("invalid color"))))
      ) |>
        cli::cli_abort()
    }
  }

  if (!is.null(color_pal)) {
    color_check <- any(is.color(c(color_pal)) == FALSE)

    if (color_check) {
      invalid_cols <- names(which(is.color(c(color_pal)) == FALSE))

      c(
        paste("{.var color_pal} contains", error("invalid colors")),
        "x" = paste("{.var color_pal} must be a vector of valid:", status("`r` color from `colors()`"), "or valid 6 digit", status("hexadecimal webcolors")),
        "i" = paste("{.var {invalid_cols}}", ifelse(length(invalid_cols) > 1, paste("are", callout("invalid colors")), paste("is an", callout("invalid color"))))
      ) |>
        cli::cli_abort()
    }
  }

  # String Preset Catches#
  fill_style_check <- !fill_style %in% c("range", "random")

  if (fill_style_check) {
    c(
      paste("{.var fill_style} must be a string value of", callout('"range"'), "or", callout('"random"')),
      "x" = paste("{.var fill_style} is of value:", error({
        fill_style
      })),
      "i" = "Check the {.var fill_style} variable"
    ) |>
      cli::cli_abort()
  }

  color_style_check <- !color_style %in% c("range", "random")

  if (color_style_check) {
    c(
      paste("{.var color_style} must be a string value of", callout('"range"'), "or", callout('"random"')),
      "x" = paste("{.var color_style} is of value:", error({
        color_style
      })),
      "i" = "Check the {.var color_style} variable"
    ) |>
      cli::cli_abort()
  }

  #===========================================================================#
  # Data Generation------------------------------------------------------------
  #===========================================================================#

  # Creating group names for each individual square#
  grp_nums <- rep(1:(size * size), each = 5)
  grp_nums <- group_numbers(grp_nums)
  group_names <- paste0("square_", grp_nums)

  # Calculating X and Y points manually based on the x and y limits#
  x_points <- seq(xlim[1], xlim[2], length.out = size + 1)
  y_points <- seq(ylim[1], ylim[2], length.out = size + 1)

  # Calculating appropriate transformations to create the grid#
  point_x_indexes <- rep(c(1, 2, 2, 1, 1) + rep(0:(size - 1), each = 5), times = size)
  point_y_indexes <- rep(c(1, 1, 2, 2, 1), times = size) + rep(0:(size - 1), each = 5 * size)

  # Applying the transformations#
  x_points_grid <- x_points[point_x_indexes]
  y_points_grid <- y_points[point_y_indexes]

  # Calculating the appropriate mapping depending on if color or fill options were toggled#
  map_toggle <-
    c(
      "none" = is.null(fill_pal) & is.null(color_pal),
      "fill" = !is.null(fill_pal) & is.null(color_pal),
      "color" = is.null(fill_pal) & !is.null(color_pal),
      "both" = !is.null(fill_pal) & !is.null(color_pal)
    )

  map_toggle <- names(map_toggle)[which(map_toggle)]

  if (map_toggle == "none") {
    grid_comps <-
      list(
        x_points_grid,
        y_points_grid,
        group_names
      )

    grid <-
      purrr::pmap(grid_comps, ~ tibble::tibble(
        x = ..1,
        y = ..2,
        group = ..3
      )) |>
      purrr::list_rbind()

    return(grid)
  } else if (map_toggle == "fill") {
    fill <- switch(fill_style,
      "range" = rep(colorRampPalette(fill_pal)(size * size), each = 5),
      "random" = rep(sample(colorRampPalette(fill_pal)(size * size)), each = 5)
    )

    grid_comps <-
      list(
        x_points_grid,
        y_points_grid,
        fill,
        group_names
      )

    grid <- purrr::pmap(grid_comps, ~ tibble::tibble(
      x = ..1,
      y = ..2,
      fill = ..3,
      group = ..4
    )) |>
      purrr::list_rbind()

    return(grid)
  } else if (map_toggle == "color") {
    color <- switch(color_style,
      "range" = rep(colorRampPalette(color_pal)(size * size), each = 5),
      "random" = rep(sample(colorRampPalette(color_pal)(size * size)), each = 5)
    )

    grid_comps <-
      list(
        x_points_grid,
        y_points_grid,
        color,
        group_names
      )

    grid <- purrr::pmap(grid_comps, ~ tibble::tibble(
      x = ..1,
      y = ..2,
      color = ..3,
      group = ..4
    )) |>
      purrr::list_rbind()

    return(grid)
  } else if (map_toggle == "both") {
    fill <- switch(fill_style,
      "range" = rep(colorRampPalette(fill_pal)(size * size), each = 5),
      "random" = rep(sample(colorRampPalette(fill_pal)(size * size)), each = 5)
    )

    color <- switch(color_style,
      "range" = rep(colorRampPalette(color_pal)(size * size), each = 5),
      "random" = rep(sample(colorRampPalette(color_pal)(size * size)), each = 5)
    )

    grid_comps <-
      list(
        x_points_grid,
        y_points_grid,
        fill,
        color,
        group_names
      )

    grid <- purrr::pmap(grid_comps, ~ tibble::tibble(
      x = ..1,
      y = ..2,
      fill = ..3,
      color = ..4,
      group = ..5
    )) |>
      purrr::list_rbind()

    return(grid)
  }
}
