#' Data Generation for Circles
#' @description
#'
#' A tool for creating a data frame of values that creates a circle with a specified radius
#' when plotted.
#'
#' The `geom_path` and `geom_polygon` geoms are recommended with this data for use in `ggplot2` for generative art.
#' @param x Numeric value of length 1 - The center `x` coordinate value of the circle.
#' @param y Numeric value of length 1 - The center `y` coordinate value of the circle.
#' @param radius Numeric value of length 1 that must be greater than 0 - The radius of the circle.
#' @param color Character value of length 1 - The intended color of the circle's border. A valid `R` color from `colors()` or a standard 6 digit hexadecimal webcolor like "#000000"
#' @param fill Character value of length 1 - The intended color of the circle. A valid `R` color from `colors()` or a standard 6 digit hexadecimal webcolor like "#000000"
#' @param n_points Numeric value. Default is 100. This determines how many points the circle will have. This option can come in handy when using jitter options or other texture/illusion methods. Must be of length 1 and at least a value of 100.
#' @param group_var Logical. Default is `FALSE`. If `TRUE`, a `group` variable will be added to the dataframe. Useful in iterative data generation.
#' @param group_prefix Character string of length 1 - The prefix used for the `group` variable. Default is "circle_"
#'
#' @return A Tibble
#'
#' @export
#'
#' @examplesIf rlang::is_installed("ggplot2")
#' @examples
#' # Creating one circle
#'
#' library(ggplot2)
#' one_circle <- circle_data(x = 0, y = 0, radius = 5)
#'
#' # Plot The Data
#' one_circle |>
#'   ggplot(aes(x, y)) +
#'   geom_path(color = "green") +
#'   coord_equal()
#'
#' # To create multiple circles, use your preferred method of iteration:
#' # Creating two circles
#'
#' library(purrr)
#' library(dplyr)
#'
#' # Make your specs
#' x_vals <- c(0, 10)
#' y_vals <- c(0, 0)
#' radi <- c(1, 3)
#' fills <- c("purple", "yellow")
#' circle_n <- 1:2
#'
#' # Prep for your iteration
#' lst_circle_specs <-
#'   list(
#'     x_vals,
#'     y_vals,
#'     radi,
#'     fills,
#'     circle_n
#'   )
#'
#' # Use `circle_data()` in your preferred iteration methods
#' two_circles <- pmap(lst_circle_specs, ~ circle_data(
#'   x = ..1,
#'   y = ..2,
#'   radi = ..3,
#'   fill = ..4,
#'   color = "#000000",
#'   group_var = TRUE
#' ) |>
#'   # circle_data adds a `group` variable if `group_var` = TRUE.
#'   # For multiple circles, a unique identifier should be added/pasted in.
#'   mutate(group = paste0(group, ..5))) |>
#'   list_rbind()
#'
#' # Plot the data
#'
#' two_circles |>
#'   ggplot(aes(x, y, group = group)) +
#'   theme(legend.position = "none") +
#'   geom_polygon(
#'     color = two_circles$color,
#'     fill = two_circles$fill
#'   ) +
#'   coord_equal() #Always use coord_equal() or coord_fixed for circles!
#'
circle_data <- function(x,
                        y,
                        radius,
                        color = NULL,
                        fill = NULL,
                        n_points = 100,
                        group_var = FALSE,
                        group_prefix = "circle_") {
  # ===========================================================================#
  # Input Checks---------------------------------------------------------------
  # ===========================================================================#

  # Check for required inputs
  required_args <- c(
    "x" = missing(x),
    "y" = missing(y)
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
      "x" = length(x) != 1,
      "y" = length(y) != 1,
      "radius" = length(radius) != 1,
      "color" = length(color) != 1 & !is.null(color),
      "fill" = length(fill) != 1 & !is.null(fill),
      "n_points" = length(n_points) != 1,
      "group_prefix" = length(group_prefix) != 1
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
      cli::cli_abort()
  }

  # Check for numeric x/y
  if (!is.numeric(x)) {
    c(
      paste("{.var x} must be of class", callout("<numeric>")),
      "x" = paste("{.var x} is of class", error("{.cls {class(x)}}")),
      "i" = "Check the {.var x} variable"
    ) |>
      cli::cli_abort()
  } else if (!is.numeric(y)) {
    c(
      paste("{.var y} must be of class", callout("<numeric>")),
      "x" = paste("{.var y} is of class", error("{.cls {class(y)}}")),
      "i" = "Check the {.var y} variable"
    ) |>
      cli::cli_abort()
  }

  # Numeric n_points
  if (!is.numeric(n_points)) {
    c(
      paste("{.var n_points} must be of class", callout("<numeric>")),
      "x" = paste("{.var n_points} is of class", error("{.cls {class(n_points)}}")),
      "i" = "Check the {.var n_points} variable"
    ) |>
      cli::cli_abort()
  }

  # Check for valid n_points
  n_point_check <- n_points < 100
  if (n_point_check) {
    c(
      paste("{.var n_points} must be", callout("greater than or equal to 100")),
      "x" = paste("{.var n_points} is", error({
        n_points
      })),
      "i" = "Check the {.var n_points} variable"
    ) |>
      cli::cli_abort()
  }


  # Check for valid radius
  radius_check <- radius <= 0

  if (radius_check) {
    c(
      paste("{.var radius} must be", callout("greater than 0")),
      "x" = paste("{.var radius} is", error({
        radius
      })),
      "i" = "Check the {.var radius} variable"
    ) |>
      cli::cli_abort()
  }

  # Check if group_prefix is provided but group_var is FALSE
  group_check <- !group_var & group_prefix != "circle_"

  if (group_check) {
    c(
      error("Warning:\n"),
      "i" = paste("You have provided a custom {.var group_prefix} of:", callout('"{group_prefix}"'), "\n"),
      "!" = paste("But {.var group_var} is `FALSE`\n"),
      ">" = "Did you mean to set {.var group_var = TRUE}?"
    ) |>
      cli::cli_inform()
  }


  # ===========================================================================#
  # Data Generation------------------------------------------------------------
  # ===========================================================================#

  # Base dataframe creation
  # Creating theta
  theta <- seq(0, 2 * pi, length = n_points)
  df <- dplyr::tibble(
    x = (cos(theta) * radius) + x,
    y = (sin(theta) * radius) + y
  )

  # If group_var is TRUE, add a group variable
  if (group_var) {
    # Check that group_prefix in a character
    if (!is.character(group_prefix)) {
      c(
        paste("{.var group_prefix} must be of class", callout("<character>")),
        "x" = paste("{.var group_prefix} is of class", error("{.cls {class({group_prefix})}}")),
        "i" = "Check the {.var group_prefix} variable"
      ) |>
        cli::cli_abort()
    }

    df <- df |>
      mutate(group = group_prefix)
  }

  # If color is not null, add it
  if (!is.null(color)) {
    # check for validity
    color_check <- is.color(color)

    if (!color_check) {
      c(
        paste("{.var color} is", error("invalid")),
        "x" = paste("{.var color} must be a valid:", status("`r` color from `colors()`"), "or a valid 6 digit", status("hexadecimal webcolor")),
        "i" = paste("{.var {color}} is an", callout("invalid color"))
      ) |>
        cli::cli_abort()
    }

    df <- df |>
      dplyr::mutate(color = color)
  }

  # If fill is not null, add it
  if (!is.null(fill)) {
    # check for validity
    fill_check <- is.color(fill)

    if (!fill_check) {
      c(
        paste("{.var fill} is", error("invalid")),
        "x" = paste("{.var fill} must be a valid:", status("`r` color from `colors()`"), "or a valid 6 digit", status("hexadecimal webcolor")),
        "i" = paste("{.var {fill}} is an", callout("invalid color"))
      ) |>
        cli::cli_abort()
    }

    df <- df |>
      mutate(fill = fill)
  }

  # Spit it out
  return(df)
}
