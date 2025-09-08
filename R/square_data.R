#' Data Generation for Squares
#' @description
#'
#' A tool for creating a data frame of values that create a square with a specified size
#' when plotted.
#'
#' The `geom_path` and `geom_polygon` geoms are recommended with this data for use in `ggplot2` for generative art.
#'
#' @param x Numeric value of length 1 - The bottom left `x` value of the square.
#' @param y Numeric value of length 1 - The bottom left `y` value of the square.
#' @param size Numeric value of length 1 that must be greater than 0 - The size of the square.
#' @param color Character value of length 1 - The color of the square's border. A valid `R` color from `colors()` or a standard 6 digit hexadecimal webcolor like "#000000"
#' @param fill Character value of length 1 - The color of the square. A valid `R` color from `colors()` or a standard 6 digit hexadecimal webcolor like "#000000"
#' @param n_points Numeric value. Default is 100. This determines how many points the square will have. This option can come in handy when using jitter options or other texture/illusion methods. Must be of length 1 and at least a value of 4.
#' @param group_var Logical. Default is `FALSE`. If `TRUE`, a `group` variable will be added to the dataframe. Useful in iterative data generation.
#' @param group_prefix Character string of length 1 - The prefix used for the `group` variable. Default is "square_"
#'
#' @return A Tibble
#'
#' @importFrom purrr map2_dbl
#'
#' @export
#'
#' @examplesIf rlang::is_installed("ggplot2")
#' @examples
#' # Creating one square

#' library(ggplot2)
#' one_square <- square_data(x = 0, y = 0, size = 5)
#'
#' # Plot The Data
#' one_square |>
#'   ggplot(aes(x,y))+
#'   geom_path(color = "green")+
#'   coord_equal()
#'
#' # To create multiple squares, use your preferred method of iteration:
#' # Creating two squares
#'
#' library(purrr)
#' library(dplyr)
#'
#' # Make your specs
#' x_vals <- c(0,4)
#' y_vals <- c(0,0)
#' sizes <- c(1,3)
#' fills <- c("purple", "yellow")
#' square_n <- 1:2
#'
#' # Prep for your iteration
#' lst_square_specs <-
#'   list(
#'     x_vals,
#'     y_vals,
#'     sizes,
#'     fills,
#'     square_n
#'   )
#'
#' # Use `square_data()` in your preferred iteration methods
#' two_squares <- pmap(lst_square_specs, ~square_data(
#'   x = ..1,
#'   y = ..2,
#'   size = ..3,
#'   fill = ..4,
#'   color = "#000000",
#'   group_var = TRUE
#' ) |>
#'   # square_data adds a `group` variable if `group_var` = TRUE.
#'   # For multiple squares, a unique identifier should be added/pasted in.
#'   mutate(group = paste0(group,..5))
#' ) |>
#'   list_rbind()
#'
#' # Plot the data
#'
#' two_squares |>
#'   ggplot(aes(x, y, group = group))+
#'   theme(legend.position = "none")+
#'   geom_polygon(color = two_squares$color,
#'                fill = two_squares$fill) +
#'   coord_equal()
#'
#'
square_data <- function(x,
                        y,
                        size,
                        color = NULL,
                        fill = NULL,
                        n_points = 100,
                        group_var = FALSE,
                        group_prefix = "square_") {
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
      "size" = length(size) != 1,
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
  n_point_check <- n_points < 4
  if (n_point_check) {
    c(
      paste("{.var n_points} must be", callout("greater than or equal to 4")),
      "x" = paste("{.var n_points} is", error({
        n_points
      })),
      "i" = "Check the {.var n_points} variable"
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

  # Check if group_prefix is provided but group_var is FALSE
  group_check <- !group_var & group_prefix != "square_"

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

  # Setting vars
  x1 <- x
  x2 <- purrr::map2_dbl(x1, size, ~ .x + .y)
  y1 <- y
  y2 <- purrr::map2_dbl(y1, size, ~ .x + .y)


  # Base dataframe creation
  # Split the datapoints
  split_points <- floor(n_points / 4)
  leftover <- n_points - split_points

  df <- dplyr::tibble(
    x = c(
      seq(x1, x2, length = split_points),
      seq(x2, x2, length = split_points),
      seq(x2, x1, length = split_points),
      seq(x1, x1, length = split_points + leftover)
    ),
    y = c(
      seq(y1, y1, length = split_points),
      seq(y1, y2, length = split_points),
      seq(y2, y2, length = split_points),
      seq(y2, y1, length = split_points + leftover)
    )
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
    is.color(color)

    df <- df |>
      dplyr::mutate(color = color)
  }

  # If fill is not null, add it
  if (!is.null(fill)) {
    # check for validity
    is.color(fill)

    df <- df |>
      mutate(fill = fill)
  }

  # Spit it out
  return(df)
}
