#' Data Generation for Circle Packing
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' A tool for creating a data frame of values that create a circle packing design when plotted.
#' When the default `circle_type` "whole" is used, the output should mapped with `geom_polygon`
#' in a ggplot. When "swirl" is used, the output should be mapped with `geom_path` for the best results.
#'
#' @param n The total number of circles you would like the function to attempt to create. A single numeric value with a minimum value of 10.
#' @param min_x The minimum limit of the x-axis - the left 'border' of the canvas A single numeric value.
#' @param max_x The maximum limit of the x-axis - the right 'border' of the canvas A single numeric value.
#' @param min_y The minimum limit of the y-axis - the bottom 'border' of the canvas A single numeric value.
#' @param max_y The maximum limit of the y-axis - the top 'border' of the canvas A single numeric value.
#' @param big_r The radius used for your 'big' sized circles A single numeric value.
#' @param med_r The radius used for your 'medium' sized circles. A single numeric value.
#' @param small_r The radius used for your 'small' sized circles. A single numeric value.
#' @param color_pal A vector of hex color codes that will be mapped to the data.
#' @param color_type Default is "regular" - The colors will be mapped in order from big circles to small circles. "reverse" - The colors will be mapped in reversed order from small to big circles. "random" - The colors will be mapped randomly to any sized circle.
#' @param circle_type Default is "whole" - Regular circles. "swirl" - circles are replaced with spirals. Spirals should be mapped with `geom_path` in a ggplot for the best results.
#'
#' @return A Tibble
#' @export
#'
#' @importFrom purrr set_names
#' @importFrom dplyr group_by group_size
#'
#' @examplesIf rlang::is_installed("ggplot2")
#' @examples
#'
#' library(ggplot2)
#' set.seed(0310)
#' packed_circles <- packer(
#'   n = 50, big_r = 5, med_r = 3, small_r = 1,
#'   min_x = 0, max_x = 100, min_y = 0, max_y = 100
#' )
#' packed_circles
#'
#' packed_circles |>
#'   ggplot(aes(x, y, group = group)) +
#'   theme_void() +
#'   theme(plot.background = element_rect(fill = "black")) +
#'   geom_polygon(fill = "white", color = "red") +
#'   coord_equal()
#'
packer <- function(n, min_x = 0, max_x = 100, min_y = 0, max_y = 100,
                   big_r = 5, med_r = 3, small_r = 1,
                   color_pal = NULL, color_type = "regular",
                   circle_type = "whole") {
  # ===========================================================================#
  # Global Variable Handling----------------------------------------------------
  # ===========================================================================#
  # Establish these for use later in the workflow
  .y <- NULL
  group <- NULL

  # ===========================================================================#
  # Logic Checks---------------------------------------------------------------
  # ===========================================================================#

  # Checks for proper "n" input
  # n is present
  if (missing(n)) {
    c(
      paste("The {.var n} variable is", callout("missing")),
      "x" = paste("A {.var n} variable with a minimum value of 10 is", error("required")),
      "i" = "Check the {.var n} variable you've supplied."
    ) |>
      cli::cli_abort()
  }

  # n has a length of 1
  if (length(n) > 1) {
    c(
      paste("The {.var n} variable has a length of", callout("{length(n)}")),
      "x" = paste("A {.var n} variable with a length of 1 is", error("required")),
      "i" = "Check the {.var n} variable you've supplied."
    ) |>
      cli::cli_abort()
  }

  # n is numeric
  if (!is.numeric(n)) {
    c(
      paste("{.var n} must be of class", callout("<numeric>")),
      "x" = paste("The {.var n} variable you've supplied is of class", error("<{class(n)}>")),
      "i" = "Check the {.var n} variable you've supplied."
    ) |>
      cli::cli_abort()
  }

  # n is at least 10
  if (n < 10) {
    c(
      paste("{.var n} must have a minimum value of", callout(10)),
      "x" = paste("The {.var n} value you've supplied is", error(n)),
      "i" = "Check the {.var n} value you've supplied."
    ) |>
      cli::cli_abort()
  }

  # xlim is numeric
  if (!is.numeric(min_x)) {
    c(
      paste("{.var min_x} must be of class", callout("<numeric>")),
      "x" = paste("The {.var min_x} variable you've supplied is of class", error("<{class(min_x)}>")),
      "i" = "Check the {.var min_x} variable you've supplied."
    ) |>
      cli::cli_abort()
  }

  if (!is.numeric(max_x)) {
    c(
      paste("{.var max_x} must be of class", callout("<numeric>")),
      "x" = paste("The {.var max_x} variable you've supplied is of class", error("<{class(max_x)}>")),
      "i" = "Check the {.var max_x} variable you've supplied."
    ) |>
      cli::cli_abort()
  }

  # xlim is a length of 1
  if (length(min_x) > 1) {
    c(
      paste("The {.var min_x} variable has a length of", callout("{length(min_x)}")),
      "x" = paste("A {.var min_x} variable with a length of 1 is", error("required")),
      "i" = "Check the {.var min_x} variable you've supplied."
    ) |>
      cli::cli_abort()
  }

  if (length(max_x) > 1) {
    c(
      paste("The {.var max_x} variable has a length of", callout("{length(max_x)}")),
      "x" = paste("A {.var max_x} variable with a length of 1 is", error("required")),
      "i" = "Check the {.var max_x} variable you've supplied."
    ) |>
      cli::cli_abort()
  }

  # ylim is numeric
  if (!is.numeric(min_y)) {
    c(
      paste("{.var min_y} must be of class", callout("<numeric>")),
      "x" = paste("The {.var min_y} variable you've supplied is of class", error("<{class(min_y)}>")),
      "i" = "Check the {.var min_y} variable you've supplied."
    ) |>
      cli::cli_abort()
  }

  if (!is.numeric(max_y)) {
    c(
      paste("{.var max_y} must be of class", callout("<numeric>")),
      "x" = paste("The {.var max_y} variable you've supplied is of class", error("<{class(max_y)}>")),
      "i" = "Check the {.var max_y} variable you've supplied."
    ) |>
      cli::cli_abort()
  }

  # ylim has a length of 1
  if (length(min_y) > 1) {
    c(
      paste("The {.var min_y} variable has a length of", callout("{length(min_y)}")),
      "x" = paste("A {.var min_y} variable with a length of 1 is", error("required")),
      "i" = "Check the {.var min_y} variable you've supplied."
    ) |>
      cli::cli_abort()
  }

  if (length(max_y) > 1) {
    c(
      paste("The {.var max_y} variable has a length of", callout("{length(max_y)}")),
      "x" = paste("A {.var max_y} variable with a length of 1 is", error("required")),
      "i" = "Check the {.var max_y} variable you've supplied."
    ) |>
      cli::cli_abort()
  }

  # Checks for radius inputs
  radis <- list(
    "big_r" = big_r,
    "med_r" = med_r,
    "small_r" = small_r
  )

  radi_check <- NULL

  radi_check <- purrr::imap(
    radis,
    ~ if (length(.x) != 1) {
      radi_check <- "1"
    } else if (!is.numeric(.x)) {
      radi_check <- "2"
    } else if (.x <= 0) {
      radi_check <- "3"
    },
    names(radi_check) <- radis[.y]
  )

  if (!all(sapply(radi_check, is.null))) {
    first_non_null <- radi_check[!sapply(radi_check, is.null)][1] |> unlist()

    switch(first_non_null,
      "1" = c(
        paste("{.var {names(first_non_null)}} must be of length", callout(1)),
        "x" = paste("The {.var {names(first_non_null)}} you've supplied has a length of", error(length(radis[[names(first_non_null)]]))),
        "i" = "Check the {.var {names(first_non_null)}} value you've supplied."
      ) |>
        cli::cli_abort(),
      "2" = c(
        paste("{.var {names(first_non_null)}} must be of type", callout("numeric")),
        "x" = paste0("You've supplied a ", error(class(radis[[names(first_non_null)]]))),
        "i" = "Check the {.var {names(first_non_null)}} value you've supplied."
      ) |>
        cli::cli_abort(),
      "3" = c(
        paste("{.var {names(first_non_null)}} must be", callout("greater than zero")),
        "x" = paste("{.var {names(first_non_null)}} =", error(radis[[names(first_non_null)]])),
        "i" = "Check the {.var {names(first_non_null)}} value you've supplied."
      ) |>
        cli::cli_abort()
    )
  }

  # Checks for pre-set inputs
  # Color Type check
  if (!color_type %in% c("regular", "reverse", "random")) {
    c(
      paste("{.var color_type} is", error("unknown")),
      "x" = paste("{.var color_type} must be one of the following:", status('"regular"'), ",", status('"reverse"'), ", or", status('"random"')),
      "i" = "You've supplied a {.var color_type} value of", callout("{.var {color_type}}")
    ) |>
      cli::cli_abort()
  }

  # Circle Type check
  if (!circle_type %in% c("whole", "swirl")) {
    c(
      paste("{.var circle_type} is", error("unknown")),
      "x" = paste("{.var circle_type} must be one of the following:", status('"whole"'), "or", status('"swirl"')),
      "i" = "You've supplied a {.var circle_type} value of", callout("{.var {circle_type}}")
    ) |>
      cli::cli_abort()
  }

  # Checks for valid color palettes
  if (!is.null(color_pal)) {
    color_check <- any(!is.color(color_pal))

    if (color_check) {
      invalid_colors <- names(is.color(color_pal)[is.color(color_pal) == FALSE])

      c(
        paste("{.var color_pal} contains", error("invalid colors")),
        "x" = paste("{.var color_pal} must contain valid:", status("`r` colors from `colors()`"), "or", status("hexadecimal webcolors")),
        "i" = paste("{knitr::combine_words(invalid_colors, before = '\"', after ='\"' )}", ifelse(length(invalid_colors) > 1, "are", "is"), callout("invalid colors"))
      ) |>
        cli::cli_abort()
    }
  }

  # ===========================================================================#
  # Packing Work---------------------------------------------------------------
  # ===========================================================================#

  # n work to determine circle amount
  # Big circles are 20%, medium is 50%, and small is 30%
  big_max <- floor(n * .2)
  med_max <- floor(n * .5)
  small_max <- floor(n * .3)
  leftover <- n - (big_max + med_max + small_max)
  small_max <- small_max + leftover - 1

  # Setting theta angles for each circle
  theta <- seq(0, 2 * pi, length = 100)

  # Distance function to determine radi overlap
  distance <- function(x1, y1, x2, y2) {
    sqrt(((x2 - x1)^2) + ((y2 - y1)^2))
  }

  # Big Circles----
  cli::cli_progress_step("Sampling for big-sized circles started", spinner = TRUE)
  big_iter <- 1:big_max
  big_x <- c(x = sample(min_x + (big_r):max_x - (big_r), 1))
  big_y <- c(y = sample(min_y + (big_r):max_y - (big_r), 1))
  i <- 1
  tries <- 0
  repeat{
    x <- sample(min_x + (big_r):max_x - (big_r), 1)
    y <- sample(min_y + (big_r):max_y - (big_r), 1)
    logic <- map2(big_x, big_y, ~ distance(.x, .y, x, y)) >= big_r * 2
    if (sum(logic) == length(big_y)) {
      big_x <- append(big_x, c(x = x))
      big_y <- append(big_y, c(y = y))
      i <- i + 1
    } else {
      tries <- tries + 1
    }
    if (i == (length(big_iter) + 1)) {
      break
    }
    if (tries == 3000 + length(big_iter)) {
      cli::cli_alert_info("Maximum sampling reached for big-sized circles")
      break
    }
  }


  new_iter <- 1:length(big_y)

  big_angles <- sample(0:360, length(big_y), replace = TRUE)


  big_circles <- switch(circle_type,
    "whole" = purrr::pmap(list(
      big_x,
      big_y,
      new_iter
    ), ~ dplyr::tibble(
      x = cos(theta) * big_r + ..1,
      y = sin(theta) * big_r + ..2,
      group = paste0("big_", ..3)
    )) |> purrr::list_rbind(),
    "swirl" = purrr::pmap(list(
      big_x,
      big_y,
      new_iter,
      big_angles
    ), ~ artpack::rotator(dplyr::tibble(
      x = (cos(theta) * seq(1, 0, length = 1000)) * big_r + ..1,
      y = (sin(theta) * seq(1, 0, length = 1000)) * big_r + ..2,
      group = paste0("big_", ..3),
      linewidth = .8
    ), x, y, ..4)) |> purrr::list_rbind()
  )


  cli::cli_progress_step("Big-sized circles complete!")

  # med Circles----
  cli::cli_progress_step("Sampling for medium-sized circles started", spinner = TRUE)
  med_iter <- 1:med_max
  med_x <- c(x = sample(min_x + (med_r):max_x - (med_r), 1))
  med_y <- c(y = sample(min_y + (med_r):max_y - (med_r), 1))
  i <- 1
  tries <- 0
  repeat{
    x <- sample(min_x + (med_r):max_x - (med_r), 1)
    y <- sample(min_y + (med_r):max_y - (med_r), 1)

    logic <- map2(big_x, big_y, ~ distance(.x, .y, x, y)) >= big_r + med_r
    logic2 <- map2(med_x, med_y, ~ distance(.x, .y, x, y)) >= med_r * 2

    if ((sum(logic2) == length(med_y)) & (sum(logic) == length(big_y))) {
      med_x <- append(med_x, c(x = x))
      med_y <- append(med_y, c(y = y))
      i <- i + 1
    } else {
      tries <- tries + 1
    }
    if (i == (length(med_iter) + 1)) {
      break
    }
    if (tries == 10000 + length(med_iter)) {
      cli::cli_alert_info("Maximum sampling reached for medium-sized circles!")
      break
    }
  }

  med_x <- med_x[-1]
  med_y <- med_y[-1]


  new_iter <- 1:length(med_y)
  med_angles <- sample(0:360, length(med_y), replace = TRUE)


  med_circles <- switch(circle_type,
    "whole" = purrr::pmap(list(
      med_x,
      med_y,
      new_iter
    ), ~ dplyr::tibble(
      x = cos(theta) * med_r + ..1,
      y = sin(theta) * med_r + ..2,
      group = paste0("med_", ..3)
    )) |> purrr::list_rbind(),
    "swirl" = purrr::pmap(list(
      med_x,
      med_y,
      new_iter,
      med_angles
    ), ~ artpack::rotator(dplyr::tibble(
      x = (cos(theta) * seq(1, 0, length = 1000)) * med_r + ..1,
      y = (sin(theta) * seq(1, 0, length = 1000)) * med_r + ..2,
      group = paste0("med_", ..3),
      linewidth = .4
    ), x, y, ..4)) |> purrr::list_rbind()
  )
  cli::cli_progress_step("Medium-sized circles complete!")


  # small Circles----
  cli::cli_progress_step("Sampling for small-sized circles started", spinner = TRUE)
  small_iter <- 1:small_max
  small_x <- c(x = sample(min_x + (small_r):max_x - (small_r), 1))
  small_y <- c(y = sample(min_y + (small_r):max_y - (small_r), 1))
  i <- 1
  tries <- 0
  repeat{
    x <- sample(min_x + (small_r):max_x - (small_r), 1)
    y <- sample(min_y + (small_r):max_y - (small_r), 1)

    logic <- map2(big_x, big_y, ~ distance(.x, .y, x, y)) >= big_r + small_r
    logic2 <- map2(med_x, med_y, ~ distance(.x, .y, x, y)) >= small_r + med_r
    logic3 <- map2(small_x, small_y, ~ distance(.x, .y, x, y)) >= small_r * 2

    if ((sum(logic3) == length(small_y)) & (sum(logic2) == length(med_y)) & (sum(logic) == length(big_y))) {
      small_x <- append(small_x, c(x = x))
      small_y <- append(small_y, c(y = y))
      i <- i + 1
    } else {
      tries <- tries + 1
    }
    if (i == (length(small_iter) + 1)) {
      break
    }
    if (tries == 9000 + length(small_iter)) {
      cli::cli_alert_info("Maximum sampling reached for small-sized circles!")
      break
    }
  }
  small_x <- small_x[-1]
  small_y <- small_y[-1]


  new_iter <- 1:length(small_y)

  small_angles <- sample(0:360, length(small_y), replace = TRUE)


  small_circles <- switch(circle_type,
    "whole" = purrr::pmap(list(
      small_x,
      small_y,
      new_iter
    ), ~ dplyr::tibble(
      x = cos(theta) * small_r + ..1,
      y = sin(theta) * small_r + ..2,
      group = paste0("small_", ..3)
    )) |> purrr::list_rbind(),
    "swirl" = purrr::pmap(list(
      small_x,
      small_y,
      new_iter,
      small_angles
    ), ~ artpack::rotator(dplyr::tibble(
      x = (cos(theta) * seq(1, 0, length = 1000)) * small_r + ..1,
      y = (sin(theta) * seq(1, 0, length = 1000)) * small_r + ..2,
      group = paste0("small_", ..3),
      linewidth = .1
    ), x, y, ..4)) |> purrr::list_rbind()
  )

  cli::cli_progress_step("Small-sized circles complete!")

  all_circles <- rbind(big_circles, med_circles, small_circles)

  all_circles <- all_circles |>
    dplyr::group_by(group)

  if (!is.null(color_pal)) {
    group_ns <- all_circles |>
      dplyr::group_size()

    total_groups <- length(group_ns)
    color_opts <- switch(color_type,
      "regular" = rep(colorRampPalette(color_pal)(total_groups), each = group_ns[1]),
      "reverse" = rev(rep(colorRampPalette(color_pal)(total_groups), each = group_ns[1])),
      "random" = rep(sample(colorRampPalette(color_pal)(total_groups)), each = group_ns[1])
    )


    if (circle_type == "whole") {
      all_circles$fill <- color_opts
    } else {
      all_circles$color <- color_opts
    }
  }
  return(all_circles)
}
