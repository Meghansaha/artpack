% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/circle_data.R
\name{circle_data}
\alias{circle_data}
\title{Data Generation for Circles}
\usage{
circle_data(
  x,
  y,
  radius,
  color = NULL,
  fill = NULL,
  n_points = 100,
  group_var = FALSE,
  group_prefix = "circle_"
)
}
\arguments{
\item{x}{Numeric value of length 1 - The center \code{x} coordinate value of the circle.}

\item{y}{Numeric value of length 1 - The center \code{y} coordinate value of the circle.}

\item{radius}{Numeric value of length 1 that must be greater than 0 - The radius of the circle.}

\item{color}{Character value of length 1 - The intended color of the circle's border. A valid \code{R} color from \code{colors()} or a standard 6 digit hexadecimal webcolor like "#000000"}

\item{fill}{Character value of length 1 - The intended color of the circle. A valid \code{R} color from \code{colors()} or a standard 6 digit hexadecimal webcolor like "#000000"}

\item{n_points}{Numeric value. Default is 100. This determines how many points the circle will have. This option can come in handy when using jitter options or other texture/illusion methods. Must be of length 1 and at least a value of 100.}

\item{group_var}{Logical. Default is \code{FALSE}. If \code{TRUE}, a \code{group} variable will be added to the dataframe. Useful in iterative data generation.}

\item{group_prefix}{Character string of length 1 - The prefix used for the \code{group} variable. Default is "circle_"}
}
\value{
A Tibble
}
\description{
A tool for creating a data frame of values that creates a circle with a specified radius
when plotted.

The \code{geom_path} and \code{geom_polygon} geoms are recommended with this data for use in \code{ggplot2} for generative art.
}
\examples{
\dontshow{if (rlang::is_installed("ggplot2")) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
\dontshow{\}) # examplesIf}
# Creating one circle

library(ggplot2)
one_circle <- circle_data(x = 0, y = 0, radius = 5)

# Plot The Data
one_circle |>
  ggplot(aes(x, y)) +
  geom_path(color = "green") +
  coord_equal()

# To create multiple circles, use your preferred method of iteration:
# Creating two circles

library(purrr)
library(dplyr)

# Make your specs
x_vals <- c(0, 10)
y_vals <- c(0, 0)
radi <- c(1, 3)
fills <- c("purple", "yellow")
circle_n <- 1:2

# Prep for your iteration
lst_circle_specs <-
  list(
    x_vals,
    y_vals,
    radi,
    fills,
    circle_n
  )

# Use `circle_data()` in your preferred iteration methods
two_circles <- pmap(lst_circle_specs, ~ circle_data(
  x = ..1,
  y = ..2,
  radi = ..3,
  fill = ..4,
  color = "#000000",
  group_var = TRUE
) |>
  # circle_data adds a `group` variable if `group_var` = TRUE.
  # For multiple circles, a unique identifier should be added/pasted in.
  mutate(group = paste0(group, ..5))) |>
  list_rbind()

# Plot the data

two_circles |>
  ggplot(aes(x, y, group = group)) +
  theme(legend.position = "none") +
  geom_polygon(
    color = two_circles$color,
    fill = two_circles$fill
  ) +
  coord_equal() #Always use coord_equal() or coord_fixed for circles!

}
