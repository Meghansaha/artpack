# =============================================================================#
# point_in_polygon - Testing Suite----------------------------------------------
# =============================================================================#

# =============================================================================#
# Input Testing-----------------------------------------------------------------
# =============================================================================#
# testing data#
point_x <- c(0.5, 1.5, 2.5, 1.0, 3.0)
point_y <- c(0.5, 1.5, 2.5, 0.0, 1.0)
poly_x <- c(0, 2, 2, 0, 0)
poly_y <- c(0, 0, 2, 2, 0)
vec_characters <- c("hi", "bye", "hola", "adios", "que tal")
vec_logic <- c(TRUE, FALSE, FALSE, TRUE, FALSE)
lst_test <-  list("hi", "bye", "hola", "adios", "que tal")

## All required inputs should be present----------------------------------------
### point_x----
cli::test_that_cli("point_x should be present",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         point_in_polygon(point_y = point_y, poly_x = poly_x, poly_y = poly_y)
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

### point_y----
cli::test_that_cli("point_y should be present",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         point_in_polygon(point_x = point_x, poly_x = poly_x, poly_y = poly_y)
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

### poly_x----
cli::test_that_cli("poly_x should be present",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         point_in_polygon(point_x = point_x, point_y = point_y, poly_y = poly_y)
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

### poly_y----
cli::test_that_cli("poly_y should be present",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {

                         point_in_polygon(point_x = point_x, point_y = point_y, poly_x = poly_x)
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

## All inputs should be the correct class (numeric)----------------------------
### point_x----
cli::test_that_cli("point_x should be numeric",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         point_in_polygon(vec_characters, point_y = point_y, poly_x = poly_x, poly_y = poly_y)
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

### point_y----
cli::test_that_cli("point_y should be numeric",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         point_in_polygon(point_x = point_x, vec_logic, poly_x = poly_x, poly_y = poly_y)
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

### poly_x----
cli::test_that_cli("poly_x should be numeric",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {

                         point_in_polygon(point_x = point_x, point_y = point_y, lst_test, poly_y = poly_y)
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

### poly_y----
cli::test_that_cli("poly_y should be numeric",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {

                         point_in_polygon(point_x = point_x, point_y = point_y, poly_x = poly_x, df_points)
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

## All inputs should be the expected length-------------------------------------
### point_x and y----
cli::test_that_cli("point_x should be of equal length to point_y",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         point_in_polygon("point", point_y = point_y, poly_x = poly_x, poly_y = poly_y)
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

### poly_x and y----
cli::test_that_cli("poly_x should be of equal length to poly_y",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {

                         point_in_polygon(point_x = point_x, point_y = point_y, 1:10, poly_y = poly_y)
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

# =============================================================================#
# Output Testing----------------------------------------------------------------
# =============================================================================#
## The function works with no error---------------------------------------------
testthat::test_that("works as expected", {
  vec_expected_output <- c(1, 1, 0, 1, 0)
  vec_actual_output <- point_in_polygon(point_x, point_y, poly_x, poly_y)

  # these should be equal
  testthat::expect_equal(vec_actual_output, vec_expected_output)
}
)

## output is integers-----------------------------------------------------------
testthat::test_that("output is integers", {
  vec_actual_output <- point_in_polygon(point_x, point_y, poly_x, poly_y)

  # these should be equal
  testthat::expect_type(vec_actual_output, "integer")
}
)

## The function works as expected in a df---------------------------------------
testthat::test_that("works as expected", {
  vec_expected_positions <- c(0, 0, 0, 1, 1, 1, 0)

  vec_df_positions <-
    dplyr::tibble(
      x = seq(-3, 3, by = 1),
      y = 1,
      position = point_in_polygon(x, y, poly_x, poly_y)
    ) |>
    dplyr::pull()

  # these should be equal
  testthat::expect_equal(vec_df_positions, vec_expected_positions)
}
)

## The function works as expected with an open polygon--------------------------
testthat::test_that("works as expected w/ open polygon", {
  vec_expected_positions <- c(0, 0, 0, 1, 1, 1, 0)

  poly_x_cut <- poly_x[-1]
  poly_y_cut <- poly_y[-1]

  vec_df_positions <-
    dplyr::tibble(
      x = seq(-3, 3, by = 1),
      y = 1,
      position = point_in_polygon(x, y, poly_x_cut, poly_y_cut)
    ) |>
    dplyr::pull()

  # these should be equal
  testthat::expect_equal(vec_df_positions, vec_expected_positions)
}
)
