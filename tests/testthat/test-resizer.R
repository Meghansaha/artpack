# =============================================================================#
# resizer() Testing ------------------------------------------------------------
# =============================================================================#

# =============================================================================#
# Testing Data------------------------------------------------------------------
# =============================================================================#
df_square <-
  tibble::tibble(
    x = c(0,1,1,0,0),
    y = c(0,0,1,1,0),
    color = "black",
    x_char = x |> as.character(),
    y_char = y |> as.character()
  )

# =============================================================================#
# Input Testing-----------------------------------------------------------------
# =============================================================================#
## All applicable inputs should be present--------------------------------------
### data----
cli::test_that_cli("data should be present",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         x <- 1:10
                         y <- 1:10

                         resizer(x, y, factor = 2)
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)
### x----
cli::test_that_cli("x should be present",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         df_square |>
                         resizer(y = y, factor = 2)
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

### y----
cli::test_that_cli("y should be present",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         df_square |>
                           resizer(x, factor = 2)
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

### factor----
cli::test_that_cli("factor should be present",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         df_square |>
                           resizer(x, y)
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

## All applicable inputs should be of length 1----------------------------------
### x_anchor----
cli::test_that_cli("x_anchor should be of length 1",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         df_square |>
                           resizer(x, y, x_anchor = 0:5, factor = 2)
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

### y_anchor----
cli::test_that_cli("y_anchor should be of length 1",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         df_square |>
                           resizer(x, y, y_anchor = 0:5, factor = 2)
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

### factor----
cli::test_that_cli("factor should be of length 1",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         df_square |>
                           resizer(x, y, factor = 1:20)
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

### direction----
cli::test_that_cli("direction should be of length 1",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         df_square |>
                           resizer(x, y, factor = 2, direction = c("up", "up"))
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)


## All inputs should be the expected class--------------------------------------
### data----
cli::test_that_cli("data should be a data frame",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         df_square |>
                           as.list() |>
                           resizer(x, y, factor = 2)
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

### x----
cli::test_that_cli("x should be numeric",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         df_square |>
                           resizer(x_char, y, factor = 2)
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

### y----
cli::test_that_cli("y should be numeric",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         df_square |>
                           resizer(x, y_char, factor = 2)
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

### factor----
cli::test_that_cli("factor should be numeric",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         df_square |>
                           resizer(y, y, factor = "chonk boi")
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

### drop----
cli::test_that_cli("drop should be logical",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         df_square |>
                           resizer(y, y, factor = 5, drop = "FALSE")
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)
## factor should be positive numeric--------------------------------------------
cli::test_that_cli("factor should be positive numeric - zero",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         df_square |>
                           resizer(y, y, factor = 0)
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

cli::test_that_cli("factor should be positive numeric - negative",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         df_square |>
                           resizer(y, y, factor = -10)
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

## direction should be an expected value----------------------------------------
cli::test_that_cli("direction should be an expected value",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         df_square |>
                           resizer(y, y, factor = 10, direction = "side")
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
  # We expect this#
  expected_output <-
    data.frame(
      x = c(0,.5,.5,0,0),
      y = c(0,0,.5,.5,0)
    )
  # When this is ran#
  actual_output <-
    df_square |>
    resizer(x, y, factor = 2, drop = TRUE, direction = "down")
  # So these should be equal
  # even if attr are different bc df vs tibble
  testthat::expect_equal(actual_output, expected_output, ignore_attr = TRUE)
})

## The output is a dataframe----------------------------------------------------
testthat::test_that("output is a numeric vector", {
  df_check <-
    df_square |>
    resizer(x, y, factor = 2) |>
    is.data.frame()

  testthat::expect_true(df_check)
})

## The nrows of the output is as expected---------------------------------------
testthat::test_that("output nrow is as expected", {

  # The row count in the original df
  expected_nrow <-
    df_square |>
    nrow()

  # Is what we expect in the output
  actual_nrow <-
    df_square |>
    resizer(x, y, factor = 2) |>
    nrow()

  # So these should be equal
  testthat::expect_equal(expected_nrow, actual_nrow)
})

## The column names of the output are as expected-------------------------------
### With drop == FALSE (default)----
testthat::test_that("output col names is as expected w/o drop", {

  # The names in the original df
  expected_names <-
    df_square |>
    names()

  # Is what we expect in the output
  actual_names <-
    df_square |>
    resizer(x, y, factor = 2) |>
    names()

  # So these should be equal
  testthat::expect_equal(expected_names, actual_names)
})

### With drop == TRUE----
testthat::test_that("output col names is as expected w drop", {

  # The names in the original df
  expected_names <- c("cookie", "monster")

  # Is what we expect in the output
  actual_names <-
    df_square |>
    dplyr::rename("cookie" = "x", "monster" = "y") |>
    resizer(cookie, monster, factor = 2, drop = TRUE) |>
    names()

  # So these should be equal
  testthat::expect_equal(expected_names, actual_names)
})

## x and y anchor works as expected---------------------------------------------
### external anchor point gives warning----
cli::test_that_cli("external anchor gives a warning as expected",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         df_square |>
                           resizer(x, y, x_anchor = 5, y_anchor = 5, factor = 2, drop = TRUE, direction = "down")
                       }
                     )
                   },
                   configs = "ansi"
)

### internal anchor point just works----
testthat::test_that("internal anchor works as expected", {

  # This is what we expect in the output
  expected_output <-
    tibble::tibble(
      x = c(-1,1,1,-1,-1),
      y = c(-1,-1,1,1,-1)
    )

  # This is what we get
  actual_output <-
    df_square |>
    resizer(x, y, x_anchor = 1, y_anchor = 1, factor = 2, drop = TRUE, direction = "up")

  # So these should be equal
  testthat::expect_equal(expected_output, actual_output)

})
