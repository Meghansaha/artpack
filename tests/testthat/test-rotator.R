# =============================================================================#
# Rotator.R Testing Suite------------------------------------------------------
# =============================================================================#
# Test Sets#
original_square <-
  tibble::tibble(
    x = c(0, 3, 3, 0, 0),
    y = c(0, 0, 3, 3, 0),
    sizes = 5:1,
    group = "group"
  )

original_square_more <-
  original_square |>
  dplyr::mutate(
    color = "black",
    size = 3
  )

wrong_vars <-
  tibble::tibble(
    x = rep("0", 6),
    y = rep("0", 6)
  )

square_named <-
  original_square |>
  dplyr::rename(
    "x_named_var" = "x",
    "y_named_var" = "y"
  )

rotated_square <-
  original_square |>
  rotator(x, y)

# =============================================================================#
# Missing Argument Checks------------------------------------------------------
# =============================================================================#
# Checks for error if dataframe Missing
cli::test_that_cli("dataframe missing",
  {
    testthat::local_edition(3)
    testthat::expect_snapshot(
      {
        rotator()
      },
      error = TRUE
    )
  },
  configs = "ansi"
)


# Checks for error if y var missing
cli::test_that_cli("y var missing",
  {
    testthat::local_edition(3)
    testthat::expect_snapshot(
      {
        original_square |>
          rotator(x = x)
      },
      error = TRUE
    )
  },
  configs = "ansi"
)

# Checks for error if x var missing
cli::test_that_cli("x var missing",
  {
    testthat::local_edition(3)
    testthat::expect_snapshot(
      {
        original_square |>
          rotator(y = y)
      },
      error = TRUE
    )
  },
  configs = "ansi"
)


# =============================================================================#
# Argument Type Checks---------------------------------------------------------
# =============================================================================#

# Test to check that error occurs if dataframe is not input
cli::test_that_cli("Test to ensure that error is thrown when dataframe is not input",
  {
    testthat::local_edition(3)
    testthat::expect_snapshot(
      {
        nums <- 1:5
        rotator(nums)
      },
      error = TRUE
    )
  },
  configs = "ansi"
)

# Test to check that variable types that are not numeric throw an error
testthat::test_that("x must be numeric", {
  testthat::expect_error(
    wrong_vars |>
      rotator(x, y)
  )
})

testthat::test_that("y must be numeric", {
  testthat::expect_error(
    wrong_vars |>
      mutate(x = as.numeric(x)) |>
      rotator(x, y)
  )
})

testthat::test_that("angle must be numeric", {
  testthat::expect_error(
    original_square |>
      rotator(x, y, angle = "one")
  )
})



# =============================================================================#
# Accepted Value Checks--------------------------------------------------------
# =============================================================================#
# Test to check that anchor is a valid anchor option
testthat::test_that("Anchor must be valid", {
  testthat::expect_error(
    original_square |>
      rotator(x, y, anchor = "topright")
  )
})

# Test to check that drop is a valid logical boolean
testthat::test_that("drop must be logical", {
  testthat::expect_error(
    original_square |>
      rotator(x, y, drop = "False")
  )
})

# =============================================================================#
# Output Checks----------------------------------------------------------------
# =============================================================================#

# Test to check if functions changes data frame inputted
testthat::test_that("Test for change to the original dataframe", {
  testthat::expect_false(identical(original_square, rotated_square))
})

# Test to check that drop toggle works
dropped_n_cols <- length(
  original_square |>
    rotator(
      x,
      y,
      angle = 45,
      anchor = "center",
      drop = TRUE
    )
)

# Dropped dfs should only have 2 columns (x and y)
testthat::test_that("Test to ensure drop toggle works", {
  testthat::expect_true(dropped_n_cols == 2)
})

# Test to check that default preserves all original columns
original_n_cols <- length(
  original_square_more |>
    rotator(
      x,
      y
    )
)

# UN-dropped dfs should have all original variables in the output
testthat::test_that("Test to ensure drop default behavior works", {
  testthat::expect_true(original_n_cols == 6)
})

# Test to make sure named x and y variables are handled properly when passed
testthat::test_that("Test to make sure data masking works", {
  testthat::expect_no_error(square_named |>
    rotator(x_named_var, y_named_var))
})
# Test to make sure named x and y variables are handled properly when named
testthat::test_that("Another Test to make sure data masking works", {
  testthat::expect_no_error(square_named |>
    rotator("x_named_var", "y_named_var"))
})

# Test to make sure variable names are preserved
original_names <-
  names(square_named)

new_names <-
  names(square_named |>
    rotator(x_named_var, y_named_var, angle = 50, anchor = "left"))

testthat::test_that("Test NO change to var names when df isn't dropped and named variables are present", {
  testthat::expect_true(identical(original_names, new_names))
})
