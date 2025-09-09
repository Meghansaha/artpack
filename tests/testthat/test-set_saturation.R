# =============================================================================#
# set_saturation - Testing Suite------------------------------------------------
# =============================================================================#

# =============================================================================#
# Input Testing-----------------------------------------------------------------
# =============================================================================#
## All required inputs should be present----------------------------------------
### color----
cli::test_that_cli("color should be present",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         set_saturation(percentage = .10)
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

### percentage----
cli::test_that_cli("percentage should be present",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         set_saturation(color = "red")
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

## All inputs should be the correct class---------------------------------------
### color----
cli::test_that_cli("color should be character",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         set_saturation(color = TRUE, percentage = .10)
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

### percentage----
cli::test_that_cli("percentage should be numeric",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         set_saturation(color = "purple", percentage = "50%")
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

## All inputs should be the accepted/valid values-------------------------------
### color----
cli::test_that_cli("color should be valid",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         set_saturation(color = "blurple", percentage = .10)
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

## All inputs should be the expected length-------------------------------------
### color----
cli::test_that_cli("color should be of length 1",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         set_saturation(color = c("#151515", "#829819"), percentage = .10)
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

### percentage----
cli::test_that_cli("percentage should be of length 1",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         set_saturation(color = "#151515", percentage = c(.10, .60))
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

### percentage (positive)----
cli::test_that_cli("percentage should be positive",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         sat <- -10
                         set_saturation(color = "brown", percentage = sat)
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

### percentage (between 0 - 1)----
cli::test_that_cli("percentage should be between 0 - 1",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         set_saturation(color = "brown", percentage = 10)
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
  original_color <- "#7755aa"

  expected_desat_color <- "#7859A6"
  actual_desat_color <- set_saturation(original_color, .3)

  expected_sat_color <- "#6E26D9"
  actual_sat_color <- set_saturation(original_color, .7)

  # these should be equal
  testthat::expect_equal(actual_desat_color, expected_desat_color)
  testthat::expect_equal(actual_sat_color, expected_sat_color)
}
)

## Output is a valid hex color--------------------------------------------------
testthat::test_that("output is valid character hex color", {
  original_color <- "#99b9cc"

  desat_color <- set_saturation(original_color, .3)

  # these should be equal
  testthat::expect_true(is.color(desat_color))
}
)
