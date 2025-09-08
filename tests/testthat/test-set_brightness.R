# =============================================================================#
# set_brightness - Testing Suite------------------------------------------------
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
                         set_brightness(percentage = .10)
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
                         set_brightness(color = "red")
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
                         set_brightness(color = TRUE, percentage = .10)
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
                         set_brightness(color = "purple", percentage = "50%")
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
                         set_brightness(color = "blurple", percentage = .10)
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
                         set_brightness(color = c("#151515", "#829819"), percentage = .10)
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
                         set_brightness(color = "#151515", percentage = c(.10, .60))
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
                         light <- -10
                         set_brightness(color = "brown", percentage = light)
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

### percentage (between 0 - 1)----
cli::test_that_cli("percentage should be between 0 and 1",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         set_brightness(color = "brown", percentage = 10)
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

  expected_darker_color <- "#473366"
  actual_darker_color <- set_brightness(original_color, .3)

  expected_lighter_color <- "#AD99CC"
  actual_lighter_color <- set_brightness(original_color, .7)

  # these should be equal
  testthat::expect_equal(actual_darker_color, expected_darker_color)
  testthat::expect_equal(actual_lighter_color, expected_lighter_color)
}
)

## Output is a valid hex color--------------------------------------------------
testthat::test_that("output is valid character hex color", {
  original_color <- "#99b9cc"

  darker_color <- set_brightness(original_color, .3)

  # these should be equal
  testthat::expect_true(is.color(darker_color))
}
)
