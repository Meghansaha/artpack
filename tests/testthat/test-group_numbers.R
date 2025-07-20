# =============================================================================#
# group_numbers - Testing Suite-------------------------------------------------
# =============================================================================#

# =============================================================================#
# Input Testing----------------------------------------------------------------
# =============================================================================#
## Only numbers should be accepted----------------------------------------------
cli::test_that_cli("Numbers only",
  {
    testthat::local_edition(3)
    testthat::expect_snapshot(
      {
        group_numbers(c("one", "two", "three"))
      },
      error = TRUE
    )
  },
  configs = "ansi"
)

## Invalid prefix throws the right error----------------------------------------
cli::test_that_cli("invalid prefix throws the expected error",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         group_numbers(1:10, prefix = array(1:5))
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

## Invalid suffix throws the right error----------------------------------------
cli::test_that_cli("invalid suffix throws the expected error",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         group_numbers(1:10, suffix = array(1:5))
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

## Invalid sep throws the right error-------------------------------------------
cli::test_that_cli("Sep must be character",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         group_numbers(1:5, prefix = "group", sep = 3)
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

## Missing affix w/ sep throws the right error----------------------------------
cli::test_that_cli("Sep must be character",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         group_numbers(1:5, sep = "_")
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

# =============================================================================#
# Output Testing----------------------------------------------------------------
# =============================================================================#
## Function just 'works'--------------------------------------------------------
testthat::test_that("works as expected", {
  regular_numbers <- 1:15
  expected_numbers <- c(paste0("0", 1:9), 10:15)
  actual_numbers <- group_numbers(regular_numbers)

  testthat::expect_equal(actual_numbers, expected_numbers)
})

## Prefix works-----------------------------------------------------------------
testthat::test_that("prefix works as expected", {
  actual_groups <- group_numbers(1:200, prefix = "group")
  expected_groups <- paste0("group", stringr::str_pad(1:200, "left", pad = "0", width = 3))
  testthat::expect_equal(actual_groups, expected_groups)
})

## Suffix works-----------------------------------------------------------------
testthat::test_that("suffix works as expected", {
  actual_groups <- group_numbers(1:200, suffix = "group")
  expected_groups <- paste0(stringr::str_pad(1:200, "left", pad = "0", width = 3), "group")
  testthat::expect_equal(actual_groups, expected_groups)
})

## Sep works--------------------------------------------------------------------
testthat::test_that("suffix works as expected", {
  actual_groups <- group_numbers(1:200, prefix = "batch", sep = "+")
  expected_groups <- paste0("batch+", stringr::str_pad(1:200, "left", pad = "0", width = 3))
  testthat::expect_equal(actual_groups, expected_groups)
})

## All inputs work together-----------------------------------------------------
testthat::test_that("All inputs work as expected", {
  actual_groups <- group_numbers(1:200, prefix = "batch", suffix = "file", sep = "_")
  expected_groups <- paste0("batch_", stringr::str_pad(1:200, "left", pad = "0", width = 3), "_file")
  testthat::expect_equal(actual_groups, expected_groups)
})
