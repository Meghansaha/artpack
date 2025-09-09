# =============================================================================#
# seq_bounce - Testing Suite----------------------------------------------------
# =============================================================================#

# =============================================================================#
# Input Testing-----------------------------------------------------------------
# =============================================================================#
## All required inputs should be present----------------------------------------
### start_n----
cli::test_that_cli("start_n should be present",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         seq_bounce(end_n = 10, length = 30, by = .247)
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

### end_n----
cli::test_that_cli("end_n should be present",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         seq_bounce(start_n = 0, length = 30, by = .247)
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

### length----
cli::test_that_cli("length should be present",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         seq_bounce(start_n = 0, end_n = 10, by = .247)
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

### by----
cli::test_that_cli("by should not be NULL",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         seq_bounce(start_n = 0, end_n = 10, length = 30, by = NULL)
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

## All inputs should be of length 1---------------------------------------------
### start_n----
cli::test_that_cli("start_n should be of length 1",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         seq_bounce(start_n = c(0,4) , end_n = 10, length = 30, by = .247)
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

### end_n----
cli::test_that_cli("end_n should be of length 1",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         seq_bounce(start_n = 0 , end_n = c(10, 20, 30), length = 30, by = .247)
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

### length----
cli::test_that_cli("length should be of length 1",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         seq_bounce(start_n = 0 , end_n = 10, length = c(30, 89, 90, 120), by = .247)
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

### by----
cli::test_that_cli("by should be of length 1",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         seq_bounce(start_n = 0 , end_n = 10, length = 30, by = c(1:90))
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)


## All inputs should be numeric-------------------------------------------------
### start_n----
cli::test_that_cli("start_n should be numeric",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         seq_bounce(start_n = "0" , end_n = 10, length = 30, by = .247)
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

### end_n----
cli::test_that_cli("end_n should be numeric",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         seq_bounce(start_n = 0 , end_n = list(10), length = 30, by = .247)
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

### length----
cli::test_that_cli("length should be numeric",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         seq_bounce(start_n = 0 , end_n = 10, length = "30", by = .247)
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

### by----
cli::test_that_cli("by should be numeric",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         seq_bounce(start_n = 0 , end_n = 10, length = 30, by = list(.247))
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

## length input should be a positive integer------------------------------------
### negative length----
cli::test_that_cli("length should be positive",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         seq_bounce(start_n = 0 , end_n = 10, length = -30)
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

### floating length----
cli::test_that_cli("length should be an integer",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         seq_bounce(start_n = 0 , end_n = 10, length = 30.5)
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

## by input should be a numeric positive----------------------------------------
cli::test_that_cli("length should be an integer",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         seq_bounce(start_n = 0 , end_n = 10, length = 30, by = -4)
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)


## start_n input must be smaller than end_n input-------------------------------
cli::test_that_cli("start_n should be < than end_n",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         seq_bounce(start_n = 10 , end_n = 0, length = 30)
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
  expected_output <- c(0,1,2,3,2,1,0,1,2)
  # When this is ran#
  actual_output <- seq_bounce(start_n = 0 , end_n = 3, length = 9)
  # So these should be equal
  testthat::expect_equal(actual_output, expected_output)
})

## The output is a numeric vector-----------------------------------------------
testthat::test_that("output is a numeric vector", {
  # Create safer randomized values
  rand_start_n <- sample(-10:10, 1)
  rand_end_n <- sample((rand_start_n + 5):(rand_start_n + 20), 1)  # Ensure bigger range
  rand_length <- sample(1:100, 1)
  # Ensure by is smaller than the range
  max_by <- (rand_end_n - rand_start_n) / 2
  rand_by <- runif(1, 0.1, max_by)

  actual_output <- seq_bounce(
    start_n = rand_start_n,
    end_n = rand_end_n,
    length = rand_length,
    by = rand_by
  )

  testthat::expect_true(actual_output |> is.numeric())
  testthat::expect_true(actual_output |> is.vector())
})

## The length of the output is as expected--------------------------------------
testthat::test_that("output length is as expected", {
  # Create randomized values for inputs
  rand_start_n <- sample(-10:10, 1)
  rand_end_n <- sample((rand_start_n + 1):(rand_start_n + 20), 1)
  rand_length <- sample(1:100, 1)
  rand_by <- sample(seq(.1, 5, by = .1), 1)

  # No matter the value, the output should be a numeric vector of the expected length
  actual_length <-
    seq_bounce(
      start_n = rand_start_n,
      end_n = rand_end_n,
      length = rand_length,
      by = rand_by
    ) |>
    length()

  # So these should be equal
  testthat::expect_equal(rand_length, actual_length)
})
