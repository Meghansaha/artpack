# =============================================================================#
# art_pals() Testing ----------------------------------------------------------
# =============================================================================#

# =============================================================================#
# Input Testing-----------------------------------------------------------------
# =============================================================================#
## All inputs should be of length 1---------------------------------------------
### pal----
cli::test_that_cli("pal should be of length 1",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         art_pals(c("brood", "ocean"))
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

### n----
cli::test_that_cli("n should be of length 1",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         art_pals(n = 1:4)
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
                         art_pals(direction = c("reg", "rev"))
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

### randomize----
cli::test_that_cli("randomize should be of length 1",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         art_pals(randomize = c(TRUE, FALSE))
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

## All inputs should be of the correct class------------------------------------
### pal----
cli::test_that_cli("pal should be of class character",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         art_pals(pal = TRUE)
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

### n----
cli::test_that_cli("n should be of class numeric",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         art_pals(n = "4")
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

### direction----
cli::test_that_cli("direction should be of class character",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         art_pals(direction = 1)
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

### randomize----
cli::test_that_cli("randomize should be of class logical",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         art_pals(randomize = "TRUE")
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

## pal should be an accepted artpack color palette------------------------------
cli::test_that_cli("pal should be an accepted artpack color palette",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         art_pals(pal = "supercool")
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

## accepted pal should work no matter the string case---------------------------
testthat::test_that("accepted pal should work no matter the string case", {
  # Regular case
  vec_expected <- art_pals(pal = "rainbow")
  # Groovy case
  vec_actual <- art_pals(pal = "RaInBoW")
  # Should match
  testthat::expect_equal(vec_actual, vec_expected)
})

## n should be a positive number------------------------------------------------
cli::test_that_cli("n should be a positive number",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         art_pals(n = -9)
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

## n should be an integer-------------------------------------------------------
cli::test_that_cli("n should be an integer",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         art_pals(n = 9.2)
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

## direction should be an accepted value----------------------------------------
cli::test_that_cli("direction should be an accepted value",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         art_pals(direction = "up")
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

## accepted direction should work no matter the string case---------------------
testthat::test_that("accepted direction should work no matter the string case", {
  # Regular case
  vec_expected <- art_pals(direction = "reg")
  # Groovy case
  vec_actual <- art_pals(direction = "ReG")
  # Should match
  testthat::expect_equal(vec_actual, vec_expected)
})

# =============================================================================#
# Output Testing----------------------------------------------------------------
# =============================================================================#
## The function works with no error---------------------------------------------
testthat::test_that("works as expected", {
  # We expect this#
  expected_output <- c("#12012E", "#144267", "#15698C", "#0695AA", "#156275")
  # When this is ran#
  actual_output <- art_pals()
  # So these should be equal
  testthat::expect_equal(actual_output, expected_output)
})

## Palettes are returned as vectors---------------------------------------------
testthat::test_that("outputs are vectors", {
  testthat::expect_true(is.vector(art_pals())
  )
}
)

## Palettes size is as expected-------------------------------------------------
testthat::test_that("output length is as expected", {
  # Create randomized value for n
  rand_length <- sample(1:150, 1)

  # No matter the value, the output should be a vector of the expected length
  actual_length <- art_pals(n = rand_length) |> length()

  # So these should be equal
  testthat::expect_equal(rand_length, actual_length)
})

### Regular direction----
testthat::test_that("output direction is as expected - regular", {
  vec_regular_brood <- c("#000000", "#0C0C0C", "#191919", "#262626", "#333333")
  regular_input <- c("reg", "Regular", "REG", "REGULAR", "ReGuLaR", "REGuLAR") |> sample(1)

  testthat::expect_equal(
    art_pals("brood", direction = regular_input),
    vec_regular_brood
  )
})

### Reverse direction----
testthat::test_that("output direction is as expected - reverse", {
  vec_reverse_brood <- c("#333333", "#262626", "#191919", "#0C0C0C", "#000000")
  reverse_input <- c("rev", "Reverse", "REV", "REVERSE", "ReVeRsE", "REVeRSE") |> sample(1)

  testthat::expect_equal(
    art_pals("brood", direction = reverse_input),
    vec_reverse_brood
  )
})

## Randomize actually randomizes------------------------------------------------
# Need withr because it'll be my luck that a sample ends up being the "regular" values -.-#
testthat::test_that("randomize argument works", {
  withr::with_seed(
    seed = 805,
    code = {
      vec_regular <- art_pals()
      vec_randomized <- art_pals(randomize = TRUE)
      testthat::expect_false(identical(vec_regular, vec_randomized))
    }
  )
})
