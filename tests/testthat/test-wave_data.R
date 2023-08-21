#=============================================================================#
# wave_data - Testing Suite----------------------------------------------------
#=============================================================================#

#=============================================================================#
# Input Testing----------------------------------------------------------------
#=============================================================================#

# Testing start and end inputs
testthat::test_that("Test that missing start throws an error", {
  testthat::expect_error(
    wave_data(end = 1)
  )
})

testthat::test_that("Test that missing end throws an error", {
  testthat::expect_error(
    wave_data(start = 1)
  )
})

# Testing that arguments of length 1 are only accepted
cli::test_that_cli("Argument length checks",
  {
    testthat::local_edition(3)
    testthat::expect_snapshot(
      {
        wave_data(
          start = 1:3,
          end = 0,
          size = 1,
          type = c("sin", "cos"),
          orientation = "horizontal",
          freq = 3,
          n_points = 500,
          color = NULL,
          fill = NULL,
          group_var = FALSE,
          dampen = NULL,
          amplify = NULL
        )
      },
      error = TRUE
    )
  },
  configs = "ansi"
)

# Testing that numeric arguments only accept numeric
cli::test_that_cli("Numeric argument check",
  {
    testthat::local_edition(3)
    testthat::expect_snapshot(
      {
        wave_data(
          start = 1,
          end = 0,
          size = "big",
          freq = "a little",
          n_points = 500,
          color = NULL,
          fill = NULL,
          dampen = NULL,
          amplify = "bigggg"
        )
      },
      error = TRUE
    )
  },
  configs = "ansi"
)

# Testing that numeric arguments only accept valid inputs
cli::test_that_cli("Numeric argument check",
  {
    testthat::local_edition(3)
    testthat::expect_snapshot(
      {
        wave_data(
          start = 1,
          end = 0,
          size = -5,
          freq = -3,
          n_points = 500,
          color = NULL,
          fill = NULL,
          dampen = NULL,
          amplify = -9
        )
      },
      error = TRUE
    )
  },
  configs = "ansi"
)

# Testing that character arguments only accept characters
cli::test_that_cli("Character argument check",
  {
    testthat::local_edition(3)
    testthat::expect_snapshot(
      {
        wave_data(
          start = 1,
          end = 0,
          size = 2,
          freq = 5,
          n_points = 500,
          color = 000000,
          fill = "#111111",
          dampen = NULL,
          amplify = 1
        )
      },
      error = TRUE
    )
  },
  configs = "ansi"
)

# Testing that only valid colors are accepted
cli::test_that_cli("Color checks",
  {
    testthat::local_edition(3)
    testthat::expect_snapshot(
      {
        wave_data(
          start = 1,
          end = 0,
          color = "#000"
        )
      },
      error = TRUE
    )
  },
  configs = "ansi"
)

cli::test_that_cli("Fill checks",
  {
    testthat::local_edition(3)
    testthat::expect_snapshot(
      {
        wave_data(
          start = 1,
          end = 0,
          fill = "#000"
        )
      },
      error = TRUE
    )
  },
  configs = "ansi"
)

# Testing that only valid wave types are accepted
cli::test_that_cli("Wave type checks",
  {
    testthat::local_edition(3)
    testthat::expect_snapshot(
      {
        wave_data(
          start = 1,
          end = 0,
          type = "COS"
        )
      },
      error = TRUE
    )
  },
  configs = "ansi"
)

# Testing that only valid orientation types are accepted
cli::test_that_cli("Wave type checks",
  {
    testthat::local_edition(3)
    testthat::expect_snapshot(
      {
        wave_data(
          start = 1,
          end = 0,
          orientation = "verticall"
        )
      },
      error = TRUE
    )
  },
  configs = "ansi"
)

# Testing that only valid group_vars are accepted
cli::test_that_cli("Wave type checks",
  {
    testthat::local_edition(3)
    testthat::expect_snapshot(
      {
        wave_data(
          start = 1,
          end = 0,
          group_var = "TRUE"
        )
      },
      error = TRUE
    )
  },
  configs = "ansi"
)

# Testing that only valid group_vars are accepted
cli::test_that_cli("Wave type checks",
  {
    testthat::local_edition(3)
    testthat::expect_snapshot(
      {
        wave_data(
          start = 1,
          end = 0,
          group_var = "TRUE"
        )
      },
      error = TRUE
    )
  },
  configs = "ansi"
)

#=============================================================================#
# Output Testing---------------------------------------------------------------
#=============================================================================#

# Testing that group var is present when toggled
testthat::test_that("group var toggle works", {
  testthat::expect_true(
    "group" %in% names(wave_data(0, 1, group_var = TRUE))
  )
})

testthat::test_that("group var toggle is off by default", {
  testthat::expect_false(
    "group" %in% names(wave_data(0, 1))
  )
})

# Testing that color vars are present when toggled
testthat::test_that("fill var toggle works", {
  testthat::expect_true(
    "fill" %in% names(wave_data(0, 1, fill = "#000000"))
  )
})

testthat::test_that("color var toggle works", {
  testthat::expect_true(
    "color" %in% names(wave_data(0, 1, color = "#000000"))
  )
})




# Check that amplify works
testthat::test_that("amplify works", {
  base_df <- tibble::tribble(
    ~x, ~y,
    0, 0,
    0.25, -3,
    0.5, 1.10214571844014e-15,
    0.75, 3,
    1, -2.20429143688028e-15,
    1, -2.20429143688028e-15,
    1, 0.999999999999998,
    0.75, 4,
    0.5, 1,
    0.25, -2,
    0, 1,
    0, 0
  )


  testthat::expect_equal(
    base_df, wave_data(0, 1, n_points = 5, amplify = 3)
  )
})

# Check that dampen works
testthat::test_that("dampen works", {
  base_df <- tibble::tribble(
    ~x, ~y,
    0, 0,
    0.25, -0.1,
    0.5, 3.67381906146713e-17,
    0.75, 0.1,
    1, -7.34763812293426e-17,
    1, -7.34763812293426e-17,
    1, 1,
    0.75, 1.1,
    0.5, 1,
    0.25, 0.9,
    0, 1,
    0, 0
  )

  testthat::expect_equal(
    base_df, wave_data(0, 1, n_points = 5, dampen = 10)
  )
})

# Check that orientation toggle works
testthat::test_that("orientation toggle works", {
  base_df <- tibble::tribble(
    ~x, ~y,
    0, 0,
    -1, 0.25,
    3.67381906146713e-16, 0.5,
    1, 0.75,
    -7.34763812293426e-16, 1,
    -7.34763812293426e-16, 1,
    0.999999999999999, 1,
    2, 0.75,
    1, 0.5,
    0, 0.25,
    1, 0,
    0, 0
  )

  testthat::expect_equal(
    base_df, wave_data(0, 1, n_points = 5, orientation = "vertical")
  )
})
