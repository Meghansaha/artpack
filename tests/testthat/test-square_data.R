#=============================================================================#
# square_data() - Testing Suite------------------------------------------------
#=============================================================================#

#=============================================================================#
# Input Testing----------------------------------------------------------------
#=============================================================================#

# Testing that missing args throw an error
cli::test_that_cli("Test that missing x throws an error",
  {
    testthat::local_edition(3)
    testthat::expect_snapshot(
      {
        square_data(y = 0, size = 10)
      },
      error = TRUE
    )
  },
  configs = "ansi"
)

cli::test_that_cli("Test that missing y throws an error",
  {
    testthat::local_edition(3)
    testthat::expect_snapshot(
      {
        square_data(x = 0, size = 10)
      },
      error = TRUE
    )
  },
  configs = "ansi"
)

cli::test_that_cli("Test that non-numeric x throws an error",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         square_data(x = "0", y = 0, size = 10)
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

cli::test_that_cli("Test that non-numeric y throws an error",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         square_data(x = 0, y = "0", size = 10)
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

cli::test_that_cli("Test that invalid size throws an error",
  {
    testthat::local_edition(3)
    testthat::expect_snapshot(
      {
        square_data(x = 0, y = 0, size = 0)
      },
      error = TRUE
    )
  },
  configs = "ansi"
)

cli::test_that_cli("Test that invalid size throws an error",
  {
    testthat::local_edition(3)
    testthat::expect_snapshot(
      {
        square_data(x = 0, y = 0, size = -10)
      },
      error = TRUE
    )
  },
  configs = "ansi"
)

# Testing that invalid arg lengths causes an error
cli::test_that_cli("Test that invalid x arg length throws an error",
  {
    testthat::local_edition(3)
    testthat::expect_snapshot(
      {
        square_data(x = 1:3, y = 0, size = 5)
      },
      error = TRUE
    )
  },
  configs = "ansi"
)

cli::test_that_cli("Test that invalid y arg length throws an error",
  {
    testthat::local_edition(3)
    testthat::expect_snapshot(
      {
        square_data(x = 0, y = 1:3, size = 5)
      },
      error = TRUE
    )
  },
  configs = "ansi"
)

cli::test_that_cli("Test that invalid size arg length throws an error",
  {
    testthat::local_edition(3)
    testthat::expect_snapshot(
      {
        square_data(x = 0, y = 0, size = 1:5)
      },
      error = TRUE
    )
  },
  configs = "ansi"
)

cli::test_that_cli("Test that invalid color arg length throws an error",
  {
    testthat::local_edition(3)
    testthat::expect_snapshot(
      {
        square_data(x = 0, y = 0, size = 1, color = c("#000000", "#333333"))
      },
      error = TRUE
    )
  },
  configs = "ansi"
)

cli::test_that_cli("Test that invalid fill arg length throws an error",
  {
    testthat::local_edition(3)
    testthat::expect_snapshot(
      {
        square_data(x = 0, y = 0, size = 1, fill = c("#000000", "#333333"))
      },
      error = TRUE
    )
  },
  configs = "ansi"
)

cli::test_that_cli("Test that multiple invalid arg lengths throws an error",
  {
    testthat::local_edition(3)
    testthat::expect_snapshot(
      {
        square_data(x = 1:3, y = 2:5, size = 1)
      },
      error = TRUE
    )
  },
  configs = "ansi"
)

# Test that invalid colors cause an error
cli::test_that_cli("Test that invalid colors throws an error",
  {
    testthat::local_edition(3)
    testthat::expect_snapshot(
      {
        square_data(x = 1, y = 2, size = 1, color = "blAck")
      },
      error = TRUE
    )
  },
  configs = "ansi"
)

cli::test_that_cli("Test that invalid fills throws an error",
  {
    testthat::local_edition(3)
    testthat::expect_snapshot(
      {
        square_data(x = 1, y = 2, size = 1, fill = "blAck")
      },
      error = TRUE
    )
  },
  configs = "ansi"
)

# =============================================================================#
# Output Testing---------------------------------------------------------------
# =============================================================================#
# Test that the output is a dataframe
testthat::test_that("Output is a dataframe", {
  testthat::expect_true(
    is.data.frame(square_data(x = 1, y = 2, size = 1, color = "#000000"))
  )
})

# Test that the output has a color variable when applicable
testthat::test_that("Output has color variable", {
  testthat::expect_true(
    "color" %in% names((square_data(x = 1, y = 2, size = 1, color = "#000000")))
  )
})

# Test that the output has a fill variable when applicable
testthat::test_that("Output has fill variable", {
  testthat::expect_true(
    "fill" %in% names((square_data(x = 1, y = 2, size = 1, fill = "#000000")))
  )
})

# Test that the output has a group variable when applicable
testthat::test_that("Output has group variable", {
  testthat::expect_true(
    "group" %in% names((square_data(x = 1, y = 2, size = 1, group_var = TRUE)))
  )
})

# Test that the output DOES NOT have a group variable when applicable
testthat::test_that("Output does not have group variable", {
  testthat::expect_false(
    "group" %in% names((square_data(x = 1, y = 2, size = 1)))
  )
})

# Test that the output transforms the group prefix properly
testthat::test_that("Output has default group prefix", {
  testthat::expect_true(
    "square_" == square_data(x = 1, y = 2, size = 1, group_var = TRUE) |> dplyr::select(group) |> dplyr::distinct()
  )
})

testthat::test_that("Output has user-defined group prefix", {
  testthat::expect_true(
    "Number 1" == square_data(x = 1, y = 2, size = 1, group_var = TRUE, group_prefix = "Number 1") |> dplyr::select(group) |> dplyr::distinct()
    )
})

cli::test_that_cli("Non character group prefix throws an error",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         square_data(x = 1, y = 2, size = 1, group_var = TRUE, group_prefix = 1)
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

cli::test_that_cli("User-defined group prefix gives a warning if group var is FALSE",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         square_data(0,0,3, group_prefix = "box")
                       }
                     )
                   },
                   configs = "ansi"
)

