# =============================================================================#
# grid_maker - Testing Suite----------------------------------------------------
# =============================================================================#

# =============================================================================#
# Input Testing----------------------------------------------------------------
# =============================================================================#
# Testing that required arguments are present
cli::test_that_cli("Missing argument checks",
  {
    testthat::local_edition(3)
    testthat::expect_snapshot(
      {
        grid_maker()
      },
      error = TRUE
    )
  },
  configs = "ansi"
)

# Testing that valid arguments lengths are only accepted
cli::test_that_cli("Argument length checks",
  {
    testthat::local_edition(3)
    testthat::expect_snapshot(
      {
        grid_maker(
          xlim = 1,
          ylim = 0,
          size = 1:9,
        )
      },
      error = TRUE
    )
  },
  configs = "ansi"
)

# Testing that valid arguments types are only accepted
cli::test_that_cli("Numeric checks",
  {
    testthat::local_edition(3)
    testthat::expect_snapshot(
      {
        grid_maker(
          xlim = c(1, 2),
          ylim = c("small", "bigggg"),
          size = 1,
        )
      },
      error = TRUE
    )
  },
  configs = "ansi"
)

# Valid size check - max limits
cli::test_that_cli("Size check - lim value",
  {
    testthat::local_edition(3)
    testthat::expect_snapshot(
      {
        grid_maker(
          xlim = c(0, 1),
          ylim = c(0, 2),
          size = 3
        )
      },
      error = TRUE
    )
  },
  configs = "ansi"
)

# Valid size check - positive number
cli::test_that_cli("Size check - positive number",
  {
    testthat::local_edition(3)
    testthat::expect_snapshot(
      {
        grid_maker(
          xlim = c(0, 1),
          ylim = c(0, 2),
          size = -1
        )
      },
      error = TRUE
    )
  },
  configs = "ansi"
)

# Character args are characters
cli::test_that_cli("Character args are character",
  {
    testthat::local_edition(3)
    testthat::expect_snapshot(
      {
        grid_maker(
          xlim = c(0, 1),
          ylim = c(0, 2),
          size = 1,
          fill_pal = list("blue", "red", "green"),
          color_pal = 1:3,
          fill_style = TRUE,
          color_style = FALSE
        )
      },
      error = TRUE
    )
  },
  configs = "ansi"
)

# Colors are valid
cli::test_that_cli("Fills are valid",
  {
    testthat::local_edition(3)
    testthat::expect_snapshot(
      {
        grid_maker(
          xlim = c(0, 1),
          ylim = c(0, 2),
          size = 1,
          fill_pal = c("blue", "red", "green", "yello")
        )
      },
      error = TRUE
    )
  },
  configs = "ansi"
)

cli::test_that_cli("Colors are valid",
  {
    testthat::local_edition(3)
    testthat::expect_snapshot(
      {
        grid_maker(
          xlim = c(0, 1),
          ylim = c(0, 2),
          size = 1,
          fill_pal = c("blue", "red", "green", "yellow"),
          color_pal = "#000"
        )
      },
      error = TRUE
    )
  },
  configs = "ansi"
)

# String presets are valid
cli::test_that_cli("Fill style presets",
  {
    testthat::local_edition(3)
    testthat::expect_snapshot(
      {
        grid_maker(
          xlim = c(0, 1),
          ylim = c(0, 2),
          size = 1,
          fill_pal = c("blue", "red", "green"),
          fill_style = "idk",
          color_pal = "#000000",
          color_style = "wild"
        )
      },
      error = TRUE
    )
  },
  configs = "ansi"
)

cli::test_that_cli("Color style presets",
  {
    testthat::local_edition(3)
    testthat::expect_snapshot(
      {
        grid_maker(
          xlim = c(0, 1),
          ylim = c(0, 2),
          size = 1,
          fill_pal = c("blue", "red", "green"),
          color_pal = "#000000",
          color_style = "wild"
        )
      },
      error = TRUE
    )
  },
  configs = "ansi"
)

cli::test_that_cli("Size is only a whole number",
  {
    testthat::local_edition(3)
    testthat::expect_snapshot(
      {
        grid_maker(
          xlim = c(0, 1),
          ylim = c(0, 2),
          size = .5
        )
      },
      error = TRUE
    )
  },
  configs = "ansi"
)

# =============================================================================#
# Output Testing---------------------------------------------------------------
# =============================================================================#

# Fx works minimally
testthat::test_that("grid maker works", {
  testthat::expect_no_error(grid_maker(c(0, 1), c(0, 1),
    size = 1,
    fill_pal = "#000000",
    color_pal = "#000000"
  ))
})

testthat::test_that("fill_style toggle works", {
  testthat::expect_no_error(grid_maker(c(0, 1), c(0, 1),
    size = 1,
    fill_pal = "#000000",
    fill_style = "random"
  ))
})

testthat::test_that("color_style toggle works", {
  testthat::expect_no_error(grid_maker(c(0, 1), c(0, 1),
    size = 1,
    color_pal = "#000000",
    color_style = "random"
  ))
})

testthat::test_that("color_pal toggles color output", {
  testthat::expect_true(
    "color" %in% names(grid_maker(c(0, 1), c(0, 1),
      size = 1,
      color_pal = "#000000",
      color_style = "random"
    ))
  )
})

testthat::test_that("fill_pal toggles color output", {
  testthat::expect_true(
    "fill" %in% names(grid_maker(c(0, 1), c(0, 1),
      size = 1,
      fill_pal = "#000000",
      fill_style = "random"
    ))
  )
})

testthat::test_that("No color outputs by default", {
  testthat::expect_true(
    all(!c("fill", "color") %in% names(grid_maker(c(0, 1), c(0, 1),
      size = 1
    )))
  )
})
