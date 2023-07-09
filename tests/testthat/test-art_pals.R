#=============================================================================#
# art_pals() Testing ----------------------------------------------------------
#=============================================================================#

#=============================================================================#
# Testing Inputs---------------------------------------------------------------
#=============================================================================#

# `pal` input testing----------------------------------------------------------
pal_checks <-
  list(
    "length" = c("ocean", "rainbow"),
    "class" = list("ocean"),
    "case" = "RaiNboW",
    "palette" = "BuPu"
  )

pal_indices <-
  1:length(pal_checks)


# Testing for case should not produce error, so need a different test#
purrr::map2(
  pal_checks, pal_indices,
  ~ if (names(pal_checks[.y]) == "case") {
    testthat::expect_no_error(
      art_pals(pal = .x)
    )
  } else {
    cli::test_that_cli("pal checks",
      {
        testthat::local_edition(3)
        testthat::expect_snapshot(
          {
            art_pals(pal = .x)
          },
          error = TRUE
        )
      },
      configs = "ansi"
    )
  }
)

# `n` input testing----------------------------------------------------------
n_checks <-
  list(
    "class" = "8",
    "length" = 1:100,
    "integer" = 7.5,
    "value" = -3
  )

purrr::map(
  n_checks,
  ~ cli::test_that_cli("n checks",
    {
      testthat::local_edition(3)
      testthat::expect_snapshot(
        {
          art_pals(pal = "rainbow", n = .x)
        },
        error = TRUE
      )
    },
    configs = "ansi"
  )
)

# `direction` input testing----------------------------------------------------
direction_checks <-
  list(
    "length" = c("rev", "reg"),
    "class" = -1,
    "case" = "REGular",
    "direction" = "vertical"
  )

direction_indices <-
  1:length(direction_checks)


# Testing for case should not produce error, so need a different test#
purrr::map2(
  direction_checks, direction_indices,
  ~ if (names(direction_checks[.y]) == "case") {
    testthat::expect_no_error(
      art_pals(direction = .x)
    )
  } else {
    cli::test_that_cli("direction checks",
      {
        testthat::local_edition(3)
        testthat::expect_snapshot(
          {
            art_pals(direction = .x)
          },
          error = TRUE
        )
      },
      configs = "ansi"
    )
  }
)

# =============================================================================#
# Testing Outputs--------------------------------------------------------------
# =============================================================================#

# Checking to make sure palettes are returned as vectors-----------------------
testthat::expect_true(
  is.vector(art_pals())
)

# Checking to make sure palettes sizes are as expected-------------------------

# Default is 5#
default_length <- length(art_pals())
testthat::expect_equal(default_length, 5)

# Smoll Palette#
smol_length <- length(art_pals(n = 3))
testthat::expect_equal(smol_length, 3)

# Chonk Palette#
chonk_length <- length(art_pals(n = 150))
testthat::expect_equal(chonk_length, 150)

# Checking to make sure directions work as expected----------------------------
# Regular Direction#
regular_brood <- c("#000000", "#0C0C0C", "#191919", "#262626", "#333333")
regular_inputs <- c("reg", "Regular", "REG", "REGULAR", "ReGuLaR", "REGuLAR")

testthat::expect_equal(
  art_pals("brood", 5, direction = sample(regular_inputs, 1)),
  regular_brood
)

# Reverse Direction#
reverse_brood <- c("#333333", "#262626", "#191919", "#0C0C0C", "#000000")
reverse_inputs <- c("rev", "Reverse", "REV", "REVERSE", "ReVeRsE", "REVeRSE")

testthat::expect_equal(
  art_pals("brood", 5, direction = sample(reverse_inputs, 1)),
  reverse_brood
)
