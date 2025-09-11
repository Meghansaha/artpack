# =============================================================================#
# group_sample() Testing -------------------------------------------------------
# =============================================================================#

# =============================================================================#
# Testing Inputs----------------------------------------------------------------
# =============================================================================#
## data is non-missing cli output----
cli::test_that_cli(
  "Missing data not accepted",
  {
    testthat::expect_snapshot(
      {
        group_sample()
      },
      error = TRUE
    )
  },
  configs = "ansi"
)

## data is df/tibble cli output----
cli::test_that_cli(
  "Data must be dataframe or tibble",
  {
    testthat::expect_snapshot(
      {
        vec_data <- 1:10

        vec_data |> group_sample()
      },
      error = TRUE
    )
  },
  configs = "ansi"
)

## data has at least 1 variable cli output----
cli::test_that_cli(
  "Data must have at least 1 variable",
  {
    testthat::expect_snapshot(
      {
        df_data <- data.frame()

        df_data |> group_sample()
      },
      error = TRUE
    )
  },
  configs = "ansi"
)

## group var is present cli output----
cli::test_that_cli(
  "Group variable must be present",
  {
    testthat::expect_snapshot(
      {
        df_data <- data.frame("x" = 1:5, "y" = 6:10)

        df_data |> group_sample()
      },
      error = TRUE
    )
  },
  configs = "ansi"
)

## group is of length 1 cli output----
cli::test_that_cli(
  "group must be of length 1",
  {
    testthat::expect_snapshot(
      {
        df_data <- data.frame("x" = 1:5, "y" = 6:10, group = 1:5)

        df_data |> group_sample(group = c(group, x), n = 5)
      },
      error = TRUE
    )
  },
  configs = "ansi"
)

## n is length 1 cli output----
cli::test_that_cli(
  "N must be of length 1",
  {
    testthat::expect_snapshot(
      {
        df_data <- data.frame("x" = 1:5, "y" = 6:10, group = 1:5)

        df_data |> group_sample(group = group, n = 1:3)
      },
      error = TRUE
    )
  },
  configs = "ansi"
)


## n is numeric cli output----
cli::test_that_cli(
  "n must be numeric",
  {
    testthat::expect_snapshot(
      {
        df_data <- data.frame("x" = 1:5, "y" = 6:10, group = 1:5)

        df_data |> group_sample(group = group, n = "five")
      },
      error = TRUE
    )
  },
  configs = "ansi"
)

## n is positive cli output----
cli::test_that_cli(
  "n must be positive",
  {
    testthat::expect_snapshot(
      {
        df_data <- data.frame("x" = 1:5, "y" = 6:10, group = 1:5)

        df_data |> group_sample(group = group, n = -3)
      },
      error = TRUE
    )
  },
  configs = "ansi"
)

## n is an integer cli output----
cli::test_that_cli(
  "n must be an integer",
  {
    testthat::expect_snapshot(
      {
        df_data <- data.frame("x" = 1:5, "y" = 6:10, group = 1:5)

        df_data |> group_sample(group = group, n = .3)
      },
      error = TRUE
    )
  },
  configs = "ansi"
)

## n is not larger than group output----
cli::test_that_cli(
  "n must not be larger than the group n",
  {
    testthat::expect_snapshot(
      {
        df_data <- data.frame("x" = 1:5, "y" = 6:10, group = 1:5)

        df_data |> group_sample(group = group, n = 100)
      },
      error = TRUE
    )
  },
  configs = "ansi"
)


## prop is numeric cli output----
cli::test_that_cli(
  "prop must be numeric",
  {
    testthat::expect_snapshot(
      {
        df_data <- data.frame("x" = 1:5, "y" = 6:10, group = 1:5)

        df_data |> group_sample(group = group, prop = "20%")
      },
      error = TRUE
    )
  },
  configs = "ansi"
)

## prop is length 1 cli output----
cli::test_that_cli(
  "prop must be of length 1",
  {
    testthat::expect_snapshot(
      {
        df_data <- data.frame("x" = 1:5, "y" = 6:10, group = 1:5)

        df_data |> group_sample(group = group, prop = 1:3)
      },
      error = TRUE
    )
  },
  configs = "ansi"
)

## prop is valid (percentage) cli output----
cli::test_that_cli(
  "prop must be a valid percentage",
  {
    testthat::expect_snapshot(
      {
        df_data <- data.frame("x" = 1:5, "y" = 6:10, group = 1:5)

        df_data |> group_sample(group = group, prop = 1.5)
      },
      error = TRUE
    )
  },
  configs = "ansi"
)

## prob is same length as groups cli output----
cli::test_that_cli(
  "prob must be the same length as the unique values in group",
  {
    testthat::expect_snapshot(
      {
        vec_bad_probs <- c(1, .2)

        df_data <- data.frame("x" = 1:5, "y" = 6:10, group = 1:5)
        df_data |> group_sample(group = group, prop = 1, prob = vec_bad_probs)
      },
      error = TRUE
    )
  },
  configs = "ansi"
)

## prob is numeric cli output----
cli::test_that_cli(
  "prob must be numeric values",
  {
    testthat::expect_snapshot(
      {
        vec_bad_probs <- c(".1", "two", ".3", "4", ".5")

        df_data <- data.frame("x" = 1:5, "y" = 6:10, group = 1:5)
        df_data |> group_sample(group = group, prop = 1, prob = vec_bad_probs)
      },
      error = TRUE
    )
  },
  configs = "ansi"
)

## prob is positive cli output----
cli::test_that_cli(
  "prob must be positive values",
  {
    testthat::expect_snapshot(
      {
        vec_bad_probs <- c(.1, -.3, .2, .1, .8)

        df_data <- data.frame("x" = 1:5, "y" = 6:10, group = 1:5)
        df_data |> group_sample(group = group, prop = 1, prob = vec_bad_probs)
      },
      error = TRUE
    )
  },
  configs = "ansi"
)

## group_output is logical cli output----
cli::test_that_cli(
  "group_output must be a logical boolean",
  {
    testthat::expect_snapshot(
      {
        df_data <- data.frame("x" = 1:5, "y" = 6:10, group = 1:5)
        df_data |> group_sample(group = group, prop = 1, group_output = "FALSE")
      },
      error = TRUE
    )
  },
  configs = "ansi"
)

# =============================================================================#
# Testing Outputs---------------------------------------------------------------
# =============================================================================#
## group_sample works----
testthat::test_that(
  "group_sample works", {
    set.seed(123)

    vec_coords <- 1:10

    df_data <-
      data.frame(
        "x" = vec_coords,
        "y" = vec_coords,
        "group_col" = group_numbers(1:5) |> rep(each = 2)
      )

    df_data_sampled <-
      df_data |>
      group_sample(group_col)

    testthat::expect_true("group_col" %in% names(df_data_sampled))
  }

)

gc()

## group_sample n works----
testthat::test_that(
  "group_sample n works", {
    set.seed(456)

    vec_coords <- 1:10

    n_groups_to_sample <- sample(1:10,1)

    df_data <-
      data.frame(
        "x" = vec_coords,
        "y" = vec_coords,
        "group_col" = group_numbers(1:10)
      )

    df_data_sampled <-
      df_data |>
      group_sample(group_col, n = n_groups_to_sample)

    testthat::expect_equal(
      df_data_sampled |> nrow(),
      n_groups_to_sample
    )
  }
)

gc()

## group_sample prop works----
testthat::test_that(
  "group_sample prop works", {
    set.seed(789)

    vec_coords <- 1:10

    prop_groups_to_sample <- seq(.1,1, l = 10) |> sample(1)
    n_expected_sampled_rows <- prop_groups_to_sample * 10

    df_data <-
      data.frame(
        "x" = vec_coords,
        "y" = vec_coords,
        "group_col" = group_numbers(1:10)
      )

    df_data_sampled <-
      df_data |>
      group_sample(group_col, prop = prop_groups_to_sample)

    testthat::expect_equal(
      df_data_sampled |> nrow(),
      n_expected_sampled_rows
    )
  }
)

gc()

## group_sample prob works----
testthat::test_that(
  "group_sample prob works", {
    set.seed(101112)
    vec_coords <- 1:10

    vec_probs <- sample(seq(0,1, l = 10))
    excluded_group <- which(vec_probs == 0) |> sprintf("%02d", ... = _)

    df_data <-
      data.frame(
        "x" = vec_coords,
        "y" = vec_coords,
        "group_col" = group_numbers(1:10)
      )

    df_data_sampled <-
      df_data |>
      group_sample(group_col, n = 8, prob = vec_probs)

    testthat::expect_false(
      excluded_group %in% (df_data_sampled |> dplyr::pull(group_col))
    )
  }
)

gc()

## group_sample group_output works----
testthat::test_that(
  "group_sample group_output works", {
    set.seed(131415)
    vec_coords <- 1:10

    df_data <-
      data.frame(
        "x" = vec_coords,
        "y" = vec_coords,
        "group_col" = group_numbers(1:10)
      )

    df_data_sampled <-
      df_data |>
      group_sample(group_col, n = 5, group_output = TRUE)

    testthat::expect_true(
      "grouped_df" %in% class(df_data_sampled)
    )
  }
)

gc()

## group_sample group_output works - grouping variable attr correct----
testthat::test_that(
  "group_sample group_output works - group variable properly attributed", {
    set.seed(161718)
    vec_coords <- 1:10

    df_data <-
      data.frame(
        "x" = vec_coords,
        "y" = vec_coords,
        "group_col" = group_numbers(1:10)
      )

    df_data_sampled <-
      df_data |>
      group_sample(group_col, n = 5, group_output = TRUE)

    group_col_ref <-
      df_data_sampled |>
      dplyr::pull(group_col)

    testthat::expect_equal(
      group_col_ref, attr(df_data_sampled, "groups") |> dplyr::pull(group_col)
    )
  }
)

gc()


