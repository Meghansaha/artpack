# =============================================================================#
# group_slice() Testing -------------------------------------------------------
# =============================================================================#

# NOTE FOR NEXT TIME: Need Tests for unsliced dataframes and row_id removal
# =============================================================================#
# Testing Inputs----------------------------------------------------------------
# =============================================================================#
## data is non-missing cli output----
cli::test_that_cli(
  "Missing data not accepted",
  {
    testthat::expect_snapshot(
      {
        group_slice()
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

        vec_data |> group_slice()
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

        df_data |> group_slice()
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

        df_data |> group_slice()
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

        df_data |> group_slice(group = c(group, x), n = 5)
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

        df_data |> group_slice(group = group, n = 1:3)
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

        df_data |> group_slice(group = group, n = "five")
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

        df_data |> group_slice(group = group, n = -3)
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

        df_data |> group_slice(group = group, n = .3)
      },
      error = TRUE
    )
  },
  configs = "ansi"
)

## n is less than or equal to amount of groups in df (by head)----
cli::test_that_cli(
  "n must less than or equal to group n",
  {
    testthat::expect_snapshot(
      {
        df_data <- data.frame("x" = 1:5, "y" = 6:10, group = 1:5)

        df_data |> group_slice(group = group, n = 10)
      }
    )
  },
  configs = "ansi"
)

## n is less than or equal to amount of groups in df (by tail)----
cli::test_that_cli(
  "n must less than or equal to group n",
  {
    testthat::expect_snapshot(
      {
        df_data <- data.frame("x" = 1:5, "y" = 6:10, group = 1:5)

        df_data |> group_slice(group = group, n = 10, position = "tail")
      }
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

        df_data |> group_slice(group = group, prop = "20%")
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

        df_data |> group_slice(group = group, prop = 1:3)
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

        df_data |> group_slice(group = group, prop = 1.5)
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
        df_data |> group_slice(group = group, prop = 1, group_output = "FALSE")
      },
      error = TRUE
    )
  },
  configs = "ansi"
)

# =============================================================================#
# Testing Outputs---------------------------------------------------------------
# =============================================================================#
## group_slice works (by head)----
testthat::test_that(
  "group_slice works", {
    vec_coords <- 1:10

    df_data <-
      data.frame(
        "x" = vec_coords,
        "y" = vec_coords,
        "group_col" = group_numbers(1:5) |> rep(each = 2)
      )

    df_data_sampled <-
      df_data |>
      group_slice(group_col)

    testthat::expect_true("group_col" %in% names(df_data_sampled))
  }

)

## group_slice works (by tail)----
testthat::test_that(
  "group_slice works", {
    vec_coords <- 1:10

    df_data <-
      data.frame(
        "x" = vec_coords,
        "y" = vec_coords,
        "group_col" = group_numbers(1:5) |> rep(each = 2)
      )

    df_data_sampled <-
      df_data |>
      group_slice(group_col, position = "tail")

    testthat::expect_true("group_col" %in% names(df_data_sampled))
  }

)

## group_slice n works----
testthat::test_that(
  "group_slice n works", {
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
      group_slice(group_col, n = n_groups_to_sample)

    testthat::expect_equal(
      df_data_sampled |> nrow(),
      n_groups_to_sample
    )
  }
)

## group_slice prop works----
testthat::test_that(
  "group_slice prop works", {
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
      group_slice(group_col, prop = prop_groups_to_sample)

    testthat::expect_equal(
      df_data_sampled |> nrow(),
      n_expected_sampled_rows
    )
  }
)


## group_slice group_output works----
testthat::test_that(
  "group_slice group_output works", {
    vec_coords <- 1:10

    df_data <-
      data.frame(
        "x" = vec_coords,
        "y" = vec_coords,
        "group_col" = group_numbers(1:10)
      )

    df_data_sampled <-
      df_data |>
      group_slice(group_col, n = 5, group_output = TRUE)

    testthat::expect_true(
      "grouped_df" %in% class(df_data_sampled)
    )
  }
)

## group_slice group_output works - grouping variable attr correct----
testthat::test_that(
  "group_slice group_output works - group variable properly attributed", {
    vec_coords <- 1:10

    df_data <-
      data.frame(
        "x" = vec_coords,
        "y" = vec_coords,
        "group_col" = group_numbers(1:10)
      )

    df_data_sampled <-
      df_data |>
      group_slice(group_col, n = 5, group_output = TRUE)

    group_col_ref <-
      df_data_sampled |>
      dplyr::pull(group_col)

    testthat::expect_equal(
      group_col_ref, attr(df_data_sampled, "groups") |> dplyr::pull(group_col)
    )
  }
)

## group_slice (by head) has the same columns as the input----
testthat::test_that(
  "group_slice works", {
    vec_coords <- 1:10

    df_data <-
      data.frame(
        "x" = vec_coords,
        "y" = vec_coords,
        "group_col" = group_numbers(1:5) |> rep(each = 2)
      )

    vec_df_data_names <-
      df_data |>
      names()

    df_data_sampled <-
      df_data |>
      group_slice(group_col)

    vec_df_data_sampled_names <-
      df_data_sampled |>
      names()

    testthat::expect_equal(vec_df_data_sampled_names, vec_df_data_names)
  }

)

