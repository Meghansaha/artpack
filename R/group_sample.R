#' Sample Data Frames by a Group Variable
#'
#' @param data A data frame or tibble with at least 1 variable.
#' @param group A variable in `data` that will be used for groupings.
#' @param n,prop Supply either `n`, the number of groups, or `prop`, the proportion of groups to select. `n` must be a positive integer that is greater than or equal to 1. `prop` must be a positive numeric value that is greater than 0 and less than or equal to 1.
#'
#' Default is `n` = 1.
#'
#' @param prob Optional. A vector of probability weights for obtaining the elements of the group being sampled. Must be the same length as the total unique values in `data`'s `group` variable.
#' @param group_output A logical boolean `TRUE` or `FALSE`. If `TRUE`, returns a grouped tibble.
#'
#' Default is `FALSE`.
#'
#' @returns A sampled dataframe
#' @export
#'
#' @examples
#' vec_coords <- 1:10
#'df_data <-
#'  data.frame(
#'    "x" = vec_coords,
#'    "y" = vec_coords,
#'    "group_col" = group_numbers(1:5) |> rep(each = 2)
#'  )
#'
#'df_sampled_data_prop <-
#'  df_data |>
#'  group_sample(group_col, prop = .2)
#'
#'df_sampled_data_prop
#'
#'df_sampled_data_n <-
#'  df_data |>
#'  group_sample(group_col, n = 2)
#'
#'df_sampled_data_n
#'
group_sample <- function(data, group, n = 1, prop = NULL, prob = NULL, group_output = FALSE){
  # ===========================================================================#
  # Logic Checks---------------------------------------------------------------
  # ===========================================================================#
  # Check that Data is present
  if (missing(data)) {
    c(
      "x" = paste("{.var data} is", error("missing")),
      "!" = paste("{.var data} should be a", status("dataframe"), "or", status("tibble"))
    ) |>
      cli::cli_abort()
  }
  # check that Data is a dataframe
  if (!is.data.frame(data)) {
    c(
      "x" = paste("{.var data} is", error("{.cls {typeof(data)}}")),
      "!" = paste("{.var data} should be a", status("dataframe"), "or", status("tibble"))
    ) |>
      cli::cli_abort()
  }

  # check that group variable is present
  missing_group_check <- missing(group)

  if (missing_group_check) {
    c(
      "x" = paste("{.var group} variable is",error("missing:")),
      "!" = paste("{.var group} variable should be present in a ", status("dataframe"), "or", status("tibble"))
    ) |>
      cli::cli_abort()
  }

  # check that group variable is of length 1
  group_col_length <- dplyr::select(data, {{ group }}) |> names() |> length()
  group_length_invalid <- group_col_length > 1

  if(group_length_invalid){
    c(
      paste0("{.var group} must be ", callout("1"), " column from {.var data}"),
      "x" = paste0("You've supplied ", error(group_col_length), "{cli::qty(group_col_length)} column{?s} to the {.var group} argument"),
      "i" = paste0(status("Check the {.var group} value"), " you've supplied.")
    ) |>
      cli::cli_abort()
  }

  # check that n is of length 1
  n_length_invalid <- length(n) != 1

  if(n_length_invalid){
    c(
      paste0("{.var n} must be a length of ", callout("1")),
      "x" = paste0("The {.var n} object you've supplied has a length of ", error(length(n))),
      "i" = paste0(status("Check the {.var n} value"), " you've supplied.")
    ) |>
      cli::cli_abort()
  }

  # n variable is numeric
  n_type_check <- !n |> is.numeric()

  if(n_type_check){
    c(
      paste0("{.var n} must be a", callout("numeric"), "integer of length 1."),
      "x" = paste0("The class of the {.var n} object you've supplied is ", error("<", class(n), ">")),
      "i" = paste0(status("Check the {.var n} object"), " you've supplied.")
    ) |>
      cli::cli_abort()
  }

  # Check if n has changed
  n_changed <- n != 1

  if(n_changed){
    # Check that provided n is valid
    n_invalid <- n < 0

    if(n_invalid){
      c(
        paste0("{.var n} must be a positive numeric integer that is", callout("greater than 0")),
        "x" = paste0("The {.var n} you've supplied is ", error(n)),
        "i" = paste0(status("Check the {.var n} value"), " you've supplied.")
      ) |>
        cli::cli_abort()
    }

    # Check that n is a whole/integer number#
    if (n %% 1 != 0) {
      c(
        paste0("{.var n} must be a ", callout("numeric integer (no decimals)")),
        "x" = paste0("The {.var n} object you've supplied is ", error(n)),
        "i" = paste0(status("Check the {.var n} value"), " you've supplied.")
      ) |>
        cli::cli_abort()
    }

  }

  # Pulling group column names
  group_name <-
    data |>
    dplyr::select({{ group }}) |>
    names()

  # Pulling group for manipulation work
  vec_groups <-
    data |>
    dplyr::select({{ group }}) |>
    dplyr::pull() |>
    unique()

  # Pulling the group total
  vec_groups_n <- vec_groups |> length()

  # Check if prop provided
  prop_present <-
    !is.null(prop)

  if(prop_present){
    # check that prop is of length 1
    prop_length_invalid <- length(prop) != 1

    if(prop_length_invalid){
      c(
        paste0("{.var prop} must be a length of ", callout("1")),
        "x" = paste0("The {.var prop} object you've supplied has a length of ", error(length(prop))),
        "i" = paste0(status("Check the {.var prop} value"), " you've supplied.")
      ) |>
        cli::cli_abort()
    }

    # Check that provided prop is valid
    prop_invalid <- prop < 0 | prop > 1

    if(prop_invalid){
      c(
        paste0("{.var prop} must be a positive numeric value that is ", callout("less than or equal to 1")),
        "x" = paste0("The {.var prop} you've supplied is ", error(prop)),
        "i" = paste0(status("Check the {.var prop} value"), " you've supplied.")
      ) |>
        cli::cli_abort()
    }

    # Convert prop to n
    n <- trunc(vec_groups_n * prop)
  }

  # Check if prob is provided
  prob_present <-
    !is.null(prob)

  if(prob_present){
    # Check that provided prob is the same length as unique group values
    prob_length_invalid <- length(prob) != vec_groups_n

    if(prob_length_invalid){
      c(
        paste0("{.var prob} must be a vector of positive numeric values", callout("with the same length as the total unique values in `group`")),
        "x" = paste0("The length of {.var prob} you've supplied is ", error(length(prob)), " and the total unique values in `group` are", error(vec_groups_n)),
        "i" = paste0(status("Check the {.var prob} object"), " you've supplied.")
      ) |>
        cli::cli_abort()
    }

    # Check that provided prob has valid values
    prob_type_invalid <- !prob |> is.numeric() |> any()
    prob_values_invalid <- any(prob < 0)
    prob_any_invalid <- prob_type_invalid  | prob_values_invalid

    if(prob_any_invalid){
      c(
        paste0("{.var prob} must be a vector of ", callout("positive numeric values that are greater than zero")),
        "x" = if(prob_type_invalid){paste0("The class of the {.var prob} object that you've supplied is of ", error("<",class(prob),">"),". ", callout("Numeric")," is expected.")},
        "x" = if(prob_values_invalid){paste0(error("Negative"), " values were detected in the `prob` object you've supplied. Only", callout(" positive"), " values are accepted.")},
        "i" = paste0(status("Check the {.var prob} object"), " you've supplied.")
      ) |>
        cli::cli_abort()
    }
  }

  # Sample the groups
  vec_groups_to_keep <- sample(vec_groups, size = n, prob = prob)

  # Sample the data
  df_sampled <-
    data |>
    dplyr::filter({{ group }} %in% vec_groups_to_keep)

  # Check that group_output is logical
  group_output_check <- !group_output |> is.logical()

  if(group_output_check) {
    c(
      paste0("{.var group_output} must be a logicl boolean of ", callout("`TRUE` or `FALSE`")),
      "x" = paste0("The class of the {.var group_output} object you've supplied is ", error("<", class(group_output), ">")),
      "i" = paste0(status("Check the {.var group_output} object"), " you've supplied.")
    ) |>
      cli::cli_abort()
  }

  # If group_output is TRUE, group_by the groups remaining
  if(group_output){
    df_sampled <-
      data |>
      dplyr::filter({{ group }} %in% vec_groups_to_keep) |>
      dplyr::group_by({{ group }})
  }

  return(df_sampled)
}
