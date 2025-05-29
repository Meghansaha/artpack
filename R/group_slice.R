#' Subset Data Frames by a Group Variable Using Their Positions
#'
#' @param data A data frame or tibble with at least 1 variable.
#' @param group A variable in `data` that will be used for groupings.
#' @param n,prop Supply either `n`, the number of groups, or `prop`, the proportion of groups to select. `n` must be a positive integer that is greater than or equal to 1. `prop` must be a positive numeric value that is greater than 0 and less than or equal to 1.
#'
#' Default is `n` = 1.
#'
#' @param position A character string of `"head"` or `"tail"`. Determines if the first group or last group in the data frame is selected where `"head"` will select the first group in the dataframe and `"tail"` will select the last group.
#' @param group_output A logical boolean `TRUE` or `FALSE`. If `TRUE`, returns a grouped tibble.
#'
#' Default is `FALSE`
#'
#' @returns A sliced dataframe
#' @export
#'
#' @examples
#' vec_coords <- 1:10
#' df_data <-
#'  data.frame(
#'    "x" = vec_coords,
#'    "y" = vec_coords,
#'    "group_col" = group_numbers(1:5) |> rep(each = 2)
#'  )
#'
#' df_sliced_data_head <-
#'  df_data |>
#'  group_slice(group_col, n = 2, position = "head")
#'
#' df_sliced_data_head
#'
#' df_sliced_data_tail <-
#'  df_data |>
#'  group_slice(group_col, n = 2, position = "tail")
#'
#' df_sliced_data_tail
#'
#' df_sliced_data_prop <-
#'  df_data |>
#'  group_slice(group_col, prop = .80)
#'
#' df_sliced_data_prop
#'
group_slice <- function(data, group, n = 1, prop = NULL, position = "head", group_output = FALSE){
  # ===========================================================================#
  # Logic Checks---------------------------------------------------------------
  # ===========================================================================#
  ## Check that Data is present----
  if (missing(data)) {
    c(
      "x" = paste("{.var data} is", error("missing")),
      "!" = paste("{.var data} should be a", status("dataframe"), "or", status("tibble"))
    ) |>
      cli::cli_abort()
  }

  ## check that Data is a dataframe----
  if (!is.data.frame(data)) {
    c(
      "x" = paste("{.var data} is", error("{.cls {typeof(data)}}")),
      "!" = paste("{.var data} should be a", status("dataframe"), "or", status("tibble"))
    ) |>
      cli::cli_abort()
  }

  ## check that Data has at least 1 variable----
  if (length(data) == 0) {
    c(
      "x" = paste("{.var data} has", error("0 variables"), "."),
      "!" = paste("{.var data} should be a dataframe or tibble with at least ", status("1 variable"))
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
      paste("{.var group} must be", callout("1"), "column from {.var data}"),
      "x" = paste("You've supplied", error(group_col_length), "{cli::qty(group_col_length)} column{?s} to the {.var group} argument"),
      "i" = paste(status("Check the {.var group} value"), "you've supplied.")
    ) |>
      cli::cli_abort()
  }

  # check that n is of length 1
  n_length_invalid <- length(n) != 1

  if(n_length_invalid){
    c(
      paste("{.var n} must be a length of", callout("1")),
      "x" = paste("The {.var n} object you've supplied has a length of", error(length(n))),
      "i" = paste(status("Check the {.var n} value"), "you've supplied.")
    ) |>
      cli::cli_abort()
  }

  # n variable is numeric
  n_type_check <- !n |> is.numeric()

  if(n_type_check){
    c(
      paste("{.var n} must be a", callout("numeric"), "integer of length 1."),
      "x" = paste("The class of the {.var n} object you've supplied is", error("<", class(n), ">")),
      "i" = paste(status("Check the {.var n} object"), "you've supplied.")
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
        paste("{.var n} must be a positive numeric integer that is", callout("greater than 0")),
        "x" = paste("The {.var n} you've supplied is", error(n)),
        "i" = paste(status("Check the {.var n} value"), "you've supplied.")
      ) |>
        cli::cli_abort()
    }

    # Check that n is a whole/integer number#
    if (n %% 1 != 0) {
      c(
        paste("{.var n} must be a", callout("numeric integer (no decimals)")),
        "x" = paste("The {.var n} object you've supplied is", error(n)),
        "i" = paste(status("Check the {.var n} value"), "you've supplied.")
      ) |>
        cli::cli_abort()
    }

  }

  # Create some row ids
  data$row_id <- 1:nrow(data)

  # Pulling group column name
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
        paste("{.var prop} must be a length of", callout("1")),
        "x" = paste("The {.var prop} object you've supplied has a length of", error(length(prop))),
        "i" = paste(status("Check the {.var prop} value"), "you've supplied.")
      ) |>
        cli::cli_abort()
    }

    # Check that provided prop is valid
    prop_invalid <- prop < 0 | prop > 1

    if(prop_invalid){
      c(
        paste("{.var prop} must be a positive numeric value that is", callout("less than or equal to 1")),
        "x" = paste("The {.var prop} you've supplied is", error(prop)),
        "i" = paste(status("Check the {.var prop} value"), "you've supplied.")
      ) |>
        cli::cli_abort()
    }

    # Convert prop to n
    n <- trunc(vec_groups_n * prop)
  }


  # Before slicing, check to see if the n is greater than the groups available
  # If so, don't slice it and just return the dataframe
  n_out_of_bounds <-  n > vec_groups_n

  if (n_out_of_bounds) {
    c(
      "i" = paste("{.var n} should be a positive numeric value that is less than or equal to the", callout("amount of groups in the {.var {group_name}} variable")),
      "i" = paste("The {.var group} variable you've supplied has", callout("{vec_groups_n}"), "groups but the {.var n} you've supplied is", error("{n}")),
      "!" = paste("The data could not be sliced")
    ) |>
      cli::cli_warn()

    if (position == "tail") {
      df_unsliced <- data[order(data[["row_id"]], decreasing = TRUE),]
      df_unsliced["row_id"] <- NULL

      return(df_unsliced)

    } else {

      data["row_id"] <- NULL
      df_unsliced <- data

      return(df_unsliced)
    }
  }

  # Slice the groups
  if(position == "head") {

    vec_groups_to_keep <- vec_groups[1:n]

  } else {

    vec_groups_to_keep <- vec_groups[vec_groups_n:(vec_groups_n - n + 1)]

  }


  # Slice the data
  df_sliced <-
    data |>
    dplyr::filter({{ group }} %in% vec_groups_to_keep)

  # If position is tail, reverse the data
  if (position == "tail") {
    df_sliced <- df_sliced [order(df_sliced [["row_id"]], decreasing = TRUE),]

    df_sliced["row_id"] <- NULL
  } else{
    # Otherwise, just remove row_id col
    df_sliced["row_id"] <- NULL
  }

  # Check that group_output is logical
  group_output_check <- !group_output |> is.logical()

  if(group_output_check) {
    c(
      paste("{.var group_output} must be a logical boolean of", callout("`TRUE` or `FALSE`")),
      "x" = paste("The class of the {.var group_output} object you've supplied is", error("<", class(group_output), ">")),
      "i" = paste(status("Check the {.var group_output} object"), "you've supplied.")
    ) |>
      cli::cli_abort()
  }

  # If group_output is TRUE, group_by the groups remaining
  if(group_output){
    df_sliced <-
      df_sliced |>
      dplyr::group_by({{ group }})
  }

  return(df_sliced)
}
