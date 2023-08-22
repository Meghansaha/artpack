#' Convert Numbers into Padded Strings for Easier Group Numbering
#'
#' @param numbers #A numeric vector with a length of at least 1.
#'
#' @return A Character Vector
#'
#' @importFrom stringr str_pad
#' @importFrom stringr str_length
#'
#' @export
#'
#' @examples
#' # Useful for easier group numbering so groups are ordered as intended
#' # Expects a numeric vector of numbers to convert to padded numbers
#' regular_numbers <- 1:19
#' padded_numbers <- group_numbers(regular_numbers)
#'
#' # The padding matters when creating labels for groupings
#' # as numbers will be converted to characters if attached to strings.
#' # Sorts as expected:
#' sort(regular_numbers)
#'
#' # Does not as a character:
#' sort(paste0("group_", regular_numbers))
#'
#' # Will sort as expected when padded:
#' sort(paste0("group_", padded_numbers))
group_numbers <- function(numbers) {
  # ===========================================================================#
  # Input Check----------------------------------------------------------------
  # ===========================================================================#

  if (!is.numeric(numbers)) {
    c(
      paste("{.var numbers} must be of class", callout("<numeric>")),
      "x" = paste("The input you've supplied: {.var {numbers}} is of class", error("{.cls {class(numbers)}}")),
      "i" = "Check the {.var numbers} input."
    ) |>
      cli::cli_abort()
  }

  # =========================================================================#
  # Numbering Conversion-----------------------------------------------------
  # =========================================================================#

  # Grab width of largest number#
  string_width <- stringr::str_length(max(numbers))

  # Sort the input
  sorted <- sort(c(numbers))

  # Convert to padded strings
  out_numbers <-
    purrr::map(sorted, ~ stringr::str_pad(.x,
      width = string_width,
      side = "left",
      pad = "0"
    )) |>
    purrr::list_c()

  return(out_numbers)
}
