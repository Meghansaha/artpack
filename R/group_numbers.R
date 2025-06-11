#' Convert Numbers into Padded Strings for Easier Group Numbering
#'
#' @param numbers A numeric vector with a length of at least 1.
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
group_numbers <- function(numbers, prefix = NULL, suffix = NULL, sep = "_") {
  # ===========================================================================#
  # Input Checks----------------------------------------------------------------
  # ===========================================================================#
  # initialize the affix toggle#
  affix_present <- FALSE
  ## numbers--------------------------------------------------------------------
  class.check(numbers, expected_class = "numeric")

  ## prefix---------------------------------------------------------------------
  prefix_present <- !is.null(prefix)

  if (prefix_present) {
    class.check(prefix, expected_class = "character")

    prefix <- paste0(prefix, sep)

    affix_present <- TRUE
  } else {
    prefix <- ""
  }

  ## suffix---------------------------------------------------------------------
  suffix_present <- !is.null(suffix)

  if (suffix_present) {
    class.check(suffix, expected_class = "character")

    suffix <- paste0(sep, suffix)

    affix_present <- TRUE
  } else {
    suffix <- ""
  }

  ## sep------------------------------------------------------------------------
  class.check(sep, expected_class = "character")

  # =========================================================================#
  # Numbering Conversion-----------------------------------------------------
  # =========================================================================#

  # Grab width of largest number#
  string_width <- stringr::str_length(max(numbers))

  # Sort the input
  sorted <- sort(c(numbers))

  # Convert to padded strings
  out_numbers <-
    purrr::map(
      sorted, ~ stringr::str_pad(
        .x,
        width = string_width,
        side = "left",
        pad = "0"
      )
    ) |>
    purrr::list_c()

  # modify the string if an affix is present
  if (affix_present){
    out_numbers_affixed <- paste0(prefix, out_numbers, suffix)

    return(out_numbers_affixed)

  } else{

    return(out_numbers)
  }
}
