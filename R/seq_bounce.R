#' Bouncing Sequence Generation
#' @description
#'
#' Generate a regular sequence that 'bounces' between the provided `start_n` and `end_n` values in increments by the `by` value for the length of the `length` value provided.
#'
#' @param start_n The lower (min) numeric bound of the sequence to be generated. Must be `< end_n`.
#' @param end_n The upper (max) numeric bound of the sequence to be generated. Must be `> start_n`.
#' @param length The desired length of the generated sequence.
#' @param by The number increment of the sequence.
#'
#' @returns A numeric vector
#'
#' @importFrom utils head tail
#' @export
#'
#' @examples
#'
#' #By default, seq_bounce creates sequences by increments of 1
#' #The length argument accepts any positive integer
#' seq_bounce(start_n = 1, end_n = 5, length = 15)
#'
#' #The by argument accepts any positive numeric
#' seq_bounce(start_n = 0, end_n = 10, length = 30, by = .247)
#'
#' #The end_n value must be greater than the start_n value
#' #This will give you an error
#' try(seq_bounce(start_n = 0, end_n = -10, length = 15))
#'
#' #Instead, reverse the values
#' seq_bounce(start_n = -10, end_n = 0, length = 15)
#'
seq_bounce <-
  function(start_n = NULL, end_n = NULL, length = NULL, by = 1){
    #==========================================================================#
    # Input Checks--------------------------------------------------------------
    #==========================================================================#
    ## Check that all inputs are provided---------------------------------------
    # Throw an error if any are missing, otherwise continue#
    ### start_n-----------------------------------------------------------------
    is.var.present(start_n)
    ### end_n-------------------------------------------------------------------
    is.var.present(end_n)
    ### length------------------------------------------------------------------
    is.var.present(length)
    ### by----------------------------------------------------------------------
    is.var.present(by)

    ## Check that inputs are of length 1----------------------------------------
    # Throw an error if any are more than 1, otherwise continue#
    ### start_n-----------------------------------------------------------------
    length.check(start_n, expected_length = 1)
    ### end_n-------------------------------------------------------------------
    length.check(end_n, expected_length = 1)
    ### length------------------------------------------------------------------
    length.check(length, expected_length = 1)
    ### by----------------------------------------------------------------------
    length.check(by, expected_length = 1)

    ## Check that all inputs are numeric----------------------------------------
    # Throw an error if any are not numeric, otherwise continue#
    ### start_n-----------------------------------------------------------------
    class.check(start_n, expected_class = "numeric")
    ### end_n-------------------------------------------------------------------
    class.check(end_n, expected_class = "numeric")
    ### length------------------------------------------------------------------
    class.check(length, expected_class = "numeric")
    ### by----------------------------------------------------------------------
    class.check(by, expected_class = "numeric")

    ## Check that all applicable inputs are numeric positive/integer------------
    # Throw an error if not#
    ### length------------------------------------------------------------------
    is.expected.numeric.type(length, expected_type = c("positive", "integer"))
    ### by----------------------------------------------------------------------
    is.expected.numeric.type(by, expected_type = "positive")

    ## Check that start_n is smaller than end_n---------------------------------
    # Throw an error if not#
    flag_n_order <- start_n >= end_n

    if(flag_n_order){
      c(
        "x" = paste("{.var start_n} must be less than {.var end_n}", error("not more than or equal to {.var end_n}")),
        "!" = paste("You've supplied:\n", callout("{.var start_n} == {start_n} and {.var end_n} == {end_n}")),
        "i" = paste(status("Check the ", "{.var start_n} and {.var end_n}"), "inputs.")
      ) |>
        cli::cli_abort()
    }

    #==========================================================================#
    # Sequence Generation-------------------------------------------------------
    #==========================================================================#
    # The rise: From start_n to end_n - 1#
    vec_rise <- seq(start_n, end_n, by = by) |> utils::tail(-1)
    # The fall: From end_n to start_n + 1#
    vec_fall <- vec_rise |> utils::head(-1) |> rev()
    # Repeat start_n, rise, fall for length#
    final_seq <- rep_len(c(start_n, vec_rise, vec_fall), length)
    # Return final sequence#
    return(final_seq)
  }
