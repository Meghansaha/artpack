#' Cli Error Handling Helpers
#'
#' @param ... Expected object to be passed through for prettier error handling.
#'
#' @return A formatted string for use in cli calls
#'
#' @noRd
#'
#' @examples
#' error("This is a red error message.")
#' callout("This is a orange callout message.")
#' status("This is a blue status message.")
#'
#' @importFrom cli combine_ansi_styles

# For pretty red error messaging
error <- function(...){
  cli::combine_ansi_styles("red", "bold")(...)
}

# For pretty orange callout messaging
callout <- function(...){
  cli::combine_ansi_styles("darkorange", "bold")(...)
}

# For pretty blue status messaging
status <- function(...){
  cli::combine_ansi_styles("#0493ba", "bold")(...)
}
