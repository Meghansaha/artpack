#' Test If A String Is A Valid `R` Color or Hexadecimal Color Code - Internal Function
#'
#' @param ... #A character string to be tested to see if it is a base R color or a hexadecimal color code.
#'
#' @return #A Logical Value
#' @noRd
#'
#' @examples
#' # Valid colors include Base R colors from `colors()` and valid hexadecimal webcolors
#' # Valid colors return `TRUE`
#' is.color("black")
#' is.color("#ffffff")
#'
#' # Invalid colors return `FALSE`
#' is.color("#000") # 6 digits only, no shorthand allowed
#' is.color("blu")
#'
#' # Multiple values can be handled as well
#' is.color(c("#000", "er4", "#ffffff"))
is.color <- function(...) {
  # is the input a base R color?
  r_color <-
    sapply(
      ...,
      function(x) x %in% colors()
    )

  # is the input a valid hex color code?
  hex_color <-
    sapply(
      ...,
      function(x) grepl("^#[A-Fa-f0-9]{6}", tolower(x))
    )

  out <- r_color + hex_color

  output <- ifelse(out > 0, TRUE, FALSE)

  return(output)
}
