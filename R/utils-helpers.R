#' Internal Helper Functions
#'
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

class.check <- function(..., expected_class, call_level = -1) {

  call_level_valid <- is.numeric(call_level)

  if(!call_level_valid){
    c(
      "x" = "`call_level` is invalid!",
      "!" = "check class.check"
    ) |>
      cli::cli_abort()
  }

  var_name <- deparse(substitute(...))

  var_check <-
    switch(
      expected_class,
      "numeric" = is.numeric(...),
      "character" = is.character(...),
      "data.frame" = is.data.frame(...),
      "list" = is.list(...),
      NA
    )

  var_check_valid <- !is.na(var_check)

  if(!var_check_valid){
    c(
      "x" = "`expected_class` is invalid!",
      "!" = "check class.check"
    ) |>
      cli::cli_abort()
  } else if(!var_check){

    var_class <- class(...)

    c(
      paste("{.var {var_name}} must be of class", callout("<{expected_class}>")),
      "x" = paste("The input you've supplied, {.var {var_name}}, is of class", error("{.cls {var_class}}")),
      "i" = "Check the {.var {var_name}} input."
    ) |>
      cli::cli_abort(call = sys.call(call_level))

  }
}


