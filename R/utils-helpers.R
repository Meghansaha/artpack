#' Internal Helper Functions
#'
#' Test If A String Is A Valid `R` Color or Hexadecimal Color Code - Internal Function
#'
#' @param ... #A character string to be tested to see if it is a valid base R color or a hexadecimal color code.
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

#' Test If An Object has an Expected Class - Internal Function
#'
#' @param ... #An object to be tested to see if it is of the expected class
#' @param expected_class #A string of the expected class. Accepted values are `"numeric"`, `"character"`, `"data.frame"`, and `"list"`
#' @param call_level #A numeric value setting the call level that's invoked when an error is thrown. This controls where in the function's environment the error is declared in the console messaging and is intended to be used for user-facing error messaging.
#'
#' @return #A Logical Value
#' @noRd
#'
#' @examples
#'
#' Valid class checks return TRUE
#'
#' a_word <- "word"
#'
#' class.check(a_word, "character")
#'
#' Invalid class checks throw an error
#'
#' some_numbers <- 1:10
#' class.check(some_numbers, "list")
#'

class.check <- function(..., expected_class, call_level = -1) {

  call_level_valid <- is.numeric(call_level)

  if(!call_level_valid){
    c(
      "x" = "`call_level` is invalid!",
      "!" = "check `class.check`"
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
      "!" = "check `class.check`"
    ) |>
      cli::cli_abort()
  }

  if(!var_check){

    var_class <- class(...)

    c(
      "x" = paste("{.var {var_name}} must be of class", error("<{expected_class}>")),
      "!" = paste("The input you've supplied, {.var {var_name}}, is of class", callout("<{var_class}>")),
      "i" = paste(status("Check the ", "{.var {var_name}}"), "input.")
    ) |>
      cli::cli_abort(call = sys.call(call_level))
  } else{
    return(TRUE)
  }
}

#' Test If An Object is `NULL` - Internal Function
#'
#' @param ... #An object to be tested to see if it is `NULL`
#' @param call_level #A numeric value setting the call level that's invoked when an error is thrown. This controls where in the function's environment the error is declared in the console messaging and is intended to be used for user-facing error messaging.
#'
#' @return #A Logical Value
#' @noRd
#'
#' @examples
#'
#' Test If An object is `NULL` - Internal Function
#'
#' Non-`NULL` objects return `TRUE`
#'
#' an_object <- "Not `NULL`"
#'
#' is.var.present(an_object)
#'
#' `NULL` objects throw an error
#'
#' null_object <- NULL
#' is.var.present(null_object)

is.var.present <- function(..., call_level = -1){

  call_level_valid <- is.numeric(call_level)

  if(!call_level_valid){
    c(
      "x" = "`call_level` is invalid!",
      "!" = "check `is.var.present`"
    ) |>
      cli::cli_abort()
  }

  var_missing <- is.null(...)

  if(var_missing){

    var_name <- deparse(substitute(...))

    c(
      "x" = paste("{.var {var_name}} must be provided", error("and not missing nor `NULL`")),
      "!" = paste("The input you've supplied, {.var {var_name}}, is", callout("missing or `NULL`")),
      "i" = paste(status("Check the ", "{.var {var_name}}"), "input.")
    ) |>
      cli::cli_abort(call = sys.call(call_level))
  }

  return(TRUE)
}

#' Test If a numeric value is as expected - Internal Function
#'
#' @param ... #A numeric value to be tested to see if it is as expected.
#' @param expected_type #A string or string vector of numeric "types" to check for. Options include `"positive"`, `"negative"`, `"integer"`.
#' @param call_level #A numeric value setting the call level that's invoked when an error is thrown. This controls where in the function's environment the error is declared in the console messaging and is intended to be used for user-facing error messaging.
#'
#' @return #A Logical Value
#' @noRd
#'
#' @examples
#'
#' Test If A Numeric Value is as Expected - Internal Function
#'
#' Numeric values that satisfy the check will return `TRUE`
#'
#' is.expected.numeric.type(8, expected_type = "integer")
#'
#' Numeric values that fail the check will return an error
#'
#' length_num <- -9
#' is.expected.numeric.type(length_num, expected_type = "positive")
#'
is.expected.numeric.type <- function(..., expected_type, call_level = -1){

  var_name <- deparse(substitute(...))

  call_level_valid <- is.numeric(call_level)

  if(!call_level_valid){
    c(
      "x" = "`call_level` is invalid!",
      "!" = "check is.var.present"
    ) |>
      cli::cli_abort()
  }
  # add an "all check to this"
  vec_expected_types <-
    c(
      "positive",
      "integer"
    )

  invalid_type_check <- !expected_type %in% vec_expected_types |> all()
  invalid_types <- setdiff(expected_type, vec_expected_types)
  invalid_types_string <- setdiff(expected_type, vec_expected_types) |> knitr::combine_words(before = "\"")

  if(invalid_type_check){
    c(
      "x" = "`expected_type` is invalid!",
      "i" = "{invalid_types_string} {cli::qty(invalid_types)} {?is an/are} invalid `expected_type` value{?s}",
      "!" = "check is.expected.numeric.type"
    ) |>
      cli::cli_abort()
  }

  # Create flags for each check
  pos_check <- "positive" %in% expected_type
  integer_check <- "integer" %in% expected_type

  if(pos_check){
    check <- ... > 0

    if(!check){
      c(
        "x" = paste("{.var {var_name}} must be a positive numeric value", error("not negative nor zero")),
        "!" = paste("The input you've supplied, {.var {var_name}}, is", callout("{(...)}")),
        "i" = paste(status("Check the ", "{.var {var_name}}"), "input.")
      ) |>
        cli::cli_abort(call = sys.call(call_level))
    }
  }

  if(integer_check){
    check <- abs(... - round(...)) < .Machine$double.eps^0.5

    if(!check){
      c(
        "x" = paste("{.var {var_name}} must be a numeric integer;", error("not a float (number with decimals)")),
        "!" = paste("The input you've supplied, {.var {var_name}}, is", callout("{(...)}")),
        "i" = paste(status("Check the ", "{.var {var_name}}"), "input.")
      ) |>
        cli::cli_abort(call = sys.call(call_level))
    }
  }

  return(TRUE)
}

#' Test If An Object has An Expected Length - Internal Function
#'
#' @param ... #An object that will have it's length checked.
#' @param expected_length #A numeric value of the expected object length to check for.
#' @param call_level #A numeric value setting the call level that's invoked when an error is thrown. This controls where in the function's environment the error is declared in the console messaging and is intended to be used for user-facing error messaging.
#'
#' @return #A Logical Value
#' @noRd
#'
#' @examples
#'
#' objects with the expected length return `TRUE`
#'
#' an_object <- 1:10
#'
#' length.check(an_object, expected_length = 10)
#'
#' objects that fail the check throw an error
#'
#' an_another_object <- 1:5
#'
#' length.check(an_another_object, expected_length = 5)
#'

length.check <-
  function(..., expected_length, call_level = -1) {

    call_level_valid <- is.numeric(call_level)

    if(!call_level_valid){
      c(
        "x" = "`call_level` is invalid!",
        "!" = "check `length.check`"
      ) |>
        cli::cli_abort()
    }

    var_name <- deparse(substitute(...))

    var_check <- is.numeric(expected_length)

    flag_expected_length <- var_check == FALSE

    if(flag_expected_length){
      c(
        "x" = "`expected_length` is invalid! (needs to be numeric)",
        "!" = "check `length.check`"
      ) |>
        cli::cli_abort()
    }

    length_check <- length(...) == expected_length

    flag_length_check <- length_check == FALSE

    if(flag_length_check){

      var_length <- length(...)

      c(
        "x" = paste("{.var {var_name}} must be of length", error("{expected_length}")),
        "!" = paste("The input you've supplied, {.var {var_name}}, is of length", callout("{var_length}")),
        "i" = paste(status("Check the ", "{.var {var_name}}"), "input.")
      ) |>
        cli::cli_abort(call = sys.call(call_level))
    } else{
      return(TRUE)
    }
  }


