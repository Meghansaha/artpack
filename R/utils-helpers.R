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

is.color <- function(..., call_level = -1) {

  call_level_valid <- is.numeric(call_level)

  if(!call_level_valid){
    c(
      "x" = "`call_level` is invalid!",
      "!" = "check `class.check`"
    ) |>
      cli::cli_abort()
  }

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

  out <- all(r_color | hex_color)

  if(out){
    return(TRUE)
  }

  if(out == FALSE){
    var_name <- deparse(substitute(...))

      c(
        "x" = paste("{.var {var_name}} must contain", error("valid 6-digit hexadecimal colors"), "or valid color names found in"), error("{.run grDevices::colors()}"),
        "!" = paste("The input you've supplied, {.var {var_name}}, contains", callout('"{(...)}"')),
        "i" = paste(status("Check the ", "{.var {var_name}}"), "input.")
      ) |>
        cli::cli_abort(call = sys.call(call_level))
  }
}

#' Test If An Object has an Expected Class - Internal Function
#'
#' @param ... #An object to be tested to see if it is of the expected class
#' @param expected_class #A string of the expected class. Accepted values are `"numeric"`, `"character"`, `"data.frame"`, `"list"`, and `"logical"`
#' @param call_level #A numeric value setting the call level that's invoked when an error is thrown. This controls where in the function's environment the error is declared in the console messaging and is intended to be used for user-facing error messaging.
#' @param required Boolean. `TRUE` of `FALSE`. If the var class being checked is required but mismatched, an error will get thrown to the console for the user. If the var is not required, but mismatched, a value of `FALSE` will be returned for dev assistance. Default is `TRUE`.
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

class.check <- function(..., expected_class, call_level = -1, required = TRUE) {

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
      "logical" = is.logical(...),
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

  if(var_check){
    return(TRUE)
  }

  if(!var_check & required){

    var_class <- class(...)

    c(
      "x" = paste("{.var {var_name}} must be of class", error("<{expected_class}>")),
      "!" = paste("The input you've supplied, {.var {var_name}}, is of class", callout("<{var_class}>")),
      "i" = paste(status("Check the ", "{.var {var_name}}"), "input.")
    ) |>
      cli::cli_abort(call = sys.call(call_level))
  }

  if(!var_check & required == FALSE){
    return(FALSE)
  }
}

#' Test If An Object is `NULL` - Internal Function
#'
#' @param ... An object to be tested to see if it is `NULL`
#' @param call_level A numeric value setting the call level that's invoked when an error is thrown. This controls where in the function's environment the error is declared in the console messaging and is intended to be used for user-facing error messaging.
#' @param required Boolean. `TRUE` of `FALSE`. If the var being checked is required but missing, an error will get thrown to the console for the user. If the var is not required, but missing, a value of `FALSE` will be returned for dev assistance. Default is `TRUE`.
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

is.var.present <- function(..., call_level = -1, required = TRUE){

  call_level_valid <- is.numeric(call_level)

  if(!call_level_valid){
    c(
      "x" = "`call_level` is invalid!",
      "!" = "check `is.var.present`"
    ) |>
      cli::cli_abort()
  }

  var_missing <- is.null(...)

  if(var_missing & required){

    var_name <- deparse(substitute(...))

    c(
      "x" = paste("{.var {var_name}} must be provided", error("and not missing nor `NULL`")),
      "!" = paste("The input you've supplied, {.var {var_name}}, is", callout("missing or `NULL`")),
      "i" = paste(status("Check the ", "{.var {var_name}}"), "input.")
    ) |>
      cli::cli_abort(call = sys.call(call_level))
  }

  if(var_missing & required == FALSE){
    return(FALSE)
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
#' @param expected_op #A string value of the required logic operation to check the length against. Default is "==", Other options are "<=", ">=", "<", and ">".
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
  function(..., expected_length, expected_op = "==", call_level = -1) {

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

    flag_op <- expected_op %in% c("==", ">=", "<=", "<", ">") == FALSE

    if(flag_op){
      c(
        "x" = "`expected_op` is invalid! (needs to be ('==', '>=', '<=', '<', '>')",
        "!" = "check `length.check`"
      ) |>
        cli::cli_abort()
    }

    actual_length <- length(...)
    operation_string <- paste0(actual_length, expected_op, expected_length)
    operation_expr <- str2expression(operation_string)
    length_check <- eval(operation_expr)

    flag_length_check <- length_check == FALSE

    if(flag_length_check){

      operator_string <-
        switch(
          expected_op,
          "==" = "be of length",
          ">="  = "have a length greater than or equal to",
          "<=" = "have a length less than or equal to",
          ">" = "have a length greater than",
          "<" = "have a length less than"
        )

      c(
        "x" = paste("{.var {var_name}} must", error("{operator_string} {expected_length}")),
        "!" = paste("The input you've supplied, {.var {var_name}}, is of length", callout("{actual_length}")),
        "i" = paste(status("Check the ", "{.var {var_name}}"), "input.")
      ) |>
        cli::cli_abort(call = sys.call(call_level))
    } else{
      return(TRUE)
    }
  }

#' Test If A Value is as Expected - Internal Function
#'
#' @param ... #An object that will be checked.
#' @param expected_values #A vector of the expected values to check for.
#' @param call_level #A numeric value setting the call level that's invoked when an error is thrown. This controls where in the function's environment the error is declared in the console messaging and is intended to be used for user-facing error messaging.
#'
#' @return #A Logical Value
#' @noRd
#'
#' @examples
#'
#' objects with an expected value return `TRUE`
#'
#' an_object <- "test"
#'
#' is.expected.value(an_object, expected_value = c("test", "tester))
#'
#' objects that fail the check throw an error
#'
#' an_another_object <- "testy"
#'
#' is.expected.value(an_another_object, expected_value = c("test", "tester))
#'
#'
is.expected.value <-
  function(..., expected_values, call_level = -1){

    call_level_valid <- is.numeric(call_level)

    if(!call_level_valid){
      c(
        "x" = "`call_level` is invalid!",
        "!" = "check `length.check`"
      ) |>
        cli::cli_abort()
    }

    var_name <- deparse(substitute(...))

    value_check <- ... %in% expected_values

    flag_value_check <- value_check == FALSE

    if(flag_value_check){

      vars_text <-
        knitr::combine_words(
          expected_values,
          sep = ", ",
          and = " or ",
          before = '"',
          after = '"',
          oxford_comma = FALSE
        )

      c(
        "x" = paste("{.var {var_name}} must be an accepted value:", cli::style_italic(error("{vars_text}"))),
        "!" = paste("The input you've supplied, {.var {var_name}}, is", callout('"{(...)}"')),
        "i" = paste(status("Check the ", "{.var {var_name}}"), "input.")
      ) |>
        cli::cli_abort(call = sys.call(call_level))
    } else{
      return(TRUE)
    }
  }


#' Convert color (hex or R color) to HSL - Internal Function
#'
#' @param ... #An character string of an R color or 6-digit hexadecimal that will be converted to an hsl color
#'
#' @return #A numeric vector of length 3
#' @noRd
#'
col_to_hsl <- function(...){
  # First make sure the color is a valid hex or a grDevices color from colors()
  is.color(...)

  # Convert valid color into normalized RGB value----
  rgb_value <- grDevices::col2rgb(...) / 255 |> unlist()
  names(rgb_value) <- c("red", "green", "blue")

  # Grab min and max values to calc chroma and lightness----
  min_rgb <- rgb_value[which.min(rgb_value)]
  max_rgb <- rgb_value[which.max(rgb_value)]

  # Calc Chroma----
  chroma_value <- (max_rgb - min_rgb) |> unname()

  # Calc lightness----
  light_value <- (max_rgb + min_rgb) / 2

  # Calc the hue----
  # first determine which color was max
  max_color <- names(max_rgb)

  if(chroma_value == 0) {
    hue_value <- 0
  } else {
    hue_value_init <-
      switch(
        max_color,
        "red" = ((rgb_value["green"] - rgb_value["blue"]) / chroma_value) %% 6,
        "green" = (rgb_value["blue"] - rgb_value["red"]) / chroma_value + 2,
        "blue" = (rgb_value["red"] - rgb_value["green"]) / chroma_value + 4,
      ) * 60
    hue_value <- ifelse(hue_value_init < 0, hue_value_init + 360, hue_value_init)
  }

  # Calc the saturation
  if(chroma_value == 0){
    sat_value <- 0
  } else if(light_value > .5){
    sat_value <- (chroma_value / (2-2 * light_value))
  } else {
    sat_value <- chroma_value / (2 * light_value)
  }

  hsl_value <- c(hue_value, sat_value, light_value)
  names(hsl_value) <- c("hue", "sat", "light")

  return(hsl_value)
}

#' HSL to RGB conversion function - Internal Function
#'
#' @param ... #An vector of 3 numeric value that make up an HSL value that will be converted to a RGB color
#' @return #A numeric vector of length 3
#' @noRd
#'
hsl_to_rgb <- function(...){
  hsl_values <- c(...)
  hue_position <- hsl_values[1] / 360  # Convert to 0-1 range
  sat <- hsl_values[2]
  light <- hsl_values[3]

  no_sat <- sat == 0

  if (no_sat) {
    # If the color is gray, calculate rgb based on luminance
    r <- g <- b <- light
  } else {

    max_rgb_component <- if(light < 0.5){
      light * (1 + sat)
    } else{
      light + sat - light * sat
    }

    min_rgb_component <- 2 * light - max_rgb_component
    # Channel checks to determine temp. rgb colors
    r_init <- hue_position + 1/3

    if(r_init > 1){
      r_init <- r_init - 1
    }

    g_init <- hue_position


    b_init <- hue_position - 1/3


    if(b_init < 0){
      b_init <- b_init + 1
    }

    # Checks to determine which formula is used for each RGB value
    # Red Calculations
    calc_channel_value <- function(init_value){
      if(init_value < 1/6){
        final_value <- min_rgb_component + (max_rgb_component - min_rgb_component) * 6 * init_value
      } else if(init_value < 1/2){
        final_value <- max_rgb_component
      } else if(init_value < 2/3){
        final_value <- min_rgb_component + (max_rgb_component - min_rgb_component) * (2/3 - init_value) * 6
      } else{
        final_value <- min_rgb_component
      }
      return(round(final_value * 255))
    }

    r <- calc_channel_value(r_init)
    g <- calc_channel_value(g_init)
    b <- calc_channel_value(b_init)

  }

  rgb_vec <- c(r, g, b)
  names(rgb_vec) <- c("red", "green", "blue")

  return(rgb_vec)
}

#' rgb to hex conversion - Internal Function
#'
#' @param ... #A vector of 3 numeric value that make up an HSL value that will be converted to a RGB color
#' @return #A valid character value - 6 digit hexadecimal color
#' @noRd
#'
#'
rgb_to_hex <- function(...){
  rgb_values <- c(...)
  grDevices::rgb(rgb_values[1], rgb_values[2], rgb_values[3], maxColorValue = 255)
}

