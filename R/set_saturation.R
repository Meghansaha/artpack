#' Change the saturation of a hexadecimal color value
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param color Character value of length 1 - The color that will have its saturation set. A standard 6 digit hexadecimal webcolor like "#000000" or a valid `R` color from `colors()` is accepted.
#' @param percentage A numeric value of length 1. The percentage of which the saturation should be set. Values from 0 - 1 are accepted.
#'
#' @returns A character string (hexadecimal color)
#' @export
#'
#' @examplesIf rlang::is_installed("ggplot2")
#' @examples
#'
#' # Load in ggplot2 so we can see the colors
#' library(ggplot2)
#'
#' # Create color values
#' original_color <- "#7340bf" #(original saturation == .5)
#' desaturated_color <- set_saturation(original_color, .2) #(saturation == %20)
#' saturated_color <- set_saturation(original_color, .9) #(saturation == %90)
#'
#' # Make a data frame with the color values
#' df_colors <-
#'   data.frame(
#'     x = 0:2,
#'     y = 1,
#'     color = c(desaturated_color, original_color, saturated_color)
#'   )
#'
#' # Add a label for clarity
#' df_colors$label <- paste(c("Desaturated", "Original", "Saturated"), ":", df_colors$color)
#'
#' # Plot to see the saturation changes
#' df_colors |>
#'   ggplot(aes(x,y)) +
#'   geom_label(aes(x = 0:2), y = 2, label = df_colors$label) +
#'   geom_point(color = df_colors$color, shape = 15, size = 50) +
#'   coord_cartesian(xlim = c(-1,3), ylim = c(0,3)) +
#'   theme_void()
#'
#'
set_saturation <- function(color = NULL, percentage = NULL){
  #==========================================================================#
  # Input Checks--------------------------------------------------------------
  #==========================================================================#
  ## Check that all inputs are provided---------------------------------------
  # Throw an error if any are missing, otherwise continue#
  ### color-------------------------------------------------------------------
  is.var.present(color)
  ### percentage--------------------------------------------------------------
  is.var.present(percentage)

  ## Check that all inputs are the correct class/type------------------------
  # Throw an error if the wrong class or numeric type is detected, otherwise continue#
  ### color------------------------------------------------------------------
  class.check(color, expected_class = "character")
  is.color(color)
  ### percentage-------------------------------------------------------------
  class.check(percentage, expected_class = "numeric")

  ## Check that inputs are of length 1----------------------------------------
  # Throw an error if any are not, otherwise continue#
  ### color-------------------------------------------------------------------
  length.check(color, expected_length = 1)
  ### percentage--------------------------------------------------------------
  length.check(percentage, expected_length = 1)

  ## Check that all applicable inputs are the correct numeric type--------------
  ### percentage----
  is.expected.numeric.type(percentage, expected_type = "positive")

  ## Check that percentage is accepted value
  percentage_value_check <-
    percentage >= 0 &
    percentage <= 1

  flag_percentage <- percentage_value_check == FALSE

  if(flag_percentage){
    c(
      "x" = paste("{.var percentage} must be a numeric value between", error("0 and 1")),
      "!" = paste("The {.var percentage} input you've supplied is", callout('"{(percentage)}"')),
      "i" = paste(status("Check the ", "{.var {percentage}}"), "input.")
    ) |>
      cli::cli_abort()
  }

  #==========================================================================#
  # Conversion Work-----------------------------------------------------------
  #==========================================================================#
  ## Convert to hsl-----------------------------------------------------------
  hsl_value <- col_to_hsl(color)

  ## Set saturation-----------------------------------------------------------
  hsl_value[2] <- percentage

  ## Convert to RGB-----------------------------------------------------------
  rgb_value <- hsl_to_rgb(hsl_value)

  ## Convert to hex-----------------------------------------------------------
  final_hex <- rgb_to_hex(rgb_value)

  ## Spit it out--------------------------------------------------------------
  return(final_hex)
}
