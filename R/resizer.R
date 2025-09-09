#' Transforms and Scales Numeric Points in a Data Frame by a Provided Factor and Direction
#'
#' @param data A data frame or tibble with at least `x` and `y` variables.
#' @param x A numeric variable in `data`. The variable intended to be plotted on the x axis in a `ggplot`.
#' @param y A numeric variable in `data`. The variable intended to be plotted on the y axis in a `ggplot`.
#' @param x_anchor A numeric value. The `x` coordinate point that the resized polygon will be scaled and anchored from. Default is the first `x` value in `data`.
#' @param y_anchor A numeric value. The `y` coordinate that the resized polygon will be scaled and anchored from. Default is the first `y` value in `data`.
#' @param factor A numeric value. The factor that will be used to resize the existing polygon in `data`.
#' @param direction A string value of either `"up"` or `"down`. Data that is scaled `"up"` (default) will increase in size when plotted. Data that is scaled `"down"` will decrease in size.
#' @param drop Logical `TRUE` or `FALSE` that determines if all other variables that are not being resized are removed from the final output. Default is `FALSE`.
#' @param ... Additional arguments passed to methods. Currently unused but reserved for future extensibility.
#'
#' @returns A data frame
#' @export
#'
#' @examplesIf rlang::is_installed("ggplot2")
#' @examples
#' library(ggplot2)
#'
#' # Resize a simple square "up" by a factor of 6
#' # Start with data that makes a shape#
#' df_square <-
#'   data.frame(
#'     x = c(0,1,1,0,0),
#'     y = c(0,0,1,1,0)
#'   )
#'
#' # Resize the shape#
#' df_square_resized <-
#'   df_square |>
#'   resizer(x, y, factor = 6)
#'
#' # Plot them
#' df_square |>
#'   ggplot(aes(x,y)) +
#'   # resized square - red dashed line
#'   geom_path(data = df_square_resized, color = "#a83246", linewidth = 2, linetype = 2) +
#'   # original square - black solid line
#'   geom_path(color = "#000000", linewidth = .8) +
#'   coord_equal()
#'
#' # Resize a circle "down" by a factor of 3
#' df_circle <-
#'   circle_data(x = 5, y = 5, radius = 5, group_var = TRUE)
#'
#' # Set then anchor point as the middle of the circle c(5,5)
#' # Although the point 5,5 is in the circle's bounds
#' # it's not actually a row in `df_circle`
#' # A message will display in cases like these and is "fine" to ignore.
#'
#' df_circle_resized <-
#'   df_circle |>
#'   resizer(x,y, x_anchor = 5, y_anchor = 5, direction = "down", factor = 3)
#'
#' # Plot them
#' df_circle |>
#'   ggplot(aes(x,y)) +
#'   # resized square - red dashed line
#'   geom_path(data = df_circle_resized, color = "#a83246", linewidth = 2, linetype = 2) +
#'   # original square - black solid line
#'   geom_path(color = "#000000", linewidth = .8) +
#'   coord_equal()
#'
#'
resizer <-
  function(data = NULL, x, y, x_anchor = NULL, y_anchor = NULL, factor = NULL, direction = "up", drop = FALSE, ...) {
    # ===========================================================================#
    # Global Variable Handling----------------------------------------------------
    # ===========================================================================#
    # Establish these for use later in the workflow
    `:=` <- NULL

    #==========================================================================#
    # Input Checks--------------------------------------------------------------
    #==========================================================================#
    ## Check that all inputs are provided---------------------------------------
    # Throw an error if any are missing, otherwise establish the x and y vars#
    ### data--------------------------------------------------------------------
    if(!is.data.frame(data)) {
      var_name <- deparse(substitute(data))

      c(
        "x" = paste("{.var data} must be present and of class", error("<data.frame>")),
        "!" = paste("The input you've supplied, {.var {var_name}}, is of class", callout("<{class(data)}>")),
        "i" = paste(status("Check the ", "{.var data}"), "input.")
      )  |>
        cli::cli_abort()
    }

    ### x-----------------------------------------------------------------------
    if(missing(x)){
      x <- NULL
      is.var.present(x)
    } else{
      #### Pulling x column name----
      x_name <-
        data |>
        dplyr::select({{ x }}) |>
        names()

      #### Pulling x column for manip work----
      x <-
        data |>
        dplyr::select({{ x }}) |>
        dplyr::pull()
    }

    ### x_anchor----------------------------------------------------------------
    x_anchor_null <- is.null(x_anchor)

    #if no x_anchor is provided#
    if(x_anchor_null){
      # Grab the first value in the df#
      x_anchor <- x[1]
    }

    ### y-----------------------------------------------------------------------
    if(missing(y)){
      y <- NULL
      is.var.present(y)
    } else {
      #### Pulling y column name----
      y_name <-
        data |>
        dplyr::select({{ y }}) |>
        names()

      #### Pulling y column for manip work----
      y <-
        data |>
        dplyr::select({{ y }}) |>
        dplyr::pull()
    }

    ### y_anchor----------------------------------------------------------------
    y_anchor_null <- is.null(y_anchor)

    #if no y_anchor is provided#
    if(y_anchor_null){
      # Grab the first value in the df#
      y_anchor <- y[1]
    }

    ### factor------------------------------------------------------------------
    is.var.present(factor)
    ### direction---------------------------------------------------------------
    is.var.present(direction)
    ### drop--------------------------------------------------------------------
    is.var.present(drop)

    ## Check that all (applicable) inputs are of expected length----------------
    ### x_anchor----------------------------------------------------------------
    check.length(x_anchor, expected_length = 1)
    ### y_anchor----------------------------------------------------------------
    check.length(y_anchor, expected_length = 1)
    ### factor------------------------------------------------------------------
    check.length(factor, expected_length = 1)
    ### direction---------------------------------------------------------------
    check.length(direction, expected_length = 1)

    ## Check that all inputs are the correct class------------------------------
    # Throw an error if any are not, otherwise continue#
    ### x-----------------------------------------------------------------------
    check.class(x, expected_class = "numeric")
    ### x_anchor----------------------------------------------------------------
    check.class(x_anchor, expected_class = "numeric")
    ### y-----------------------------------------------------------------------
    check.class(y, expected_class = "numeric")
    ### y_anchor----------------------------------------------------------------
    check.class(y_anchor, expected_class = "numeric")
    ### factor------------------------------------------------------------------
    check.class(factor, expected_class = "numeric")
    ### direction---------------------------------------------------------------
    check.class(direction, expected_class = "character")
    ### drop--------------------------------------------------------------------
    check.class(drop, expected_class = "logical")

    ## Check that factor is positive--------------------------------------------
    # Throw an error if not, otherwise continue#
    is.expected.numeric.type(factor, expected_type = "positive")

    ## Check that direction is a valid value------------------------------------
    is.expected.value(direction, expected_values = c("up", "down"))

    ### Check if anchor points exist in the data--------------------------------
    # and if not, just pass a warning through
    x_in_data <- x_anchor %in% x
    y_in_data <- y_anchor %in% y

    if(!x_in_data || !y_in_data) {
      c(
        "!" = paste("The anchor point you've supplied ({x_anchor}, {y_anchor})", error("is not found in your data.")),
        "i" = paste("The data will be", callout("scaled relative to this external point"))
      ) |>
        cli::cli_inform()
    }

    #==========================================================================#
    # Resizing work-------------------------------------------------------------
    #===========================================================================
    # Calculate the new values based on direction
    if(direction == "up") {
      new_x <- (x - x_anchor) * factor + x_anchor
      new_y <- (y - y_anchor) * factor + y_anchor
    } else {
      new_x <- (x - x_anchor) / factor + x_anchor
      new_y <- (y - y_anchor) / factor + y_anchor
    }

    # Apply the transformations
    data_resized <-
      data |>
      dplyr::mutate(
        !!x_name := new_x,
        !!y_name := new_y
      )

    # If drop is TRUE - just return the resized values w/o other vars...
    if (drop) {
      df_out <-
        data_resized |>
        dplyr::select(dplyr::all_of(c(x_name, y_name)))

      return(df_out)

    } else {
      # ...Or else leave it as-is
      return(data_resized)
    }
  }
