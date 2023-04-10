#' Data Generation for 2D Sine and Cosine Waves
#'
#' @description
#' A tool for making data frames filled with data that displays sine or cosine waves when graphed.
#' The `geom_path` and `geom_polygon` geoms are recommended with this data for use in `ggplot2` for generative art.
#'
#' @param start Numeric value. The starting point of the wave on the coordinate system.
#' @param end Numeric value. The ending point of the wave on the coordinate system.
#' @param size Numeric value. The height or width of the wave. Orientation is set to `horizontal` by default, thus size will affect height by default. When orientation is set to `vertical`, size controls the width of the wave.
#' @param type String value. "sin" or "cos" for sine or cosine waves. `sin` is default.
#' @param rotate Optional. If rotation is desired, accepts a numerical value between -360 to 360 to rotate the wave by input degrees.
#' @param orientation String value. Default is `horizontal` which will draw the wave from left to right on the coordinate system. `vertical` will draw the wave from bottom to top on the coordinate system.
#' @param freq Numeric value. Default is 3 cycles per second. This affects how many "peaks" are created in the wave. Must be a positive numeric value.
#' @param n_points Numeric value. Default is 1000. This determines how many points each half of the wave will have. This option can come in handy when using jitter options or other texture/illusion methods.
#' @param color Optional. A hex color code, or `R` color string for the border color of the wave.
#' @param fill Optional. A hex color code, or `R` color string for the fill color of the wave.
#' @param group Logic value. `TRUE` or `FALSE`. Default is `FALSE`. If `TRUE`, Adds a group variable to the data frame. Useful for iterative work to make multiple waves in a single data frame.
#' @param dampen Optional. A factor in which to dampen the wave (make "flatter").
#' @param amplify Optional. A factor in which to amplify the wave (make "sharper").
#'
#' @return A Tibble
#' @export
#' @importFrom dplyr tibble
#' @importFrom dplyr mutate
#' @importFrom grDevices colors
#'
#' @examples
#'library(ggplot2)
#'wave_df <- wave_data(start = 0, end = 10,
#'                   fill = "purple",
#'                   color = "green")
#'
#'wave_df |>
#'ggplot(aes(x,y))+
#'theme_void()+
#'geom_polygon(fill = wave_df$fill,
#'             color = wave_df$color,
#'             linewidth = 3)+
#'coord_equal()
#'
#'
wave_data <- function(start, end, size = 1,
                      type = "sin", rotate = NULL, orientation = "horiz",
                      freq = 3, n_points = 1000, color = NULL, fill = NULL, group = FALSE,
                      dampen = NULL, amplify = NULL ){

  #Numeric Catches#
  if(!is.numeric(start) == TRUE){
    stop("`start` must be a numeric value.") # nocov
  } else if(!is.numeric(end) == TRUE){
    stop("`end` must be a numeric value.") # nocov
  }

  if(!is.numeric(size) == TRUE){
    stop("`size` must be a positive numeric value.") # nocov
  } else if((size > 0) != TRUE){
    stop("`size` must be a positive numeric value.") # nocov
  }

  if(is.null(rotate) == FALSE){
    if(!is.numeric(rotate) == TRUE){
      stop("`rotate` must be a positive numeric value between -360 to 360.") # nocov
    } else if(!rotate >= -360 & rotate <= 360){
      stop("`rotate` must be a positive numeric value between -360 to 360.") # nocov
    }
  }

  if(!is.numeric(n_points) == TRUE){
    stop("`n_points` must be a positive numeric value.") # nocov
  } else if((n_points > 0) != TRUE){
    stop("`n_points` must be a positive numeric value.") # nocov
  }

  if(!is.null(dampen)){
  if(!is.numeric(dampen) == TRUE){
    stop("`dampen` factor must be a positive numeric value.") # nocov
  } else if((dampen > 0) != TRUE){
    stop("`dampen` factor must be a positive numeric value.") # nocov
  }
  }

  if(!is.null(amplify)){
  if(!is.numeric(amplify) == TRUE){
    stop("`amplify` factor must be a positive numeric value.") # nocov
  } else if((amplify > 0) != TRUE){
    stop("`amplify` factor must be a positive numeric value.") # nocov
  }
  }


  #String Catches#
  if(!type %in% c("sin","cos")){
    stop("`type` must be a string value of:\n\"sin\" or \"cos\".") # nocov
  }

  if(!orientation %in% c("horiz","horizontal", "vert", "vertical")){
    stop("`orientation` must be a string value of:\n\"horiz\", \"horizontal\", \"vert\", or \"vertical\"") # nocov
  }

  if(is.null(color) == FALSE){
    if(!grepl("^#[[:alnum:]]{6}",color)){
      if(!color %in% colors()){
        stop("`color` must be a valid R color or a 6 digit alphanumeric hex code.") # nocov
      }
    }
  }

  if(is.null(fill) == FALSE){
    if(!grepl("^#[[:alnum:]]{6}",fill)){
      if(!fill %in% colors()){
        stop("`fill` must be a valid R color or a 6 digit alphanumeric hex code.") # nocov
      }
    }
  }

  #Logic Catches#
  if(!is.logical(group)){
    stop("`group` variable must be a logical `TRUE` or `FALSE` value.") # nocov
  }

freq <- (2*pi)*freq


wave <- switch(type,
               "sin" = sin(seq(0,freq, length = n_points)),
               "cos" = cos(seq(0,freq, length = n_points))
               )

if(!is.null(dampen)){
  wave <- wave/dampen
}

if(!is.null(amplify)){
  wave <- wave * amplify
}

path <- seq(start, end, length = n_points)

if(orientation %in% c("vert", "vertical")){
  wave_df <- tibble(x = c(wave, wave[n_points],rev(wave + size), wave[1]),
                    y = c(path, path[n_points], rev(path), path[1])
                    )
} else{
  wave_df <- tibble(x = c(path, path[n_points], rev(path), path[1]),
                    y = c(wave, wave[n_points], rev(wave + size), wave[1])
                    )
}

if(!is.null(color)){
  wave_df <- wave_df |>
    mutate(color = color)
}

if(!is.null(fill)){
  wave_df <- wave_df |>
    mutate(fill = fill)
}

if(!is.null(group)){
  wave_df <- wave_df |>
    mutate(group = "wave")
}

if(!is.null(rotate)){
  wave_df <- rotator(wave_df, angle = rotate)
}

return(wave_df)

}


