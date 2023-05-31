#' Triangle_data
#'
#' @param center_x
#' @param center_y
#' @param width
#' @param height
#' @param group_var
#' @param color
#' @param fill
#'
#' @return #A Tibble
#' @export
#'
#' @examples
triangle_data <- function(center_x, center_y, width, height, group_var = NULL, color = NULL, fill = NULL){

  halfwidth = width/2
  halfheight = height/2

  df <- tibble(x = c(center_x - halfwidth,
                     center_x + halfwidth,
                     center_x,
                     center_x - halfwidth),
               y = c(rep(center_y - halfheight, 2),
                     center_y + halfheight,
                     center_y - halfheight
               )
  )

  #Adding group#
  if(!is.null(group_var)){
    df <- df |>
      mutate(group = "triangle_0")
  }

  #Adding color#
  if(!is.null(color)){
    df <- df |>
      mutate(color = color)
  }

  #Adding fill#
  if(!is.null(fill)){
    df <- df |>
      mutate(fill = fill)
  }


 return(df)
}
