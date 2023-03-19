#' Data Generation for Squares
#' @description
#'
#' A tool for creating a data frame of values that create a square with a specified size
#' when plotted.
#'
#' @param x Numeric - The bottom left `x` value of the square.
#' @param y Numeric - The bottom left `y` value of the square.
#' @param size Numeric - The size of the square.
#'
#' Must be a value greater than zero.
#' @param group_var `TRUE`/`FALSE`. Default is `FALSE`.
#'
#' If `TRUE`, adds a grouping variable to the data frame.
#' Default is switched to `TRUE` when more than one `x`, `y`, and `size` values are present.
#'
#' @return A Tibble
#'
#' @importFrom purrr map2_dbl
#' @importFrom purrr list_rbind
#' @importFrom purrr pmap
#' @importFrom dplyr tibble
#'
#' @export
#'
#' @examples
#' one_square <- square_data(x = 0, y = 0, size = 5)
#'
#' one_square |>
#' ggplot(aes(x,y))+
#' geom_path(color = "green")+
#' coord_equal()
#'
#' x_vals <- c(0,4)
#' y_vals <- c(0,0)
#' sizes <- c(1,3)
#' sq_cols <- c("purple", "yellow")
#'
#' two_squares <- square_data(x = x_vals, y = y_vals, size = sizes)
#'
#' two_squares |>
#' ggplot(aes(x,y, group = group, fill = group))+
#' scale_fill_manual(values = sq_cols)+
#' theme(legend.position = "none")+
#' geom_polygon()+
#' coord_equal()
#'
#'
square_data <- function(x,y, size, group_var = FALSE){

  #Check for numeric x/y
  if(!is.numeric(x)){
    stop("`x` must be a numeric value")
  } else if(!is.numeric(y)){
    stop("`y` must be a numeric value")
  }

  #Check for numeric size greater than zero
  if(!is.numeric(size)){
    stop("`size` must be numeric.")
  } else if(sum(size > 0) != length(size)){
    stop("`size` must be greater than zero.")
  }

  #Check for equal lengths of x/y and size
  if(length(x) != length(y)){
    stop(paste("`x`, `y`, and `size` inputs must be equal in length\n`x` is of length",length(x),"\n`y` is of length",length(y),"\n`size` is of length",length(size)))
  } else if(length(y) != length(size)){
    stop(paste("`x`, `y`, and `size` inputs must be equal in length\n`x` is of length",length(x),"\n`y` is of length",length(y),"\n`size` is of length",length(size)))

  }

  #If x of length more than one, automatically add group variable
  if(length(x) > 1){
    group_var = TRUE
  }

  #Setting vars
  x1 = x
  x2 = map2_dbl(x1,size, ~.x+.y)
  y1 = y
  y2 = map2_dbl(y1,size, ~.x+.y)


  #Data w/ and w/o group var
  if(!group_var){
  list_vars <- list(x1,x2,y1,y2)
  df <- tibble(x = c(x1,x2,x2,x1,x1),
               y = c(y1,y1,y2,y2,y1))

  } else{
    n = 1:length(x)
    list_vars <- list(x1,x2,y1,y2,n)

    df <- pmap(list_vars, ~ tibble(x = c(..1,..2,..2,..1,..1),
                                   y = c(..3,..3,..4,..4,..3),
                                   group = paste0("square_0",..5)))|>
      list_rbind()


  }

  df

}



