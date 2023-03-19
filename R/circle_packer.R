#' Data Generation for Circle Packing
#' @description
#'
#' A tool for creating a data frame of values that create a circle packing design when plotted.
#' When the default `circle_type` "whole" is used, the output should mapped with `geom_polygon`
#' in a ggplot. When "swirl" is used, the output should be mapped with `geom_path` for the best results.
#'
#' @param n The total number of circles you would like the function to attempt to create. A single numeric value.
#' @param min_x The minimum limit of the x-axis - the left 'border' of the canvas A single numeric value.
#' @param max_x The maximum limit of the x-axis - the right 'border' of the canvas A single numeric value.
#' @param min_y The minimum limit of the y-axis - the bottom 'border' of the canvas A single numeric value.
#' @param max_y The maximum limit of the y-axis - the top 'border' of the canvas A single numeric value.
#' @param big_r The radius used for your 'big' sized circles A single numeric value.
#' @param med_r The radius used for your 'medium' sized circles. A single numeric value.
#' @param small_r The radius used for your 'small' sized circles. A single numeric value.
#' @param color_pal A vector of hex color codes that will be mapped to the data.
#' @param color_type Default is "regular" - The colors will be mapped in order from big circles to small circles. "reverse" - The colors will be mapped in reversed order from small to big circles. "random" - The colors will be mapped randomly to any sized circle.
#' @param circle_type Default is "whole" - Regular circles. "swirl" - circles are replaced with spirals. Spirals should be mapped with `geom_polygon` in a ggplot for the best results.
#'
#' @return A Tibble
#' @export
#'
#' @importFrom purrr map2
#' @importFrom purrr list_rbind
#' @importFrom purrr pmap
#' @importFrom dplyr tibble
#' @importFrom dplyr group_by
#' @importFrom dplyr group_size
#'
#' @examples
#'packed_circles <- circle_packer(n = 50, big_r = 5, med_r = 3, small_r = 1,
#'min_x = 0, max_x = 100, min_y = 0, max_y = 100)
#'packed_circles
#'
#'packed_circles |>
#'ggplot(aes(x,y, group = group))+
#'theme_void()+
#'theme(plot.background = element_rect(fill = "black"))+
#'geom_polygon(fill = "white", color = "red")+
#'coord_equal()
#'

circle_packer <- function(n, min_x = 0, max_x = 10, min_y = 0, max_y = 10,
                          big_r = 5, med_r = 2, small_r = 1,
                          color_pal = NULL, color_type = "regular",
                          circle_type = "whole"){

  if(!is.numeric(big_r)){
    stop("`size` must be numeric.")
  } else if(big_r <= 0){
    stop("`size` must be greater than zero.")
  }

  if(!is.numeric(med_r)){
    stop("`size` must be numeric.")
  } else if(med_r <= 0){
    stop("`size` must be greater than zero.")
  }

  if(!is.numeric(small_r)){
    stop("`size` must be numeric.")
  } else if(small_r <= 0){
    stop("`size` must be greater than zero.")
  }

  theta <- seq(0,2*pi, length = 100)

  distance <- function(x1,y1,x2,y2){
    sqrt(((x2-x1)^2) + ((y2-y1)^2))
  }
  #Big Circles----
  big_iter = 1:(n*.02)
  big_x <- c(x = sample(min_x+(big_r):max_x-(big_r),1))
  big_y <- c(y = sample(min_y+(big_r):max_y-(big_r),1))
  i = 1
  tries = 0
  repeat{
    x <- sample(min_x+(big_r):max_x-(big_r),1)
    y <- sample(min_y+(big_r):max_y-(big_r),1)
    logic = map2(big_x,big_y, ~distance(.x,.y,x,y)) >= big_r*2
    if(sum(logic) == length(big_y)){
      big_x <- append(big_x, c(x=x))
      big_y <- append(big_y, c(y=y))
      i <- i + 1
    } else{
      tries = tries + 1
    }
    if(i == (length(big_iter) + 1)){
      break
    }
    if(tries == 3000){
      break
    }
  }
  new_iter = 1:length(big_y)
  big_angles = sample(0:360, length(big_y), replace = TRUE)

  if(length(big_x) != length(big_y)){
  stop("length of big_x and big_y don't match.")
  } else if(length(big_y) != length(new_iter)){
    stop("length of big_y and new_iter don't match.")
  } else if(length(new_iter) != length(big_angles)){
    stop("length of new_iter and big_angles")
  }


  big_circles <- switch(circle_type,
                        "whole"  =  pmap(list(big_x,
                                              big_y,
                                              new_iter), ~tibble(x = cos(theta)*big_r + ..1,
                                                                 y = sin(theta)*big_r + ..2,
                                                                 group = paste0("big_",..3))) |> list_rbind(),
                        "swirl"  =  pmap(list(big_x,
                                              big_y,
                                              new_iter,
                                              big_angles), ~artpack::rotator(tibble(x = (cos(theta)*seq(1,0, length = 1000))*big_r + ..1,
                                                                                    y = (sin(theta)*seq(1,0, length = 1000))*big_r + ..2,
                                                                                    group = paste0("big_",..3),
                                                                                    linewidth = .8), ..4)) |> list_rbind(),
                        stop(paste(circle_type, "is not a valid `type` option.\nPlease input `whole` or `swirl`"))
  )


  message("Big Circles Complete!")

  #med Circles----
  med_iter = 1:((n*.50) +1)
  med_x <- c(x = sample(min_x+(med_r):max_x-(med_r),1))
  med_y <- c(y = sample(min_y+(med_r):max_y-(med_r),1))
  i = 1
  tries = 0
  repeat{
    x <- sample(min_x+(med_r):max_x-(med_r),1)
    y <- sample(min_y+(med_r):max_y-(med_r),1)

    logic = map2(big_x,big_y, ~distance(.x,.y,x,y)) >=  big_r+med_r
    logic2 = map2(med_x,med_y, ~distance(.x,.y,x,y)) >= med_r*2

    if((sum(logic2) == length(med_y)) & (sum(logic) == length(big_y))){
      med_x <- append(med_x, c(x=x))
      med_y <- append(med_y, c(y=y))
      i <- i + 1
    } else{
      tries = tries + 1
    }
    if(i == (length(med_iter) + 1)){
      break
    }
    if(tries == 10000){
      break
    }
  }

  med_x <- med_x[-1]
  med_y <- med_y[-1]
  new_iter = 1:length(med_y)
  mid_angles = sample(0:360, length(med_y), replace = TRUE)

  if(length(med_x) != length(med_y)){
    stop("length of med_x and med_y don't match.")
  } else if(length(med_y) != length(new_iter)){
    stop("length of med_y and new_iter don't match.")
  } else if(length(new_iter) != length(med_angles)){
    stop("length of new_iter and med_angles")
  }


  med_circles <- switch(circle_type,
                        "whole"  =  pmap(list(med_x,
                                              med_y,
                                              new_iter), ~tibble(x = cos(theta)*med_r + ..1,
                                                                 y = sin(theta)*med_r + ..2,
                                                                 group = paste0("med_",..3))) |> list_rbind(),
                        "swirl"  =  pmap(list(med_x,
                                              med_y,
                                              new_iter,
                                              mid_angles), ~artpack::rotator(tibble(x = (cos(theta)*seq(1,0, length = 1000))*med_r + ..1,
                                                                                    y = (sin(theta)*seq(1,0, length = 1000))*med_r + ..2,
                                                                                    group = paste0("med_",..3),

                                                                                    linewidth = .4), ..4)) |> list_rbind(),
                        stop(paste(circle_type, "is not a valid `type` option.\nPlease input `whole` or `swirl`"))
  )
  message("Med Circles Complete!")


  #small Circles----
  small_iter = 1:((n*.75)+1)
  small_x <- c(x = sample(min_x+(small_r):max_x-(small_r),1))
  small_y <- c(y = sample(min_y+(small_r):max_y-(small_r),1))
  i = 1
  tries = 0
  repeat{
    x <- sample(min_x+(small_r):max_x-(small_r),1)
    y <- sample(min_y+(small_r):max_y-(small_r),1)

    logic = map2(big_x,big_y, ~distance(.x,.y,x,y)) >=  big_r+small_r
    logic2 = map2(med_x,med_y, ~distance(.x,.y,x,y)) >=  small_r+med_r
    logic3 = map2(small_x,small_y, ~distance(.x,.y,x,y)) >= small_r*2

    if((sum(logic3) == length(small_y)) & (sum(logic2) == length(med_y)) & (sum(logic) == length(big_y))){
      small_x <- append(small_x, c(x=x))
      small_y <- append(small_y, c(y=y))
      i <- i + 1
    } else{
      tries = tries + 1
    }
    if(i == (length(small_iter) + 1)){
      break
    }
    if(tries == 9000){
      break
    }
  }
  small_x <- small_x[-1]
  small_y <- small_y[-1]
  new_iter = 1:length(small_y)
  small_angles = sample(0:360, length(small_y), replace = TRUE)

  if(length(big_x) != length(big_y)){
    stop("length of big_x and big_y don't match.")
  } else if(length(big_y) != length(new_iter)){
    stop("length of big_y and new_iter don't match.")
  } else if(length(new_iter) != length(big_angles)){
    stop("length of new_iter and big_angles")
  }


  small_circles <- switch(circle_type,
                          "whole"  =  pmap(list(small_x,
                                                small_y,
                                                new_iter), ~tibble(x = cos(theta)*small_r + ..1,
                                                                   y = sin(theta)*small_r + ..2,
                                                                   group = paste0("small_",..3))) |> list_rbind(),
                          "swirl"  =  pmap(list(small_x,
                                                small_y,
                                                new_iter,
                                                small_angles), ~artpack::rotator(tibble(x = (cos(theta)*seq(1,0, length = 1000))*small_r + ..1,
                                                                                        y = (sin(theta)*seq(1,0, length = 1000))*small_r + ..2,
                                                                                        group = paste0("small_",..3),
                                                                                        linewidth = .1), ..4)) |> list_rbind(),
                          stop(paste(circle_type, "is not a valid `type` option.\nPlease input `whole` or `swirl`"))
  )
  message("Small Circles Complete!")

  all_circles <- rbind(big_circles, med_circles, small_circles)

  all_circles <- all_circles |>
    group_by(group)

  if(!is.null(color_pal)){
    group_ns <- all_circles |>
      group_size()

    total_groups <- length(group_ns)
    color_opts <- switch(color_type,
                         "regular" = rep(colorRampPalette(color_pal)(total_groups), each = group_ns[1]),
                         "reverse" = rev(rep(colorRampPalette(color_pal)(total_groups), each = group_ns[1])),
                         "random" = rep(sample(colorRampPalette(color_pal)(total_groups)),each = group_ns[1]),
                         stop(paste0("Invalid `color_type`: '",color_type,"' is not a valid option.\nPlease input one of the following: 'regular', 'reverse', or 'random'." ))
    )


    if(circle_type == "whole"){
      all_circles$fill <- color_opts
    } else{
      all_circles$color <- color_opts
    }

  }
  return(all_circles)
}
