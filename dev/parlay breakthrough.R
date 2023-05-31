library(tidyverse)
library(artpack)

triangle <- tibble(x = c(0,2,1,0),
                   y = c(0,0,2,0))

n = 500

squares_opts <- list(x = sample(seq(0,100, l = 50),n, replace = TRUE),
                     y = sample(seq(0,100, l = 50),n, replace = TRUE),
                     size = sample(seq(1,25, l = 50),n, replace = TRUE),
                     fills = art_pals("rainbow",n)
                     )

square_list <- pmap(squares_opts, ~square_data(..1,..2,..3) |>
                      mutate(fill = ..4) )

parlay <- function(df, step_size = 1000){
  
  # Add something to keep other variables in new data frame#
  
  from <- 1:nrow(df)
  to <- c(from[-1],1)
  
  long_df <- map2(from,to, ~tibble(x = seq(df$x[.x], df$x[.y], l = step_size),
                                   y = seq(df$y[.x], df$y[.y], l = step_size)
  )
  ) |> list_rbind()
  return(long_df)
} 


df <- pmap(list(square_list,
                sample(art_pals("rainbow",n)),
                1:n), ~parlay(..1, step_size = 100) |> mutate(fill = ..2, group = paste0("group_0",..3))) |> list_rbind()


df |>
  ggplot(aes(x,y, group = group)) +
  theme_void()+
  geom_polygon(fill = df$fill, alpha = .3, linewidth = .2, color = "#000000", position = position_jitter(width = .3, height = .3)) +
  coord_equal(xlim = c(0,100), ylim = c(0,100))