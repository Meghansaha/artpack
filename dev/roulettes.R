library(tidyverse)

jit = 10
outline_star <- tibble(x = c(seq(0,.5,length = jit),
                     seq(.5,1,length = jit),
                     seq(1,-.1, length = jit),
                     seq(-.1,1.1, length = jit),
                     seq(1.1,0, length = jit)),
               y = c(seq(0,1, length = jit),
                     seq(1,0, length = jit),
                     seq(0,.58, length = jit),
                     rep(.58, jit),
                     seq(.58,0, length = jit))
)

solid_star <- tibble(x = c(seq(0,.2,length = jit),
                           seq(.2,-.1, length = jit),
                           seq(-.1,.3, length = jit),
                           seq(.3,.5, length = jit),
                           seq(.5,.7, length = jit),
                           seq(.7,1.1, length = jit),
                           seq(1.1,.8, length = jit),
                           seq(.8,1, length = jit),
                           seq(1,.5, length = jit),
                           seq(.5,.0, length = jit)),
                     y = c(seq(0,.4, length = jit),
                           seq(.4, .58, length = jit),
                           rep(.58, jit),
                           seq(.58,1, length = jit),
                           seq(1,.58, length = jit),
                           rep(.58,jit),
                           seq(.58,.4, length = jit),
                           seq(.4,0, length = jit),
                           seq(0,.25, length = jit),
                           seq(.25,0, length = jit)))

outline_star |>
  ggplot(aes(x,y))+
  geom_path(position = position_jitter(width = .005, height = .005), color =  "black", size = 4)+
  geom_polygon(data = solid_star,fill="red")+
  coord_equal()

n = 1000
R = seq(0,5, length = n)
r = seq(0,3, length = n)
d = seq(0,5, length = n)

theta <- seq(0, 2*pi, length = n)

test_df <- tibble(x = (R-r)*cos(theta) +d*(cos(((R-r)/r)*cos(theta))),
                  y =(R-r)*sin(theta) +d*(sin(((R-r)/r)*sin(theta))))



test_df|>
  ggplot(aes(x,y))+
  geom_path(size = .1)+
  coord_equal()
