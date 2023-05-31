library(tidyverse)
library(artpack)

posit_pal <- c("#9B4665", "#419498", "#783850", "#EF6332")

# df <- circle_packer(n = 1000, max_x = 100, max_y = 100, color_pal = posit_pal, color_type = "random",
#                     big_r = 6)

# df <- circle_packer(n = 1000, max_x = 100, max_y = 100, color_pal = posit_pal, color_type = "random",
#                     big_r = 6, circle_type = "swirl")

# df <- square_packer(n = 1000, max_x = 100, max_y = 100, color_pal = posit_pal, color_type = "random",
#                     big_r = 6, angles = TRUE)

df <- square_packer(n = 1000, max_x = 100, max_y = 100, color_pal = posit_pal, color_type = "random",
                    big_r = 6, angles = FALSE)




rect1 <- tibble(x = c(0,20,20,0,0),
               y = c(5,5,105,105,5),
               group = "rect_1")

rect2 <- tibble(x = c(25,45,45,25,25),
               y = c(-5,-5,95,95,-5),
               group = "rect_2")

rect3 <- tibble(x = c(50,70,70,50,50),
                y = c(5,5,105,105,5),
                group = "rect_3")

rect4 <- tibble(x = c(75,95,95,75,75),
                y = c(-5,-5,95,95,-5),
                group = "rect_4")

rects <- rbind(rect1,rect2,rect3,rect4)


df2 <- df |>
  mutate(y = case_when(sp::point.in.polygon(x,y,rect1$x, rect1$y) == 1 ~ y + 5,
                       sp::point.in.polygon(x,y,rect2$x, rect2$y) == 1 ~ y - 5,
                       sp::point.in.polygon(x,y,rect3$x, rect3$y) == 1 ~ y + 5,
                       sp::point.in.polygon(x,y,rect4$x, rect4$y) == 1 ~ y - 5,

                           TRUE ~ y))

# df2 |>
#   ggplot(aes(x,y, group = group))+
#   theme_void()+
#   theme(plot.background = element_rect(fill = "#A1B7CC"))+
#   geom_polygon(fill = df$fill, color = "#ffffff", size = .2)+
#   geom_path(data = rects, aes(x = x + .1, y = y + .1), color = "#000000", size = 2, lineend = "round")+
#   geom_path(data = rects, color = "#A1B7CC", size = 1, lineend = "round")+
#
#   coord_equal()

# df2 |>
#   ggplot(aes(x,y, group = group))+
#   theme_void()+
#   theme(plot.background = element_rect(fill = "#A1B7CC"))+
#   geom_path(color = df$color, size = .2)+
#   geom_path(data = rects, aes(x = x + .1, y = y + .1), color = "#000000", size = 2, lineend = "round")+
#   geom_path(data = rects, color = "#A1B7CC", size = 1, lineend = "round")+
#
#   coord_equal()

# df2 |>
#   ggplot(aes(x,y, group = group))+
#   theme_void()+
#   theme(plot.background = element_rect(fill = "#3B1B27"))+
#   geom_path(color = df$color, size = 1)+
#   geom_path(color = "#3B1B27", size = .2)+
#   geom_path(data = rects, aes(x = x + .1, y = y + .1), color = "#000000", size = 2, lineend = "round")+
#   geom_path(data = rects, color = "#3B1B27", size = 1, lineend = "round")+
#
#   coord_equal()

# df2 |>
#   ggplot(aes(x,y, group = group))+
#   theme_void()+
#   theme(plot.background = element_rect(fill = "#FF6A35"))+
#   geom_polygon(fill = df$fill, size = .5, color = "#ffffff")+
#   geom_path(data = rects, aes(x = x + .1, y = y + .1), color = "#000000", size = 2, lineend = "round")+
#   geom_path(data = rects, color = "#FF6A35", size = 1, lineend = "round")+
#   coord_equal()

df2 |>
  ggplot(aes(x,y, group = group))+
  theme_void()+
  theme(plot.background = element_rect(fill = "#D4608A"))+
  geom_polygon(fill = df$fill, size = .5, color = "#ffffff")+
  geom_path(data = rects, aes(x = x + .1, y = y + .1), color = "#000000", size = 2, lineend = "round")+
  geom_path(data = rects, color = "#D4608A", size = 1, lineend = "round")+
  coord_equal()

