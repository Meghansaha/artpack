#wave data test#

wave_df <- wave_data(0,10, size = .5, color = "#000000", fill = "red",
                     n_points = 10, freq = .5, rotate = NULL, orientation = "horiz",
                     dampen = 3)
n <- 10
starts <- sample(seq(-10,10, length = n))
ends <- sample(seq(-10,10, length = n))
sizes <- sample(seq(.1,2, length = n))
angles <- sample(seq(-360,360, length = n))
colors <- art_pals(pal = "nature", n = n)
freqs <- sample(seq(2*pi,50, length = 50),n, replace = TRUE)

list_options <- list(1:n,
                     starts,
                     ends,
                     sizes,
                     angles,
                     colors,
                     freqs)

waves_df <- pmap(list_options, ~wave_data(..2,..3, size = ..4,
                                        color = "#000000",
                                        fill = ..6, freq = ..7,
                                        rotate = ..5,
                                        n_points = 1000,
                                        dampen = 3,
                                        group = TRUE) |>
                             mutate(group = paste0(group,"_0",.x)))|>
  list_rbind()




waves_df |>
  ggplot(aes(x,y, group = group))+
  theme_void()+
  theme(plot.background = element_rect(fill = "#000000"))+
  geom_polygon(size = .1, fill = waves_df$fill, color = "#000000")+
  coord_equal()
