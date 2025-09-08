# Missing argument checks [ansi]

    Code
      grid_maker()
    Condition
      [1m[33mError[39m in `grid_maker()`:[22m
      [1m[22m[33m![39m `xlim`, `ylim`, and `size` are [1m[31mmissing[39m[22m
      [31mx[39m `xlim`, `ylim`, and `size` are [1m[36mrequired[39m[22m and should be a numeric value with a length of 2
      [36mi[39m Check the `xlim`, `ylim`, and `size` variables

# Argument length checks [ansi]

    Code
      grid_maker(xlim = 1, ylim = 0, size = 1:9, )
    Condition
      [1m[33mError[39m in `grid_maker()`:[22m
      [1m[22m[33m![39m `xlim` must have a [1m[33mlength of 2[39m[22m, `ylim` must have a [1m[33mlength of 2[39m[22m, and `size` must have a [1m[33mlength of 1[39m[22m
      [31mx[39m `xlim` has a length of [1m[31m1[39m[22m, `ylim` has a length of [1m[31m1[39m[22m, and `size` has a length of [1m[31m9[39m[22m
      [36mi[39m Check the `xlim`, `ylim`, and `size` variables

# Numeric checks [ansi]

    Code
      grid_maker(xlim = c(1, 2), ylim = c("small", "bigggg"), size = 1, )
    Condition
      [1m[33mError[39m in `grid_maker()`:[22m
      [1m[22m[33m![39m The `ylim` argument must have a class of [1m[33m<numeric>[39m[22m
      [31mx[39m `ylim` has a class of [1m[34m<character>[39m[22m
      [36mi[39m Check the `ylim` variable

# Size check - lim value [ansi]

    Code
      grid_maker(xlim = c(0, 1), ylim = c(0, 2), size = 3)
    Condition
      [1m[33mError[39m in `grid_maker()`:[22m
      [1m[22m[33m![39m `size` must be [1m[33mless than or equal to the max limits for x and y[39m[22m
      [31mx[39m `size` is [1m[31m3[39m[22m
      [36mi[39m max xlim is [1m[36m1[39m[22m
      [36mi[39m max ylim is [1m[36m2[39m[22m
      [36mi[39m Check the `size` variable

# Size check - positive number [ansi]

    Code
      grid_maker(xlim = c(0, 1), ylim = c(0, 2), size = -1)
    Condition
      [1m[33mError[39m in `grid_maker()`:[22m
      [1m[22m[33m![39m `size` must be [1m[33mgreater than 0[39m[22m
      [31mx[39m `size` is [1m[31m-1[39m[22m
      [36mi[39m Check the `size` variable

# Character args are character [ansi]

    Code
      grid_maker(xlim = c(0, 1), ylim = c(0, 2), size = 1, fill_pal = list("blue",
        "red", "green"), color_pal = 1:3, fill_style = TRUE, color_style = FALSE)
    Condition
      [1m[33mError[39m in `grid_maker()`:[22m
      [1m[22m[33m![39m The `fill_pal`, `fill_style`, `color_pal`, and `color_style` arguments must have a class of [1m[33m<character>[39m[22m
      [31mx[39m `fill_pal` has a class of [1m[34m<list>[39m[22m, `fill_style` has a class of [1m[34m<logical>[39m[22m, `color_pal` has a class of [1m[34m<integer>[39m[22m, and `color_style` has a class of [1m[34m<logical>[39m[22m
      [36mi[39m Check the `fill_pal`, `fill_style`, `color_pal`, and `color_style` variables

# Fills are valid [ansi]

    Code
      grid_maker(xlim = c(0, 1), ylim = c(0, 2), size = 1, fill_pal = c("blue", "red",
        "green", "yello"))
    Condition
      [1m[33mError[39m in `grid_maker()`:[22m
      [1m[22m[31mx[39m `fill_pal` must contain [1m[31mvalid 6-digit hexadecimal colors[39m[22m or valid color names found in
      [1m[31m`grDevices::colors()`[39m[22m
      [33m![39m The input you've supplied, `fill_pal`, contains [1m[33m"yello"[39m[22m
      [36mi[39m [1m[36mCheck the `fill_pal`[39m[22m input.

# Colors are valid [ansi]

    Code
      grid_maker(xlim = c(0, 1), ylim = c(0, 2), size = 1, fill_pal = c("blue", "red",
        "green", "yellow"), color_pal = "#000")
    Condition
      [1m[33mError[39m in `grid_maker()`:[22m
      [1m[22m[31mx[39m `color_pal` must contain [1m[31mvalid 6-digit hexadecimal colors[39m[22m or valid color names found in
      [1m[31m`grDevices::colors()`[39m[22m
      [33m![39m The input you've supplied, `color_pal`, contains [1m[33m"#000"[39m[22m
      [36mi[39m [1m[36mCheck the `color_pal`[39m[22m input.

# Fill style presets [ansi]

    Code
      grid_maker(xlim = c(0, 1), ylim = c(0, 2), size = 1, fill_pal = c("blue", "red",
        "green"), fill_style = "idk", color_pal = "#000000", color_style = "wild")
    Condition
      [1m[33mError[39m in `grid_maker()`:[22m
      [1m[22m[33m![39m `fill_style` must be a string value of [1m[33m"range"[39m[22m or [1m[33m"random"[39m[22m
      [31mx[39m `fill_style` is of value: [1m[31midk[39m[22m
      [36mi[39m Check the `fill_style` variable

# Color style presets [ansi]

    Code
      grid_maker(xlim = c(0, 1), ylim = c(0, 2), size = 1, fill_pal = c("blue", "red",
        "green"), color_pal = "#000000", color_style = "wild")
    Condition
      [1m[33mError[39m in `grid_maker()`:[22m
      [1m[22m[33m![39m `color_style` must be a string value of [1m[33m"range"[39m[22m or [1m[33m"random"[39m[22m
      [31mx[39m `color_style` is of value: [1m[31mwild[39m[22m
      [36mi[39m Check the `color_style` variable

# Size is only a whole number [ansi]

    Code
      grid_maker(xlim = c(0, 1), ylim = c(0, 2), size = 0.5)
    Condition
      [1m[33mError[39m in `grid_maker()`:[22m
      [1m[22m[33m![39m `size` must be [1m[33ma whole number[39m[22m with [1m[33mno decimals[39m[22m
      [31mx[39m `size` is [1m[31m0.5[39m[22m
      [36mi[39m Check the `size` variable

