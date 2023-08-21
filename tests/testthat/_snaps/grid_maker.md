# Missing argument checks [ansi]

    Code
      grid_maker()
    Error <rlang_error>
      [1m[22m`xlim`, `ylim`, and `size` are [1m[31mmissing[39m[22m
      [31mx[39m `xlim`, `ylim`, and `size` are [1m[36mrequired[39m[22m and should be a numeric value with a length of 2
      [36mi[39m Check the `xlim`, `ylim`, and `size` variables

# Argument length checks [ansi]

    Code
      grid_maker(xlim = 1, ylim = 0, size = 1:9, )
    Error <rlang_error>
      [1m[22m`xlim` must have a [1m[33mlength of 2[39m[22m, `ylim` must have a [1m[33mlength of 2[39m[22m, and `size` must have a [1m[33mlength of 1[39m[22m
      [31mx[39m `xlim` has a length of [1m[31m1[39m[22m, `ylim` has a length of [1m[31m1[39m[22m, and `size` has a length of [1m[31m9[39m[22m
      [36mi[39m Check the `xlim`, `ylim`, and `size` variables

# Numeric checks [ansi]

    Code
      grid_maker(xlim = c(1, 2), ylim = "bigggg", size = 1, )
    Error <rlang_error>
      [1m[22m`ylim` must have a [1m[33mlength of 2[39m[22m
      [31mx[39m `ylim` has a length of [1m[31m1[39m[22m
      [36mi[39m Check the `ylim` variable

# Size check - lim value [ansi]

    Code
      grid_maker(xlim = c(0, 1), ylim = c(0, 2), size = 3)
    Error <rlang_error>
      [1m[22m`size` must be [1m[33mless than or equal to the max limits for x and y[39m[22m
      [31mx[39m `size` is [1m[31m3[39m[22m
      [36mi[39m max xlim is [1m[36m1[39m[22m
      [36mi[39m max ylim is [1m[36m2[39m[22m
      [36mi[39m Check the `size` variable

