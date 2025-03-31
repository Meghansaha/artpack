# Test that missing x throws an error [ansi]

    Code
      circle_data(y = 0, radius = 10)
    Condition
      [1m[33mError[39m in `circle_data()`:[22m
      [1m[22m[33m![39m `x` is [1m[31mmissing[39m[22m
      [31mx[39m `x` is [1m[36mrequired[39m[22m and should be a numeric value with a length of 1
      [36mi[39m Check the `x` variable

# Test that missing y throws an error [ansi]

    Code
      circle_data(x = 0, radius = 10)
    Condition
      [1m[33mError[39m in `circle_data()`:[22m
      [1m[22m[33m![39m `y` is [1m[31mmissing[39m[22m
      [31mx[39m `y` is [1m[36mrequired[39m[22m and should be a numeric value with a length of 1
      [36mi[39m Check the `y` variable

# Test that non-numeric x throws an error [ansi]

    Code
      circle_data(x = "0", y = 0, radius = 10)
    Condition
      [1m[33mError[39m in `circle_data()`:[22m
      [1m[22m[33m![39m `x` must be of class [1m[33m<numeric>[39m[22m
      [31mx[39m `x` is of class [1m[34m<character>[39m[22m
      [36mi[39m Check the `x` variable

# Test that non-numeric y throws an error [ansi]

    Code
      circle_data(x = 0, y = "0", radius = 10)
    Condition
      [1m[33mError[39m in `circle_data()`:[22m
      [1m[22m[33m![39m `y` must be of class [1m[33m<numeric>[39m[22m
      [31mx[39m `y` is of class [1m[34m<character>[39m[22m
      [36mi[39m Check the `y` variable

# Test that invalid radius throws an error [ansi]

    Code
      circle_data(x = 0, y = 0, radius = 0)
    Condition
      [1m[33mError[39m in `circle_data()`:[22m
      [1m[22m[33m![39m `radius` must be [1m[33mgreater than 0[39m[22m
      [31mx[39m `radius` is [1m[31m0[39m[22m
      [36mi[39m Check the `radius` variable

---

    Code
      circle_data(x = 0, y = 0, radius = -10)
    Condition
      [1m[33mError[39m in `circle_data()`:[22m
      [1m[22m[33m![39m `radius` must be [1m[33mgreater than 0[39m[22m
      [31mx[39m `radius` is [1m[31m-10[39m[22m
      [36mi[39m Check the `radius` variable

# Test that invalid x arg length throws an error [ansi]

    Code
      circle_data(x = 1:3, y = 0, radius = 5)
    Condition
      [1m[33mError[39m in `circle_data()`:[22m
      [1m[22m[33m![39m All arguments must have a [1m[33mlength of 1[39m[22m
      [31mx[39m `x` has a length of [1m[31m3[39m[22m
      [36mi[39m Check the `x` variable

# Test that invalid y arg length throws an error [ansi]

    Code
      circle_data(x = 0, y = 1:3, radius = 5)
    Condition
      [1m[33mError[39m in `circle_data()`:[22m
      [1m[22m[33m![39m All arguments must have a [1m[33mlength of 1[39m[22m
      [31mx[39m `y` has a length of [1m[31m3[39m[22m
      [36mi[39m Check the `y` variable

# Test that invalid radius arg length throws an error [ansi]

    Code
      circle_data(x = 0, y = 0, radius = 1:5)
    Condition
      [1m[33mError[39m in `circle_data()`:[22m
      [1m[22m[33m![39m All arguments must have a [1m[33mlength of 1[39m[22m
      [31mx[39m `radius` has a length of [1m[31m5[39m[22m
      [36mi[39m Check the `radius` variable

# Test that invalid color arg length throws an error [ansi]

    Code
      circle_data(x = 0, y = 0, radius = 1, color = c("#000000", "#333333"))
    Condition
      [1m[33mError[39m in `circle_data()`:[22m
      [1m[22m[33m![39m All arguments must have a [1m[33mlength of 1[39m[22m
      [31mx[39m `color` has a length of [1m[31m2[39m[22m
      [36mi[39m Check the `color` variable

# Test that invalid fill arg length throws an error [ansi]

    Code
      circle_data(x = 0, y = 0, radius = 1, fill = c("#000000", "#333333"))
    Condition
      [1m[33mError[39m in `circle_data()`:[22m
      [1m[22m[33m![39m All arguments must have a [1m[33mlength of 1[39m[22m
      [31mx[39m `fill` has a length of [1m[31m2[39m[22m
      [36mi[39m Check the `fill` variable

# Test that multiple invalid arg lengths throws an error [ansi]

    Code
      circle_data(x = 1:3, y = 2:5, radius = 1)
    Condition
      [1m[33mError[39m in `circle_data()`:[22m
      [1m[22m[33m![39m All arguments must have a [1m[33mlength of 1[39m[22m
      [31mx[39m `x` has a length of [1m[31m3[39m[22m and `y` has a length of [1m[31m4[39m[22m
      [36mi[39m Check the `x` and `y` variables

# Test that invalid colors throws an error [ansi]

    Code
      circle_data(x = 1, y = 2, radius = 1, color = "blAck")
    Condition
      [1m[33mError[39m in `circle_data()`:[22m
      [1m[22m[33m![39m `color` is [1m[31minvalid[39m[22m
      [31mx[39m `color` must be a valid: [1m[36m`r` color from `colors()`[39m[22m or a valid 6 digit [1m[36mhexadecimal webcolor[39m[22m
      [36mi[39m `blAck` is an [1m[33minvalid color[39m[22m

# Test that invalid fills throws an error [ansi]

    Code
      circle_data(x = 1, y = 2, radius = 1, fill = "blAck")
    Condition
      [1m[33mError[39m in `circle_data()`:[22m
      [1m[22m[33m![39m `fill` is [1m[31minvalid[39m[22m
      [31mx[39m `fill` must be a valid: [1m[36m`r` color from `colors()`[39m[22m or a valid 6 digit [1m[36mhexadecimal webcolor[39m[22m
      [36mi[39m `blAck` is an [1m[33minvalid color[39m[22m

# Numeric argument check [ansi]

    Code
      circle_data(x = 0, y = 0, radius = 5, n_points = "500")
    Condition
      [1m[33mError[39m in `circle_data()`:[22m
      [1m[22m[33m![39m `n_points` must be of class [1m[33m<numeric>[39m[22m
      [31mx[39m `n_points` is of class [1m[34m<character>[39m[22m
      [36mi[39m Check the `n_points` variable

---

    Code
      circle_data(x = 0, y = 0, radius = 5, n_points = 1)
    Condition
      [1m[33mError[39m in `circle_data()`:[22m
      [1m[22m[33m![39m `n_points` must be [1m[33mgreater than or equal to 100[39m[22m
      [31mx[39m `n_points` is [1m[31m1[39m[22m
      [36mi[39m Check the `n_points` variable

# Non character group prefix throws an error [ansi]

    Code
      circle_data(x = 1, y = 2, radius = 1, group_var = TRUE, group_prefix = 1)
    Condition
      [1m[33mError[39m in `circle_data()`:[22m
      [1m[22m[33m![39m `group_prefix` must be of class [1m[33m<character>[39m[22m
      [31mx[39m `group_prefix` is of class [1m[34m<numeric>[39m[22m
      [36mi[39m Check the `group_prefix` variable

# User-defined group prefix gives a warning if group var is FALSE [ansi]

    Code
      circle_data(0, 0, 3, group_prefix = "box")
    Message
      [1m[22m[1m[31mWarning:[39m[22m
      [36mi[39m You have provided a custom `group_prefix` of: [1m[33m"box"[39m[22m
      [33m![39m But `group_var` is `FALSE`
      > Did you mean to set `group_var = TRUE`?
    Output
      [90m# A tibble: 100 x 2[39m
             x     y
         [3m[90m<dbl>[39m[23m [3m[90m<dbl>[39m[23m
      [90m 1[39m  3    0    
      [90m 2[39m  2.99 0.190
      [90m 3[39m  2.98 0.380
      [90m 4[39m  2.95 0.568
      [90m 5[39m  2.90 0.753
      [90m 6[39m  2.85 0.936
      [90m 7[39m  2.79 1.11 
      [90m 8[39m  2.71 1.29 
      [90m 9[39m  2.62 1.46 
      [90m10[39m  2.52 1.62 
      [90m# i 90 more rows[39m

