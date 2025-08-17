# data should be present [ansi]

    Code
      x <- 1:10
      y <- 1:10
      resizer(x, y, factor = 2)
    Condition
      [1m[33mError[39m in `resizer()`:[22m
      [1m[22m[31mx[39m `data` must be present and of class [1m[31m<data.frame>[39m[22m
      [33m![39m The input you've supplied, `x`, is of class [1m[33m<integer>[39m[22m
      [36mi[39m [1m[36mCheck the `data`[39m[22m input.

# x should be present [ansi]

    Code
      resizer(df_square, y = y, factor = 2)
    Condition
      [1m[33mError[39m in `resizer()`:[22m
      [1m[22m[31mx[39m `x` must be provided [1m[31mand not missing nor `NULL`[39m[22m
      [33m![39m The input you've supplied, `x`, is [1m[33mmissing or `NULL`[39m[22m
      [36mi[39m [1m[36mCheck the `x`[39m[22m input.

# y should be present [ansi]

    Code
      resizer(df_square, x, factor = 2)
    Condition
      [1m[33mError[39m in `resizer()`:[22m
      [1m[22m[31mx[39m `y` must be provided [1m[31mand not missing nor `NULL`[39m[22m
      [33m![39m The input you've supplied, `y`, is [1m[33mmissing or `NULL`[39m[22m
      [36mi[39m [1m[36mCheck the `y`[39m[22m input.

# factor should be present [ansi]

    Code
      resizer(df_square, x, y)
    Condition
      [1m[33mError[39m in `resizer()`:[22m
      [1m[22m[31mx[39m `factor` must be provided [1m[31mand not missing nor `NULL`[39m[22m
      [33m![39m The input you've supplied, `factor`, is [1m[33mmissing or `NULL`[39m[22m
      [36mi[39m [1m[36mCheck the `factor`[39m[22m input.

# x_anchor should be of length 1 [ansi]

    Code
      resizer(df_square, x, y, x_anchor = 0:5, factor = 2)
    Condition
      [1m[33mError[39m in `resizer()`:[22m
      [1m[22m[31mx[39m `x_anchor` must be of length [1m[31m1[39m[22m
      [33m![39m The input you've supplied, `x_anchor`, is of length [1m[33m6[39m[22m
      [36mi[39m [1m[36mCheck the `x_anchor`[39m[22m input.

# y_anchor should be of length 1 [ansi]

    Code
      resizer(df_square, x, y, y_anchor = 0:5, factor = 2)
    Condition
      [1m[33mError[39m in `resizer()`:[22m
      [1m[22m[31mx[39m `y_anchor` must be of length [1m[31m1[39m[22m
      [33m![39m The input you've supplied, `y_anchor`, is of length [1m[33m6[39m[22m
      [36mi[39m [1m[36mCheck the `y_anchor`[39m[22m input.

# factor should be of length 1 [ansi]

    Code
      resizer(df_square, x, y, factor = 1:20)
    Condition
      [1m[33mError[39m in `resizer()`:[22m
      [1m[22m[31mx[39m `factor` must be of length [1m[31m1[39m[22m
      [33m![39m The input you've supplied, `factor`, is of length [1m[33m20[39m[22m
      [36mi[39m [1m[36mCheck the `factor`[39m[22m input.

# direction should be of length 1 [ansi]

    Code
      resizer(df_square, x, y, factor = 2, direction = c("up", "up"))
    Condition
      [1m[33mError[39m in `resizer()`:[22m
      [1m[22m[31mx[39m `direction` must be of length [1m[31m1[39m[22m
      [33m![39m The input you've supplied, `direction`, is of length [1m[33m2[39m[22m
      [36mi[39m [1m[36mCheck the `direction`[39m[22m input.

# data should be a data frame [ansi]

    Code
      resizer(as.list(df_square), x, y, factor = 2)
    Condition
      [1m[33mError[39m in `resizer()`:[22m
      [1m[22m[31mx[39m `data` must be present and of class [1m[31m<data.frame>[39m[22m
      [33m![39m The input you've supplied, `as.list(df_square)`, is of class [1m[33m<list>[39m[22m
      [36mi[39m [1m[36mCheck the `data`[39m[22m input.

# x should be numeric [ansi]

    Code
      resizer(df_square, x_char, y, factor = 2)
    Condition
      [1m[33mError[39m in `resizer()`:[22m
      [1m[22m[31mx[39m `x` must be of class [1m[31m<numeric>[39m[22m
      [33m![39m The input you've supplied, `x`, is of class [1m[33m<character>[39m[22m
      [36mi[39m [1m[36mCheck the `x`[39m[22m input.

# y should be numeric [ansi]

    Code
      resizer(df_square, x, y_char, factor = 2)
    Condition
      [1m[33mError[39m in `resizer()`:[22m
      [1m[22m[31mx[39m `y` must be of class [1m[31m<numeric>[39m[22m
      [33m![39m The input you've supplied, `y`, is of class [1m[33m<character>[39m[22m
      [36mi[39m [1m[36mCheck the `y`[39m[22m input.

# factor should be numeric [ansi]

    Code
      resizer(df_square, y, y, factor = "chonk boi")
    Condition
      [1m[33mError[39m in `resizer()`:[22m
      [1m[22m[31mx[39m `factor` must be of class [1m[31m<numeric>[39m[22m
      [33m![39m The input you've supplied, `factor`, is of class [1m[33m<character>[39m[22m
      [36mi[39m [1m[36mCheck the `factor`[39m[22m input.

# drop should be logical [ansi]

    Code
      resizer(df_square, y, y, factor = 5, drop = "FALSE")
    Condition
      [1m[33mError[39m in `resizer()`:[22m
      [1m[22m[31mx[39m `drop` must be of class [1m[31m<logical>[39m[22m
      [33m![39m The input you've supplied, `drop`, is of class [1m[33m<character>[39m[22m
      [36mi[39m [1m[36mCheck the `drop`[39m[22m input.

# factor should be positive numeric - zero [ansi]

    Code
      resizer(df_square, y, y, factor = 0)
    Condition
      [1m[33mError[39m in `resizer()`:[22m
      [1m[22m[31mx[39m `factor` must be a positive numeric value [1m[31mnot negative nor zero[39m[22m
      [33m![39m The input you've supplied, `factor`, is [1m[33m0[39m[22m
      [36mi[39m [1m[36mCheck the `factor`[39m[22m input.

# factor should be positive numeric - negative [ansi]

    Code
      resizer(df_square, y, y, factor = -10)
    Condition
      [1m[33mError[39m in `resizer()`:[22m
      [1m[22m[31mx[39m `factor` must be a positive numeric value [1m[31mnot negative nor zero[39m[22m
      [33m![39m The input you've supplied, `factor`, is [1m[33m-10[39m[22m
      [36mi[39m [1m[36mCheck the `factor`[39m[22m input.

# direction should be an expected value [ansi]

    Code
      resizer(df_square, y, y, factor = 10, direction = "side")
    Condition
      [1m[33mError[39m in `resizer()`:[22m
      [1m[22m[31mx[39m `direction` must be an accepted value: [1m[3m[31m"up" or "down"[39m[23m[22m
      [33m![39m The input you've supplied, `direction`, is [1m[33m"side"[39m[22m
      [36mi[39m [1m[36mCheck the `direction`[39m[22m input.

# external anchor gives a warning as expected [ansi]

    Code
      resizer(df_square, x, y, x_anchor = 5, y_anchor = 5, factor = 2, drop = TRUE,
        direction = "down")
    Message
      [1m[22m[33m![39m The anchor point you've supplied (5, 5) [1m[31mis not found in your data.[39m[22m
      [36mi[39m The data will be [1m[33mscaled relative to this external point[39m[22m
    Output
      [90m# A tibble: 5 x 2[39m
            x     y
        [3m[90m<dbl>[39m[23m [3m[90m<dbl>[39m[23m
      [90m1[39m   2.5   2.5
      [90m2[39m   3     2.5
      [90m3[39m   3     3  
      [90m4[39m   2.5   3  
      [90m5[39m   2.5   2.5

