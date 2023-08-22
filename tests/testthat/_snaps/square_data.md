# Test that missing x throws an error [ansi]

    Code
      square_data(y = 0, size = 10)
    Error <rlang_error>
      [1m[22m`x` is [1m[31mmissing[39m[22m
      [31mx[39m `x` is [1m[36mrequired[39m[22m and should be a numeric value with a length of 1
      [36mi[39m Check the `x` variable

# Test that missing y throws an error [ansi]

    Code
      square_data(x = 0, size = 10)
    Error <rlang_error>
      [1m[22m`y` is [1m[31mmissing[39m[22m
      [31mx[39m `y` is [1m[36mrequired[39m[22m and should be a numeric value with a length of 1
      [36mi[39m Check the `y` variable

# Test that non-numeric x throws an error [ansi]

    Code
      square_data(x = "0", y = 0, size = 10)
    Error <rlang_error>
      [1m[22m`x` must be of class [1m[33m<numeric>[39m[22m
      [31mx[39m `x` is of class [1m[34m<character>[39m[22m
      [36mi[39m Check the `x` variable

# Test that non-numeric y throws an error [ansi]

    Code
      square_data(x = 0, y = "0", size = 10)
    Error <rlang_error>
      [1m[22m`y` must be of class [1m[33m<numeric>[39m[22m
      [31mx[39m `y` is of class [1m[34m<character>[39m[22m
      [36mi[39m Check the `y` variable

# Test that invalid size throws an error [ansi]

    Code
      square_data(x = 0, y = 0, size = 0)
    Error <rlang_error>
      [1m[22m`size` must be [1m[33mgreater than 0[39m[22m
      [31mx[39m `size` is [1m[31m0[39m[22m
      [36mi[39m Check the `size` variable

---

    Code
      square_data(x = 0, y = 0, size = -10)
    Error <rlang_error>
      [1m[22m`size` must be [1m[33mgreater than 0[39m[22m
      [31mx[39m `size` is [1m[31m-10[39m[22m
      [36mi[39m Check the `size` variable

# Test that invalid x arg length throws an error [ansi]

    Code
      square_data(x = 1:3, y = 0, size = 5)
    Error <rlang_error>
      [1m[22mAll arguments must have a [1m[33mlength of 1[39m[22m
      [31mx[39m `x` has a length of [1m[31m3[39m[22m
      [36mi[39m Check the `x` variable

# Test that invalid y arg length throws an error [ansi]

    Code
      square_data(x = 0, y = 1:3, size = 5)
    Error <rlang_error>
      [1m[22mAll arguments must have a [1m[33mlength of 1[39m[22m
      [31mx[39m `y` has a length of [1m[31m3[39m[22m
      [36mi[39m Check the `y` variable

# Test that invalid size arg length throws an error [ansi]

    Code
      square_data(x = 0, y = 0, size = 1:5)
    Error <rlang_error>
      [1m[22mAll arguments must have a [1m[33mlength of 1[39m[22m
      [31mx[39m `size` has a length of [1m[31m5[39m[22m
      [36mi[39m Check the `size` variable

# Test that invalid color arg length throws an error [ansi]

    Code
      square_data(x = 0, y = 0, size = 1, color = c("#000000", "#333333"))
    Error <rlang_error>
      [1m[22mAll arguments must have a [1m[33mlength of 1[39m[22m
      [31mx[39m `color` has a length of [1m[31m2[39m[22m
      [36mi[39m Check the `color` variable

# Test that invalid fill arg length throws an error [ansi]

    Code
      square_data(x = 0, y = 0, size = 1, fill = c("#000000", "#333333"))
    Error <rlang_error>
      [1m[22mAll arguments must have a [1m[33mlength of 1[39m[22m
      [31mx[39m `fill` has a length of [1m[31m2[39m[22m
      [36mi[39m Check the `fill` variable

# Test that multiple invalid arg lengths throws an error [ansi]

    Code
      square_data(x = 1:3, y = 2:5, size = 1)
    Error <rlang_error>
      [1m[22mAll arguments must have a [1m[33mlength of 1[39m[22m
      [31mx[39m `x` has a length of [1m[31m3[39m[22m and `y` has a length of [1m[31m4[39m[22m
      [36mi[39m Check the `x` and `y` variables

# Test that invalid colors throws an error [ansi]

    Code
      square_data(x = 1, y = 2, size = 1, color = "blAck")
    Error <rlang_error>
      [1m[22m`color` is [1m[31minvalid[39m[22m
      [31mx[39m `color` must be a valid: [1m[36m`r` color from `colors()`[39m[22m or a valid 6 digit [1m[36mhexadecimal webcolor[39m[22m
      [36mi[39m `blAck` is an [1m[33minvalid color[39m[22m

# Test that invalid fills throws an error [ansi]

    Code
      square_data(x = 1, y = 2, size = 1, fill = "blAck")
    Error <rlang_error>
      [1m[22m`fill` is [1m[31minvalid[39m[22m
      [31mx[39m `fill` must be a valid: [1m[36m`r` color from `colors()`[39m[22m or a valid 6 digit [1m[36mhexadecimal webcolor[39m[22m
      [36mi[39m `blAck` is an [1m[33minvalid color[39m[22m

# Numeric argument check [ansi]

    Code
      square_data(x = 0, y = 0, size = 5, n_points = "500")
    Error <rlang_error>
      [1m[22m`n_points` must be of class [1m[33m<numeric>[39m[22m
      [31mx[39m `n_points` is of class [1m[34m<character>[39m[22m
      [36mi[39m Check the `n_points` variable

---

    Code
      square_data(x = 0, y = 0, size = 5, n_points = 1)
    Error <rlang_error>
      [1m[22m`n_points` must be [1m[33mgreater than or equal to 4[39m[22m
      [31mx[39m `n_points` is [1m[31m1[39m[22m
      [36mi[39m Check the `n_points` variable

# Non character group prefix throws an error [ansi]

    Code
      square_data(x = 1, y = 2, size = 1, group_var = TRUE, group_prefix = 1)
    Error <rlang_error>
      [1m[22m`group_prefix` must be of class [1m[33m<character>[39m[22m
      [31mx[39m `group_prefix` is of class [1m[34m<numeric>[39m[22m
      [36mi[39m Check the `group_prefix` variable

# User-defined group prefix gives a warning if group var is FALSE [ansi]

    Code
      square_data(0, 0, 3, group_prefix = "box")
    Message <rlang_message>
      [1m[22m[1m[31mWarning:[39m[22m
      [36mi[39m You have provided a custom `group_prefix` of: [1m[33m"box"[39m[22m
      [33m![39m But `group_var` is `FALSE`
      > Did you mean to set `group_var = TRUE`?
    Output
      [90m# A tibble: 175 x 2[39m
             x     y
         [3m[90m<dbl>[39m[23m [3m[90m<dbl>[39m[23m
      [90m 1[39m 0         0
      [90m 2[39m 0.125     0
      [90m 3[39m 0.25      0
      [90m 4[39m 0.375     0
      [90m 5[39m 0.5       0
      [90m 6[39m 0.625     0
      [90m 7[39m 0.75      0
      [90m 8[39m 0.875     0
      [90m 9[39m 1         0
      [90m10[39m 1.12      0
      [90m# i 165 more rows[39m

