# Argument length checks [ansi]

    Code
      wave_data(start = 1:3, end = 0, size = 1, type = c("sin", "cos"), orientation = "horizontal",
      freq = 3, n_points = 500, color = NULL, fill = NULL, group_var = FALSE, dampen = NULL,
      amplify = NULL)
    Error <rlang_error>
      [1m[22mAll arguments must have a [1m[33mlength of 1[39m[22m
      [31mx[39m `start` has a length of [1m[31m3[39m[22m and `type` has a length of [1m[31m2[39m[22m
      [36mi[39m Check the `start` and `type` variables

# Numeric argument check [ansi]

    Code
      wave_data(start = 1, end = 0, size = "big", freq = "a little", n_points = 500,
        color = NULL, fill = NULL, dampen = NULL, amplify = "bigggg")
    Error <rlang_error>
      [1m[22mThe `size`, `freq`, and `amplify` arguments must have a class of [1m[33m<numeric>[39m[22m
      [31mx[39m `size` has a class of [1m[34m<character>[39m[22m, `freq` has a class of [1m[34m<character>[39m[22m, and `amplify` has a class of [1m[34m<character>[39m[22m
      [36mi[39m Check the `size`, `freq`, and `amplify` variables

---

    Code
      wave_data(start = 1, end = 0, size = -5, freq = -3, n_points = 500, color = NULL,
        fill = NULL, dampen = NULL, amplify = -9)
    Error <rlang_error>
      [1m[22mThe `size`, `freq`, and `amplify` arguments must be [1m[33mgreater than zero[39m[22m
      [31mx[39m `size` has a value of [1m[31m-5[39m[22m, `freq` has a value of [1m[31m-3[39m[22m, and `amplify` has a value of [1m[31m-9[39m[22m
      [36mi[39m Check the `size`, `freq`, and `amplify` variables

# Character argument check [ansi]

    Code
      wave_data(start = 1, end = 0, size = 2, freq = 5, n_points = 500, color = 0,
        fill = "#111111", dampen = NULL, amplify = 1)
    Error <rlang_error>
      [1m[22mThe `color` argument must have a class of [1m[33m<character>[39m[22m
      [31mx[39m `color` has a class of [1m[34m<numeric>[39m[22m
      [36mi[39m Check the `color` variable

# Color checks [ansi]

    Code
      wave_data(start = 1, end = 0, color = "#000")
    Error <rlang_error>
      [1m[22m`color` is [1m[31minvalid[39m[22m
      [31mx[39m `color` must be a valid: [1m[36m`r` color from `colors()`[39m[22m or a valid 6 digit [1m[36mhexadecimal webcolor[39m[22m
      [36mi[39m `#000` is an [1m[33minvalid color[39m[22m

# Fill checks [ansi]

    Code
      wave_data(start = 1, end = 0, fill = "#000")
    Error <rlang_error>
      [1m[22m`fill` is [1m[31minvalid[39m[22m
      [31mx[39m `fill` must be a valid: [1m[36m`r` color from `colors()`[39m[22m or a valid 6 digit [1m[36mhexadecimal webcolor[39m[22m
      [36mi[39m `#000` is an [1m[33minvalid color[39m[22m

# Wave type checks [ansi]

    Code
      wave_data(start = 1, end = 0, type = "COS")
    Error <rlang_error>
      [1m[22m`type` must be a string value of [1m[33m"sin"[39m[22m or [1m[33m"cos"[39m[22m
      [31mx[39m `type` is of value: [1m[31mCOS[39m[22m
      [36mi[39m Check the `type` variable

---

    Code
      wave_data(start = 1, end = 0, orientation = "verticall")
    Error <rlang_error>
      [1m[22m`orientation` must be a string value of [1m[33m"horizontal"[39m[22m or [1m[33m"vertical"[39m[22m
      [31mx[39m `orientation` is of value: [1m[31mverticall[39m[22m
      [36mi[39m Check the `orientation` variable

---

    Code
      wave_data(start = 1, end = 0, group_var = "TRUE")
    Error <rlang_error>
      [1m[22m`group_var` is [1m[31minvalid[39m[22m
      [31mx[39m `group_var` must be of class [1m[36m<logical>[39m[22m
      [36mi[39m `group_var` is of class [1m[34m<character>[39m[22m

---

    Code
      wave_data(start = 1, end = 0, group_var = "TRUE")
    Error <rlang_error>
      [1m[22m`group_var` is [1m[31minvalid[39m[22m
      [31mx[39m `group_var` must be of class [1m[36m<logical>[39m[22m
      [36mi[39m `group_var` is of class [1m[34m<character>[39m[22m

