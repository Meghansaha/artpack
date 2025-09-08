# Argument length checks [ansi]

    Code
      wave_data(start = 1:3, end = 0, size = 1, type = c("sin", "cos"), orientation = "horizontal",
      freq = 3, n_points = 500, color = NULL, fill = NULL, group_var = FALSE, dampen = NULL,
      amplify = NULL)
    Condition
      [1m[33mError[39m in `wave_data()`:[22m
      [1m[22m[33m![39m All arguments must have a [1m[33mlength of 1[39m[22m
      [31mx[39m `start` has a length of [1m[31m3[39m[22m and `type` has a length of [1m[31m2[39m[22m
      [36mi[39m Check the `start` and `type` variables

# Numeric argument check [ansi]

    Code
      wave_data(start = 1, end = 0, size = "big", freq = "a little", n_points = 500,
        color = NULL, fill = NULL, dampen = NULL, amplify = "bigggg")
    Condition
      [1m[33mError[39m in `wave_data()`:[22m
      [1m[22m[33m![39m The `size`, `freq`, and `amplify` arguments must have a class of [1m[33m<numeric>[39m[22m
      [31mx[39m `size` has a class of [1m[34m<character>[39m[22m, `freq` has a class of [1m[34m<character>[39m[22m, and `amplify` has a class of [1m[34m<character>[39m[22m
      [36mi[39m Check the `size`, `freq`, and `amplify` variables

---

    Code
      wave_data(start = 1, end = 0, size = -5, freq = -3, n_points = 500, color = NULL,
        fill = NULL, dampen = NULL, amplify = -9)
    Condition
      [1m[33mError[39m in `wave_data()`:[22m
      [1m[22m[33m![39m The `size`, `freq`, and `amplify` arguments must be [1m[33mgreater than zero[39m[22m
      [31mx[39m `size` has a value of [1m[31m-5[39m[22m, `freq` has a value of [1m[31m-3[39m[22m, and `amplify` has a value of [1m[31m-9[39m[22m
      [36mi[39m Check the `size`, `freq`, and `amplify` variables

# Character argument check [ansi]

    Code
      wave_data(start = 1, end = 0, size = 2, freq = 5, n_points = 500, color = 0,
        fill = "#111111", dampen = NULL, amplify = 1)
    Condition
      [1m[33mError[39m in `wave_data()`:[22m
      [1m[22m[33m![39m The `color` argument must have a class of [1m[33m<character>[39m[22m
      [31mx[39m `color` has a class of [1m[34m<numeric>[39m[22m
      [36mi[39m Check the `color` variable

# Color checks [ansi]

    Code
      wave_data(start = 1, end = 0, color = "#000")
    Condition
      [1m[33mError[39m in `wave_data()`:[22m
      [1m[22m[31mx[39m `color` must contain [1m[31mvalid 6-digit hexadecimal colors[39m[22m or valid color names found in
      [1m[31m`grDevices::colors()`[39m[22m
      [33m![39m The input you've supplied, `color`, contains [1m[33m"#000"[39m[22m
      [36mi[39m [1m[36mCheck the `color`[39m[22m input.

# Fill checks [ansi]

    Code
      wave_data(start = 1, end = 0, fill = "#000")
    Condition
      [1m[33mError[39m in `r_color | hex_color`:[22m
      [33m![39m operations are possible only for numeric, logical or complex types

# Wave type checks [ansi]

    Code
      wave_data(start = 1, end = 0, type = "COS")
    Condition
      [1m[33mError[39m in `wave_data()`:[22m
      [1m[22m[33m![39m `type` must be a string value of [1m[33m"sin"[39m[22m or [1m[33m"cos"[39m[22m
      [31mx[39m `type` is of value: [1m[31mCOS[39m[22m
      [36mi[39m Check the `type` variable

---

    Code
      wave_data(start = 1, end = 0, orientation = "verticall")
    Condition
      [1m[33mError[39m in `wave_data()`:[22m
      [1m[22m[33m![39m `orientation` must be a string value of [1m[33m"horizontal"[39m[22m or [1m[33m"vertical"[39m[22m
      [31mx[39m `orientation` is of value: [1m[31mverticall[39m[22m
      [36mi[39m Check the `orientation` variable

---

    Code
      wave_data(start = 1, end = 0, group_var = "TRUE")
    Condition
      [1m[33mError[39m in `r_color | hex_color`:[22m
      [33m![39m operations are possible only for numeric, logical or complex types

---

    Code
      wave_data(start = 1, end = 0, group_var = "TRUE")
    Condition
      [1m[33mError[39m in `r_color | hex_color`:[22m
      [33m![39m operations are possible only for numeric, logical or complex types

