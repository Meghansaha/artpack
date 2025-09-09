# color should be present [ansi]

    Code
      set_brightness(percentage = 0.1)
    Condition
      [1m[33mError[39m in `set_brightness()`:[22m
      [1m[22m[31mx[39m `color` must be provided [1m[31mand not missing nor `NULL`[39m[22m
      [33m![39m The input you've supplied, `color`, is [1m[33mmissing or `NULL`[39m[22m
      [36mi[39m [1m[36mCheck the `color`[39m[22m input.

# percentage should be present [ansi]

    Code
      set_brightness(color = "red")
    Condition
      [1m[33mError[39m in `set_brightness()`:[22m
      [1m[22m[31mx[39m `percentage` must be provided [1m[31mand not missing nor `NULL`[39m[22m
      [33m![39m The input you've supplied, `percentage`, is [1m[33mmissing or `NULL`[39m[22m
      [36mi[39m [1m[36mCheck the `percentage`[39m[22m input.

# color should be character [ansi]

    Code
      set_brightness(color = TRUE, percentage = 0.1)
    Condition
      [1m[33mError[39m in `set_brightness()`:[22m
      [1m[22m[31mx[39m `color` must be of class [1m[31m<character>[39m[22m
      [33m![39m The input you've supplied, `color`, is of class [1m[33m<logical>[39m[22m
      [36mi[39m [1m[36mCheck the `color`[39m[22m input.

# percentage should be numeric [ansi]

    Code
      set_brightness(color = "purple", percentage = "50%")
    Condition
      [1m[33mError[39m in `set_brightness()`:[22m
      [1m[22m[31mx[39m `percentage` must be of class [1m[31m<numeric>[39m[22m
      [33m![39m The input you've supplied, `percentage`, is of class [1m[33m<character>[39m[22m
      [36mi[39m [1m[36mCheck the `percentage`[39m[22m input.

# color should be valid [ansi]

    Code
      set_brightness(color = "blurple", percentage = 0.1)
    Condition
      [1m[33mError[39m in `set_brightness()`:[22m
      [1m[22m[31mx[39m `color` must contain [1m[31mvalid 6-digit hexadecimal colors[39m[22m or valid color names found in
      [1m[31m`grDevices::colors()`[39m[22m
      [33m![39m The input you've supplied, `color`, contains [1m[33m"blurple"[39m[22m
      [36mi[39m [1m[36mCheck the `color`[39m[22m input.

# color should be of length 1 [ansi]

    Code
      set_brightness(color = c("#151515", "#829819"), percentage = 0.1)
    Condition
      [1m[33mError[39m in `set_brightness()`:[22m
      [1m[22m[31mx[39m `color` must [1m[31mbe of length 1[39m[22m
      [33m![39m The input you've supplied, `color`, is of length [1m[33m2[39m[22m
      [36mi[39m [1m[36mCheck the `color`[39m[22m input.

# percentage should be of length 1 [ansi]

    Code
      set_brightness(color = "#151515", percentage = c(0.1, 0.6))
    Condition
      [1m[33mError[39m in `set_brightness()`:[22m
      [1m[22m[31mx[39m `percentage` must [1m[31mbe of length 1[39m[22m
      [33m![39m The input you've supplied, `percentage`, is of length [1m[33m2[39m[22m
      [36mi[39m [1m[36mCheck the `percentage`[39m[22m input.

# percentage should be positive [ansi]

    Code
      light <- -10
      set_brightness(color = "brown", percentage = light)
    Condition
      [1m[33mError[39m in `set_brightness()`:[22m
      [1m[22m[31mx[39m `percentage` must be a positive numeric value [1m[31mnot negative nor zero[39m[22m
      [33m![39m The input you've supplied, `percentage`, is [1m[33m-10[39m[22m
      [36mi[39m [1m[36mCheck the `percentage`[39m[22m input.

# percentage should be between 0 and 1 [ansi]

    Code
      set_brightness(color = "brown", percentage = 10)
    Condition
      [1m[33mError[39m in `set_brightness()`:[22m
      [1m[22m[31mx[39m `percentage` must be a numeric value between [1m[31m0 and 1[39m[22m
      [33m![39m The `percentage` input you've supplied is [1m[33m"10"[39m[22m
      [36mi[39m [1m[36mCheck the `10`[39m[22m input.

