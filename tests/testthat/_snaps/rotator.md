# dataframe missing [ansi]

    Code
      rotator()
    Condition
      [1m[33mError[39m in `rotator()`:[22m
      [33m![39m argument "data" is missing, with no default

# y var missing [ansi]

    Code
      rotator(original_square, x = x)
    Condition
      [1m[33mError[39m in `dplyr::pull()`:[22m
      [33m![39m Can't extract columns past the end.
      [1m[22m[36mi[39m Location 1 doesn't exist.
      [36mi[39m There are only 0 columns.

# x var missing [ansi]

    Code
      rotator(original_square, y = y)
    Condition
      [1m[33mError[39m in `dplyr::pull()`:[22m
      [33m![39m Can't extract columns past the end.
      [1m[22m[36mi[39m Location 1 doesn't exist.
      [36mi[39m There are only 0 columns.

# Test to ensure that error is thrown when dataframe is not input [ansi]

    Code
      nums <- 1:5
      rotator(nums)
    Condition
      [1m[33mError[39m in `UseMethod()`:[22m
      [33m![39m no applicable method for 'select' applied to an object of class "c('integer', 'numeric')"

# Test the correct error is thrown when anchor is not the right class [ansi]

    Code
      rotator(original_square, x, y, anchor = TRUE)
    Condition
      [1m[33mError[39m in `rotator()`:[22m
      [1m[22m[31mx[39m `anchor` must be of class [1m[31m<character> -OR- <numeric>[39m[22m
      [33m![39m The input you've supplied, `anchor`, is of class [1m[33m<logical>[39m[22m
      [36mi[39m [1m[36mCheck the `anchor`[39m[22m input.

# Test the correct error is thrown when numeric anchor is not the right legnth [ansi]

    Code
      rotator(original_square, x, y, anchor = c(0, 2, 3))
    Condition
      [1m[33mError[39m in `rotator()`:[22m
      [1m[22m[31mx[39m `anchor` must [1m[31mbe of length 2[39m[22m
      [33m![39m The input you've supplied, `anchor`, is of length [1m[33m3[39m[22m
      [36mi[39m [1m[36mCheck the `anchor`[39m[22m input.

