# dataframe missing [ansi]

    Code
      rotator()
    Error <rlang_error>
      [1m[22m[31mx[39m `data` is [1m[31mmissing[39m[22m
      [33m![39m `data` should be a [1m[36mdataframe[39m[22m or [1m[36mtibble[39m[22m

# y var missing [ansi]

    Code
      rotator(original_square, x = x)
    Error <rlang_error>
      [1m[22m[31mx[39m 1 variable is [1m[31mmissing:[39m[22m
      [33m![39m `y` should be [34m<numeric>[39m variable in a [1m[36mdataframe[39m[22m or [1m[36mtibble[39m[22m

# x var missing [ansi]

    Code
      rotator(original_square, y = y)
    Error <rlang_error>
      [1m[22m[31mx[39m 1 variable is [1m[31mmissing:[39m[22m
      [33m![39m `x` should be [34m<numeric>[39m variable in a [1m[36mdataframe[39m[22m or [1m[36mtibble[39m[22m

# Test to ensure that error is thrown when dataframe is not input [ansi]

    Code
      nums <- 1:5
      rotator(nums)
    Error <rlang_error>
      [1m[22m[31mx[39m `data` is [1m[34m<integer>[39m[22m
      [33m![39m `data` should be a [1m[36mdataframe[39m[22m or [1m[36mtibble[39m[22m

