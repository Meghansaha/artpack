# Missing data not accepted [ansi]

    Code
      group_slice()
    Condition
      [1m[33mError[39m in `group_slice()`:[22m
      [1m[22m[31mx[39m `data` is [1m[31mmissing[39m[22m
      [33m![39m `data` should be a [1m[36mdataframe[39m[22m or [1m[36mtibble[39m[22m

# Data must be dataframe or tibble [ansi]

    Code
      vec_data <- 1:10
      group_slice(vec_data)
    Condition
      [1m[33mError[39m in `group_slice()`:[22m
      [1m[22m[31mx[39m `data` is [1m[34m<integer>[39m[22m
      [33m![39m `data` should be a [1m[36mdataframe[39m[22m or [1m[36mtibble[39m[22m

# Data must have at least 1 variable [ansi]

    Code
      df_data <- data.frame()
      group_slice(df_data)
    Condition
      [1m[33mError[39m in `group_slice()`:[22m
      [1m[22m[31mx[39m `data` has [1m[31m0 variables[39m[22m .
      [33m![39m `data` should be a dataframe or tibble with at least [1m[36m1 variable[39m[22m

# Group variable must be present [ansi]

    Code
      df_data <- data.frame(x = 1:5, y = 6:10)
      group_slice(df_data)
    Condition
      [1m[33mError[39m in `group_slice()`:[22m
      [1m[22m[31mx[39m `group` variable is [1m[31mmissing:[39m[22m
      [33m![39m `group` variable should be present in a [1m[36mdataframe[39m[22m or [1m[36mtibble[39m[22m

# group must be of length 1 [ansi]

    Code
      df_data <- data.frame(x = 1:5, y = 6:10, group = 1:5)
      group_slice(df_data, group = c(group, x), n = 5)
    Condition
      [1m[33mError[39m in `group_slice()`:[22m
      [1m[22m[33m![39m `group` must be [1m[33m1[39m[22m column from `data`
      [31mx[39m You've supplied [1m[31m2[39m[22m columns to the `group` argument
      [36mi[39m [1m[36mCheck the `group` value[39m[22m you've supplied.

# N must be of length 1 [ansi]

    Code
      df_data <- data.frame(x = 1:5, y = 6:10, group = 1:5)
      group_slice(df_data, group = group, n = 1:3)
    Condition
      [1m[33mError[39m in `group_slice()`:[22m
      [1m[22m[33m![39m `n` must be a length of [1m[33m1[39m[22m
      [31mx[39m The `n` object you've supplied has a length of [1m[31m3[39m[22m
      [36mi[39m [1m[36mCheck the `n` value[39m[22m you've supplied.

# n must be numeric [ansi]

    Code
      df_data <- data.frame(x = 1:5, y = 6:10, group = 1:5)
      group_slice(df_data, group = group, n = "five")
    Condition
      [1m[33mError[39m in `group_slice()`:[22m
      [1m[22m[33m![39m `n` must be a [1m[33mnumeric[39m[22m integer of length 1.
      [31mx[39m The class of the `n` object you've supplied is [1m[31m<character>[39m[22m
      [36mi[39m [1m[36mCheck the `n` object[39m[22m you've supplied.

# n must be positive [ansi]

    Code
      df_data <- data.frame(x = 1:5, y = 6:10, group = 1:5)
      group_slice(df_data, group = group, n = -3)
    Condition
      [1m[33mError[39m in `group_slice()`:[22m
      [1m[22m[33m![39m `n` must be a positive numeric integer that is [1m[33mgreater than 0[39m[22m
      [31mx[39m The `n` you've supplied is [1m[31m-3[39m[22m
      [36mi[39m [1m[36mCheck the `n` value[39m[22m you've supplied.

# n must be an integer [ansi]

    Code
      df_data <- data.frame(x = 1:5, y = 6:10, group = 1:5)
      group_slice(df_data, group = group, n = 0.3)
    Condition
      [1m[33mError[39m in `group_slice()`:[22m
      [1m[22m[33m![39m `n` must be a [1m[33mnumeric integer (no decimals)[39m[22m
      [31mx[39m The `n` object you've supplied is [1m[31m0.3[39m[22m
      [36mi[39m [1m[36mCheck the `n` value[39m[22m you've supplied.

# n must less than or equal to group n [ansi]

    Code
      df_data <- data.frame(x = 1:5, y = 6:10, group = 1:5)
      group_slice(df_data, group = group, n = 10)
    Condition
      [1m[33mWarning[39m:[22m
      [1m[22m[36mi[39m `n` should be a positive numeric value that is less than or equal to the [1m[33mamount of groups in the `group` variable[39m[22m
      [36mi[39m The `group` variable you've supplied has [1m[33m5[39m[22m groups but the `n` you've supplied is [1m[31m10[39m[22m
      [33m![39m The data could not be sliced
    Output
        x  y group
      1 1  6     1
      2 2  7     2
      3 3  8     3
      4 4  9     4
      5 5 10     5

---

    Code
      df_data <- data.frame(x = 1:5, y = 6:10, group = 1:5)
      group_slice(df_data, group = group, n = 10, position = "tail")
    Condition
      [1m[33mWarning[39m:[22m
      [1m[22m[36mi[39m `n` should be a positive numeric value that is less than or equal to the [1m[33mamount of groups in the `group` variable[39m[22m
      [36mi[39m The `group` variable you've supplied has [1m[33m5[39m[22m groups but the `n` you've supplied is [1m[31m10[39m[22m
      [33m![39m The data could not be sliced
    Output
        x  y group
      5 5 10     5
      4 4  9     4
      3 3  8     3
      2 2  7     2
      1 1  6     1

# prop must be numeric [ansi]

    Code
      df_data <- data.frame(x = 1:5, y = 6:10, group = 1:5)
      group_slice(df_data, group = group, prop = "20%")
    Condition
      [1m[33mError[39m in `group_slice()`:[22m
      [1m[22m[33m![39m `prop` must be a positive numeric value that is [1m[33mless than or equal to 1[39m[22m
      [31mx[39m The `prop` you've supplied is [1m[31m20%[39m[22m
      [36mi[39m [1m[36mCheck the `prop` value[39m[22m you've supplied.

# prop must be of length 1 [ansi]

    Code
      df_data <- data.frame(x = 1:5, y = 6:10, group = 1:5)
      group_slice(df_data, group = group, prop = 1:3)
    Condition
      [1m[33mError[39m in `group_slice()`:[22m
      [1m[22m[33m![39m `prop` must be a length of [1m[33m1[39m[22m
      [31mx[39m The `prop` object you've supplied has a length of [1m[31m3[39m[22m
      [36mi[39m [1m[36mCheck the `prop` value[39m[22m you've supplied.

# prop must be a valid percentage [ansi]

    Code
      df_data <- data.frame(x = 1:5, y = 6:10, group = 1:5)
      group_slice(df_data, group = group, prop = 1.5)
    Condition
      [1m[33mError[39m in `group_slice()`:[22m
      [1m[22m[33m![39m `prop` must be a positive numeric value that is [1m[33mless than or equal to 1[39m[22m
      [31mx[39m The `prop` you've supplied is [1m[31m1.5[39m[22m
      [36mi[39m [1m[36mCheck the `prop` value[39m[22m you've supplied.

# group_output must be a logical boolean [ansi]

    Code
      df_data <- data.frame(x = 1:5, y = 6:10, group = 1:5)
      group_slice(df_data, group = group, prop = 1, group_output = "FALSE")
    Condition
      [1m[33mError[39m in `group_slice()`:[22m
      [1m[22m[33m![39m `group_output` must be a logical boolean of [1m[33m`TRUE` or `FALSE`[39m[22m
      [31mx[39m The class of the `group_output` object you've supplied is [1m[31m<character>[39m[22m
      [36mi[39m [1m[36mCheck the `group_output` object[39m[22m you've supplied.

