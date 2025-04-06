# Missing data not accepted [ansi]

    Code
      group_sample()
    Condition
      [1m[33mError[39m in `group_sample()`:[22m
      [1m[22m[31mx[39m `data` is [1m[31mmissing[39m[22m
      [33m![39m `data` should be a [1m[36mdataframe[39m[22m or [1m[36mtibble[39m[22m

# Data must be dataframe or tibble [ansi]

    Code
      vec_data <- 1:10
      group_sample(vec_data)
    Condition
      [1m[33mError[39m in `group_sample()`:[22m
      [1m[22m[31mx[39m `data` is [1m[34m<integer>[39m[22m
      [33m![39m `data` should be a [1m[36mdataframe[39m[22m or [1m[36mtibble[39m[22m

# Data must have at least 1 variable [ansi]

    Code
      df_data <- data.frame()
      group_sample(df_data)
    Condition
      [1m[33mError[39m in `group_sample()`:[22m
      [1m[22m[31mx[39m `data` has [1m[31m0 variables[39m[22m .
      [33m![39m `data` should be a dataframe or tibble with at least [1m[36m1 variable[39m[22m

# Group variable must be present [ansi]

    Code
      df_data <- data.frame(x = 1:5, y = 6:10)
      group_sample(df_data)
    Condition
      [1m[33mError[39m in `group_sample()`:[22m
      [1m[22m[31mx[39m `group` variable is [1m[31mmissing:[39m[22m
      [33m![39m `group` variable should be present in a [1m[36mdataframe[39m[22m or [1m[36mtibble[39m[22m

# group must be of length 1 [ansi]

    Code
      df_data <- data.frame(x = 1:5, y = 6:10, group = 1:5)
      group_sample(df_data, group = c(group, x), n = 5)
    Condition
      [1m[33mError[39m in `group_sample()`:[22m
      [1m[22m[33m![39m `group` must be [1m[33m1[39m[22m column from `data`
      [31mx[39m You've supplied [1m[31m2[39m[22m columns to the `group` argument
      [36mi[39m [1m[36mCheck the `group` value[39m[22m you've supplied.

# N must be of length 1 [ansi]

    Code
      df_data <- data.frame(x = 1:5, y = 6:10, group = 1:5)
      group_sample(df_data, group = group, n = 1:3)
    Condition
      [1m[33mError[39m in `group_sample()`:[22m
      [1m[22m[33m![39m `n` must be a length of [1m[33m1[39m[22m
      [31mx[39m The `n` object you've supplied has a length of [1m[31m3[39m[22m
      [36mi[39m [1m[36mCheck the `n` value[39m[22m you've supplied.

# n must be numeric [ansi]

    Code
      df_data <- data.frame(x = 1:5, y = 6:10, group = 1:5)
      group_sample(df_data, group = group, n = "five")
    Condition
      [1m[33mError[39m in `group_sample()`:[22m
      [1m[22m[33m![39m `n` must be a [1m[33mnumeric[39m[22m integer of length 1.
      [31mx[39m The class of the `n` object you've supplied is [1m[31m<character>[39m[22m
      [36mi[39m [1m[36mCheck the `n` object[39m[22m you've supplied.

# n must be positive [ansi]

    Code
      df_data <- data.frame(x = 1:5, y = 6:10, group = 1:5)
      group_sample(df_data, group = group, n = -3)
    Condition
      [1m[33mError[39m in `group_sample()`:[22m
      [1m[22m[33m![39m `n` must be a positive numeric integer that is [1m[33mgreater than 0[39m[22m
      [31mx[39m The `n` you've supplied is [1m[31m-3[39m[22m
      [36mi[39m [1m[36mCheck the `n` value[39m[22m you've supplied.

# n must be an integer [ansi]

    Code
      df_data <- data.frame(x = 1:5, y = 6:10, group = 1:5)
      group_sample(df_data, group = group, n = 0.3)
    Condition
      [1m[33mError[39m in `group_sample()`:[22m
      [1m[22m[33m![39m `n` must be a [1m[33mnumeric integer (no decimals)[39m[22m
      [31mx[39m The `n` object you've supplied is [1m[31m0.3[39m[22m
      [36mi[39m [1m[36mCheck the `n` value[39m[22m you've supplied.

# prop must be numeric [ansi]

    Code
      df_data <- data.frame(x = 1:5, y = 6:10, group = 1:5)
      group_sample(df_data, group = group, prop = "20%")
    Condition
      [1m[33mError[39m in `group_sample()`:[22m
      [1m[22m[33m![39m `prop` must be a positive numeric value that is [1m[33mless than or equal to 1[39m[22m
      [31mx[39m The `prop` you've supplied is [1m[31m20%[39m[22m
      [36mi[39m [1m[36mCheck the `prop` value[39m[22m you've supplied.

# prop must be of length 1 [ansi]

    Code
      df_data <- data.frame(x = 1:5, y = 6:10, group = 1:5)
      group_sample(df_data, group = group, prop = 1:3)
    Condition
      [1m[33mError[39m in `group_sample()`:[22m
      [1m[22m[33m![39m `prop` must be a length of [1m[33m1[39m[22m
      [31mx[39m The `prop` object you've supplied has a length of [1m[31m3[39m[22m
      [36mi[39m [1m[36mCheck the `prop` value[39m[22m you've supplied.

# prop must be a valid percentage [ansi]

    Code
      df_data <- data.frame(x = 1:5, y = 6:10, group = 1:5)
      group_sample(df_data, group = group, prop = 1.5)
    Condition
      [1m[33mError[39m in `group_sample()`:[22m
      [1m[22m[33m![39m `prop` must be a positive numeric value that is [1m[33mless than or equal to 1[39m[22m
      [31mx[39m The `prop` you've supplied is [1m[31m1.5[39m[22m
      [36mi[39m [1m[36mCheck the `prop` value[39m[22m you've supplied.

# prob must be the same length as the unique values in group [ansi]

    Code
      vec_bad_probs <- c(1, 0.2)
      df_data <- data.frame(x = 1:5, y = 6:10, group = 1:5)
      group_sample(df_data, group = group, prop = 1, prob = vec_bad_probs)
    Condition
      [1m[33mError[39m in `group_sample()`:[22m
      [1m[22m[33m![39m `prob` must be a vector of positive numeric values [1m[33mwith the same length as the total unique values in `group`[39m[22m
      [31mx[39m The length of `prob` you've supplied is [1m[31m2[39m[22m and the total unique values in `group` are [1m[31m5[39m[22m
      [36mi[39m [1m[36mCheck the `prob` object[39m[22m you've supplied.

# prob must be numeric values [ansi]

    Code
      vec_bad_probs <- c(".1", "two", ".3", "4", ".5")
      df_data <- data.frame(x = 1:5, y = 6:10, group = 1:5)
      group_sample(df_data, group = group, prop = 1, prob = vec_bad_probs)
    Condition
      [1m[33mError[39m in `group_sample()`:[22m
      [1m[22m[33m![39m `prob` must be a vector of [1m[33mpositive numeric values that are greater than zero[39m[22m
      [31mx[39m The class of the `prob` object that you've supplied is of [1m[31m<character>.[39m[22m [1m[33mNumeric[39m[22m is expected.
      [31mx[39m [1m[31mNegative[39m[22m values were detected in the `prob` object you've supplied. Only [1m[33mpositive[39m[22m values are accepted.
      [36mi[39m [1m[36mCheck the `prob` object[39m[22m you've supplied.

# prob must be positive values [ansi]

    Code
      vec_bad_probs <- c(0.1, -0.3, 0.2, 0.1, 0.8)
      df_data <- data.frame(x = 1:5, y = 6:10, group = 1:5)
      group_sample(df_data, group = group, prop = 1, prob = vec_bad_probs)
    Condition
      [1m[33mError[39m in `group_sample()`:[22m
      [1m[22m[33m![39m `prob` must be a vector of [1m[33mpositive numeric values that are greater than zero[39m[22m
      [31mx[39m [1m[31mNegative[39m[22m values were detected in the `prob` object you've supplied. Only [1m[33mpositive[39m[22m values are accepted.
      [36mi[39m [1m[36mCheck the `prob` object[39m[22m you've supplied.

# group_output must be a logical boolean [ansi]

    Code
      df_data <- data.frame(x = 1:5, y = 6:10, group = 1:5)
      group_sample(df_data, group = group, prop = 1, group_output = "FALSE")
    Condition
      [1m[33mError[39m in `group_sample()`:[22m
      [1m[22m[33m![39m `group_output` must be a logical boolean of [1m[33m`TRUE` or `FALSE`[39m[22m
      [31mx[39m The class of the `group_output` object you've supplied is [1m[31m<character>[39m[22m
      [36mi[39m [1m[36mCheck the `group_output` object[39m[22m you've supplied.

