# start_n should be present [ansi]

    Code
      seq_bounce(end_n = 10, length = 30, by = 0.247)
    Condition
      [1m[33mError[39m in `seq_bounce()`:[22m
      [1m[22m[31mx[39m `start_n` must be provided [1m[31mand not missing nor `NULL`[39m[22m
      [33m![39m The input you've supplied, `start_n`, is [1m[33mmissing or `NULL`[39m[22m
      [36mi[39m [1m[36mCheck the `start_n`[39m[22m input.

# end_n should be present [ansi]

    Code
      seq_bounce(start_n = 0, length = 30, by = 0.247)
    Condition
      [1m[33mError[39m in `seq_bounce()`:[22m
      [1m[22m[31mx[39m `end_n` must be provided [1m[31mand not missing nor `NULL`[39m[22m
      [33m![39m The input you've supplied, `end_n`, is [1m[33mmissing or `NULL`[39m[22m
      [36mi[39m [1m[36mCheck the `end_n`[39m[22m input.

# length should be present [ansi]

    Code
      seq_bounce(start_n = 0, end_n = 10, by = 0.247)
    Condition
      [1m[33mError[39m in `seq_bounce()`:[22m
      [1m[22m[31mx[39m `length` must be provided [1m[31mand not missing nor `NULL`[39m[22m
      [33m![39m The input you've supplied, `length`, is [1m[33mmissing or `NULL`[39m[22m
      [36mi[39m [1m[36mCheck the `length`[39m[22m input.

# by should not be NULL [ansi]

    Code
      seq_bounce(start_n = 0, end_n = 10, length = 30, by = NULL)
    Condition
      [1m[33mError[39m in `seq_bounce()`:[22m
      [1m[22m[31mx[39m `by` must be provided [1m[31mand not missing nor `NULL`[39m[22m
      [33m![39m The input you've supplied, `by`, is [1m[33mmissing or `NULL`[39m[22m
      [36mi[39m [1m[36mCheck the `by`[39m[22m input.

# start_n should be of length 1 [ansi]

    Code
      seq_bounce(start_n = c(0, 4), end_n = 10, length = 30, by = 0.247)
    Condition
      [1m[33mError[39m in `seq_bounce()`:[22m
      [1m[22m[31mx[39m `start_n` must [1m[31mbe of length 1[39m[22m
      [33m![39m The input you've supplied, `start_n`, is of length [1m[33m2[39m[22m
      [36mi[39m [1m[36mCheck the `start_n`[39m[22m input.

# end_n should be of length 1 [ansi]

    Code
      seq_bounce(start_n = 0, end_n = c(10, 20, 30), length = 30, by = 0.247)
    Condition
      [1m[33mError[39m in `seq_bounce()`:[22m
      [1m[22m[31mx[39m `end_n` must [1m[31mbe of length 1[39m[22m
      [33m![39m The input you've supplied, `end_n`, is of length [1m[33m3[39m[22m
      [36mi[39m [1m[36mCheck the `end_n`[39m[22m input.

# length should be of length 1 [ansi]

    Code
      seq_bounce(start_n = 0, end_n = 10, length = c(30, 89, 90, 120), by = 0.247)
    Condition
      [1m[33mError[39m in `seq_bounce()`:[22m
      [1m[22m[31mx[39m `length` must [1m[31mbe of length 1[39m[22m
      [33m![39m The input you've supplied, `length`, is of length [1m[33m4[39m[22m
      [36mi[39m [1m[36mCheck the `length`[39m[22m input.

# by should be of length 1 [ansi]

    Code
      seq_bounce(start_n = 0, end_n = 10, length = 30, by = c(1:90))
    Condition
      [1m[33mError[39m in `seq_bounce()`:[22m
      [1m[22m[31mx[39m `by` must [1m[31mbe of length 1[39m[22m
      [33m![39m The input you've supplied, `by`, is of length [1m[33m90[39m[22m
      [36mi[39m [1m[36mCheck the `by`[39m[22m input.

# start_n should be numeric [ansi]

    Code
      seq_bounce(start_n = "0", end_n = 10, length = 30, by = 0.247)
    Condition
      [1m[33mError[39m in `seq_bounce()`:[22m
      [1m[22m[31mx[39m `start_n` must be of class [1m[31m<numeric>[39m[22m
      [33m![39m The input you've supplied, `start_n`, is of class [1m[33m<character>[39m[22m
      [36mi[39m [1m[36mCheck the `start_n`[39m[22m input.

# end_n should be numeric [ansi]

    Code
      seq_bounce(start_n = 0, end_n = list(10), length = 30, by = 0.247)
    Condition
      [1m[33mError[39m in `seq_bounce()`:[22m
      [1m[22m[31mx[39m `end_n` must be of class [1m[31m<numeric>[39m[22m
      [33m![39m The input you've supplied, `end_n`, is of class [1m[33m<list>[39m[22m
      [36mi[39m [1m[36mCheck the `end_n`[39m[22m input.

# length should be numeric [ansi]

    Code
      seq_bounce(start_n = 0, end_n = 10, length = "30", by = 0.247)
    Condition
      [1m[33mError[39m in `seq_bounce()`:[22m
      [1m[22m[31mx[39m `length` must be of class [1m[31m<numeric>[39m[22m
      [33m![39m The input you've supplied, `length`, is of class [1m[33m<character>[39m[22m
      [36mi[39m [1m[36mCheck the `length`[39m[22m input.

# by should be numeric [ansi]

    Code
      seq_bounce(start_n = 0, end_n = 10, length = 30, by = list(0.247))
    Condition
      [1m[33mError[39m in `seq_bounce()`:[22m
      [1m[22m[31mx[39m `by` must be of class [1m[31m<numeric>[39m[22m
      [33m![39m The input you've supplied, `by`, is of class [1m[33m<list>[39m[22m
      [36mi[39m [1m[36mCheck the `by`[39m[22m input.

# length should be positive [ansi]

    Code
      seq_bounce(start_n = 0, end_n = 10, length = -30)
    Condition
      [1m[33mError[39m in `seq_bounce()`:[22m
      [1m[22m[31mx[39m `length` must be a positive numeric value [1m[31mnot negative nor zero[39m[22m
      [33m![39m The input you've supplied, `length`, is [1m[33m-30[39m[22m
      [36mi[39m [1m[36mCheck the `length`[39m[22m input.

# length should be an integer [ansi]

    Code
      seq_bounce(start_n = 0, end_n = 10, length = 30.5)
    Condition
      [1m[33mError[39m in `seq_bounce()`:[22m
      [1m[22m[31mx[39m `length` must be a numeric integer; [1m[31mnot a float (number with decimals)[39m[22m
      [33m![39m The input you've supplied, `length`, is [1m[33m30.5[39m[22m
      [36mi[39m [1m[36mCheck the `length`[39m[22m input.

---

    Code
      seq_bounce(start_n = 0, end_n = 10, length = 30, by = -4)
    Condition
      [1m[33mError[39m in `seq_bounce()`:[22m
      [1m[22m[31mx[39m `by` must be a positive numeric value [1m[31mnot negative nor zero[39m[22m
      [33m![39m The input you've supplied, `by`, is [1m[33m-4[39m[22m
      [36mi[39m [1m[36mCheck the `by`[39m[22m input.

# start_n should be < than end_n [ansi]

    Code
      seq_bounce(start_n = 10, end_n = 0, length = 30)
    Condition
      [1m[33mError[39m in `seq_bounce()`:[22m
      [1m[22m[31mx[39m `start_n` must be less than `end_n` [1m[31mnot more than or equal to `end_n`[39m[22m
      [33m![39m You've supplied: [1m[33m`start_n` == 10 and `end_n` == 0[39m[22m
      [36mi[39m [1m[36mCheck the `start_n` and `end_n`[39m[22m inputs.

