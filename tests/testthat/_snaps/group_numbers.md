# Numbers only [ansi]

    Code
      group_numbers(c("one", "two", "three"))
    Condition
      [1m[33mError[39m in `group_numbers()`:[22m
      [1m[22m[31mx[39m `numbers` must be of class [1m[31m<numeric>[39m[22m
      [33m![39m The input you've supplied, `numbers`, is of class [1m[33m<character>[39m[22m
      [36mi[39m [1m[36mCheck the `numbers`[39m[22m input.

# invalid prefix throws the expected error [ansi]

    Code
      group_numbers(1:10, prefix = array(1:5))
    Condition
      [1m[33mError[39m in `group_numbers()`:[22m
      [1m[22m[31mx[39m `prefix` must be of class [1m[31m<character>[39m[22m
      [33m![39m The input you've supplied, `prefix`, is of class [1m[33m<array>[39m[22m
      [36mi[39m [1m[36mCheck the `prefix`[39m[22m input.

# invalid suffix throws the expected error [ansi]

    Code
      group_numbers(1:10, suffix = array(1:5))
    Condition
      [1m[33mError[39m in `group_numbers()`:[22m
      [1m[22m[31mx[39m `suffix` must be of class [1m[31m<character>[39m[22m
      [33m![39m The input you've supplied, `suffix`, is of class [1m[33m<array>[39m[22m
      [36mi[39m [1m[36mCheck the `suffix`[39m[22m input.

# Sep must be character [ansi]

    Code
      group_numbers(1:5, prefix = "group", sep = 3)
    Condition
      [1m[33mError[39m in `group_numbers()`:[22m
      [1m[22m[31mx[39m `sep` must be of class [1m[31m<character>[39m[22m
      [33m![39m The input you've supplied, `sep`, is of class [1m[33m<numeric>[39m[22m
      [36mi[39m [1m[36mCheck the `sep`[39m[22m input.

---

    Code
      group_numbers(1:5, sep = "_")
    Condition
      [1m[33mError[39m in `group_numbers()`:[22m
      [1m[22m[31mx[39m You cannot use `sep` if `prefix` or `suffix` is [1m[31mnot present[39m[22m.
      [33m![39m `prefix` and `suffix` [1m[33mare missing[39m[22m.
      [36mi[39m To use `sep`, please [1m[36mprovide a character value for `prefix` or `suffix`[39m[22m.

