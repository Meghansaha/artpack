# invalid call_level throws the expected error [ansi]

    Code
      class.check("test", "character", call_level = "five")
    Condition
      [1m[33mError[39m in `class.check()`:[22m
      [1m[22m[31mx[39m `call_level` is invalid!
      [33m![39m check `class.check`

---

    Code
      is.var.present("some variable", call_level = "one")
    Condition
      [1m[33mError[39m in `is.var.present()`:[22m
      [1m[22m[31mx[39m `call_level` is invalid!
      [33m![39m check `is.var.present`

---

    Code
      is.expected.numeric.type(8, expecected_type = c("negative", "integer"),
      call_level = "two")
    Condition
      [1m[33mError[39m in `is.expected.numeric.type()`:[22m
      [1m[22m[31mx[39m `call_level` is invalid!
      [33m![39m check is.var.present

---

    Code
      length.check("test", expected_length = 1, call_level = "five")
    Condition
      [1m[33mError[39m in `length.check()`:[22m
      [1m[22m[31mx[39m `call_level` is invalid!
      [33m![39m check `length.check`

---

    Code
      is.expected.value("hi", expecected_values = c("hi", "bye"), call_level = "two")
    Condition
      [1m[33mError[39m in `is.expected.value()`:[22m
      [1m[22m[31mx[39m `call_level` is invalid!
      [33m![39m check `length.check`

# invalid expected_class throws the expected error [ansi]

    Code
      class.check("test", expected_class = "char")
    Condition
      [1m[33mError[39m in `class.check()`:[22m
      [1m[22m[31mx[39m `expected_class` is invalid!
      [33m![39m check `class.check`

# invalid expected_type throws the expected error (singular) [ansi]

    Code
      is.expected.numeric.type(8, expected_type = c("negtaive", "integer"))
    Condition
      [1m[33mError[39m in `is.expected.numeric.type()`:[22m
      [1m[22m[31mx[39m `expected_type` is invalid!
      [36mi[39m "negtaive" is an invalid `expected_type` value
      [33m![39m check is.expected.numeric.type

# invalid expected_type throws the expected error (plural) [ansi]

    Code
      is.expected.numeric.type(8, expected_type = c("negtaive", "zeroo"))
    Condition
      [1m[33mError[39m in `is.expected.numeric.type()`:[22m
      [1m[22m[31mx[39m `expected_type` is invalid!
      [36mi[39m "negtaive" and "zeroo" are invalid `expected_type` values
      [33m![39m check is.expected.numeric.type

# invalid expected_length throws the expected error [ansi]

    Code
      length.check("test", expected_length = "one")
    Condition
      [1m[33mError[39m in `length.check()`:[22m
      [1m[22m[31mx[39m `expected_length` is invalid! (needs to be numeric)
      [33m![39m check `length.check`

# invalid expected_op throws the expected error [ansi]

    Code
      length.check("test", expected_length = 1, expected_op = "=")
    Condition
      [1m[33mError[39m in `length.check()`:[22m
      [1m[22m[31mx[39m `expected_op` is invalid! (needs to be ('==', '>=', '<=', '<', '>')
      [33m![39m check `length.check`

# invalid value throws the expected error [ansi]

    Code
      is.expected.value(2, expected_values = 5:10)
    Condition
      [1m[33mError[39m:[22m
      [1m[22m[31mx[39m `2` must be an accepted value: [1m[3m[31m"5", "6", "7", "8", "9" or "10"[39m[23m[22m
      [33m![39m The input you've supplied, `2`, is [1m[33m"2"[39m[22m
      [36mi[39m [1m[36mCheck the `2`[39m[22m input.

