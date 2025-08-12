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

# illogical expected_type vector throws the expected error (pos v neg) [ansi]

    Code
      is.expected.numeric.type(8, expected_type = c("positive", "negative"))
    Condition
      [1m[33mError[39m in `is.expected.numeric.type()`:[22m
      [1m[22m[31mx[39m `expected_type` is invalid!
      [36mi[39m `"positive"` and `"negative"` values are both present in `expected_type`. Pick one
      [33m![39m check `is.expected.numeric.type`

# invalid expected_length throws the expected error [ansi]

    Code
      length.check("test", expected_length = "one")
    Condition
      [1m[33mError[39m in `length.check()`:[22m
      [1m[22m[31mx[39m `expected_length` is invalid! (needs to be numeric)
      [33m![39m check `length.check`

