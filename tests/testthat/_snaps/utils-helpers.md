# invalid call_level throws the expected error [ansi]

    Code
      class.check("test", "character", call_level = "five")
    Condition
      [1m[33mError[39m in `class.check()`:[22m
      [1m[22m[31mx[39m `call_level` is invalid!
      [33m![39m check class.check

# invalid expected_class throws the expected error [ansi]

    Code
      class.check("test", expected_class = "char")
    Condition
      [1m[33mError[39m in `class.check()`:[22m
      [1m[22m[31mx[39m `expected_class` is invalid!
      [33m![39m check class.check

