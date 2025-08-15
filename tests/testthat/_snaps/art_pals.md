# pal should be of length 1 [ansi]

    Code
      art_pals(c("brood", "ocean"))
    Condition
      [1m[33mError[39m in `art_pals()`:[22m
      [1m[22m[31mx[39m `pal` must be of length [1m[31m1[39m[22m
      [33m![39m The input you've supplied, `pal`, is of length [1m[33m2[39m[22m
      [36mi[39m [1m[36mCheck the `pal`[39m[22m input.

# n should be of length 1 [ansi]

    Code
      art_pals(n = 1:4)
    Condition
      [1m[33mError[39m in `art_pals()`:[22m
      [1m[22m[31mx[39m `n` must be of length [1m[31m1[39m[22m
      [33m![39m The input you've supplied, `n`, is of length [1m[33m4[39m[22m
      [36mi[39m [1m[36mCheck the `n`[39m[22m input.

# direction should be of length 1 [ansi]

    Code
      art_pals(direction = c("reg", "rev"))
    Condition
      [1m[33mError[39m in `art_pals()`:[22m
      [1m[22m[31mx[39m `direction` must be of length [1m[31m1[39m[22m
      [33m![39m The input you've supplied, `direction`, is of length [1m[33m2[39m[22m
      [36mi[39m [1m[36mCheck the `direction`[39m[22m input.

# randomize should be of length 1 [ansi]

    Code
      art_pals(randomize = c(TRUE, FALSE))
    Condition
      [1m[33mError[39m in `art_pals()`:[22m
      [1m[22m[31mx[39m `randomize` must be of length [1m[31m1[39m[22m
      [33m![39m The input you've supplied, `randomize`, is of length [1m[33m2[39m[22m
      [36mi[39m [1m[36mCheck the `randomize`[39m[22m input.

# pal should be of class character [ansi]

    Code
      art_pals(pal = TRUE)
    Condition
      [1m[33mError[39m in `art_pals()`:[22m
      [1m[22m[31mx[39m `pal` must be of class [1m[31m<character>[39m[22m
      [33m![39m The input you've supplied, `pal`, is of class [1m[33m<logical>[39m[22m
      [36mi[39m [1m[36mCheck the `pal`[39m[22m input.

# n should be of class numeric [ansi]

    Code
      art_pals(n = "4")
    Condition
      [1m[33mError[39m in `art_pals()`:[22m
      [1m[22m[31mx[39m `n` must be of class [1m[31m<numeric>[39m[22m
      [33m![39m The input you've supplied, `n`, is of class [1m[33m<character>[39m[22m
      [36mi[39m [1m[36mCheck the `n`[39m[22m input.

# direction should be of class character [ansi]

    Code
      art_pals(direction = 1)
    Condition
      [1m[33mError[39m in `art_pals()`:[22m
      [1m[22m[31mx[39m `direction` must be of class [1m[31m<character>[39m[22m
      [33m![39m The input you've supplied, `direction`, is of class [1m[33m<numeric>[39m[22m
      [36mi[39m [1m[36mCheck the `direction`[39m[22m input.

# randomize should be of class logical [ansi]

    Code
      art_pals(randomize = "TRUE")
    Condition
      [1m[33mError[39m in `art_pals()`:[22m
      [1m[22m[31mx[39m `randomize` must be of class [1m[31m<logical>[39m[22m
      [33m![39m The input you've supplied, `randomize`, is of class [1m[33m<character>[39m[22m
      [36mi[39m [1m[36mCheck the `randomize`[39m[22m input.

# pal should be an accepted artpack color palette [ansi]

    Code
      art_pals(pal = "supercool")
    Condition
      [1m[33mError[39m in `art_pals()`:[22m
      [1m[22m[31mx[39m "supercool" is not a known [1m[33martpack[39m[22m color palette.
      [36mi[39m [1m[36mPlease enter one of the following: [39m[22m"arctic", "beach", "bw", "brood", "cosmos", "explorer", "gemstones", "grays", "icecream", "imagination", "majestic", "nature", "neon", "ocean", "plants", "rainbow", "sunnyside" or "super"

# n should be a positive number [ansi]

    Code
      art_pals(n = -9)
    Condition
      [1m[33mError[39m in `art_pals()`:[22m
      [1m[22m[31mx[39m `n` must be a positive numeric value [1m[31mnot negative nor zero[39m[22m
      [33m![39m The input you've supplied, `n`, is [1m[33m-9[39m[22m
      [36mi[39m [1m[36mCheck the `n`[39m[22m input.

# n should be an integer [ansi]

    Code
      art_pals(n = 9.2)
    Condition
      [1m[33mError[39m in `art_pals()`:[22m
      [1m[22m[31mx[39m `n` must be a numeric integer; [1m[31mnot a float (number with decimals)[39m[22m
      [33m![39m The input you've supplied, `n`, is [1m[33m9.2[39m[22m
      [36mi[39m [1m[36mCheck the `n`[39m[22m input.

# direction should be an accepted value [ansi]

    Code
      art_pals(direction = "up")
    Condition
      [1m[33mError[39m in `art_pals()`:[22m
      [1m[22m[31mx[39m "up" is not a [1m[33mvalid direction[39m[22m
      [36mi[39m [1m[36mPlease enter one of the following:[39m[22m"rev", "reverse", "reg" or "regular"

