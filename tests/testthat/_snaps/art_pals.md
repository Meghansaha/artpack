# pal checks [ansi]

    Code
      art_pals(pal = .x)
    Error <rlang_error>
      [1m[22m`pal` must be a length of [1m[33m1[39m[22m
      [31mx[39m The `pal` object you've supplied has a length of [1m[31m2[39m[22m
      [36mi[39m [1m[36mCheck the `pal` value[39m[22m you've supplied.

---

    Code
      art_pals(pal = .x)
    Error <rlang_error>
      [1m[22m`pal` must be of type [1m[33m<character>[39m[22m
      [31mx[39m The `pal` object you've supplied is of type [1m[31m<list>[39m[22m
      [36mi[39m [1m[36mCheck the `pal` value[39m[22m you've supplied.

---

    Code
      art_pals(pal = .x)
    Error <rlang_error>
      [1m[22m[31mx[39m "bupu" is not a known [1m[33maRtpack[39m[22m color palette.
      [36mi[39m [1m[36mPlease enter one of the following: [39m[22m"arctic", "beach", "bw", "brood", "cosmos", "explorer", "gemstones", "grays", "icecream", "imagination", "majestic", "nature", "neon", "ocean", "plants", "rainbow", "sunnyside" or "super"

# n checks [ansi]

    Code
      art_pals(pal = "rainbow", n = .x)
    Error <rlang_error>
      [1m[22m`n` must be of type [1m[33m<numeric>[39m[22m
      [31mx[39m The `n` object you've supplied is of type [1m[31m<character>[39m[22m
      [36mi[39m [1m[36mCheck the `n` value[39m[22m you've supplied.

---

    Code
      art_pals(pal = "rainbow", n = .x)
    Error <rlang_error>
      [1m[22m`n` must be a length of [1m[33m1[39m[22m
      [31mx[39m The `n` object you've supplied has a length of [1m[31m100[39m[22m
      [36mi[39m [1m[36mCheck the `n` value[39m[22m you've supplied.

---

    Code
      art_pals(pal = "rainbow", n = .x)
    Error <rlang_error>
      [1m[22m`n` must be a [1m[33mnumeric integer (no decimals)[39m[22m
      [31mx[39m The `n` object you've supplied is [1m[31m7.5[39m[22m
      [36mi[39m [1m[36mCheck the `n` value[39m[22m you've supplied.

---

    Code
      art_pals(pal = "rainbow", n = .x)
    Error <rlang_error>
      [1m[22m`n` must be a numeric value greater than or equal to [1m[33m1[39m[22m
      [31mx[39m The `n` object you've supplied has a value of [1m[31m-3[39m[22m
      [36mi[39m [1m[36mCheck the `n` value[39m[22m you've supplied.

# direction checks [ansi]

    Code
      art_pals(direction = .x)
    Error <rlang_error>
      [1m[22m`direction` must be a length of [1m[33m1[39m[22m
      [31mx[39m The `direction` object you've supplied has a length of [1m[31m2[39m[22m
      [36mi[39m [1m[36mCheck the `direction` value[39m[22m you've supplied.

---

    Code
      art_pals(direction = .x)
    Error <rlang_error>
      [1m[22m`direction` must be of type [1m[33m<character>[39m[22m
      [31mx[39m The `direction` object you've supplied is of type [1m[31m<numeric>[39m[22m
      [36mi[39m [1m[36mCheck the `direction` value[39m[22m you've supplied.

---

    Code
      art_pals(direction = .x)
    Error <rlang_error>
      [1m[22m[31mx[39m "vertical" is not a [1m[33mvalid direction[39m[22m
      [36mi[39m [1m[36mPlease enter one of the following:[39m[22m"rev", "reverse", "reg" or "regular"

