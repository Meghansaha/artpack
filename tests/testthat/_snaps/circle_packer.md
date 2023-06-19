# big_r checks [ansi]

    Code
      circle_packer(big_r = .x)
    Error <rlang_error>
      [1m[22m`big_r` must be of length [1m[33m1[39m[22m
      [31mx[39m The `big_r` you've supplied has a length of [1m[31m2[39m[22m
      [36mi[39m Check the `big_r` value you've supplied.

---

    Code
      circle_packer(big_r = .x)
    Error <rlang_error>
      [1m[22m`big_r` must be of type [1m[33mnumeric[39m[22m
      [31mx[39m You've supplied a [1m[31mcharacter[39m[22m
      [36mi[39m Check the `big_r` value you've supplied.

---

    Code
      circle_packer(big_r = .x)
    Error <rlang_error>
      [1m[22m`big_r` must be [1m[33mgreater than zero[39m[22m
      [31mx[39m `big_r` = [1m[31m-10[39m[22m
      [36mi[39m Check the `big_r` value you've supplied.

---

    Code
      circle_packer(med_r = .x)
    Error <rlang_error>
      [1m[22m`med_r` must be of length [1m[33m1[39m[22m
      [31mx[39m The `med_r` you've supplied has a length of [1m[31m2[39m[22m
      [36mi[39m Check the `med_r` value you've supplied.

---

    Code
      circle_packer(med_r = .x)
    Error <rlang_error>
      [1m[22m`med_r` must be of type [1m[33mnumeric[39m[22m
      [31mx[39m You've supplied a [1m[31mcharacter[39m[22m
      [36mi[39m Check the `med_r` value you've supplied.

---

    Code
      circle_packer(med_r = .x)
    Error <rlang_error>
      [1m[22m`med_r` must be [1m[33mgreater than zero[39m[22m
      [31mx[39m `med_r` = [1m[31m-1[39m[22m
      [36mi[39m Check the `med_r` value you've supplied.

---

    Code
      circle_packer(small_r = .x)
    Error <rlang_error>
      [1m[22m`small_r` must be of length [1m[33m1[39m[22m
      [31mx[39m The `small_r` you've supplied has a length of [1m[31m2[39m[22m
      [36mi[39m Check the `small_r` value you've supplied.

---

    Code
      circle_packer(small_r = .x)
    Error <rlang_error>
      [1m[22m`small_r` must be of type [1m[33mnumeric[39m[22m
      [31mx[39m You've supplied a [1m[31mcharacter[39m[22m
      [36mi[39m Check the `small_r` value you've supplied.

---

    Code
      circle_packer(small_r = .x)
    Error <rlang_error>
      [1m[22m`small_r` must be [1m[33mgreater than zero[39m[22m
      [31mx[39m `small_r` = [1m[31m-13[39m[22m
      [36mi[39m Check the `small_r` value you've supplied.

# big_r too big [ansi]

    Code
      circle_packer(big_r = 50)
    Error <simpleError>
      argument "n" is missing, with no default

# med_r too big [ansi]

    Code
      circle_packer(med_r = 50)
    Error <simpleError>
      argument "n" is missing, with no default

# small_r too big [ansi]

    Code
      circle_packer(small_r = 50)
    Error <simpleError>
      argument "n" is missing, with no default

