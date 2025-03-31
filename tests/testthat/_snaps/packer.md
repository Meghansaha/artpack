# big_r checks [ansi]

    Code
      packer(n = 100, big_r = .x)
    Condition
      [1m[33mError[39m in `packer()`:[22m
      [1m[22m[33m![39m `big_r` must be of length [1m[33m1[39m[22m
      [31mx[39m The `big_r` you've supplied has a length of [1m[31m2[39m[22m
      [36mi[39m Check the `big_r` value you've supplied.

---

    Code
      packer(n = 100, big_r = .x)
    Condition
      [1m[33mError[39m in `packer()`:[22m
      [1m[22m[33m![39m `big_r` must be of type [1m[33mnumeric[39m[22m
      [31mx[39m You've supplied a [1m[31mcharacter[39m[22m
      [36mi[39m Check the `big_r` value you've supplied.

---

    Code
      packer(n = 100, big_r = .x)
    Condition
      [1m[33mError[39m in `packer()`:[22m
      [1m[22m[33m![39m `big_r` must be [1m[33mgreater than zero[39m[22m
      [31mx[39m `big_r` = [1m[31m-10[39m[22m
      [36mi[39m Check the `big_r` value you've supplied.

# med_r checks [ansi]

    Code
      packer(n = 100, med_r = .x)
    Condition
      [1m[33mError[39m in `packer()`:[22m
      [1m[22m[33m![39m `med_r` must be of length [1m[33m1[39m[22m
      [31mx[39m The `med_r` you've supplied has a length of [1m[31m2[39m[22m
      [36mi[39m Check the `med_r` value you've supplied.

---

    Code
      packer(n = 100, med_r = .x)
    Condition
      [1m[33mError[39m in `packer()`:[22m
      [1m[22m[33m![39m `med_r` must be of type [1m[33mnumeric[39m[22m
      [31mx[39m You've supplied a [1m[31mcharacter[39m[22m
      [36mi[39m Check the `med_r` value you've supplied.

---

    Code
      packer(n = 100, med_r = .x)
    Condition
      [1m[33mError[39m in `packer()`:[22m
      [1m[22m[33m![39m `med_r` must be [1m[33mgreater than zero[39m[22m
      [31mx[39m `med_r` = [1m[31m-1[39m[22m
      [36mi[39m Check the `med_r` value you've supplied.

# small_r checks [ansi]

    Code
      packer(n = 100, small_r = .x)
    Condition
      [1m[33mError[39m in `packer()`:[22m
      [1m[22m[33m![39m `small_r` must be of length [1m[33m1[39m[22m
      [31mx[39m The `small_r` you've supplied has a length of [1m[31m2[39m[22m
      [36mi[39m Check the `small_r` value you've supplied.

---

    Code
      packer(n = 100, small_r = .x)
    Condition
      [1m[33mError[39m in `packer()`:[22m
      [1m[22m[33m![39m `small_r` must be of type [1m[33mnumeric[39m[22m
      [31mx[39m You've supplied a [1m[31mcharacter[39m[22m
      [36mi[39m Check the `small_r` value you've supplied.

---

    Code
      packer(n = 100, small_r = .x)
    Condition
      [1m[33mError[39m in `packer()`:[22m
      [1m[22m[33m![39m `small_r` must be [1m[33mgreater than zero[39m[22m
      [31mx[39m `small_r` = [1m[31m-13[39m[22m
      [36mi[39m Check the `small_r` value you've supplied.

# Invalid Color Type [ansi]

    Code
      packer(n = 100, color_type = "This is Wrong")
    Condition
      [1m[33mError[39m in `packer()`:[22m
      [1m[22m[33m![39m `color_type` is [1m[31munknown[39m[22m
      [31mx[39m `color_type` must be one of the following: [1m[36m"regular"[39m[22m , [1m[36m"reverse"[39m[22m , or [1m[36m"random"[39m[22m
      [36mi[39m You've supplied a `color_type` value of
      [1m[33m`This is Wrong`[39m[22m

# Invalid Circle Type [ansi]

    Code
      packer(n = 100, circle_type = "This is Wrong")
    Condition
      [1m[33mError[39m in `packer()`:[22m
      [1m[22m[33m![39m `circle_type` is [1m[31munknown[39m[22m
      [31mx[39m `circle_type` must be one of the following: [1m[36m"whole"[39m[22m or [1m[36m"swirl"[39m[22m
      [36mi[39m You've supplied a `circle_type` value of
      [1m[33m`This is Wrong`[39m[22m

