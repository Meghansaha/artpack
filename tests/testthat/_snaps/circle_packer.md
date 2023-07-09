# big_r checks [ansi]

    Code
      circle_packer(n = 100, big_r = .x)
    Error <rlang_error>
      [1m[22m`big_r` must be of length [1m[33m1[39m[22m
      [31mx[39m The `big_r` you've supplied has a length of [1m[31m2[39m[22m
      [36mi[39m Check the `big_r` value you've supplied.

---

    Code
      circle_packer(n = 100, big_r = .x)
    Error <rlang_error>
      [1m[22m`big_r` must be of type [1m[33mnumeric[39m[22m
      [31mx[39m You've supplied a [1m[31mcharacter[39m[22m
      [36mi[39m Check the `big_r` value you've supplied.

---

    Code
      circle_packer(n = 100, big_r = .x)
    Error <rlang_error>
      [1m[22m`big_r` must be [1m[33mgreater than zero[39m[22m
      [31mx[39m `big_r` = [1m[31m-10[39m[22m
      [36mi[39m Check the `big_r` value you've supplied.

