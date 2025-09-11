# point_x should be present [ansi]

    Code
      point_in_polygon(point_y = point_y, poly_x = poly_x, poly_y = poly_y)
    Condition
      [1m[33mError[39m in `point_in_polygon()`:[22m
      [1m[22m[31mx[39m `point_x` must be provided [1m[31mand not missing nor `NULL`[39m[22m
      [33m![39m The input you've supplied, `point_x`, is [1m[33mmissing or `NULL`[39m[22m
      [36mi[39m [1m[36mCheck the `point_x`[39m[22m input.

# point_y should be present [ansi]

    Code
      point_in_polygon(point_x = point_x, poly_x = poly_x, poly_y = poly_y)
    Condition
      [1m[33mError[39m in `point_in_polygon()`:[22m
      [1m[22m[31mx[39m `point_y` must be provided [1m[31mand not missing nor `NULL`[39m[22m
      [33m![39m The input you've supplied, `point_y`, is [1m[33mmissing or `NULL`[39m[22m
      [36mi[39m [1m[36mCheck the `point_y`[39m[22m input.

# poly_x should be present [ansi]

    Code
      point_in_polygon(point_x = point_x, point_y = point_y, poly_y = poly_y)
    Condition
      [1m[33mError[39m in `point_in_polygon()`:[22m
      [1m[22m[31mx[39m `poly_x` must be provided [1m[31mand not missing nor `NULL`[39m[22m
      [33m![39m The input you've supplied, `poly_x`, is [1m[33mmissing or `NULL`[39m[22m
      [36mi[39m [1m[36mCheck the `poly_x`[39m[22m input.

# poly_y should be present [ansi]

    Code
      point_in_polygon(point_x = point_x, point_y = point_y, poly_x = poly_x)
    Condition
      [1m[33mError[39m in `point_in_polygon()`:[22m
      [1m[22m[31mx[39m `poly_y` must be provided [1m[31mand not missing nor `NULL`[39m[22m
      [33m![39m The input you've supplied, `poly_y`, is [1m[33mmissing or `NULL`[39m[22m
      [36mi[39m [1m[36mCheck the `poly_y`[39m[22m input.

# point_x should be numeric [ansi]

    Code
      point_in_polygon(vec_characters, point_y = point_y, poly_x = poly_x, poly_y = poly_y)
    Condition
      [1m[33mError[39m in `point_in_polygon()`:[22m
      [1m[22m[31mx[39m `point_x` must be of class [1m[31m<numeric>[39m[22m
      [33m![39m The input you've supplied, `point_x`, is of class [1m[33m<character>[39m[22m
      [36mi[39m [1m[36mCheck the `point_x`[39m[22m input.

# point_y should be numeric [ansi]

    Code
      point_in_polygon(point_x = point_x, vec_logic, poly_x = poly_x, poly_y = poly_y)
    Condition
      [1m[33mError[39m in `point_in_polygon()`:[22m
      [1m[22m[31mx[39m `point_y` must be of class [1m[31m<numeric>[39m[22m
      [33m![39m The input you've supplied, `point_y`, is of class [1m[33m<logical>[39m[22m
      [36mi[39m [1m[36mCheck the `point_y`[39m[22m input.

# poly_x should be numeric [ansi]

    Code
      point_in_polygon(point_x = point_x, point_y = point_y, lst_test, poly_y = poly_y)
    Condition
      [1m[33mError[39m in `point_in_polygon()`:[22m
      [1m[22m[31mx[39m `poly_x` must be of class [1m[31m<numeric>[39m[22m
      [33m![39m The input you've supplied, `poly_x`, is of class [1m[33m<list>[39m[22m
      [36mi[39m [1m[36mCheck the `poly_x`[39m[22m input.

# poly_y should be numeric [ansi]

    Code
      point_in_polygon(point_x = point_x, point_y = point_y, poly_x = poly_x,
        df_points)
    Condition
      [1m[33mError[39m:[22m
      [33m![39m object 'df_points' not found

# point_x should be of equal length to point_y [ansi]

    Code
      point_in_polygon("point", point_y = point_y, poly_x = poly_x, poly_y = poly_y)
    Condition
      [1m[33mError[39m in `point_in_polygon()`:[22m
      [1m[22m[31mx[39m The length of `point_x` [1m[31mmust be equal[39m[22m to the length of `point_y`
      [33m![39m You've supplied: [1m[33m`point_x` Length: 1 and `point_y` Length: 5[39m[22m
      [36mi[39m [1m[36mCheck the `point_x` and `point_y`[39m[22m inputs.

# poly_x should be of equal length to poly_y [ansi]

    Code
      point_in_polygon(point_x = point_x, point_y = point_y, 1:10, poly_y = poly_y)
    Condition
      [1m[33mError[39m in `point_in_polygon()`:[22m
      [1m[22m[31mx[39m The length of `poly_x` [1m[31mmust be equal[39m[22m to the length of `poly_y`
      [33m![39m You've supplied: [1m[33m`poly_x` Length: 10 and `poly_y` Length: 5[39m[22m
      [36mi[39m [1m[36mCheck the `poly_x` and `poly_y`[39m[22m inputs.

