original_square <- data.frame(x = c(0,3,3,0,0),
                              y = c(0,0,3,3,0))

rotated_square <- rotator(data = original_square, angle = 45, anchor = "center")

testthat::expect_error(testthat::expect_equal(original_square, rotated_square))
