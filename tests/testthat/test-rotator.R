original_square <- data.frame(x = c(0,3,3,0,0),
                              y = c(0,0,3,3,0))

rotated_square <- rotator(data = original_square,
                          angle = 45,
                          anchor = "center")



# Test to check if functions changes data frame inputted
test_that("Test for change to the original dataframe", {
  expect_false(identical(original_square,rotated_square))
})

# Test to check that error occurs if dataframe is not input
test_that("Test to ensure that error is thrown when dataframe is not input", {
  nums <- 1:5
  expect_snapshot_error(rotator(nums))
})


