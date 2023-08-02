

original_square <- tibble::tibble(blue = c(0,3,3,0,0),
                          red = c(0,0,3,3,0))
rotated_square <-
  original_square |>
  rotator(
    x = "blue",
    y = "red",
    angle = 45,
    anchor = "center"
    )


# Test to check if functions changes data frame inputted
testthat::test_that("Test for change to the original dataframe", {
  testthat::expect_false(identical(original_square,rotated_square))
})


# Test to check that error occurs if dataframe is not input
testthat::test_that("Test to ensure that error is thrown when dataframe is not input", {
  nums <- 1:5
  testthat::expect_error(rotator(nums))
})


