#Testing Radi Inputs

big_radis <- list("multi" = c(10,2),
                  "type" = "10",
                  "negative" = -10
)

med_radis <- list("multi" = c(9,4),
                  "type" = "5",
                  "negative" = -1
)

small_radis <- list("multi" = c(1,25),
                    "type" = "8",
                    "negative" = -13
)


purrr::map(big_radis,  ~cli::test_that_cli("big_r checks", {
  testthat::local_edition(3)
  testthat::expect_snapshot({
    circle_packer(n = 100, big_r = .x)},
    error = TRUE)},
  configs = "ansi")
)

# Testing the Break Logic works for all circle sizes
testthat::test_that("Max sampling with big circles works", {
testthat::expect_message(
  withr::with_seed(
    55,
    circle_packer(10, big_r = 29))
  , "Maximum sampling reached for big circles"
  )
}
)

testthat::test_that("Max sampling with medium circles works", {
  testthat::expect_message(
    withr::with_seed(
      55,
      circle_packer(10, med_r = 29))
    , "Maximum sampling reached for medium circles"
  )
}
)

testthat::test_that("Max sampling with small circles works", {
  testthat::expect_message(
    withr::with_seed(
      55,
      circle_packer(10, small_r = 29))
    , "Maximum sampling reached for small circles"
  )
}
)

