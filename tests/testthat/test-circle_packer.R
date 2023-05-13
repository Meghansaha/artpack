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
    circle_packer(big_r = .x)},
    error = TRUE)},
  configs = "ansi")
)

purrr::map(med_radis,  ~cli::test_that_cli("big_r checks", {
  testthat::local_edition(3)
  testthat::expect_snapshot({
    circle_packer(med_r = .x)},
    error = TRUE)},
  configs = "ansi")
)

purrr::map(small_radis,  ~cli::test_that_cli("big_r checks", {
  testthat::local_edition(3)
  testthat::expect_snapshot({
    circle_packer(small_r = .x)},
    error = TRUE)},
  configs = "ansi")
)

# Fx outputs a dataframe
testthat::expect_true(is.data.frame(circle_packer(n = 100)))

#testing big x,y appendages
# testthat::test_that("test", {testthat::expect_message(circle_packer(100), "increased")})
# expect_message(circle_packer(100), "big y appended")
# expect_message(circle_packer(100), "i increased")
# expect_message(circle_packer(100), "max big length reached")

