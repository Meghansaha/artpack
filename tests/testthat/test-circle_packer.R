#=============================================================================#
# Circle Packer - Testing Suite------------------------------------------------
#=============================================================================#

#=============================================================================#
# Input Testing----------------------------------------------------------------
#=============================================================================#

#Testing n input
testthat::test_that("Test that missing n throws an error", {
  testthat::expect_error(
    circle_packer()
  )}
)

n_inputs <-
  list(
    "Test that n with more than a length of 1 throws an error" = 100:200,
    "Test that ns less than 10 throws an error" = 5,
    "Test that non-numeric n throws an error" = "10"
  )

# tests for n
purrr::imap(n_inputs, ~testthat::test_that(.y, {
  testthat::expect_error(
    circle_packer(.x)
  )}
)
)

#Testing Radius Inputs
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

purrr::map(med_radis,  ~cli::test_that_cli("med_r checks", {
  testthat::local_edition(3)
  testthat::expect_snapshot({
    circle_packer(n = 100, med_r = .x)},
    error = TRUE)},
  configs = "ansi")
)

purrr::map(small_radis,  ~cli::test_that_cli("small_r checks", {
  testthat::local_edition(3)
  testthat::expect_snapshot({
    circle_packer(n = 100, small_r = .x)},
    error = TRUE)},
  configs = "ansi")
)

# Test x and y lim inputs
limit_inputs <- list(
      "Test that more than 1 throws an error" = c(0,100),
      "Test that it must be numeric" = "100"
)

#Xlim checks
purrr::imap(limit_inputs, ~testthat::test_that(.y, {
  testthat::expect_error(
    circle_packer(100, max_x = .x)
  )}
)
)

purrr::imap(limit_inputs, ~testthat::test_that(.y, {
  testthat::expect_error(
    circle_packer(100, min_x = .x)
  )}
)
)

#ylim checks
purrr::imap(limit_inputs, ~testthat::test_that(.y, {
  testthat::expect_error(
    circle_packer(100, max_y = .x)
  )}
)
)

purrr::imap(limit_inputs, ~testthat::test_that(.y, {
  testthat::expect_error(
    circle_packer(100, min_y = .x)
  )}
)
)

# Color Type checks
cli::test_that_cli("Invalid Color Type", {
  testthat::local_edition(3)
  testthat::expect_snapshot({
    circle_packer(n = 100, color_type = "This is Wrong")},
    error = TRUE)},
  configs = "ansi")

testthat::test_that("Valid color type", {
  testthat::expect_no_error(
    circle_packer(100, color_type = "random")
  )}
)

# Circle Type checks

cli::test_that_cli("Invalid Circle Type", {
  testthat::local_edition(3)
  testthat::expect_snapshot({
    circle_packer(n = 100, circle_type = "This is Wrong")},
    error = TRUE)},
  configs = "ansi")

testthat::test_that("Valid circle type", {
  testthat::expect_no_error(
    circle_packer(100, circle_type = "whole")
  )}
)

# Color pal checks
valid_hex <- c("#000000","#1a1a1a", "#ffffff")
invalid_hex <- c("#000", "er4", "#ffffff")
valid_color <- c("red", "blue", "green")
invalid_color <- c("marron","#000000","blu")

purrr::map(list(valid_color, valid_hex),
           ~testthat::test_that("Valid color palette", {
             testthat::expect_no_error(
               circle_packer(100, color_pal = .x)
             )}
           )
)


purrr::map(list(invalid_color, invalid_hex),
           ~testthat::test_that("inValid color palette", {
             testthat::expect_error(
               circle_packer(100, color_pal = .x)
             )}
           )
)

#=============================================================================#
# Side Effect/Internal Testing-------------------------------------------------
#=============================================================================#

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

#=============================================================================#
# Output Testing---------------------------------------------------------------
#=============================================================================#

# Test that a data frame is returned
testthat::test_that("test that a df is returned", {
  expect_true(
    is.data.frame(circle_packer(100))
  )
})

# Test that picking a circle type of "whole" with a color gives a fill variable
testthat::test_that("test that a circle type of 'whole' gives a 'fill' variable", {

  df_names <- circle_packer(100, circle_type = "whole", color_pal = "#000000") |> names()
  expect_true(
    "fill" %in% df_names
  )
})

# Test that picking a circle type of "swirl" with a color gives a color variable
testthat::test_that("test that a circle type of 'swirl' gives a 'color' variable", {

  df_names <- circle_packer(100, circle_type = "swirl", color_pal = "#000000") |> names()
  expect_true(
    "color" %in% df_names
  )
})
