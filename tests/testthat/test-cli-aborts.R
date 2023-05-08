#numeric inputs

cli::test_that_cli("big_r length", {
  testthat::local_edition(3)
  testthat::expect_snapshot({
    circle_packer(big_r = c(10,2))
  }, error = TRUE
  )
}, configs = "ansi")

cli::test_that_cli("big_r type", {
  testthat::local_edition(3)
  testthat::expect_snapshot({
    circle_packer(big_r = "10")
  }, error = TRUE
  )
}, configs = "ansi")

cli::test_that_cli("big_r value", {
  testthat::local_edition(3)
  testthat::expect_snapshot({
    circle_packer(big_r = -10)
  }, error = TRUE
  )
}, configs = "ansi")
