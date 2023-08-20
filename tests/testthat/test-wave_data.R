testthat::expect_true(is.data.frame(wave_data(0, 10,
  size = 1,
  type = "sin", orientation = "horiz",
  freq = 3, n_points = 10
)))
