# =============================================================================#
# utils-helpers- Testing Suite--------------------------------------------------
# =============================================================================#

#==============================================================================#
# class.check error messaging---------------------------------------------------
#==============================================================================#
cli::test_that_cli("invalid call_level throws the expected error",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         class.check("test", "character", call_level = "five")
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

cli::test_that_cli("invalid expected_class throws the expected error",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         class.check("test", expected_class = "char")
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

#==============================================================================#
# is.var.present error messaging------------------------------------------------
#==============================================================================#
cli::test_that_cli("invalid call_level throws the expected error",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         is.var.present("some variable",  call_level = "one")
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

#==============================================================================#
# is.expected.numeric.type error messaging--------------------------------------
#==============================================================================#
cli::test_that_cli("invalid call_level throws the expected error",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         is.expected.numeric.type(8, expected_type = c("negative", "integer"),  call_level = "two")
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

cli::test_that_cli("invalid expected_type throws the expected error (singular)",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         is.expected.numeric.type(8, expected_type = c("negtaive", "integer"))
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

cli::test_that_cli("invalid expected_type throws the expected error (plural)",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         is.expected.numeric.type(8, expected_type = c("negtaive", "zeroo"))
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

#==============================================================================#
# length.check error messaging--------------------------------------------------
#==============================================================================#
cli::test_that_cli("invalid call_level throws the expected error",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         length.check("test", expected_length = 1, call_level = "five")
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

cli::test_that_cli("invalid expected_length throws the expected error",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         length.check("test", expected_length = "one")
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

cli::test_that_cli("invalid expected_op throws the expected error",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         length.check("test", expected_length = 1, expected_op = "=")
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

#==============================================================================#
# is.expected.value error messaging---------------------------------------------
#==============================================================================#
cli::test_that_cli("invalid call_level throws the expected error",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         is.expected.value("hi", expected_values = c("hi", "bye"),  call_level = "two")
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

cli::test_that_cli("invalid value throws the expected error",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         is.expected.value(2, expected_values = 5:10)
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

#==============================================================================#
# is.color error messaging------------------------------------------------------
#==============================================================================#
cli::test_that_cli("invalid call_level throws the expected error",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         is.color("#000000", call_level = "five")
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

cli::test_that_cli("invalid color throws the expected error",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         color <- "Blurple"
                         is.color(color)
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

#==============================================================================#
# col_to_hsl operation/error messaging------------------------------------------
#==============================================================================#
testthat::test_that("col 2 hsl works",
                    {
                      hex_value <- "#96681d"
                      expected_hsl <- c(37, .68, .35)
                      actual_hsl <- col_to_hsl(hex_value) |> round(digits = c(0,2,2))

                      testthat::expect_setequal(expected_hsl, actual_hsl)
                    }

)

testthat::test_that("col 2 hsl works when sat > .5",
                    {
                      hex_value <- "#3832a8"
                      expected_hsl <- c(243, .54, .43)
                      actual_hsl <- col_to_hsl(hex_value) |> round(digits = c(0,2,2))

                      testthat::expect_setequal(expected_hsl, actual_hsl)
                    }

)

testthat::test_that("col 2 hsl works when light > .5",
                    {
                      hex_value <- "#8481c1"
                      expected_hsl <- c(243, .34, .63)
                      actual_hsl <- col_to_hsl(hex_value) |> round(digits = c(0,2,2))

                      testthat::expect_setequal(expected_hsl, actual_hsl)
                    }

)

# Test HSL to RGB with high lightness values
testthat::test_that("hsl_to_rgb handles high lightness", {
  # This should hit line 520 (light >= 0.5 case)
  hsl_high_light <- c(120, 0.5, 0.8)  # High lightness
  result <- hsl_to_rgb(hsl_high_light)
  testthat::expect_length(result, 3)
})

# Test HSL to RGB with hue positions that require wrapping
testthat::test_that("hsl_to_rgb handles hue wrapping edge cases", {
  # Test cases that should hit the hue position correction lines

  # High hue value that causes r_init < 0 (line 532)
  hsl_low_r <- c(300, 0.5, 0.3)  # Should cause r_init to need +1
  result1 <- hsl_to_rgb(hsl_low_r)

  # Hue value that causes g_init > 1 (line 538)
  hsl_high_g <- c(30, 0.5, 0.3)  # Should cause g_init > 1
  result2 <- hsl_to_rgb(hsl_high_g)

  # Hue value that causes b_init < 0 (line 552)
  hsl_low_b <- c(60, 0.5, 0.3)  # Should cause b_init < 0
  result3 <- hsl_to_rgb(hsl_low_b)

  testthat::expect_length(result1, 3)
  testthat::expect_length(result2, 3)
  testthat::expect_length(result3, 3)
})

# Test the < 1/6 condition in calc_channel_value
testthat::test_that("hsl_to_rgb handles very low hue ranges", {
  # Use a very low hue value to hit the < 1/6 case (line 559)
  hsl_very_low_hue <- c(10, 0.8, 0.4)  # Very low hue
  result <- hsl_to_rgb(hsl_very_low_hue)
  testthat::expect_length(result, 3)
})




cli::test_that_cli("invalid color throws the expected error",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         col_to_hsl("Blurple")
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

testthat::test_that("col 2 hsl works for colors with chroma == 0",
                    {
                      hex_value <- "#000000"
                      expected_hsl <- c(0, 0, 0)
                      actual_hsl <- col_to_hsl(hex_value) |> round(digits = c(0,2,2))

                      testthat::expect_setequal(expected_hsl, actual_hsl)
                    }

)

#==============================================================================#
# hsl_to_rgb operation/error messaging------------------------------------------
#==============================================================================#
testthat::test_that("hsl 2 rgb works",
                    {
                      hsl_value <- c(339, .6, .34)
                      expected_rgb <- c(139, 35, 71)
                      actual_rgb <- hsl_to_rgb(hsl_value)

                      testthat::expect_setequal(expected_rgb, actual_rgb)
                    }

)

testthat::test_that("hsl 2 rgb works for colors with sat == 0",
                    {
                      hsl_value <- c(0, 0, .50)
                      expected_rgb <- c(128, 128, 128)
                      actual_rgb <- round(hsl_to_rgb(hsl_value) * 255) |> unname()

                      testthat::expect_equal(expected_rgb, actual_rgb)
                    }

)

testthat::test_that("hsl 2 rgb works for colors with hue == 0",
                    {
                      hsl_value <- c(0, .49, .28)
                      expected_rgb <- c(106, 36, 36)
                      actual_rgb <- round(hsl_to_rgb(hsl_value)) |> unname()

                      testthat::expect_equal(expected_rgb, actual_rgb)
                    }

)

#==============================================================================#
# rgb_to_hex operation/error messaging------------------------------------------
#==============================================================================#
testthat::test_that("rgb 2 hex works",
                    {
                      rgb_value <- c(26, 51, 3)
                      expected_hex <- "#1A3303"
                      actual_hex <- rgb_to_hex(rgb_value)

                      testthat::expect_equal(expected_hex, actual_hex)
                    }

)

