#=============================================================================#
# wave_data - Testing Suite----------------------------------------------------
#=============================================================================#

#=============================================================================#
# Input Testing----------------------------------------------------------------
#=============================================================================#
# Only numbers should be accepted

cli::test_that_cli("Numbers only",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         group_numbers(c("one","two","three"))
                       },
                       error = TRUE
                     )
                   },
                   configs = "ansi"
)

testthat::test_that("works as expected",
                    {
                      regular_numbers <- 1:15
                      expected_numbers <- c(paste0("0",1:9),10:15)
                      actual_numbers <- group_numbers(regular_numbers)

                      testthat::expect_equal(actual_numbers, expected_numbers)
                    })
