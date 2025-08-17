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
                         is.expected.numeric.type(8, expecected_type = c("negative", "integer"),  call_level = "two")
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

#==============================================================================#
# is.expected.value error messaging---------------------------------------------
#==============================================================================#
cli::test_that_cli("invalid call_level throws the expected error",
                   {
                     testthat::local_edition(3)
                     testthat::expect_snapshot(
                       {
                         is.expected.value("hi", expecected_values = c("hi", "bye"),  call_level = "two")
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
