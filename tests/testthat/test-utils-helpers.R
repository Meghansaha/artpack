# =============================================================================#
# utils-helpers- Testing Suite-------------------------------------------------
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
