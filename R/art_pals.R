#' Custom-built artpack Color Palettes
#' @description
#'
#' The artpack palette picker. The `art_pals` function consists of 18 palettes: "arctic",
#' "beach", "bw", "brood", "cosmos", "explorer", "gemstones", "grays", "icecream", "imagination",
#' "majestic", "nature", "neon", "ocean", "plants", "rainbow", "sunnyside", "super"
#'
#' @param pal A character string of the desired artpack palette.
#'
#' The 18 artpack palettes include:
#' - "arctic" - Icy blue and white colors
#' - "beach" - Sand-colored tans and ocean-colored blue colors
#' - "bw" - A gradient of black to white colors
#' - "brood" - A gradient of different shades of dark gray and black colors
#' - "cosmos" - Nebula-inspired blue, purple, and pink colors
#' - "explorer" - Pokemon-type inspired colors
#' - "gemstones" - Birthstone/Mineral-inspired colors
#' - "grays" - A gradient of dark, medium, and light gray colors
#' - "icecream" - A light pastel palette of cream, blue, brown, and pink colors
#' - "imagination" - 90's school supply-inspired colors
#' - "majestic" - Shades of majestic purple colors
#' - "nature" - A mix of tan, brown, green, and red colors
#' - "neon" - A neon spectrum of rainbow colors
#' - "ocean" - A gradient of dark to light blue colors
#' - "plants" - A gradient of dark to light green colors
#' - "rainbow" - A vibrant mix of rainbow colors
#' - "sunnyside" - A retro-inspired mix of pink, orange, and yellow colors
#' - "super" - A marveling mix of heroic colors
#'
#' @param n The numbers of colors desired in the output.
#'
#' Default is `5`. `n` must be a positive integer with a value greater than 0
#'
#' @param direction The direction of the palette
#'
#' Default is "regular". Only two options accepted: "regular" or "reverse"
#'
#' @param randomize Determines if the colors in the palette appear in a randomized order.
#'
#' Default is `FALSE`. Boolean where only `TRUE` or `FALSE` is accepted.
#'
#' @return A Character Vector.
#'
#' @importFrom knitr combine_words
#' @importFrom rlang is_bare_numeric
#'
#' @export
#'
#' @examplesIf rlang::is_installed("ggplot2")
#' @examples
#' library(ggplot2)
#' dots <- data.frame(x = c(1:10), y = 2.5)
#' dots$fills <- art_pals("rainbow", 10)
#'
#' dots |>
#'   ggplot(aes(x, y)) +
#'   theme_void() +
#'   geom_point(
#'     shape = 21,
#'     fill = dots$fills,
#'     color = "#000000",
#'     size = 10,
#'     stroke = 2
#'   )
#'
#'
#' dots_rev <- data.frame(x = c(1:10), y = 2.5)
#' dots_rev$fills <- art_pals("rainbow", 10, "reverse")
#'
#' dots_rev |>
#'   ggplot(aes(x, y)) +
#'   theme_void() +
#'   geom_point(
#'     shape = 21,
#'     fill = dots_rev$fills,
#'     color = "#000000",
#'     size = 10,
#'     stroke = 2
#'   )
#'
art_pals <- function(pal = NULL, n = 5, direction = "regular", randomize = FALSE) {
  # ===========================================================================#
  # artpack Palettes------------------------------------------------------------
  # ===========================================================================#
  pals <-
    list(
      arctic = list(c("#006ACD", "#4596D7", "#8AC2E1", "#BDDFEB", "#DEEFF5", "#FFFFFF")),
      beach = list(c("#E8B381", "#E7D2C1", "#7EC7F1", "#3DB0DD", "#009DEA", "#006ACD")),
      bw = list(c("#000000", "#1a1a1a", "#333333", "#666666", "#999999", "#ebe1e1", "#ffffff")),
      brood = list(c("#000000", "#0A0A0A", "#141414", "#1F1F1F", "#292929", "#333333")),
      cosmos = list(c("#562B91", "#5B1A61", "#BC3AA5", "#E73C88", "#4A77B5", "#1F186C")),
      explorer = list(c("#F75231", "#399CFF", "#FFC631", "#7BCE52", "#5ACEE7", "#A55239", "#B55AA5", "#D6B55A", "#6363B5")),
      gemstones = list(c("#29295D", "#7C5EB8", "#18AFB8", "#005038", "#819126", "#E6C14E", "#A14C0C", "#9F232E")),
      grays = list(c("#222222", "#333333", "#444444", "#555555", "#666666", "#777777", "#888888", "#999999")),
      icecream = list(c("#FCFEFD", "#FFECA8", "#FAD8E9", "#8FD9F4", "#BF7014", "#7B4000")),
      imagination = list(c("#DD0B80", "#E58E54", "#F1E109", "#A8CD35", "#07ADEB", "#572E76")),
      majestic = list(c("#0F061B", "#1D0B35", "#350A3C", "#471049", "#881F74")),
      nature = list(c("#686C20", "#1D3A1D", "#C77F42", "#532F00", "#9C1800", "#5B0000")),
      neon = list(c("#fc0000", "#fc4800", "#fcdb00", "#3ffc00", "#00ecfc", "#001dfc", "#6900fc", "#fc00f0", "#fc007e")),
      ocean = list(c("#12012E", "#144267", "#15698C", "#0695AA", "#00F3FF")),
      plants = list(c("#5ebf61", "#2f8032", "#206322", "#0c570f", "#0a380b", "#041f05")),
      rainbow = list(c("#AF3918", "#A21152", "#822B75", "#612884", "#154BAF", "#0B82B9", "#277E9D", "#488E35", "#E3A934", "#f26e0a")),
      sunnyside = list(c("#F6BF07", "#F67C21", "#ED155A", "#F61867")),
      super = list(c("#000000", "#292929", "#5F0E0E", "#363131", "#662E8A", "#B80000", "#C60018", "#005C94", "#E72124", "#4D982E", "#987EC1", "#F7C700"))
    )


  # ===========================================================================#
  # Input Checks----------------------------------------------------------------
  # ===========================================================================#
  ## n--------------------------------------------------------------------------
  ### is numeric----
  class.check(n, expected_class = "numeric")
  ### is length of 1----
  length.check(n, expected_length = 1)
  ### is > 0 and an integer#
  is.expected.numeric.type(n, expected_type = c("positive", "integer"))

  ## pal------------------------------------------------------------------------
  # Setting a default palette if none selected#
  pal_missing <- is.var.present(pal, required = FALSE) == FALSE

  if (pal_missing) {
    pal <- "ocean"
  }
  ### is length of 1----
  length.check(pal, expected_length = 1)
  ### is character----
  class.check(pal, expected_class = "character")

  # Checking for capitalization#
  case_check <- grepl("[[:upper:]]", pal)

  # converting to lowercase if caps detected#
  if (case_check) {
    pal <- tolower(pal)
  }

  # Checking that palette input is a valid palette#
  pal_check <- pal %in% names(pals)

  if (!pal_check) {
    c(
      "x" = paste0('"', pal, '"', " is not a known ", callout("artpack"), " color palette."),
      "i" = paste0(
        status("Please enter one of the following:\n"),
        knitr::combine_words(
          names(pals),
          sep = ", ",
          and = " or ",
          before = '"',
          after = '"',
          oxford_comma = FALSE
        )
      )
    ) |>
      cli::cli_abort()
  }

  ## direction------------------------------------------------------------------
  ### is length of 1----
  length.check(direction, expected_length = 1)
  ### is character----
  class.check(direction, expected_class = "character")

  # Checking for capitalization#
  case_check <- grepl("[[:upper:]]", direction)

  # Converting to lowercase if caps detected#
  if (case_check) {
    direction <- tolower(direction)
  }

  # Checking for valid directions#
  direction_check <-
    direction %in% c("rev", "reverse", "reg", "regular")

  if (!direction_check) {
    c(
      "x" = paste0('\"', direction, '\" is not a ', callout("valid direction")),
      "i" = paste0(
        status("Please enter one of the following:"),
        knitr::combine_words(
          c("rev", "reverse", "reg", "regular"),
          before = '"',
          after = '"',
          sep = ", ",
          and = " or ",
          oxford_comma = FALSE
        )
      )
    ) |>
      cli::cli_abort()
  }
  ## randomized-----------------------------------------------------------------
  ### is length of 1----
  length.check(randomize, expected_length = 1)
  ### is logical----
  class.check(randomize, expected_class = "logical")

  # ===========================================================================#
  # Palette Generation----------------------------------------------------------
  # ===========================================================================#
  ## Determine direction of the palette-----------------------------------------
  pal_init <-
    switch(
      direction,
      "rev" = pals[[pal]] |> unlist() |> (\(x) colorRampPalette(x)(n))() |> rev(),
      "reverse" = pals[[pal]] |> unlist() |> (\(x) colorRampPalette(x)(n))() |> rev(),
      pals[[pal]] |> unlist() |> (\(x) colorRampPalette(x)(n))()
    )

  ## Determine if the palette should be randomized------------------------------
  pal_final <-
    switch(
      randomize |> as.character(),
      "TRUE" = pal_init |> sample(),
      pal_init
    )

  return(pal_final)
}
