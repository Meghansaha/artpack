#' Custom-built aRtpack Color Palettes
#' @description
#'
#' The aRtpack palette picker. The `art_pals` function consists of 17 palettes: "arctic",
#' "beach", "bw", "brood", "cosmos", "explorer", "gemstones", "grays", "icecream", "imagination",
#' "majestic", "nature", "neon", "ocean", "plants", "rainbow", "sunnyside"
#'
#' @param pal A character string of the desired aRtpack palette.
#'
#' The 17 aRtpack palettes include:
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
#'
#' @param n The numbers of colors desired in the output.
#'
#'Default is `5`. `n` must be a positive integer with a value greater than 0
#'
#' @param direction The direction of the palette
#'
#' Default is "regular". Only two options accepted: "regular" or "reverse"
#'
#' @return A character vector.
#'
#' @export
#'
#' @examples
#'library(ggplot2)
#'dots <- data.frame(x = c(1:10), y = 2.5)
#'dots$fills <- art_pals("rainbow", 10)
#'
#'dots |>
#'ggplot(aes(x,y))+
#'theme_void()+
#'geom_point(shape = 21,
#'          fill = dots$fills,
#'          color = "#000000",
#'          size = 10,
#'          stroke = 2)
#'
#'
#'dots_rev <- data.frame(x = c(1:10), y = 2.5)
#'dots_rev$fills <- art_pals("rainbow", 10, "reverse")
#'
#'dots_rev |>
#'ggplot(aes(x,y))+
#'theme_void()+
#'geom_point(shape = 21,
#'          fill = dots_rev$fills,
#'          color = "#000000",
#'          size = 10,
#'          stroke = 2)
#'
art_pals <- function(pal = NULL, n = 5, direction = "regular"){

#=============================================================================#
#Setting a default palette if none selected#
#=============================================================================#
if(rlang::is_null(pal)){
  pal <- "ocean"
}

#=============================================================================#
#Logic checks for `n`
#=============================================================================#
  #Is numeric#
  if(!rlang::is_bare_numeric(n)){
    cli::cli_abort(c(
      paste0("{.var n} must be of type ", callout("<numeric>")),
      "x" = paste0("The {.var n} object you've supplied is of type ", error(paste0("<",class(n),">"))),
      "i" = paste0(status("Check the {.var n} value")," you've supplied.")
    )
    )
  }

  #Is only one number#
  if(length(n) != 1){
    cli::cli_abort(c(
      paste0("{.var n} must be a length of ", callout("1")),
      "x" = paste0("The {.var n} object you've supplied has a length of ", error(length(n))),
      "i" = paste0(status("Check the {.var n} value")," you've supplied.")
    )
    )
  }

  #Is one or more#
  if(n < 1){
    cli::cli_abort(c(
      paste0("{.var n} must be a numeric value greater than or equal to ", callout("1")),
      "x" = paste0("The {.var n} object you've supplied has a value of ", error(n)),
      "i" = paste0(status("Check the {.var n} value")," you've supplied.")
    )
    )
  }



  #Is number a whole/integer number#
  if(n %% 1 != 0){
    cli::cli_abort(c(
      paste0("{.var n} must be a ", callout("numeric integer (no decimals)")),
      "x" = paste0("The {.var n} object you've supplied is ", error(n)),
      "i" = paste0(status("Check the {.var n} value")," you've supplied.")
    )
    )
  }



pals <- list(arctic = list(c("#006ACD","#4596D7","#8AC2E1","#BDDFEB","#DEEFF5","#FFFFFF")),
             beach = list(c("#E8B381", "#E7D2C1", "#7EC7F1", "#3DB0DD", "#009DEA", "#006ACD")),
             bw = list(c("#000000", "#1a1a1a", "#333333", "#666666", "#999999","#ebe1e1","#ffffff")),
             brood = list(c("#000000", "#0A0A0A", "#141414", "#1F1F1F", "#292929", "#333333")),
             cosmos = list(c("#562B91", "#5B1A61", "#BC3AA5", "#E73C88", "#4A77B5", "#1F186C")),
             explorer = list(c("#F75231", "#399CFF", "#FFC631", "#7BCE52", "#5ACEE7", "#A55239", "#B55AA5", "#D6B55A", "#6363B5")),
             gemstones = list(c("#29295D", "#7C5EB8", "#18AFB8", "#005038", "#819126", "#E6C14E", "#A14C0C", "#9F232E")),
             grays = list(c("#222222", "#333333", "#444444", "#555555", "#666666", "#777777", "#888888", "#999999")),
             icecream = list(c("#FCFEFD", "#FFECA8", "#FAD8E9", "#8FD9F4", "#BF7014", "#7B4000")),
             imagination = list(c("#DD0B80", "#E58E54", "#F1E109", "#A8CD35", "#07ADEB", "#572E76")),
             majestic = list(c("#0F061B", "#1D0B35", "#350A3C", "#471049", "#881F74")),
             nature = list(c("#686C20", "#1D3A1D", "#C77F42", "#532F00", "#9C1800", "#5B0000")),
             neon = list(c("#fc0000","#fc4800","#fcdb00","#3ffc00", "#00ecfc","#001dfc","#6900fc","#fc00f0","#fc007e")),
             ocean = list(c("#12012E", "#144267", "#15698C", "#0695AA", "#00F3FF")),
             plants = list(c("#5ebf61", "#2f8032", "#206322", "#0c570f", "#0a380b", "#041f05")),
             rainbow = list(c("#AF3918", "#A21152", "#822B75", "#612884", "#154BAF", "#0B82B9", "#277E9D", "#488E35", "#E3A934", "#f26e0a")),
             sunnyside = list(c("#F6BF07", "#F67C21", "#ED155A", "#F61867"))
             )

#=============================================================================#
#Palette Logic Checks#
#=============================================================================#

#Checking that pal is only a length of 1#
if(length(pal) != 1){
  cli::cli_abort(c(
    paste0("{.var pal} must be a length of ", callout("1")),
    "x" = paste0("The {.var pal} object you've supplied has a length of ", error(length(pal))),
    "i" = paste0(status("Check the {.var pal} value")," you've supplied.")
  )
  )
}


#Checking that palette input is a string#
string_check <- rlang::is_character(pal)

if(!string_check){
  cli::cli_abort(c(
    paste0("{.var pal} must be of type ", callout("<character>")),
    "x" = paste0("The {.var pal} object you've supplied is of type ", error(paste0("<",class(pal),">"))),
    "i" = paste0(status("Check the {.var pal} value")," you've supplied.")
  )
  )
}

#Checking that palette input is a valid palette#
pal_check <- pal %in% names(pals)

if(!pal_check){
  cli::cli_abort(c(
    "x" = paste0(pal," is not a known ", callout("aRtpack"), " color palette."),
    "i" = paste0(status("Please enter one of the following:\n"),
                 paste(names(pals), collapse = c(rep(", ", length(pals) - 1), ", or")))
  )
  )
}


#=============================================================================#
#Direction set up#
#=============================================================================#
if(direction %in% c("rev","reverse")){

  return(rev(colorRampPalette(unlist(pals[[pal]]))(n)))

} else {
return(colorRampPalette(unlist(pals[[pal]]))(n))

}
}
