#' Art Pals
#'
#' @param pal A character vector of the desired aRttools palette type.
#' @param n The numbers of colors desired in the output.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#'icecream_pal <- art_pals("icecream", 20)
#'
art_pals <- function(pal = NULL, n = 5){

if(is.null(pal)){
  pal <- "ocean"
}

pals <- list(beach = list(c("#E8B381", "#E7D2C1", "#7EC7F1", "#3DB0DD", "#009DEA", "#006ACD")),
             cosmos = list(c("#562B91", "#5B1A61", "#BC3AA5", "#E73C88", "#4A77B5", "#1F186C")),
             explorer = list(c("#F75231", "#399CFF", "#FFC631", "#7BCE52", "#5ACEE7", "#A55239", "#B55AA5", "#D6B55A", "#6363B5")),
             gemstones = list(c("#29295D", "#7C5EB8", "#18AFB8", "#005038", "#819126", "#E6C14E", "#A14C0C", "#9F232E")),
             icecream = list(c("#FCFEFD", "#FFECA8", "#FAD8E9", "#8FD9F4", "#BF7014", "#7B4000")),
             imagination = list(c("#DD0B80", "#E58E54", "#F1E109", "#A8CD35", "#07ADEB", "#572E76")),
             majestic = list(c("#0F061B", "#1D0B35", "#350A3C", "#471049", "#881F74")),
             nature = list(c("#686C20", "#1D3A1D", "#C77F42", "#532F00", "#9C1800", "#5B0000")),
             ocean = list(c("#12012E", "#144267", "#15698C", "#0695AA", "#00F3FF")),
             plants = list(c("#5ebf61", "#2f8032", "#206322", "#0c570f", "#0a380b", "#041f05")),
             rainbow = list(c("#AF3918", "#A21152", "#822B75", "#612884", "#154BAF", "#0B82B9", "#277E9D", "#488E35", "#E3A934")),
             sunnyside = list(c("#F6BF07", "#F67C21", "#ED155A", "#F61867"))
             )

pal_check <- pal %in% names(pals)

if(!pal_check){
  stop(paste0("`",pal,"` is not a known aRttools color palette. Please enter one of the following:\n",paste(names(pals), collapse = c(rep(", ", length(pals) - 1), ", or"))))

}

return(colorRampPalette(unlist(pals[[pal]]))(n))

}
