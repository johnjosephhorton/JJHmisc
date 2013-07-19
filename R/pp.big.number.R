#' Formats a large number with commas. 
#'
#' @param e the number. 
#' @return the number with comments. 
#' @export

pp.big.number <- function(e) formatC(as.numeric(e), big.mark=",", format="f", digits=0)
