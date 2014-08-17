#' Returns significance stars from a p-value 
#'
#' @param p p-value
#' @return symbol 
#' @export
starLabel <- function(p) {
    cuts <- c(0.001, 0.01, 0.05, 0.10)
    if( any(p <= cuts) ){
        symbol <- c("***", "**", "*", "$\\dagger$")[p <= cuts][1]
    } else {
        symbol <- ""
    }
    symbol 
}
