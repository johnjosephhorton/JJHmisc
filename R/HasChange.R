#' Writes an list of regression models generated via table to a file. 
#'
#' @param x vector 
#' @param k indicators whether there is a change
#' @param epsilon minimimally detectabl echange. 
#' @return a vector 
#' @export

HasChange <- function(x, k, epsilon = 0.001){
    if (k > 0) {
        shift.type <- "lag"
    } else {
        shift.type <- "lead"
    }
    cv <- as.numeric(I(abs(x - shift(x, abs(k), type = shift.type)) > epsilon))
    cv[is.na(cv)] <- 0
    cv
}


