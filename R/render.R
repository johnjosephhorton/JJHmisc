#' Creata line for a LaTeX codebook. 
#'
#' @param x tempalte
#' @param lst a hash table list of replacement rules 
#' @return filled-in template 
#' @export

render <- function(x, lst){
   "Takes a list of replacement rules and applies them to a string x."
   for(pattern in names(lst)){
        x <- gsub(pattern, lst[[pattern]], x)
    }
    x
}
