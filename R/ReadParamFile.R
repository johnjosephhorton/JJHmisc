#' Reads in a parameter file constructed from a genParam file 
#'
#' Reads in a parameter file constructed from a genParam file
#'
#' @param file.name the file where the parameters
#' @return a list with keys and values corresponding to the parameter file

ReadParamFile <- function(file.name){
    # returns the contents of a parameter file as a list 
    lines <- readLines(file.name, file.info(file.name)$size)
    do.call("c", lapply(lines, convertToListEntry))
}
