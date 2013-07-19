#' Returns a line from a file. 
#'
#' @param linenumber is the line number of the line to grab. 
#' @param file is the file to grab the line from. 
#' @return A line of text. 
#' @export

get.line <- function(linenumber, file) readLines(file)[linenumber]
