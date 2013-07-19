#' Removes all lines between two line numbers in a text file. 
#'
#' @param start is the line number of the start of the delete block. 
#' @param end is the line number of the end of the delete block. 
#' @param file is the name of the text file. 
#' @return Nothing---run for the side-effect. 
#' @export

kill.lines <- function(start, end, file){
  l <- readLines(file)[-(start:end)]
  writeLines(l, file)
}
