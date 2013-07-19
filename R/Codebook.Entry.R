#' Creata line for a LaTeX codebook. 
#'
#' @param var.name name of the variable. 
#' @param label is the longer, label for the note. 
#' @param note is a note. 
#' @return line to be written. 
#' @export

Codebook.Entry <- function(var.name, label, note)
    paste("\\item[", label, "](\\verb|", var.name, "|). ", note, sep = "")
