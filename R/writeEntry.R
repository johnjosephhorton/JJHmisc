#' Writes a key value pair to a LaTeX parameter file
#'
#' Writes a key value pair to a LaTeX parameter file
#'
#' @param key the name of the LaTeX variable 
#' @param value the value of the variable
#' @param parameter.file the paramater file

writeEntry <- function(key, value, parameter.file){
    # Writes a single entry back to the parameter file. 
    line <- paste0("\\newcommand{", key, "}{", value, "}")
    write(line, parameter.file, ncolumns = 1, append = TRUE)
}


