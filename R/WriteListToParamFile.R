#' Writes a list of all parameters to a file
#'
#' Writes a list of all parameters to a file
#'
#' @param full.list
#' @param file.name
#' @return none 

WriteListToParamFile <- function(full.list, file.name){
    for(key in names(full.list)){
        writeEntry(key, full.list[key], file.name)
    }
}
