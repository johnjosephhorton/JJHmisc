#' Takes an R list and creates commands for each of the keys.
#'
#' @param key.list list of keys and values. 
#' @param parameter.file.location is the location of the parameter file.
#' @param append is a boolean for whether we shoudl append the parameter file. 
#' @return None - run for side-effects.  
#' @export

createLaTeXparameters <- function(key.list, parameter.file.location, append = FALSE){
    command.list <- sapply(names(key.list), function(key) createCommand(key, key.list[[key]]))
    if(append){
        fileConn <- file(parameter.file.location, open = "a")
        writeLines(command.list, fileConn)
        close(fileConn)
    } else {
        writeLines(command.list, parameter.file.location)
    }
}
