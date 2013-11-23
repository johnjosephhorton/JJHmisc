#' For each variable in a dataset, tests whether or not that variable was used. 
#'
#' @param f.file
#' @param data.frame
#' @param note is a note. 
#' @return line to be written. 
#' @export



wasItUsed <- function(r.file, data.frame){
    source.code <- readChar(r.file, file.info(r.file)$size)
    vars <- colnames(data.frame)
    used <- list()
    for(var in vars){
        used[var] <- FALSE
        if (grepl(var, source.code)){
            used[var] <- TRUE
        }
    }
    used
}
