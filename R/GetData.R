#' #' A function for generating a function that adds parameters to a tex file (parameter.file)
#' which is imported in the base LaTeX document. It is useful for passing numbers, dates and so on
#' programmatically to a write-up rather than writing them in by hand. 
#'
#' @param table.name name of the table on ODW that you want
#' @param file.name full path to where you want to store it locally
#' @param refresh go grab the file? 
#' @return the datatable in question 
#' @export

GetData <- function(table.name, file.name, refresh){
    if (refresh) {
        df.tmp <- dbGetQuery(con, paste0("select * from ", table.name))
        saveRDS(df.tmp, file.name)
    } else {
        df.tmp <- readRDS(file.name)
    }
    data.table(df.tmp)
}

