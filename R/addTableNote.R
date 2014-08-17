#' Writes an list of regression models generated via table to a file. 
#'
#' @param stargazer.object is a list of models. 
#' @param out.file path to where table should be written
#' @param note the note to add at the bottom of the table 
#' @return Nothing - writes to file out.file
#' @export

AddTableNote <- function(stargazer.object, out.file, note, adjust = 3){
    "Adds a table note to a stargazer table. It assumes that the
     last three lines of stargazer output are useless."
    n <- length(stargazer.object)
    write(stargazer.object[1:(n - adjust)], out.file)
    write(note, out.file, append = TRUE)
    write(c("\\end{tabular}", "\\end{table}"), out.file, append = TRUE)
} 


