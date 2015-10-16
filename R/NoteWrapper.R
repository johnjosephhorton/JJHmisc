#' Places a table note in a minitable
#'
#' Places a table note in a minitable
#'
#' @param text.width the with of the page to use up (0 to 1.0)
#' @export 

NoteWrapper <- function(x, text.width = 0.90){
    paste0("\\begin{minipage}{", text.width, "\\textwidth} \n \\emph{Notes:} ",
           x, "\n \\end{minipage}")
}
