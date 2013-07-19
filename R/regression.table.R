#' Writes an list of regression models generated via table to a file. 
#'
#' @param models is a list of models. 
#' @param renames is a list of renames.
#' @param out.file path to where table shoul be written
#' @return Nothing - writes to file out.file
#' @export
regression.table <- function(models, renames, out.file){
  table.object <- do.call(mtable, models)
  latex.table <- toLatex(relabel(table.object, renames))
  print(latex.table)
  fileConn <- file(out.file)
  writeLines(latex.table, fileConn)
  close(fileConn)
}
