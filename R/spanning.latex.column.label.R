#' Creates a spanning multicolumn for a LaTeX table. 
#'
#' @param num.columns is the number of columns the multicolumn should span
#' @param width is the width in cm
#' @param label is the label for the column. 
#' @return text snippet. 
#' @export

spanning.latex.column.label <- function(num.columns, width, label){
  string.literal <- "\\multicolumn{<num.columns>}{c}{\\parbox[t][][t]{<width>cm}{\\emph{DV = } <label>}}"
  t1 <- gsub("<num.columns>", num.columns, string.literal)
  t2 <- gsub("<width>", width, t1)
  gsub("<label>", label, t2)
}
