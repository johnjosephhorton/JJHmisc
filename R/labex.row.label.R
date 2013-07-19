#' Creates a row label for a LaTeX table. 
#'
#' @param width of cell, in cm. 
#' @param rule.pts is how much to indent the row (say to created a nested variable)
#' @param new.lines is the number of new lines before row text
#' @param new.line.char is boolean for whether to append a newline character
#' @return returns text to insert.  
#' @export

latex.row.label <- function(width, label, rule.pts = 0, new.lines = 0, new.line.char = FALSE){
  rule.block <- gsub("<rule.pts>", rule.pts, "\\rule{0pt}{<rule.pts>pt}")
  new.line.block <- paste(do.call(paste, as.list(rep(" \\\\", new.lines))), ifelse(new.line.char,"\n",""), sep=" ")
  string.literal <- "\\multirow{2}{*}{\\parbox{<width>cm}{<label>}}"
  t1 <- gsub("<width>", width, string.literal)
  t2 <- gsub("<label>", label, t1)
  paste(rule.block, t2, new.line.block, sep = "")
}
