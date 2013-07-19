#' Creates a parbox column label for a LaTeX table
#'
#' @param width is the column width, in centimeters. 
#' @param number is the model number, e.g. (1)
#' @param label is the label of the model e.g., "Employer hired anyone?" 
#' @param include.DV is a boolean for whether box you have a "DV = " bit
#' @return Returns a text snippet 
#' @export
latex.column.label <- function(width, number, label, include.DV = TRUE){
  if(include.DV) dv.snippet <- "\\emph{DV=}" else dv.snippet <- "" 
  string.literal <- paste("\\parbox[t][][t]{<width>cm}{\\centering (<number>) \\\\", dv.snippet, "<label>}", sep = " ")
  t1 <- gsub("<width>", width, string.literal)
  t2 <- gsub("<number>", number, t1)
  gsub("<label>", label, t2)
}
