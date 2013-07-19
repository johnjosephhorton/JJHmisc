#' Takes the sample size line from a table and replaces all N's with the big.mark notation
#' E.g., 1234 becomes 1,234
#' @param table.line LaTeX line of sample sizes. 
#' @return Line w/ all the numbers properly commented. 
#' @export

fix.sample.size.line <- function(table.line){
  entries <- strsplit(gsub("\\s","", gsub("\\\\","",table.line)) , '&')[[1]]
  new <- ""
  for(e in entries){
    if (e == "N"){
     new <- paste(new,  e)
    }
    if (e == ""){
      new <- paste(new, "&&")
    }
    if (e != "N" && e != ""){
      new <- paste(new, "\\multicolumn{1}{c}{", formatC(as.numeric(e), big.mark=",", format="f", digits=0), "}")
    }
  }
  new <- paste(new, "\\\\ \n ")
  new
}
