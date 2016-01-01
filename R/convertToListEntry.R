#' Take a line from a genParam parameter file and turns it into a list element
#'
#' Take a line from a genParam parameter file and turns it into a list element
#'
#' @param line a line from a parameter file
#' @return list selement 

convertToListEntry <- function(line){
    first.pattern <- "\\{.*\\}\\{"
    m <- regexpr(first.pattern, line)
    key <- gsub("[}{]", "", regmatches(line, m))

    second.pattern <- "\\}\\{.*\\}"
    m <- regexpr(second.pattern, line)
    value <- gsub("[}{]", "", regmatches(line, m))
    l = list()
    l[key] = value
    l 
}
