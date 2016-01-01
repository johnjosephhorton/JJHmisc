#' A function for generating a function that adds parameters to a tex file (parameter.file)
#' which is imported in the base LaTeX document. It is useful for passing numbers, dates and so on
#' programmatically to a write-up rather than writing them in by hand. 
#'
#' @param parameter.file file you want to hold the customer latex new commands
#' @return A function that can be used to add parameters to the file created. 
#' @export


genParamAdder <- function (parameter.file) {
    if (file.exists(parameter.file)) {
        file.remove(parameter.file)
    }
    f <- function(name, value) {
        ## Check if file exists already 
        if (file.exists(parameter.file)) {
            l <- ReadParamFile(parameter.file)
            # if this value was already in the params file
            if(name %in% names(l)){
                print(paste0("Updated: ", name, " from ", l[name], " to ", value))
            }
            l[name] = value
            file.remove(parameter.file)
            WriteListToParamFile(l, parameter.file)
        } else { 
            line <- paste0("\\newcommand{", name, "}{", value, "}")
            write(line, parameter.file, ncolumns = 1, append = TRUE)
        }
    }
    f
}
