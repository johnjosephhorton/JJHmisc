#' Approximates the Python 'zip' function - creates a list that serves as a dictionary between
#' the keys and values vectors. They keys are coerced into characters.
#' 
#' @param keys the vector you want to be the keys
#' @param values the vector you want the keys to map to . 
#' @return A dictionary mapping the keys to values 
#' @export 


dzip <- function(keys, values){
    if(length(keys) != length(values)){
        print("Vectors have to be the same length!")
        return(NULL)
    }
    N <- length(keys)
    dict <- list()
    for(i in 1:N){
        key <- as.character(keys[i])
        value <- values[i]
        dict[key] <- value 
    }
    dict
}

