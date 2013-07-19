#' Takes a list serving as a dictionary and appends a string to the key of each list 
#' Suppose we have 'dog = list(legs = 4, smell = good)'
#' flattenList('DOG', list(legs = 4, smell = good))
#' return list(DOGlegs = 4, DOGsmell = good)
#' @param first.key the new key string to append 
#' @param values the list serving as a dictionary
#' @return new.list   
#' @export

flattenList <- function(first.key, values){
    new.list <- list()
    for(second.key in names(values)){
        new.list[[ paste(first.key, toupper(second.key), sep ="") ]] = values[[ second.key ]]
    }
    new.list
}
