#' Inserts a line into a text file. 
#'
#' @param linenumber is the line to insert the note at; the note will then be at this linenumber. 
#' @param note the the line to inserts.
#' @param file is the file to operate on
#' @return Nothing - run for side-effect. 
#' @export 

insert.note <-function(linenumber, note, file){
  l <- readLines(file)
  new.l <- c(l[1:(linenumber-1)], paste(note, l[linenumber], sep=""), l[(linenumber + 1):length(l)])
  writeLines(new.l, file) 
}
