#' Writes a plot as an image to some specificed location.  
#'
#' @param filename where to write the file
#' @param g is the plot
#' @param width width
#' @param heigh height
#' @param format is the file format to use. 
#' @return None - run for side-effects.  
#' @export

write.image <- function(filename, g, width = 5, height = 5,  format = "png"){
  "Writes a passed ggplot, g, to the filename. The default format is png."
  do.call(format, list(filename, width, height))
   print(g)
  dev.off()
}
