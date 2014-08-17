#' Create a theme with large font sizes 
#'
#' @param font.size 
#' @return ggplot theme 
#' @export

big.letter.theme <- function(font.size){
    theme(axis.text.y = element_text(size = font.size),
                          axis.text.x = element_text(size = font.size),
                          strip.text.x = element_text(size = font.size),
                          axis.title.x = element_text(size = font.size),
                          axis.title.y = element_text(size = font.size)
          )
}
