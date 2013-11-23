#' Write images 
#'
#' @param g  graphic 
#' @param file.name.no.ext file name without an extension 
#' @param width in inches
#' @param height in inches
#' @param path path to direction where image should be written 
#' @return None 
#' @export

writeImage <- function(g, file.name.no.ext, width = 15, height = 8, path = "../../writeup/plots/"){
    png.width <- width * 72 #  72 is the default ppi for png
    png.height <- height * 72
    png.file.name <- paste0(path, file.name.no.ext, ".png")
    pdf.width <- width
    pdf.height <- height
    pdf.file.name <- paste0(path, file.name.no.ext, ".pdf")
    svg.file.name <- paste0(path, file.name.no.ext, ".svg")
    write.image(png.file.name, g, format = "png", width = png.width, height = png.height)
    write.image(pdf.file.name, g, format = "pdf", width = pdf.width, height = pdf.height)
    write.image(svg.file.name, g, format = "svg", width = pdf.width, height = pdf.height)
}
