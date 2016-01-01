#' Write images and a tex file for input into a document
#'
#' @param g  graphic 
#' @param file.name.no.ext file name without an extension 
#' @param width in inches
#' @param height in inches
#' @param path path to direction where image should be written 
#' @param position figure position for tex
#' @param line.width fraction of line width for tex
#' @param caption the caption for tex
#' @param notes the notes for tex
#' @return None 
#' @export

writeImage <- function (g, file.name.no.ext, width = 15, height = 8, path = "../../writeup/plots/"
          , position = "h", line.width = "0.8", caption = "", notes = "", include.tex.wrapper = FALSE){
  png.width <- width * 72
  png.height <- height * 72
  png.file.name <- paste0(path, file.name.no.ext, ".png")
  pdf.width <- width
  pdf.height <- height
  pdf.file.name <- paste0(path, file.name.no.ext, ".pdf")
  svg.file.name <- paste0(path, file.name.no.ext, ".svg")
  pdf.file.name.no.path <- paste0(file.name.no.ext, ".pdf")
  write.image(png.file.name, g, format = "png", width = png.width, 
              height = png.height)
  write.image(pdf.file.name, g, format = "pdf", width = pdf.width, 
              height = pdf.height)
  write.image(svg.file.name, g, format = "svg", width = pdf.width, 
              height = pdf.height)
  if (include.text.wrapper){
      tex.file.name <- paste0(path, file.name.no.ext, ".tex")
      replacements = list("<<POSITION>>" = position
                        , "<<WIDTH>>" = line.width
                        , "<<CAPTION>>" = caption
                        , "<<LABEL>>" = file.name.no.ext
                        , "<<FILE>>" = pdf.file.name.no.path
                        , "<<NOTES>>" = notes
                          )		
      cat(render("\\begin{figure}[<<POSITION>>]
             \\centering
             \\begin{minipage}{<<WIDTH>> \\linewidth}
             \\caption{<<CAPTION>>} \\label{fig:<<LABEL>>}
             \\includegraphics[width = \\linewidth]{./plots/<<FILE>>}
{\\footnotesize \\emph{Notes:} <<NOTES>>} 
             \\end{minipage} 
             \\end{figure}", replacements), file= tex.file.name)
  }
}
