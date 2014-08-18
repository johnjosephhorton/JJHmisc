\name{GetData}
\alias{GetData}
\title{#' A function for generating a function that adds parameters to a tex file (parameter.file)
which is imported in the base LaTeX document. It is useful for passing numbers, dates and so on
programmatically to a write-up rather than writing them in by hand.}
\usage{
GetData(table.name, file.name, refresh)
}
\arguments{
  \item{table.name}{name of the table on ODW that you want}

  \item{file.name}{full path to where you want to store it
  locally}

  \item{refresh}{go grab the file?}
}
\value{
the datatable in question
}
\description{
#' A function for generating a function that adds
parameters to a tex file (parameter.file) which is imported
in the base LaTeX document. It is useful for passing
numbers, dates and so on programmatically to a write-up
rather than writing them in by hand.
}

