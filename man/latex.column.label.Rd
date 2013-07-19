\name{latex.column.label}
\alias{latex.column.label}
\title{Creates a parbox column label for a LaTeX table}
\usage{
  latex.column.label(width, number, label,
    include.DV = TRUE)
}
\arguments{
  \item{width}{is the column width, in centimeters.}

  \item{number}{is the model number, e.g. (1)}

  \item{label}{is the label of the model e.g., "Employer
  hired anyone?"}

  \item{include.DV}{is a boolean for whether box you have a
  "DV = " bit}
}
\value{
  Returns a text snippet
}
\description{
  Creates a parbox column label for a LaTeX table
}

