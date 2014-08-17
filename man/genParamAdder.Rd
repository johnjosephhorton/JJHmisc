\name{genParamAdder}
\alias{genParamAdder}
\title{A function for generating a function that adds parameters to a tex file (parameter.file)
which is imported in the base LaTeX document. It is useful for passing numbers, dates and so on
programmatically to a write-up rather than writing them in by hand.}
\usage{
genParamAdder(parameter.file)
}
\arguments{
  \item{parameter.file}{file you want to hold the customer
  latex new commands}
}
\value{
A function that can be used to add parameters to the file
created.
}
\description{
A function for generating a function that adds parameters
to a tex file (parameter.file) which is imported in the
base LaTeX document. It is useful for passing numbers,
dates and so on programmatically to a write-up rather than
writing them in by hand.
}

