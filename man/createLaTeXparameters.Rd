\name{createLaTeXparameters}
\alias{createLaTeXparameters}
\title{Takes an R list and creates commands for each of the keys.}
\usage{
  createLaTeXparameters(key.list, parameter.file.location,
    append = FALSE)
}
\arguments{
  \item{key.list}{list of keys and values.}

  \item{parameter.file.location}{is the location of the
  parameter file.}

  \item{append}{is a boolean for whether we shoudl append
  the parameter file.}
}
\value{
  None - run for side-effects.
}
\description{
  Takes an R list and creates commands for each of the
  keys.
}

