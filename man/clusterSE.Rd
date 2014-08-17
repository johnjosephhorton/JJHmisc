\name{clusterSE}
\alias{clusterSE}
\title{Create clustered standard errors for fixed effect regression.
Note that the dataframe has to be sorted by the cluster.name to work.}
\usage{
clusterSE(cluster.name, model, df)
}
\arguments{
  \item{cluster.name}{is tje cluster identifier.}

  \item{model}{is the lm object estimated on the demeanded
  data}

  \item{df}{is the demead data.frame the model is fit on}
}
\value{
se
}
\description{
Create clustered standard errors for fixed effect
regression. Note that the dataframe has to be sorted by the
cluster.name to work.
}

