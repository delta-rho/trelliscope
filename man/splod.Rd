\name{splod}
\alias{splod}
\title{Create a Scatterplot Display}
\usage{
  splod(data, id.vars = NULL, name = NULL, desc = NULL,
    cogFn = splodCogFn, plotFn = splodPlotFn,
    verbose = TRUE, ...)
}
\arguments{
  \item{data}{a data.frame or an object of class
  "splodDat"}

  \item{id.vars}{variables to ignore when computing all
  pairs of variables}

  \item{name,}{desc, cogFn, plotFn, verbose, \ldots
  parameters passed to \code{\link{makeDisplay}}}
}
\value{
  an object of class 'localDiv' that can be passed to
  \code{\link{splod}}
}
\description{
  Create a scatterplot display (splod)
}
\author{
  Ryan Hafen
}
\references{
  Wilkinson's scagnostics paper.
}
\seealso{
  \code{\link{makeDisplay}}, \code{\link{makeSplodDat}},
  \code{\link{trsCogFn}}, \code{\link{splodPlotFn}}
}

