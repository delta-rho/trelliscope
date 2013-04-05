\name{vdbSplod}
\alias{vdbSplod}
\title{Create a Scatterplot Display}
\usage{
  vdbSplod(data, id.vars = NULL, name = NULL, desc = NULL,
    cogFn = splodCogFn, plotFn = splodPlotFn,
    verbose = TRUE, ...)
}
\arguments{
  \item{data}{a data.frame or an object of class
  "splodDat"}

  \item{id.vars}{variables to ignore when computing all
  pairs of variables}

  \item{name,}{desc, cogFn, plotFn, verbose, \ldots
  parameters passed to \code{\link{vdbPlot}}}
}
\value{
  an object of class 'localDiv' that can be passed to
  \code{\link{vdbSplod}}
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
  \code{\link{vdbPlot}}, \code{\link{makeSplodDat}},
  \code{\link{vdbCogFn}}, \code{\link{splodPlotFn}}
}

