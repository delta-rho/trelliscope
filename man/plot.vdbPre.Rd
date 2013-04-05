\name{plot.vdbPre}
\alias{plot.vdbPre}
\title{Plot results form vdbPrepanel}
\usage{
  plot.vdbPre(lims, layout = c(2, 2), as.table = TRUE,
    strip = FALSE, strip.left = TRUE,
    between = list(y = 0.25), xlab = "Rank",
    ylab = "Panel Limits", ...)
}
\arguments{
  \item{lims}{object of class "vdbPre" created by
  \code{\link{vdbPrepanel}}}

  \item{layout,}{as.table, strip, strip.left, between,
  xlab, ylab, \ldots parameters for the lattice plot that
  is output (these are defaults - can ignore unless you
  want fine control)}
}
\value{
  object of class "trellis" (plotted by default)
}
\description{
  Plot results form vdbPrepanel
}
\details{
  This function plots the sorted axis ranges for the x and
  y axis for the case of "same" (all axis limits share the
  same range) and "sliced" (all axis limits share the) and
  can be useful in helping determine how to ultimately set
  the limits.
}
\examples{
\dontrun{
irisSplit <- divide(iris, "Species")
irisPreFn <- function(x) {
   list(
      xlim = range(x$Sepal.Length),
      ylim = range(x$Sepal.Width)
   )
}
irisPre <- vdbPrepanel(irisSplit, preFn=irisPreFn)
plot(irisPre)
}
}
\author{
  Ryan Hafen
}
\seealso{
  \code{\link{vdbPrepanel}}, \code{\link{vdbPlot}}
}

