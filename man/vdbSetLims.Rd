\name{vdbSetLims}
\alias{vdbSetLims}
\title{Specify rules for x and y limits for a vdbPlot}
\usage{
  vdbSetLims(lims, x = "same", y = "same",
    xQuant = c(0, 1), yQuant = c(0, 1), xRangeQuant = 1,
    yRangeQuant = 1, prop = 0.07)
}
\arguments{
  \item{lims}{object of class "vdbPre"}

  \item{x}{x-axis limits rule (either "same", "sliced", or
  "free" - see details)}

  \item{y}{y-axis limits rule (either "same", "sliced", or
  "free" - see details)}

  \item{xQuant}{lower and upper quantiles at which to cut
  off x-axis limits, in the case of outliers.  Used when
  x="same".}

  \item{yQuant}{same as xQuant but for y-axis}

  \item{xQuantRange}{a single upper quantile at which to
  cut off the x-axis range, used when x="sliced", used in
  the case of a few splits having abnormally high range,
  which are wished to be excluded}

  \item{yQuantRange}{same as xQuantRange but for y-axis}
}
\value{
  object of class "vdbLims", which can be used in a call to
  \code{\link{vdbPlot}}
}
\description{
  Based on results from \code{\link{vdbPrepanel}}, specify
  rules that will determine x and y axis limits.
}
\details{
  This function reduces the list of axis limits computed
  for each split of a data set to an overall axis limit
  rule for the plot.

  About "x" and "y" parameters: This is the same as in
  lattice.  From lattice documentation: A character string
  that determines how axis limits are calculated for each
  panel. Possible values are "same" (default), "free" and
  "sliced". For relation="same", the same limits, usually
  large enough to encompass all the data, are used for all
  the panels. For relation="free", limits for each panel is
  determined by just the points in that panel. Behavior for
  relation="sliced" is similar, except that the length (max
  - min) of the scales are constrained to remain the same
  across panels.
}
\examples{
irisSplit <- divide(iris, "Species")
irisPreFn <- function(x) {
   list(
      xlim = range(x$Sepal.Length),
      ylim = range(x$Sepal.Width)
   )
}
irisPre <- vdbPrepanel(irisSplit, preFn=irisPreFn)
irisLims <- vdbSetLims(irisPre, x="same", y="sliced")
}
\author{
  Ryan Hafen
}
\seealso{
  \code{\link{vdbPrepanel}}, \code{\link{vdbPlot}}
}

