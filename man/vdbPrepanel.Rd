\name{vdbPrepanel}
\alias{vdbPrepanel}
\title{Prepanel Function for vdb Plots}
\usage{
  vdbPrepanel(data, preFn = NULL, mapred = NULL,
    verbose = TRUE, calledFromRhipe = FALSE)
}
\arguments{
  \item{dat}{an object of class "localDiv" or "rhData"}

  \item{preFn}{a prepanel function that returns a list
  specifying \code{xlim} and \code{ylim} for determining
  axis limits, and optionally \code{dx} and \code{dy} for
  determining aspect ratio (used to define slopes of line
  segments used for banking computations).  preFn can also
  be a plotFn (see \code{\link{vdbPlot}}) that returns
  either an object of class "trellis" or "ggplot", since
  xlim and ylim can be determined from these.}

  \item{mapred}{a list of parameters to be sent to
  \code{\link{rhwatch}} if dat is of class "rhData"}

  \item{verbose}{print status messages?}

  \item{calledFromRhipe}{ignore this parameter (don't use
  it)}
}
\value{
  object of class "vdbPre".  This is a list of the x and y
  axis ranges for each split, along with the aspect ratio
  banking value if \code{dx} and \code{dy} are supplied in
  \code{preFn}.  Can be used with \code{\link{plot.vdbPre}}
  and \code{\link{vdbSetLims}}.
}
\description{
  Prepanel function for vdb plots
}
\details{
  Apply a prepanel function to objects of class "localDiv"
  (obtained from splitDF()) or "rhData" to determine ranges
  of x and y axis limits.  Useful in conjunction with
  \code{\link{vdbSetLims}}.

  The plot method plots the sorted axis ranges for the x
  and y axis for the case of "same" (all axis limits share
  the same range) and "sliced" (all axis limits share the)
  and can be useful in helping determine how to ultimately
  set the limits.

  You don not need to use \code{vdbPrepanel} to ultimately
  create a \code{\link{vdbPlot}}, but if you bypass, you
  will either need to specify your own limits in your plot
  command, or do nothing, in which case each individual
  plot will have limits based on the data in the split
  being plotted (the axes will be "free").

  Axis limits are very important.  What makes viewing
  groups of plots of subsets of data ("small multiples") so
  powerful is being able to make meaningful visual
  comparisons across plots.  This is much easier to do if
  scales for each plot are commensurate.

  This function is also useful for identifying subsets with
  very large outlying values, and in conjunction with
  \code{\link{vdbSetLims}}, allows you to account for that
  prior to the expensive process of creating all of the
  plots.

  In the future, there will be helper functions for special
  types of plots, such as histograms, etc. to help the user
  more easily provide the \code{xlim} and \code{ylim}
  components of the prepanel function.
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
plot(irisPre)#' }
}
\author{
  Ryan Hafen
}
\seealso{
  \code{\link{x}} \code{\link{plot.vdbPre}},
  \code{\link{vdbSetLims}}, \code{\link{vdbPlot}},
  \code{\link{localDiv}}, \code{\link{rhData}}
}

