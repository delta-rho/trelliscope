\name{vdbPlot}
\alias{vdbPlot}
\title{Create a vdb Display}
\usage{
  vdbPlot(data, name, group = "common", desc = "",
    plotDim = list(height = NULL, width = NULL, aspect = NULL, res = NULL),
    plotFn = NULL,
    lims = list(x = "free", y = "free", preFn = NULL),
    cogFn = NULL, cogDesc = NULL, inputVars = NULL,
    conn = getOption("vdbConn"), storage = NULL,
    subDirSize = 1000, subDirN = 0, verbose = TRUE,
    parallel = FALSE, rhFail = TRUE, mapred = NULL,
    calledFromRhipe = FALSE)
}
\arguments{
  \item{dat}{data of class "localDiv", "rhData", "trellis",
  "ggplot", or "expression"}

  \item{name}{the name of the display (no spaces or special
  characters)}

  \item{group}{the group the display belongs to (displays
  are organized into groups).  Defaults to "common"}

  \item{desc}{a description of the display (used in the
  viewer and in notebooks)}

  \item{plotDim}{a list defining aspects of the plot
  dimension, including height, width, aspect, and res
  (resolution of raster image).  defaults are 480 (px), 40,
  "fill", and 150, respectively}

  \item{plotFn}{a function that produces a plot and takes
  one argument, which will be the current split of the data
  being passed to it.  Useful to test with
  plotFn(divExample(dat)).  Must return either an object of
  class "ggplot", "trellis", or "expression" (of base plot
  commands)}

  \item{lims}{either an object of class "vdbLims" as
  obtained from \code{\link{vdbSetLims}} or a list with
  elements x, y, and preFn, that specify how to apply
  \code{\link{vdbPrepanel}} and \code{\link{vdbSetLims}}}

  \item{cogFn}{a function that produces a single row of a
  data frame where each column is a cognostic feature .
  The function should takes one argument, which will be the
  current split of the data being passed to it.  Useful to
  test with cogFn(divExample(dat))}

  \item{cogDesc}{a vector of descriptions for the cognostic
  variables specified by cogFn}

  \item{inputVars}{input variables that will allow user
  input in the viewer, defined by \code{\link{inputVars}}}

  \item{conn}{vdb connection info, typically stored in
  options("vdbConn") at the beginning of a session, and not
  necessary to specify here if a valid "vdbConn" object
  exists}

  \item{storage}{how to store the plots and metadata for
  the display.  See details}

  \item{subDirSize}{the approximate number plots per
  subdirectory.  Only used of \code{storage=="local"}.  If
  number of plots is less, there will not be subdiretories.
  If set to 0, there will not be subdirectories.}

  \item{verbose}{print status messages?}

  \item{parallel}{for storage="local" create plots in
  parallel (currently not working)}

  \item{mapred}{parameters to be passed to the Rhipe
  mapreduce job (see \code{\link{rhwatch}})}

  \item{calledFromRhipe}{don't mess with this}
}
\description{
  Create a vdb display
}
\details{
  there are a lot of details... see the vignette:
  \code{browseVignettes("vdb")}

  Many of the parameters are optional or have defaults.

  Storage options for plots: \describe{ \item{local:}{plots
  for each of the panels will be stored in the displays
  directory of the vdb directory} \item{mongo:}{plots will
  be stored in mongodb using the mongodb connection as
  specified by "vdbConn" (see vignette) - this is very
  experimental} \item{hdfs:}{plots will be stored in a
  mapfile on HDFS - can only be done with data of class
  "rhSplit"} \item{localData:}{instead of storing plots,
  data, plotFn, etc. will be stored locally and plotFn will
  be applied to the data on-the-fly in the viewer} } There
  are so many options because there are several tradeoffs,
  described in the vignette.
}
\examples{
# see docs
}
\author{
  Ryan Hafen
}
\seealso{
  \code{\link{vdbPrepanel}}, \code{\link{vdbSetLims}},
  \code{\link{inputVars}}, \code{\link{divide}}
}

