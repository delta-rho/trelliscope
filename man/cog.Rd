\name{cog}
\alias{cog}
\title{Create a Cognostics Object}
\usage{
  cog(val = NULL, desc = "", type = NULL)
}
\arguments{
  \item{val}{a scalar value (numeric, characer, date,
  etc.)}

  \item{desc}{a description for this cognostic value}

  \item{the}{desired type of cognostic you would like to
  compute (see details)}
}
\value{
  object of class "cog"
}
\description{
  Create a cognostics object.  To be used inside of the
  function passed to the \code{cogFn} argument of
  \code{\link{makeDisplay}} for each cognostics value to be
  computed for each subset.
}
\details{
  Different types of cognostics can be specified through
  the \code{type} argument that will effect how the user is
  able to interact with those cognostics in the viewer.
  This can usually be ignored because it will be inferred
  from the implicit data type of \code{val}.  But there are
  special types of cognostics, such as geographic
  coordinates and relations (not implemented) that can be
  specified as well.  Current possibilities for \code{type}
  are "key", "int", "num", "fac", "date", "time", "geo",
  "rel", "hier".
}
\author{
  Ryan Hafen
}
\seealso{
  \code{\link{makeDisplay}}, \code{\link{cogRange}},
  \code{\link{cogMean}}, \code{\link{cogScagnostics}},
  \code{\link{cogLoessRMSE}}
}

