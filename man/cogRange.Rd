\name{cogRange}
\alias{cogRange}
\title{Compute Range Cognostic}
\usage{
cogRange(x, desc = "range (max - min)")
}
\arguments{
  \item{x}{numeric vector from which to compute the range}

  \item{desc}{description of cognostic}
}
\description{
Compute range to be used as cognostics in a trelliscope
display.
}
\examples{
cogRange(rnorm(100))
}
\author{
Ryan Hafen
}
\seealso{
\code{\link{cog}}
}

