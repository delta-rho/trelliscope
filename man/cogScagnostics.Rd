\name{cogScagnostics}
\alias{cogScagnostics}
\title{Compute Scagnostics}
\usage{
cogScagnostics(x, y)
}
\arguments{
  \item{x,y}{}

  \item{desc}{description of cognostic}
}
\description{
Compute list of scagnostics (see \code{\cite{scagnostics}})
to be used as cognostics in a trelliscope display.
}
\examples{
cogScagnostics(cars$dist, cars$speed)
}
\author{
Ryan Hafen
}
\seealso{
\code{\link{cog}}
}

