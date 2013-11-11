\name{cogLoessRMSE}
\alias{cogLoessRMSE}
\title{Compute RMSE of Loess Fit Cognostic}
\usage{
  cogLoessRMSE(...,
    desc = "RMSE of residuals from loess fit")
}
\arguments{
  \item{\ldots}{arguments to be passed to
  \code{link{loess}}, such as the formula, data, smoothing
  parameters, etc.}

  \item{desc}{description of cognostic}
}
\description{
  Compute RMSE of loess fit as a cognostic to be used as
  cognostics in a trelliscope display.
}
\examples{
cogLoessRMSE(dist ~ speed, span=0.5, data=cars)
}
\author{
  Ryan Hafen
}
\seealso{
  \code{\link{cog}}
}

