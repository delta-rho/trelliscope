\name{makePNG}
\alias{makePNG}
\title{Make a png for a Subset}
\usage{
makePNG(dat, panelFn = NULL, file, width, height, res, lims = NULL)
}
\arguments{
  \item{dat}{a key-value pair}

  \item{panelFn}{panel function}

  \item{file}{file name for png}

  \item{width,height,res}{width, height, and resolution}

  \item{lims}{axis limits}
}
\description{
Make a png for a subset of an object of class "ddo" or
"ddf".  The user should never need to call this directly,
but the viewer needs it, so it is exported with this
documentation.
}
\author{
Ryan Hafen
}
\seealso{
\code{\link{makeDisplay}}
}

