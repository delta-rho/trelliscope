\name{vdbUpdateViewer}
\alias{vdbUpdateViewer}
\title{Update Project VDB Client Side Viewer Files}
\usage{
  vdbUpdateViewer(conn = getOption("vdbConn"))
}
\arguments{
  \item{conn}{vdb connection info, typically stored in
  options("vdbConn") at the beginning of a session, and not
  necessary to specify here if a valid "vdbConn" object
  exists}
}
\value{
  nothing
}
\description{
  Useful to do after a new update of the \code{vdb} package
  has been installed.  Moves the viewer files from the
  package to the project directory.
}
\author{
  Ryan Hafen
}
\seealso{
  \code{\link{vdbInit}}, \code{\link{vdbUpdateOptions}}
}

