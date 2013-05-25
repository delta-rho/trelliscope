\name{websync}
\alias{websync}
\title{Sync VDB and notebook Files to a Web Server}
\usage{
  websync(conn = getOption("vdbConn"), rsync = NULL)
}
\arguments{
  \item{conn}{The VDB connection settings}

  \item{rsync}{Location of rsync binary}
}
\value{
  nothing
}
\description{
  Sync VDB and notebook files to a web server
}
\details{
  If you are syncing via ssh, this only works if public key
  authentication is enabled between your local machine and
  the remote server.
}
\author{
  Ryan Hafen
}
\seealso{
  \code{\link{typeset}}, \code{\link{makeDisplay}}
}

