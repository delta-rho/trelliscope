\name{vdbConnect}
\alias{vdbConnect}
\title{Connect to a VDB}
\usage{
  vdbConnect(path = NULL)
}
\arguments{
  \item{path}{The location of the VDB directory on the
  local file system

  #details This locates the VDB connection info (located in
  *vdbPrefix*/conn.R) and loads it into the environment}
}
\value{
  nothing
}
\description{
  Connect to a VDB
}
\author{
  Ryan Hafen
}
\seealso{
  \code{\link{vdbMakeConnTemplate}}, \code{\link{vdbInit}}
}

