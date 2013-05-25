\name{vdbMakeConnTemplate}
\alias{vdbMakeConnTemplate}
\title{Create a Template VDB Connection File}
\usage{
  vdbMakeConnTemplate(vdbPrefix, name = "myVDB",
    overwrite = FALSE)
}
\arguments{
  \item{vdbPrefix}{The location of the VDB directory on the
  local file system}

  \item{name}{name of the VDB}

  \item{overwrite}{Overwrite any existing 'conn.R' file}
}
\value{
  nothing
}
\description{
  Create a template VDB connection file
}
\details{
  This is called by vdbInit() so it should rarely need to
  be used.
}
\author{
  Ryan Hafen
}
\seealso{
  \code{\link{vdbConnect}}, \code{\link{vdbInit}}
}

