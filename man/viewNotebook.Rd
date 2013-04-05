\name{viewNotebook}
\alias{viewNotebook}
\title{View a Notebook File}
\usage{
  viewNotebook(name = "index", conn = getOption("vdbConn"))
}
\arguments{
  \item{name}{name of the notebook file (.html extension
  not required)}

  \item{conn}{vdb connection info, typically stored in
  options("vdbConn") at the beginning of a session, and not
  necessary to specify here if a valid "vdbConn" object
  exists}
}
\description{
  Use \code{\link{browseURL}} to open a notebook .html file
  that is in the "notebook" directory of the vdb directory
  and that has typically been created with
  \code{\link{newNotebook}} and typeset with
  \code{\link{typeset}}.
}
\author{
  Ryan Hafen
}
\seealso{
  \code{\link{newNotebook}}, \code{\link{typeset}}
}

