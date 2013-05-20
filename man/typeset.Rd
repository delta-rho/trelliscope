\name{typeset}
\alias{typeset}
\title{Typeset a notebook (.Rmd) File}
\usage{
  typeset(name = "index", conn = getOption("vdbConn"))
}
\arguments{
  \item{file}{file name (extension of .Rmd is not
  required)}

  \item{conn}{VDB connection info, typically stored in
  options("vdbConn") at the beginning of a session, and not
  necessary to specify here if a valid "vdbConn" object
  exists}
}
\value{
  output .html file goes in the "notebook" directory of the
  vdb directory, to be viewed with
  \code{\link{viewNotebook}}
}
\description{
  Processes a .Rmd file created with
  \code{\link{newNotebook}}.  Basically a wrapper around
  knitr that additionally takes care of several layout
  issues to get a Bootstrap notebook web page.
}
\author{
  Ryan Hafen
}
\seealso{
  \code{\link{viewNotebook}}, \code{\link{newNotebook}}
}

