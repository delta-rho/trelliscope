\name{newNotebook}
\alias{newNotebook}
\title{Create a New Notebook File}
\usage{
  newNotebook(name = "index", blank = FALSE,
    title = "My Test Page", pageTitle = title,
    author = "Author Name", toc = TRUE, css = NULL,
    conn = getOption("vdbConn"))
}
\arguments{
  \item{name}{name of the .Rmd file to be created}

  \item{blank}{do you just want a blank page, or do you
  want a template with several markdown examples filled out
  for reference?}

  \item{pageTitle,}{title, author, toc, css these are
  parameters to be placed in the \code{\link{bsSetup}} call
  at the top of the .Rmd file, and can be ignored if you
  plan to change them later in the file itself}

  \item{conn}{VDB connection info, typically stored in
  options("vdbConn") at the beginning of a session, and not
  necessary to specify here if a valid "vdbConn" object
  exists}
}
\value{
  creates a new .Rmd file that will go in the "notebook"
  directory of the vdb directory
}
\description{
  Create a new notebook file skeleton.
}
\author{
  Ryan Hafen
}
\seealso{
  \code{\link{typeset}}, \code{\link{viewNotebook}},
  \code{\link{bsSetup}}
}

