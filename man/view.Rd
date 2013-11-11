\name{view}
\alias{view}
\title{Run Shiny Display Viewer}
\usage{
  view(conn = getOption("vdbConn"), group = NULL,
    name = NULL, port = 8100L, openBrowser = TRUE)
}
\arguments{
  \item{conn}{VDB connection info, typically stored in
  options("vdbConn") at the beginning of a session, and not
  necessary to specify here if a valid "vdbConn" object
  exists}

  \item{group,}{name optional parameters to load the viewer
  with a pre-specified display}

  \item{port}{what port to use for the viewer}

  \item{openBrowser}{should the browser be automatically
  launched?}
}
\description{
  Run server-side viewer on local machine
}
\author{
  Ryan Hafen
}

