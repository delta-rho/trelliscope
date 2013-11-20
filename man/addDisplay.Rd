\name{addDisplay}
\alias{addDisplay}
\title{Add a Display to a VDB}
\usage{
  addDisplay(p, name, group = "common", desc = "",
    dim = list(height = NULL, width = NULL, aspect = NULL, res = NULL),
    conn = getOption("vdbConn"))
}
\arguments{
  \item{p}{an R plot object (lattice or ggplot) or an
  expression containing code to create a plot}

  \item{name}{the name of the display (no spaces or special
  characters)}

  \item{group}{the group the display belongs to (displays
  are organized into groups).  Defaults to "common"}

  \item{desc}{a description of the display (used in the
  viewer and in notebooks)}

  \item{dim}{a list defining aspects of the plot dimension,
  including height, width, aspect, and res (resolution of
  raster image).  defaults are 1000 (px), 1000 (px),
  "fill", and 150, respectively}

  \item{conn}{VDB connection info, typically stored in
  options("vdbConn") at the beginning of a session, and not
  necessary to specify here if a valid "vdbConn" object
  exists}
}
\description{
  Add an R display (lattice, ggplot, base R) to a
  visualization database (VDB)
}
\examples{
# see docs
}
\author{
  Ryan Hafen
}

