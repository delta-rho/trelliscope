\name{makeInputVars}
\alias{makeInputVars}
\title{Create Input Variables for a VDB Display}
\usage{
  makeInputVars(name, type, dataType = "character",
    default = "", description = "Input variable",
    args = NULL)
}
\arguments{
  \item{name}{name of the input variable}

  \item{type}{variable type - must be "text", "textarea",
  or "select" (dropdown) - (checkbox to come?)}

  \item{dataType}{the desired type of the data for this
  variable - must be "character", or "numeric"}

  \item{default}{the default value for the variable to take
  prior to user input}

  \item{args}{a character vector of dropdown values if
  type="select"}
}
\description{
  Create a 'inputVars' object which can be passed to the
  'inputVars' argument of \code{\link{vdbPlot}} to create
  per-panel input variables in the VDB viewer.
}
\author{
  Ryan Hafen
}
\seealso{
  \code{\link{vdbPlot}}
}

