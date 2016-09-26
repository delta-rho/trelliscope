utils::globalVariables(c("type", "collect", "logMsg", "displayList", "displayListDF", "displayListNames", "dataClass", "obj", "x", "a", "name"))

#' Trelliscope: Create and Navigate Large Multi-Panel Visual Displays
#'
#' An extension of Trellis Display that enables creation,
#' organization, and interactive viewing of multi-panel displays created
#' against potentially very large data sets.  The dynamic viewer tiles
#' panels of a display across the screen in a web browser and allows the
#' user to interactively page through the panels and sort and filter them
#' based on "cognostic" metrics computed for each panel.  Panels can be
#' created using many of R's plotting capabilities, including base R
#' graphics, lattice, ggplot2, and many htmlwidgets.  Conditioning is
#' handled through the datadr package, which enables Trelliscope displays
#' with potentially millions of panels to be created against terabytes of
#' data on systems like Hadoop.  While designed to scale, Trelliscope
#' displays can also be very useful for small datasets.
#'
#' \url{http://tessera.io/docs-trelliscope/}
#'
#' @name trelliscope-package
#'
#' @aliases trelliscope
#'
#' @docType package
#'
#' @import datadr ggplot2 lattice
#'
#' @author Ryan Hafen
#'
#' @keywords package
#'
#' @importFrom grDevices dev.cur dev.off png
#' @importFrom graphics hist par rasterImage
#' @importFrom stats coef lm loess median quantile setNames xtabs
#' @importFrom utils capture.output combn object.size tail
#'
#' @examples
#' help(package = trelliscope)
NULL
