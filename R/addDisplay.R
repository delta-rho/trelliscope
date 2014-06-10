
#' Add a Display to a VDB
#' 
#' Add an R display (lattice, ggplot, base R) to a visualization database (VDB)
#' 
#' @param p an R plot object (lattice or ggplot) or an expression containing code to create a plot
#' @param name the name of the display (no spaces or special characters)
#' @param group the group the display belongs to (displays are organized into groups).  Defaults to "common"
#' @param desc a description of the display (used in the viewer and in notebooks)

#' @param conn VDB connection info, typically stored in options("vdbConn") at the beginning of a session, and not necessary to specify here if a valid "vdbConn" object exists
#' 
#' @author Ryan Hafen
#' 
#' @examples
#' # see docs
#' 
#' @export
addDisplay <- function(
   p,
   name,
   group = "common",
   desc = "",
   height = 800,
   width = 800,
   conn = getOption("vdbConn")
) {
   validateConn(conn)
   vdbPrefix <- conn$path
   
   # get display prefix (and move old display to backup if it already exists)
   displayPrefix <- file.path(vdbPrefix, "displays", group, name)
   checkDisplayPath(displayPrefix, verbose)
   
   makePNG(dat = p, 
      file = file.path(displayPrefix, "thumb.png"), 
      width = width, height = height)
   
   modTime <- Sys.time()
   
   updateDisplayList(list(
      group = group, 
      name = name, 
      desc = desc, 
      n = 1, 
      preRender = NA, 
      dataClass = NA, 
      cogClass = NA, 
      height = height,
      width = width,
      updated = modTime, 
      keySig = NA
   ), conn)
   
   displayObj <- list(
      name = name,
      group = group,
      desc = desc,
      preRender = NA,
      panelFn = NA,
      panelDataSource = NA,
      cogFn = NA,
      n = 1, 
      cogDatConn = NA,
      cogDesc = NA,
      updated = modTime,
      keySig = NA,
      height = height,
      width = width,
      lims = NA,
      relatedData = NA
   )
   class(displayObj) <- "displayObj"
   
   save(displayObj, file = file.path(displayPrefix, "displayObj.Rdata"))
   
}
