
#' Add a Display to a VDB
#' 
#' Add an R display (lattice, ggplot, base R) to a visualization database (VDB)
#' 
#' @param p an R plot object (lattice or ggplot) or an expression containing code to create a plot
#' @param name the name of the display (no spaces or special characters)
#' @param group the group the display belongs to (displays are organized into groups).  Defaults to "common"
#' @param desc a description of the display (used in the viewer and in notebooks)
#' @param dim a list defining aspects of the plot dimension, including height, width, aspect, and res (resolution of raster image).  defaults are 1000 (px), 1000 (px), "fill", and 150, respectively
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
   dim = list(height=NULL, width=NULL, aspect=NULL, res=NULL),
   conn = getOption("vdbConn")
) {
   validateConn(conn)
   vdbPrefix <- conn$path
   dim <- validatepanelDim(dim)
      
   # get display prefix (and move old display to backup if it already exists)
   displayPrefix <- file.path(vdbPrefix, "displays", group, name)
   checkDisplayPath(displayPrefix, verbose)
   
   makePNG(dat=p, 
      file=file.path(displayPrefix, "thumb.png"), width=dim$width, 
      height=dim$height, res=dim$res)
   
   modTime <- Sys.time()
   
   updateDisplayList(list(
      group = group, 
      name = name, 
      desc = desc, 
      n = 1, 
      preRender = NA, 
      dataClass = NA, 
      cogClass = NA, 
      panelDim = dim, 
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
      panelDim = dim, 
      lims = NA,
      relatedData = NA
   )
   class(displayObj) <- "displayObj"
   
   save(displayObj, file=file.path(displayPrefix, "displayObj.Rdata"))
   
}
