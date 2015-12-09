#' Path to VDB global data storage file
#'
#' Returns an appropriate file name to save VDB-wide globals to for use in Trelliscope displays
#'
#' @param conn vdb connection
#'
#' @author Jeremiah Rounds
#' @return character file name
#' @details Objects in this rdata file will be loaded by the Trelliscope Viewer when the viewer is launched and the data will be available to all displays throughout the viewing session.
#' @export
#'
#' @examples
#' \dontrun{
#' save(foo, file = globalsPath())
#' }
vdbGlobalsFile <- function(conn = getOption("vdbConn")) {
  validateVdbConn(conn)

  dir <- file.path(conn$path, "data")
  if(!dir.exists(dir))
    dir.create(dir)

  file.path(dir, "globals.Rdata")
}

#' Copy files ending in .R from source directory into a VDB-wide global code directory for use in Trelliscope displays
#'
#' These R files will be sourced in the global environment of the Trelliscope Viewer when the viewer is launched and the resulting objects will be available to all displays throughout the viewing session.
#' Useful for getting custom functions into the global environment.
#'
#' @param fromDir directory with R source
#' @param conn vdb connection
#'
#' @author Jeremiah Rounds
#' @examples
#' \dontrun{
#' vdbCopyRSource(".") # copies R files from current directory
#' }
#'
#' @export
vdbCopyRSource <- function(fromDir, conn = getOption("vdbConn")) {
  validateVdbConn(conn)

  dir <- file.path(conn$path, "data", "R")
  if(!dir.exists(dir))
    dir.create(dir, recursive = TRUE)

  flist <- list.files(fromDir, "*.R$", full.names = TRUE)

  message("Copying...")
  file.copy(flist, dir)
}

#' Check to see if the VDB-wide global data file exists
#'
#' @param conn vdb connection
#'
#' @author Jeremiah Rounds
#' @return logical
#' @export
#'
#' @examples
#' vdbGlobalsExist()
vdbGlobalsExist <- function(conn = getOption("vdbConn")) {
  file.exists(file.path(conn$path,  "data", "globals.Rdata"))
}
