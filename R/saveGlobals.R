#---------------------------------------------------------------------------------
# Author: Jeremiah Rounds
# Email: jeremiah.rounds@pnnl.gov
# Time:  Fri Oct 23 15:33:03 2015
#---------------------------------------------------------------------------------

#' Save global environment data for displays
#'
#' BUG: This function doesn't work as intended because of scope for ...
#' Makes data available in the global environment of all displays of a connection in trelliscope
#'
#' @param ... objects to be saved in globals.Rdata for trelliscope displays.
#' @param conn vdb connection
#' @details Useful to avoid bloating individual displays with global variables common to all displays in a data base.
#' 
#' @author Jeremiah Rounds
#'
#'
#' @examples
#' data(iris)
#' saveGlobals(iris)
saveGlobals = function(..., conn=getOption("vdbConn")){
	validateVdbConn(conn)
	dir = file.path(conn$path, "data")
	if(!dir.exists(dir))
		dir.create(dir)
	save(..., file = file.path(dir, "globals.Rdata"))
	
}
#' Path to global rdata in Trelliscope viewer.
#' 
#' \code{globalsFile} returns an appropriate file name to save to.
#' Objects in this rdata will be loaded by the server viewer code prior to showing 
#' displays
#'
#' @param conn vdb connection 
#'
#' @return character file name
#' @export
#'
#' @examples
#' save(foo, file=globalsPath(conn=conn))
globalsFile = function(conn=getOption("vdbConn")){
	validateVdbConn(conn)
	dir = file.path(conn$path, "data")
	if(!dir.exists(dir))
		dir.create(dir)
	 file = file.path(dir, "globals.Rdata")
	 return(file)
	
}

#' Copy R Files From Package
#'
#' Just a helper function that wraps copyRSource
#' 
#' @param from_package 
#'
#' @param conn vdbConn
#'
#' @export
copyRFromPackage = function(from_package, conn=getOption("vdbConn")){
	copyRSource(system.file("R", from_package), conn)
}
#' Copy files ending in .R from source directory
#'
#' These R files will be sourced in the global environment of the trelliscope viewer.
#' Useful for getting custom functions into the global environment.
#'
#' @param from_dir directory with R source
#' @param conn vdb connection
#' @author Jeremiah Rounds
#'
#'
#' @examples
#' copyRSource(".") #copies R files from current directory
#'
#' @export
copyRSource = function(from_dir, conn=getOption("vdbConn")){
	validateVdbConn(conn)
	dir = file.path(conn$path, "data", "R")
	if(!dir.exists(dir))
		dir.create(dir, recursive=TRUE)
	flist <- list.files(from_dir, "*.R$", full.names = TRUE)
	message("Copying...")
	print(flist)
	file.copy(flist, dir)

}


#' Check to see if the global data file already exist
#' 
#' Does not check to see if any particular data set is in the global file.
#' 
#'
#' @param conn vdb connection
#'
#' @return TRUE/FALSE
#' @export
#'
#' @examples
#' globalsExist()
globalsExist = function(conn=getOption("vdbConn")){
	file.exists(file.path(conn$path,	"data","globals.Rdata"))
}
