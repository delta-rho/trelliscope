#---------------------------------------------------------------------------------
# Author: Jeremiah Rounds
# Email: jeremiah.rounds@pnnl.gov
# Time:  Fri Oct 23 15:33:03 2015
#---------------------------------------------------------------------------------

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
	#print(flist)
	ret = file.copy(flist, dir)
	ret
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
