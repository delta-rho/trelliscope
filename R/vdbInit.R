#' Set up a new VDB
#' 
#' Set up a new vdb directory
#'
#' @param path The location on the local file system where the directory for the vdb will be placed
#' @param autoYes should questions to proceed with operations be automarically answered with "yes"?
#'
#' @return nothing
#'
#' @author Ryan Hafen
#'
#' @seealso \code{\link{updateViewer}}
#' @export
vdbInit <- function(path=NULL, name="myVDB", autoYes=FALSE) {
   if(is.null(path))
      stop("Must specify path")
      
   if(file.exists(path)) {
      if(length(list.files(path)) > 0) {
         if(autoYes) {
            ans <- "y"
         } else {
            ans <- readline(paste("The path ", path, " already contains files.  This function is meant for one-time initialization of a vdb directory.  It is not recommended to proceed.  Proceed? (y = yes) ", sep=""))
         }
      	if(!tolower(substr(ans, 1, 1)) == "y")
      	   return(FALSE)
      }
   } else {
      if(autoYes) {
         ans <- "y"
      } else {
         ans <- readline(paste("The path ", path, " does not exist.  Should it be created? (y = yes) ", sep=""))
      }
   	if(!tolower(substr(ans, 1, 1)) == "y")
   	   return(FALSE)
		if(!dir.create(path))
			stop("Could not create directory.\n")
   }
   
   # now move files over
	pkgPath <- system.file(package="trelliscope")
	
   message("Moving viewer files over")
	file.copy(file.path(pkgPath, "viewer_cs"), path, recursive=TRUE, overwrite=TRUE)
   # file.copy(file.path(pkgPath, "viewer_ss"), path, recursive=TRUE, overwrite=TRUE)
   
   message("Moving notebook files over")
	file.copy(file.path(pkgPath, "notebook"), path, recursive=TRUE, overwrite=TRUE)

   message(paste("Creating default 'conn.R' file in ", path, ".  Update as necessary.", sep=""))
   vdbMakeConnTemplate(path, name)
   
   vdbConnect(path)
	TRUE
}

#' Update Trelliscope Project Client Side Viewer Files
#' 
#' Useful to do after a new update of the \code{trelliscope} package has been installed.  Moves the viewer files from the package to the project directory.
#'
#' @param conn VDB connection info, typically stored in options("vdbConn") at the beginning of a session, and not necessary to specify here if a valid "vdbConn" object exists
#' 
#' @return nothing
#' 
#' @author Ryan Hafen
#'
#' @seealso \code{\link{vdbInit}}
#' @export
updateViewer <- function(conn=getOption("vdbConn")) {
   prefix <- trsValidatePrefix(conn)
   
	packagePath <- system.file(package="trelliscope")
   
   message("Moving viewer files over")
	file.copy(file.path(packagePath, "viewer_cs"), prefix, recursive=TRUE, overwrite=TRUE)
   # file.copy(file.path(pkgPath, "viewer_ss"), prefix, recursive=TRUE, overwrite=TRUE)
   
   # TODO: need to update notebook assets too...
}



