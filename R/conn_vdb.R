
#' Connect to a VDB
#' 
#' Connect to a visualization catabase
#' 
#' @param path The path on the local file system where the directory for the VDB is located
#' @param name the name for the VDB (if null, uses the name of the parent directory)
#' @param autoYes should questions to proceed with directory creation operations be automatically answered with "yes"?
#' @param reset should existing metadata for this VDB connection be overwritten?
#' @param verbose logical - print messages about what is being done
#' 
#' @author Ryan Hafen
#' 
#' @seealso \code{\link{updateViewer}}
#' @export
vdbConn <- function(path, name = NULL, autoYes = FALSE, reset = FALSE, verbose=TRUE) {
   
   if(file.exists(file.path(path, "conn.Rdata")) && !reset) {
      load(file.path(path, "conn.Rdata"))
      # if the vdb has moved, keep the vdb name, but change the path
      path <- normalizePath(path)
      
      conn$path <- path
      save(conn, file = file.path(path, "conn.Rdata"))
      options(vdbConn = conn)
      return(conn)
   } else {
      if(is.null(name)) {
         message("* 'name' was not provided for vdb connection, using the parent directory name...")
         name <- basename(dirname(path))
      }
      
      # if directory doesn't exist, create and initialize
      if(!file.exists(path)) {
         stopifnot(vdbInit(path, autoYes, verbose))
      }
      path <- normalizePath(path)
      
      ff <- list.files(path)
      
      # if there are no files in the directory, initialize
      if(length(ff) == 0) {
         vdbInit(path, autoYes, verbose)
      }
      
      # make sure it looks like a VDB directory
      ff <- list.files(path)
      if(!all(c("displays", "notebook") %in% ff))
         stop(paste(path, "is not a valid VDB directory"))      
      
      conn <- structure(list(
            path = path,
            name = name
         ), class = "vdbConn")
      if(!file.exists(file.path(path, "conn.Rdata")) || reset) {
         save(conn, file = file.path(path, "conn.Rdata"))
      } else {
         load(file.path(path, "conn.Rdata"))
      }
      
      options(vdbConn = conn)
      return(conn)
   }
}

#' @S3method print vdbConn
print.vdbConn <- function(x, ...) {
   cat(paste("vdb connection object: name=", x$name, "; path=", x$path, sep = ""))
}

## internal
vdbInit <- function(path, autoYes, verbose) {
   if(!file.exists(path)) {
      if(autoYes) {
         ans <- "y"
      } else {
         ans <- readline(paste("The path ", path, " does not exist.  Should it be created? (y = yes) ", sep = ""))
      }
   	if(!tolower(substr(ans, 1, 1)) == "y")
   	   return(FALSE)
   	if(!dir.create(path, recursive = TRUE))
   		stop("Could not create directory.\n")
   }
   
   # now move files over
	pkgPath <- system.file(package = "trelliscope")
      
   dir.create(file.path(path, "displays"))
   
   if(verbose)
      message("* Moving notebook files over")
	file.copy(file.path(pkgPath, "notebook"), path, recursive = TRUE, overwrite = TRUE)

	TRUE
}
