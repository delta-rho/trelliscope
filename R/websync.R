#' Sync VDB and notebook Files to a Web Server
#' 
#' Sync VDB and notebook files to a web server
#' 
#' @param conn The VDB connection settings
#' @param verbose show rsync output
#' @param rsync Location of rsync binary
#' 
#' @return nothing
#' 
#' @details If you are syncing via ssh, this only works if public key authentication is enabled between your local machine and the remote server.
#' 
#' @author Ryan Hafen
#' 
#' @seealso \code{\link{typeset}}, \code{\link{makeDisplay}}
#' 
#' @export
websync <- function(
   conn=getOption("vdbConn"),
   verbose=FALSE,
   rsync=NULL
) {
   
   # conn <- getOption("vdbConn")
   # rsync <- NULL
   
   ## sync local vdb directory to web server's vdb directory
   if(is.null(rsync))
      rsync <- findRsync()
      
	# TODO: check to make sure passwordless ssh is set up
   
   sshFlag <- "-e ssh "
   user <- ""
   if(!is.null(conn$webConn$user))
      user <- paste(conn$webConn$user, "@", sep="")
      
   message("Syncing local vdb directory to web server...")
   system(paste(rsync, " -a -v ", sshFlag, 
      path.expand(conn$vdbPrefix), "/* ",
      user, conn$webConn$ip, ":", conn$webConn$appDir, "/", conn$vdbName,
      sep=""
   ), intern=verbose, ignore.stderr=!verbose, ignore.stdout=!verbose)
   
   ## make a copy from viewerDir to appDir/vdbName
   pkgPath <- system.file(package="trelliscope")
   
   message("Syncing latest shiny viewer to web app directory...")
   # system("ssh host \"mkdir -p /home/hafe647/shiny\"")
   
   system(paste(rsync, " -a -v ", sshFlag, 
      file.path(pkgPath, "_viewer_ss", "*"), " ",
      user, conn$webConn$ip, ":", conn$webConn$appDir, "/", conn$vdbName, "/displays/",
      sep=""
   ), intern=verbose, ignore.stderr=!verbose, ignore.stdout=!verbose)
   
   NULL
}

findRsync <- function(rsync=NULL, verbose = "FALSE") {
   
   # http://www.rsync.net/resources/howto/windows_rsync.html
   
   errorMsg <- "rsync executable not found. Use rsync= argument to specify the correct path."
   
   if (is.null(rsync))
      rsync = "rsync"
   
   rsync = Sys.which(rsync)
   if (rsync=="" || rsync=="rsync")
      stop(errorMsg)
   
   if (.Platform$OS == "windows") {
      if (length(grep("rtools", tolower(rsync))) > 0) {
         rsync.ftype <- shell("ftype rsync", intern = TRUE)
         if (length(grep("^rsync=", rsync.ftype)) > 0) {
            rsync <- sub('^rsync="([^"]*)".*', "\\1", rsync.ftype)
         }
      }
   }
  
   if (verbose) cat("Using rsync at", rsync, "\n")
   
   rsync
}

