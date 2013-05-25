#' Sync VDB and notebook Files to a Web Server
#' 
#' Sync VDB and notebook files to a web server
#' 
#' @param conn The VDB connection settings
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
      
	cmd <- paste(rsync, " -a -v ", sshFlag, path.expand(conn$vdbPrefix), " ", user, conn$webConn$ip, ":", conn$webConn$vdbPrefix, "/", sep="")
   
   message("Syncing local vdb directory with web server...")
	system(cmd, wait=FALSE)
   
   # write the conn stuff to VDB directory if it isn't there
   if(!file.exists(file.path(conn$vdbPrefix, "conn.R"))) {
      stop("There is not a conn.R file in your vdb directory.  You can generate a template with vdbMakeConnTemplate(vdbPrefix) and then edit it as necessary.")
   }
   
   ## make a copy from viewerDir to appDir/vdbName
   pkgPath <- system.file(package="trelliscope")
   
   message("Copying viewer to web app directory...")
   # system("ssh martingale \"mkdir -p /home/hafe647/shiny\"")
   
   system(paste(rsync, " -a -v ", sshFlag, 
      file.path(pkgPath, "viewer_ss", "*"), " ",
      user, conn$webConn$ip, ":", conn$webConn$appDir, "/", conn$vdbName,
      sep=""
   ))
   
   message("Copying connection info to web app directory...")
   system(paste(rsync, " -a -v ", sshFlag, 
      file.path(path.expand(conn$vdbPrefix), "conn.R"), " ",
      user, conn$webConn$ip, ":", conn$webConn$appDir, "/", conn$vdbName, "/",
      sep=""
   ))
   
   message("Copying notebook files to web app directory...")
   system(paste(rsync, " -a -v ", sshFlag, 
      file.path(path.expand(conn$vdbPrefix), "notebook"), " ",
      user, conn$webConn$ip, ":", conn$webConn$appDir, "/", conn$vdbName, "/",
      sep=""
   ))
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

