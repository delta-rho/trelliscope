if(getRversion() >= "2.15.1") {
  utils::globalVariables(c("dataClass"))
}
 
#' Sync VDB and notebook Files to a Web Server
#' 
#' Sync VDB and notebook files to a web server
#' 
#' @param vdbConn VDB connection settings
#' @param webConn web connection settings
#' @param fixPermissions should an attempt be made to fix permissions in the web directory?
#' @param verbose show rsync output
#' @param rsync location of rsync binary
#' 
#' @details This requires rsync to be installed on your machine.  If you are syncing via ssh, this only works if public key authentication is enabled between your local machine and the remote server.
#' 
#' @author Ryan Hafen
#' 
#' @seealso \code{\link{typeset}}, \code{\link{webConn}}, \code{\link{syncLocalData}}
#'
#' @export
webSync <- function(
   vdbConn = getOption("vdbConn"),
   webConn = getOption("vdbWebConn"),
   fixPermissions = TRUE,
   verbose = FALSE,
   rsync = NULL
) {
   
   # conn <- getOption("vdbConn")
   # rsync <- NULL
   
   ## sync local vdb directory to web server's vdb directory
   if(is.null(rsync))
      rsync <- findRsync()
   
	# TODO: check to make sure passwordless ssh is set up
   
   if(is.null(webConn)) {
      message("* webConn has not been set up... attempting default config...")
      webConn <- webConn()
   }
   
   syncLocalData(vdbConn)
   
   if(is.null(webConn$ip)) {
      sshFlag <- ""
      ip <- ""      
   } else {
      sshFlag <- "-e ssh "
      ip <- paste(webConn$ip, ":", sep = "")
   }
   user <- ""
   if(!is.null(webConn$user))
      user <- paste(webConn$user, "@", sep = "")
   
   message("* Syncing local vdb directory to web server...")
   
   lns <- capture.output(system(paste(rsync, " -a -v ", sshFlag,
      path.expand(vdbConn$path), "/* ",
      user, ip, webConn$appDir, "/", webConn$name,
      sep = ""
   ), intern = TRUE, ignore.stderr = FALSE, ignore.stdout = FALSE))
   if(verbose)
      cat(paste(c("*** Output ***", lns), collapse = "\n"))
   
   ## make a copy from viewerDir to appDir/vdbName
   pkgPath <- system.file(package = "trelliscope")
   
   message("* Syncing latest shiny viewer to web app directory...")
   
   lns <- capture.output(system(paste(rsync, " -a -v ", sshFlag,
      file.path(pkgPath, "trelliscopeViewer/*"), " ",
      user, ip, webConn$appDir, "/", webConn$name,
      sep = ""
   ), intern = TRUE, ignore.stderr = FALSE, ignore.stdout = FALSE))
   if(verbose)
      cat(paste(c("*** Output ***", lns), collapse = "\n"))
   
   # attempt to fix permissions
   if(fixPermissions) {
      if(is.null(webConn$ip)) {
         sshString <- ""
         sshQuote <- ""
      } else {
         sshString <- paste("ssh ", user, webConn$ip, " ", sep = "")
         sshQuote <- "'"
      }
      message("* Attempting to fix permissions...")
      lns <- capture.output(system(paste(sshString, sshQuote, "sudo chown -R shiny ", webConn$appDir, "/", webConn$name, sshQuote, sep = ""), intern = TRUE, ignore.stderr = FALSE, ignore.stdout = FALSE))
      if(verbose)
         cat(paste(c("*** Output ***", lns), collapse = "\n"))
   }
   
   NULL
}

#' Sync localDisk Objects to VDB
#' 
#' Sync localDisk data that is used for VDB displays located throughout the system to a 'data' directory inside the VDB - useful for collecting data before syncing with a web server, and used inside of \code{\link{webSync}}.
#' 
#' @param vdbConn VDB connection settings
#' @param rsync location of rsync binary
#' 
#' @author Ryan Hafen
#' 
#' @seealso \code{\link{webSync}}, \code{\link{webConn}}
#' 
#' @export
syncLocalData <- function(vdbConn = getOption("vdbConn"), rsync = NULL) {
   if(is.null(rsync))
      rsync <- findRsync()
 
   # if there are localDisk objects, put them in VDB
   load(file.path(vdbConn$path, "displays", "_displayList.Rdata"))
   ldDisp <- subset(displayListDF, dataClass == "kvLocalDisk")
   if(nrow(ldDisp) > 0) {
      message("* Syncing localDisk objects to vdb 'data' directory...")
 
      newDatDir <- file.path(vdbConn$path, "data")
      if(!file.exists(newDatDir))
         dir.create(newDatDir, recursive = TRUE)
 
      locs <- sapply(seq_len(nrow(ldDisp)), function(i) {
         tmp <- getDisplay(ldDisp$name[i], ldDisp$group[i])
         getAttribute(tmp$panelDataSource, "conn")$loc
      })
 
      locs <- locs[!grepl(normalizePath(vdbConn$path), locs)]
      if(length(locs) > 0) {
         locs <- unique(locs)
         if(any(duplicated(basename(locs))))
            stop("Currently can't sync multiple localDisk data sets with same name but different paths.")
 
         for(ll in locs) {
            system(paste(rsync, "-avh", ll, newDatDir))
         }
      }
   }
}

findRsync <- function(rsync = NULL, verbose = "FALSE") {
   
   # http://www.rsync.net/resources/howto/windows_rsync.html
   
   errorMsg <- "rsync executable not found. Use 'rsync' argument to specify the correct path."
   
   if (is.null(rsync))
      rsync = "rsync"
   
   rsync = Sys.which(rsync)
   if (rsync == "" || rsync == "rsync")
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
