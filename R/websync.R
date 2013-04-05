#' Sync vdb and notebook Files to a Web Server
#'
#' Sync vdb and notebook files to a web server
#'
#' @param vdbName The name of the notebook.
#' @param vdbPrefix The location on the local file system of the current vdb project
#' @param vdbWebSyncMethod Either "rsync" or "share".  This specifies whether you want to transfer files to the web server using rsync or using a shared drive.  Use "share" if on Windows.  If using "rsync", make sure vdbPrefix points to the web directory root on the remote machine.  If using "share", make sure vdbPrefix contains the full path of the shared drive up to the web directory root.
#' @param vdbWebServer The location of the web server.  If it is mounted as a shared drive, this should be the path to your web root directory (this is currently not an option).  If syncing via ssh, this should contain everything necessary to make a successful ssh connection, such as "username@@server:~/www"
#' @param rsync Location of rsync binary.  Only important in this function if dolocal=TRUE and vdbWebSyncMethod="rsync".
#' @param vdbUsers A vector of user names with which to create a .htaccess file restricting access to these users.
#'
#' @return nothing
#'
#' @details If you are syncing via ssh, this only works if public key authentication is enabled between your local machine and the remote server.
#'
#' @author Ryan Hafen
#'
#' @seealso \code{\link{typeset}}, \code{\link{vdbPlot}}
#'
#' @export
websync <- function(
   vdbName=getOption("vdbConn")$vdbName, 
   vdbPrefix=getOption("vdbConn")$vdbPrefix,
   vdbWebSyncMethod=getOption("vdbConn")$vdbWebSyncMethod,
   vdbWebServer=getOption("vdbConn")$vdbWebServer, 
   rsync=NULL, 
   vdbUsers=getOption("vdbConn")$vdbWebServer) {
   
	if(is.null(vdbPrefix))
		vdbPrefix <- findWebDir()
	
	vdbPrefix <- paste(checkpath(vdbPrefix), "/", sep="")
   
   
   htaccessGen(vdbPrefix, users=users)

   if(!is.null(vdbWebServer) && !is.null(vdbName)) {
		if(vdbWebSyncMethod=="rsync") {
		   if(is.null(rsync))
		      rsync <- findRsync()
			# TODO: check to make sure they have passwordless ssh set up
	      sshFlag <- ""
	      if(grepl("@", vdbWebServer))
	         sshFlag <- "-e ssh "
	      # cmd <- paste("rsync -a -v -e ssh --force --delete ", vdbPrefix, "/* ", server, "/", name, "/", sep="")
	      # if there is an "@" in vdbWebServer, make it ssh, else just regular rsync
	   	cmd <- paste(rsync, " -a -v ", sshFlag, vdbPrefix, " ", vdbWebServer, "/", vdbName, "/", sep="")
	   	system(cmd, wait=FALSE)
		} else if(vdbWebSyncMethod=="share") {
			localprefix <- vdbPrefix
			remoteprefix <- paste(vdbWebServer, "/", vdbName, "/", sep="")

			# TODO: should I remove files on remote if they don't exist on local?

			# get list of local and remote files
			localfiles <- list.files(localprefix, recursive=TRUE, all.files=TRUE)
			remotefiles <- list.files(remoteprefix, recursive=TRUE, all.files=TRUE)

			# see which files do not exist on remote (newfiles) and modify localfiles to ignore these
			# also remove files from remote file list that aren't on local (should these be deleted?)
			newfiles <- localfiles[!is.element(localfiles, remotefiles)]
			localfiles2 <- localfiles[is.element(localfiles, remotefiles)]
			remotefiles2 <- remotefiles[is.element(remotefiles, localfiles)]

			# make sure the file lists are the same
			localfiles2 <- sort(localfiles2)
			remotefiles2 <- sort(remotefiles2)
			if(!all(localfiles2 == remotefiles2))
				stop("Something went wrong with web sync.")

			localfiles.info <- file.info(paste(localprefix, localfiles2, sep=""))
			remotefiles.info <- file.info(paste(remoteprefix, remotefiles2, sep=""))

			modfiles <- localfiles2[which(localfiles.info$mtime > remotefiles.info$mtime)]

			modfiles.info <- localfiles.info[which(localfiles.info$mtime > remotefiles.info$mtime),]
			newfiles.info <- file.info(paste(localprefix, newfiles, sep=""))

			if(length(newfiles)==0 & length(modfiles)==0)
				cat("All files up to date. Nothing to be done...\n")

			# now copy new and modified files over
			if(length(modfiles) > 0) {
				mfsize <- sum(modfiles.info$size)
				class(mfsize) <- "object_size"
				mfsize <- capture.output(print(mfsize, units = "auto"))
				cat(paste("Modifying ", length(modfiles), " files totaling ", mfsize, "\n", sep=""))
				copyres <- file.copy(paste(localprefix, modfiles, sep=""), paste(remoteprefix, modfiles, sep=""), overwrite=TRUE)
				if(all(copyres)) {
					cat(paste(paste(modfiles, collapse="\n"), "\n"))
					cat("Successful\n")
				} else {
					cat("Something went wrong with modifying files.\n")
				}
			}

			if(length(newfiles) > 0) {
				nfsize <- sum(newfiles.info$size)
				class(nfsize) <- "object_size"
				nfsize <- capture.output(print(nfsize, units = "auto"))
				cat(paste("Moving ", length(newfiles), " new files totaling ", nfsize, "\n", sep=""))
				newdirs <- unique(dirname(newfiles))
				dirres <- sapply(paste(remoteprefix, newdirs, sep=""), function(x) dir.create(x, recursive=TRUE, showWarnings=FALSE))
				if(!all(dirres)) 
					cat("Something went wrong with creating directories for new files files.\n")
				copyres <- file.copy(paste(localprefix, newfiles, sep=""), paste(remoteprefix, newfiles, sep=""), overwrite=TRUE)
				if(all(copyres)) {
					cat(paste(paste(newfiles, collapse="\n"), "\n"))
					cat("Successful\n")
				} else {
					cat("Something went wrong with copying new files.\n")
				}
			}
		} else {
			cat("The option vdbWebSyncMethod was not either 'rsync' or 'share'.  Please specify it in your wnb_options.R file or explicitly in your call to websync.")
		}
   } else {
      cat("Must specify a vdbWebServer and vdbName!\n")
   }
}