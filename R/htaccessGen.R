#' Generate a .htaccess file
#' 
#' Generate a .htaccess file that restricts html access to specified PNL users.
#' 
#' @param path The location on the local file system of the current project's "web" directory or the parent folder of the web directory.
#' @param users A vector of users to which access should be granted.
#' 
#' @return nothing
#'
#' @details Uses Kerberos authentication based on the User ID, which can be looked up at icam.pnl.gov
#'
#' @author Ryan Hafen
#'
#' @seealso \code{\link{typeset}}
#' @export
htaccessGen <- function(vdbPath=getOption("vdbConn")$vdbPath, users=getOption("vdbConn")$vdbUsers) {
	if(is.null(path))
      path <- findWebDir()

	if(is.null(users)) {
      cat("No users specified for .htaccess file.  Nothing has been done.\n")	   
	} else {
      webPath <- checkpath(path)
      htaccessFile <- file(paste(webPath, "/.htaccess", sep=""), "w")
   	textString <- paste("AuthType Kerberos
AuthName \"PNNL Domain Login\"
Krb5Keytab /etc/httpd/conf/keytab
KrbAuthRealm PNL.GOV
KrbMethodNegotiate off
KrbSaveCredentials off
KrbVerifyKDC off
require user ", paste(users, collapse=" "), sep="")
      # cat(textString)
   	writeLines(textString, htaccessFile)
   	close(htaccessFile)
   	cat(".htaccess file written successfully.\n")
	}
}