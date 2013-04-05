\name{websync}
\alias{websync}
\title{Sync vdb and notebook Files to a Web Server}
\usage{
  websync(vdbName = getOption("vdbConn")$vdbName,
    vdbPrefix = getOption("vdbConn")$vdbPrefix,
    vdbWebSyncMethod = getOption("vdbConn")$vdbWebSyncMethod,
    vdbWebServer = getOption("vdbConn")$vdbWebServer,
    rsync = NULL,
    vdbUsers = getOption("vdbConn")$vdbWebServer)
}
\arguments{
  \item{vdbName}{The name of the notebook.}

  \item{vdbPrefix}{The location on the local file system of
  the current vdb project}

  \item{vdbWebSyncMethod}{Either "rsync" or "share".  This
  specifies whether you want to transfer files to the web
  server using rsync or using a shared drive.  Use "share"
  if on Windows.  If using "rsync", make sure vdbPrefix
  points to the web directory root on the remote machine.
  If using "share", make sure vdbPrefix contains the full
  path of the shared drive up to the web directory root.}

  \item{vdbWebServer}{The location of the web server.  If
  it is mounted as a shared drive, this should be the path
  to your web root directory (this is currently not an
  option).  If syncing via ssh, this should contain
  everything necessary to make a successful ssh connection,
  such as "username@server:~/www"}

  \item{rsync}{Location of rsync binary.  Only important in
  this function if dolocal=TRUE and
  vdbWebSyncMethod="rsync".}

  \item{vdbUsers}{A vector of user names with which to
  create a .htaccess file restricting access to these
  users.}
}
\value{
  nothing
}
\description{
  Sync vdb and notebook files to a web server
}
\details{
  If you are syncing via ssh, this only works if public key
  authentication is enabled between your local machine and
  the remote server.
}
\author{
  Ryan Hafen
}
\seealso{
  \code{\link{typeset}}, \code{\link{vdbPlot}}
}

