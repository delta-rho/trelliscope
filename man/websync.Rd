\name{webSync}
\alias{webSync}
\title{Sync VDB and notebook Files to a Web Server}
\usage{
webSync(vdbConn = getOption("vdbConn"), webConn = getOption("vdbWebConn"),
  verbose = FALSE, rsync = NULL)
}
\arguments{
  \item{vdbConn}{VDB connection settings}

  \item{webConn}{web connection settings}

  \item{verbose}{show rsync output}

  \item{rsync}{location of rsync binary}
}
\description{
Sync VDB and notebook files to a web server
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
\code{\link{typeset}}, \code{\link{webConn}},
\code{\link{syncLocalData}}
}

