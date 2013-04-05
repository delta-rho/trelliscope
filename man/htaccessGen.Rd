\name{htaccessGen}
\alias{htaccessGen}
\title{Generate a .htaccess file}
\usage{
  htaccessGen(vdbPath = getOption("vdbConn")$vdbPath,
    users = getOption("vdbConn")$vdbUsers)
}
\arguments{
  \item{path}{The location on the local file system of the
  current project's "web" directory or the parent folder of
  the web directory.}

  \item{users}{A vector of users to which access should be
  granted.}
}
\value{
  nothing
}
\description{
  Generate a .htaccess file that restricts html access to
  specified PNL users.
}
\details{
  Uses Kerberos authentication based on the User ID, which
  can be looked up at icam.pnl.gov
}
\author{
  Ryan Hafen
}
\seealso{
  \code{\link{typeset}}
}

