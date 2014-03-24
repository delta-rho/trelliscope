\name{webConn}
\alias{webConn}
\title{Initialize a Web Connection}
\usage{
webConn(user = NULL, ip, appDir, name)
}
\arguments{
  \item{user}{the username to log on to the web server}

  \item{ip}{the IP address of the web server}

  \item{appDir}{the directory where Shiny apps go on the
  web server}

  \item{name}{the name under which to store the
  application}
}
\description{
Initialize a connection to a web server
}
\author{
Ryan Hafen
}
\seealso{
\code{\link{websync}}
}

