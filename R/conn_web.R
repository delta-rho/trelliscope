#' Initialize a Web Connection
#' 
#' Initialize a connection to a web server
#' 
#' @param user the username to log on to the web server
#' @param ip the IP address of the web server
#' @param appDir the directory where Shiny apps go on the web server
#' @param name the name under which to store the application
#' 
#' @author Ryan Hafen
#' 
#' @seealso \code{\link{websync}}
#' @export
webConn <- function(
   user = NULL, 
   ip,
   appDir,
   name
) {
   res <- structure(list(
      user = user,
      ip = ip,
      appDir = appDir,
      name = name
   ), class="vdbWebConn")
   options(vdbWebConn = res)
   res
}

