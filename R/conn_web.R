#' Initialize a Web Connection
#' 
#' Initialize a connection to a web server
#' 
#' @param user the username to log on to the web server
#' @param ip the IP address of the web server - if \code{NULL}, it is assumed that your web server is on the same machine that you are working on
#' @param appDir the directory where Shiny apps go on the web server - defaults to the default location of /srv/shiny-server
#' @param name the name of the directory in \code{appDir} under which to store the application - if not supplied, it defaults to the name provided in the vdb connection
#' 
#' @author Ryan Hafen
#' 
#' @seealso \code{\link{webSync}}
#' @export
webConn <- function(
   user = NULL,
   ip = NULL,
   appDir = "/srv/shiny-server",
   name = NULL
) {
   if(is.null(name)) {
      message("* Name not provided - attempting to use name in vdbConn")
      name <- getOption("vdbConn")$name
      if(is.null(name))
         stop("Name not specified", call. = FALSE)
   }
   
   res <- structure(list(
      user = user,
      ip = ip,
      appDir = appDir,
      name = name
   ), class = "vdbWebConn")
   options(vdbWebConn = res)
   res
}

