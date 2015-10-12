
#' Connect to a VDB
#'
#' Connect to a new or existing visualization database
#'
#' Connecting to a VDB is required prior to calling \code{\link{makeDisplay}} or \code{\link{view}}.
#'
#' @param path The path on the local file system where the directory for the VDB is located
#'
#' @param name A character string giving the name of the VDB. If the VDB already exists and \code{name = NULL},
#' the previous name is used.  If the VDB exists and a string is provided for \code{name}, the name is overwritten.
#' The primary purpose of the \code{name} argument is to facilitate deploying the trelliscope display as a Shiny app.
#' See the \code{appName} argument of \code{\link{deployToShinyApps}}
#'
#' @param autoYes should questions to proceed with directory creation operations be automatically answered with "yes"?
#'
#' @param verbose should messages be printed about what is being done?
#'
#' @author Ryan Hafen
#'
#' @return An object of class \code{vdbConn} that contains the path and name of the VDB. This object is also assigned
#' to the \code{vdbConn} option, and can be retrieved via \code{getOption("vdbConn")}
#'
#' @export
vdbConn <- function(path, name = NULL, autoYes = FALSE, verbose = TRUE) {

  # Check arguments
  if(!is.null(name)) {
    stopifnot(is.character(name),
              length(name) == 1)
  }
  stopifnot(is.character(path),
            is.logical(autoYes),
            is.logical(verbose))

  connPath <- file.path(path, "conn.Rdata")

  # get VDB name, if saved
  if(file.exists(connPath)) {
    load(connPath)
    if(!is.null(name)) {
      if(!is.null(conn$name))
        if(name != conn$name)
          message("* Note: replacing previous VDB name '", conn$name, "' with specified name '", name, "'")
    } else {
      name <- conn$name
    }
  }

  # if directory doesn't exist, create and initialize
  if(!file.exists(path)) {
    stopifnot(vdbInit(path, autoYes, verbose))
  }
  path <- normalizePath(path)

  ff <- list.files(path)

  # if there are no files in the directory, initialize
  if(length(ff) == 0) {
    vdbInit(path, autoYes, verbose)
  }

  # make sure it looks like a VDB directory
  ff <- list.files(path)
  if(! "displays" %in% ff)
    stop(paste(path, "is not a valid VDB directory"))

  # Create the connection object
  conn <- structure(list(
      path = path,
      name = name
  ), class = "vdbConn")
  save(conn, file = connPath)

  # copy latest trelliscope viewer files over
  message("*** Copying latest viewer to vdb directory...")
  copyViewerFiles(conn)

  options(vdbConn = conn)
  return(conn)
}

#' @export
print.vdbConn <- function(x, ...) {
  if(is.null(x$name)) {
    nm <- "[name:none]"
  } else {
    nm <- paste("\"", x$name, "\"", sep = "")
  }

  cat(paste("vdb connection object: ", nm, "; path=", x$path, "\n", sep = ""))
}

## internal
vdbInit <- function(path, autoYes, verbose) {

  if(!file.exists(path)) {
    if(autoYes) {
      ans <- "y"
    } else {
      ans <- readline(paste("The path ", path, " does not exist.  Should it be created? (y = yes) ", sep = ""))
    }
    if(!tolower(substr(ans, 1, 1)) == "y")
      return(FALSE)
    if(!dir.create(path, recursive = TRUE))
      stop("Could not create directory.\n")
  }

  # now move files over
  pkgPath <- system.file(package = "trelliscope")

  dir.create(file.path(path, "displays"))

  TRUE
}
