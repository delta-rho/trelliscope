if(getRversion() >= "2.15.1") {
  utils::globalVariables(c(".shinyServerMinVersion"))
}

#' View a Display or Run Shiny Display Viewer
#'
#' View a display or run Shiny display viewer
#'
#' @param name,group optional parameters to load the viewer with a pre-specified display - if not specified, the viewer will launch with a list to choose from
#' @param state an optional list of state variables to set the default viewing state for layout, sorting, filtering, and labels (see details)
#' @param openBrowser should the browser be automatically launched?
#' @param conn VDB connection info, typically stored in options("vdbConn") at the beginning of a session, and not necessary to specify here if a valid "vdbConn" object exists
#' @param port what port to use for the viewer - if not specified, will look for "trelliscopePort" set in R's global options, followed by a search for a system-level environment variable "TRELLISCOPE_PORT".  If none of these are defined, a random port assigned provided by shiny will be used.
#'
#' @author Ryan Hafen
#'
#' @export
#' @importFrom shiny runApp
#' @import hexbin fastICA
#' @importFrom jsonlite toJSON
view <- function(name = NULL, group = NULL, state = NULL, openBrowser = TRUE, conn = getOption("vdbConn"), port = getOption("trelliscopePort")) {

  if(is.null(port) && nzchar(Sys.getenv("TRELLISCOPE_PORT")))
    port <- as.integer(Sys.getenv("TRELLISCOPE_PORT"))

  validateVdbConn(conn, mustHaveDisplays = TRUE)
  vdbPrefix <- conn$path
	packagePath <- system.file(package = "trelliscope")

  copyViewerFiles(conn)

  message("Attempting to launch shiny vdb viewer...")
  message("Press Ctrl+C or Esc to stop viewer")

  # require(shiny)
  # TODO: pass state setting R options instead of using hash
  # modify shiny app so that it can take hash or options
  # view(..., state = list(...))
  # hash <- ""
  # if(!is.null(name)) {
  #   if(is.null(group))
  #     group <- "common"
  #   hash <- paste("/#group=", group, "&name=", name, sep = "")
  # }
  # make sure that the viewer has the prefix
  options(vdbShinyPrefix = vdbPrefix)

  if(!is.null(name)) {
    disp <- findDisplay(name = name, group = group)
    if(is.null(state)) {
      state <- disp
    } else {
      state <- c(disp, validateState(state))
    }
  } else {
    disp <- NULL
  }

  if(!is.null(state)) {
    if(is.null(disp)) {
      message("Note: a valid display name/group must be supplied when specifying state - state will be ignored...")
      options("trsCurrentViewState" = NULL)
    } else {
      options("trsCurrentViewState" = state)
    }
  } else {
    options("trsCurrentViewState" = NULL)
  }

  runApp(vdbPrefix, port = port, launch.browser = openBrowser)
}

copyViewerFiles <- function(vdbConn) {
  shinyAppPrefix <- Sys.getenv("TRELLISCOPE_DEV_APP_PREFIX")
  if(shinyAppPrefix == "")
    shinyAppPrefix <- file.path(system.file(package = "trelliscope"), "trelliscopeViewer")

  serverLoc <- file.path(vdbConn$path, "server")
  if(file.exists(serverLoc))
    unlink(serverLoc, recursive = TRUE)

  wwwLoc <- file.path(vdbConn$path, "www")
  if(file.exists(wwwLoc))
    unlink(wwwLoc, recursive = TRUE)

  file.copy(file.path(shinyAppPrefix, "www"), vdbConn$path, recursive = TRUE)
  file.copy(file.path(shinyAppPrefix, "server"), vdbConn$path, recursive = TRUE)
  file.copy(file.path(shinyAppPrefix, "server.R"), vdbConn$path, overwrite=TRUE)
  file.copy(file.path(shinyAppPrefix, "global.R"), vdbConn$path, overwrite=TRUE)
}
