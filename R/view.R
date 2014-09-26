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
#' @param port what port to use for the viewer
#' 
#' @details The 'state'
#' 
#' @author Ryan Hafen
#' 
#' @export
#' @importFrom shiny runApp
view <- function(name = NULL, group = NULL, state = NULL, openBrowser = TRUE, conn = getOption("vdbConn"), port = 8100L) {
   
   port <- as.integer(port)
   
   validateConn(conn)
   vdbPrefix <- conn$path
	packagePath <- system.file(package = "trelliscope")
   
   if(!is.null(name)) {
      displayObj <- getDisplay(name = name, group = group)
      name <- displayObj$name
      group <- displayObj$group
      
      # if it's a simple display, just view it in a web browser
      displayPrefix <- file.path(vdbPrefix, "displays", group, name)
      load(file.path(displayPrefix, "displayObj.Rdata"))
      if(is.null(attributes(displayObj$panelDataSource))) {
         browseURL(file.path(displayPrefix, "thumb.png"))
         return()
      }
   }
   
   message("attempting to launch shiny vdb viewer...")
   message("press Ctrl+C or Esc to stop viewer")
   
   # require(shiny)
   # TODO: pass state setting R options instead of using hash
   # modify shiny app so that it can take hash or options
   # view(..., state = list(...))
   # hash <- ""
   # if(!is.null(name)) {
   #    if(is.null(group))
   #       group <- "common"
   #    hash <- paste("/#group=", group, "&name=", name, sep = "")
   # }
   # make sure that the viewer has the prefix
   options(vdbShinyPrefix = vdbPrefix)
   # if on dev machine, make the viewer path be the code source directory
   # (not the package path)
   shinyAppPrefix <- Sys.getenv("TRELLISCOPE_DEV_APP_PREFIX")
   if(shinyAppPrefix == "")
      shinyAppPrefix <- file.path(packagePath, "trelliscopeViewer")
   
   if(!is.null(state) || !is.null(name)) {
      state <- c(list(name = name, group = group), validateState(state))
      options("trsCurrentViewState" = state)
   } else {
      options("trsCurrentViewState" = NULL)
   }
   
   runApp(shinyAppPrefix, port = port, launch.browser = openBrowser)
}

