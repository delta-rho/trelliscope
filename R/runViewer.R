#' Run Shiny Display Viewer
#' 
#' Run server-side viewer on local machine
#' 
#' @param conn VDB connection info, typically stored in options("vdbConn") at the beginning of a session, and not necessary to specify here if a valid "vdbConn" object exists
#' @param group, name optional parameters to load the viewer with a pre-specified display
#' @param port what port to use for the viewer
#' @param openBrowser should the browser be automatically launched?
#' 
#' @author Ryan Hafen
#' 
#' @export
view <- function(conn=getOption("vdbConn"), group=NULL, name=NULL, port=8100L, openBrowser=TRUE) {
   vdbPrefix <- conn$path
	packagePath <- system.file(package="trelliscope")
   
   if(!is.null(name)) {
      displayObj <- getDisplay(name=name, group=group)
      name <- displayObj$name
      group <- displayObj$group      
   }
   
   message("attempting to launch shiny vdb viewer...")
   message("press Ctrl+C or Esc to stop viewer")
   
   require(shiny)
   hash <- ""
   if(!is.null(name)) {
      if(is.null(group))
         group <- "common"
      hash <- paste("/#group=", group, "&name=", name, sep="")
   }
   # make sure that the viewer has the prefix
   options(vdbShinyPrefix=vdbPrefix)
   # if on dev machine, make the viewer path be the code source directory
   # (not the package path)
   if(Sys.getenv("MYDEVMACHINE") == "TRUE") {
      shinyAppPrefix <- file.path(getwd(), "inst/trelliscopeViewer/")
   } else {
      shinyAppPrefix <- file.path(packagePath, "trelliscopeViewer")
   }
   
   myRunApp(shinyAppPrefix, port=port, hash=hash, launch.browser=openBrowser)      
}

## internal (from shiny's runApp() - need to be able to add hash string)
# myRunApp <- function (appDir = getwd(), port = 8100L, launch.browser = getOption("shiny.launch.browser", interactive()), hash="") {
#    ops <- options(warn = 1)
#    on.exit(options(ops))
#    orig.wd <- getwd()
#    setwd(appDir)
#    on.exit(setwd(orig.wd))
#    require(shiny)
#    ws_env <- shiny:::startApp(port = port)
#    if (launch.browser) {
#       appUrl <- paste("http://localhost:", port, hash, sep = "")
#       utils::browseURL(appUrl)
#    }
#    tryCatch(while (TRUE) {
#       shiny:::serviceApp(ws_env)
#    }, finally = {
#       shiny:::timerCallbacks$clear()
#       websockets:::websocket_close(ws_env)
#    })
# }

# for shiny version < 0.6
# myRunApp <- function (appDir = getwd(), port = 8100L, launch.browser = getOption("shiny.launch.browser", interactive()), hash="") {
#     ops <- options(warn = 1)
#     on.exit(options(ops))
#     if (nzchar(Sys.getenv("SHINY_PORT"))) {
#         ver <- Sys.getenv("SHINY_SERVER_VERSION")
#         if (compareVersion(ver, .shinyServerMinVersion) < 0) {
#             warning("Shiny Server v", .shinyServerMinVersion, 
#                 " or later is required; please upgrade!")
#         }
#     }
#     orig.wd <- getwd()
#     setwd(appDir)
#     on.exit(setwd(orig.wd), add = TRUE)
#     require(shiny)
#     server <- shiny:::startApp(port = port)
#     on.exit({
#         httpuv:::stopServer(server)
#     }, add = TRUE)
#     if (launch.browser) {
#         appUrl <- paste("http://localhost:", port, hash, sep = "")
#         utils::browseURL(appUrl)
#     }
#     tryCatch(while (TRUE) {
#         shiny:::serviceApp()
#         Sys.sleep(0.001)
#     }, finally = {
#         shiny:::timerCallbacks$clear()
#     })
# }

myRunApp <- function(appDir=getwd(), port=8100L, launch.browser=getOption('shiny.launch.browser', interactive()), hash="") {
  
   workerId <- ""

   # Make warnings print immediately
   ops <- options(warn = 1)
   on.exit(options(ops))
   
   if (nzchar(Sys.getenv('SHINY_PORT'))) {
      # If SHINY_PORT is set, we're running under Shiny Server. Check the version
      # to make sure it is compatible. Older versions of Shiny Server don't set
      # SHINY_SERVER_VERSION, those will return "" which is considered less than
      # any valid version.
      ver <- Sys.getenv('SHINY_SERVER_VERSION')
      if (compareVersion(ver, .shinyServerMinVersion) < 0) {
         warning('Shiny Server v', .shinyServerMinVersion,
            ' or later is required; please upgrade!')
      }
   }
  
   require(shiny)
   
   if (is.character(appDir)) {
      orig.wd <- getwd()
      setwd(appDir)
      on.exit(setwd(orig.wd), add = TRUE)
      server <- shiny:::startAppDir(port=port, workerId)
   } else {
      server <- shiny:::startAppObj(appDir$ui, appDir$server, port=port, workerId)
   }
   
   on.exit({
      httpuv:::stopServer(server)
   }, add = TRUE)
   
   if (launch.browser && !is.character(port)) {
      appUrl <- paste("http://localhost:", port, hash, sep="")
      utils::browseURL(appUrl)
   }
   
   env <- shiny:::.globals
   assign("retval", NULL, envir=env)
   assign("stopped", FALSE, envir=env)
   tryCatch(
      while (!get("stopped", envir=env)) {
         shiny:::serviceApp()
         Sys.sleep(0.001)
      },
      finally = {
         shiny:::timerCallbacks$clear()
      }
   )
   
   return(get("retval", envir=env))
}

