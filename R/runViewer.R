#' Run Server-Side Viewer
#' 
#' Run server-side viewer on local machine
#' 
#' @param conn VDB connection info, typically stored in options("vdbConn") at the beginning of a session, and not necessary to specify here if a valid "vdbConn" object exists
#' @param type there are two viewers - the server-side "ss" and client-side "cs"
#' @param group, name optional parameters to load the viewer with a pre-specified display
#' @param port if type="ss", what port to use for the viewer
#' 
#' @author Ryan Hafen
#' 
#' @export
view <- function(conn=getOption("vdbConn"), type="ss", group=NULL, name=NULL, port=8100L) {
   vdbPrefix <- trelliscope:::trsValidatePrefix(conn)
	packagePath <- system.file(package="trelliscope")
   
   if(!is.null(name)) {
      displayObj <- getDisplay(name=name, group=group)
      name <- displayObj$name
      group <- displayObj$group      
   }
   
   if(type=="cs") {
      browseURL(paste("file:///", path.expand(vdbPrefix), "/viewer_cs?group=", group, "&name=", name, sep=""))
   } else {
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
         shinyAppPrefix <- "~/Documents/Code/trelliscope/inst/viewer_ss/"
      } else {
         shinyAppPrefix <- file.path(packagePath, "viewer_ss")
      }
      
      trelliscope:::myRunApp(shinyAppPrefix, port=port, hash=hash)      
   }
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

myRunApp <- function (appDir = getwd(), port = 8100L, launch.browser = getOption("shiny.launch.browser", interactive()), hash="") {
    ops <- options(warn = 1)
    on.exit(options(ops))
    if (nzchar(Sys.getenv("SHINY_PORT"))) {
        ver <- Sys.getenv("SHINY_SERVER_VERSION")
        if (compareVersion(ver, .shinyServerMinVersion) < 0) {
            warning("Shiny Server v", .shinyServerMinVersion, 
                " or later is required; please upgrade!")
        }
    }
    orig.wd <- getwd()
    setwd(appDir)
    on.exit(setwd(orig.wd), add = TRUE)
    require(shiny)
    server <- shiny:::startApp(port = port)
    on.exit({
        httpuv:::stopServer(server)
    }, add = TRUE)
    if (launch.browser) {
        appUrl <- paste("http://localhost:", port, hash, sep = "")
        utils::browseURL(appUrl)
    }
    tryCatch(while (TRUE) {
        shiny:::serviceApp()
        Sys.sleep(0.001)
    }, finally = {
        shiny:::timerCallbacks$clear()
    })
}