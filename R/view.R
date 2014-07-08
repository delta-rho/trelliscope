if(getRversion() >= "2.15.1") {
  utils::globalVariables(c(".shinyServerMinVersion"))
}


#' View a Display or Run Shiny Display Viewer
#'
#' View a display or run Shiny display viewer
#'
#' @param name,group optional parameters to load the viewer with a pre-specified display
#' @param openBrowser should the browser be automatically launched?
#' @param conn VDB connection info, typically stored in options("vdbConn") at the beginning of a session, and not necessary to specify here if a valid "vdbConn" object exists
#' @param port what port to use for the viewer
#'
#' @author Ryan Hafen
#'
#' @export
view <- function(name = NULL, group = NULL, openBrowser = TRUE, conn = getOption("vdbConn"), port = 8100L) {

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

   require(shiny)
   hash <- ""
   if(!is.null(name)) {
      if(is.null(group))
         group <- "common"
      hash <- paste("/#group=", group, "&name=", name, sep = "")
   }
   # make sure that the viewer has the prefix
   options(vdbShinyPrefix = vdbPrefix)
   # if on dev machine, make the viewer path be the code source directory
   # (not the package path)
   shinyAppPrefix <- Sys.getenv("TRELLISCOPE_DEV_APP_PREFIX")
   if(shinyAppPrefix == "")
      shinyAppPrefix <- file.path(packagePath, "trelliscopeViewer")
   
   myRunApp(shinyAppPrefix, port = port, hash = hash, launch.browser = openBrowser)
}

#' internal runApp (from shiny's runApp() - need to be able to add hash string)
#'
#' @param appDir,port,launch.browser,host,workerId,quiet,display.mode,hash TODO
#' @importFrom httpuv startServer stopServer
#'
#' @author Ryan Hafen
#' @import shiny
myRunApp <- function (appDir = getwd(), port = NULL, launch.browser = getOption("shiny.launch.browser", interactive()), host = getOption("shiny.host", "127.0.0.1"), workerId = "", quiet = FALSE, display.mode = c("auto", "normal", "showcase"), hash = "") {
   on.exit({
      shiny:::handlerManager$clear()
   }, add = TRUE)
   if (is.null(host) || is.na(host))
      host <- "0.0.0.0"
   ops <- options(warn = 1)
   on.exit(options(ops), add = TRUE)
   shiny:::workerId(workerId)
   if (nzchar(Sys.getenv("SHINY_PORT"))) {
      ver <- Sys.getenv("SHINY_SERVER_VERSION")
      if (compareVersion(ver, .shinyServerMinVersion) < 0) {
         warning("Shiny Server v", .shinyServerMinVersion, " or later is required; please upgrade!")
      }
   }
   shiny:::setShowcaseDefault(0)
   if (is.character(appDir)) {
      desc <- shiny:::file.path.ci(appDir, "DESCRIPTION")
      if (file.exists(desc)) {
         settings <- read.dcf(desc)
         if ("DisplayMode" %in% colnames(settings)) {
            mode <- settings[1, "DisplayMode"]
            if (mode == "Showcase") {
               shiny:::setShowcaseDefault(1)
            }
         }
      }
   }
   display.mode <- match.arg(display.mode)
   if (display.mode == "normal")
      shiny:::setShowcaseDefault(0)
   else if (display.mode == "showcase")
      shiny:::setShowcaseDefault(1)
   require(shiny)
   .globals <- get(".globals", loadNamespace("shiny")) ##
   if (is.null(port)) {
      for (i in 1:20) {
         if (!is.null(.globals$lastPort)) {
            port <- .globals$lastPort
            .globals$lastPort <- NULL
         }
         else {
            port <- shiny:::p_randomInt(3000, 8000)
         }
         tmp <- try(startServer(host, port, list()), silent = TRUE)
         if (!inherits(tmp, "try-error")) {
            stopServer(tmp)
            .globals$lastPort <- port
            break
         }
      }
   }
   appParts <- as.shiny.appobj(appDir)
   if (!is.null(appParts$onStart))
      appParts$onStart()
   if (!is.null(appParts$onEnd))
      on.exit(appParts$onEnd(), add = TRUE)
   server <- shiny:::startApp(appParts, port, host, quiet)
   on.exit({
      stopServer(server)
   }, add = TRUE)
   if (!is.character(port)) {
      browseHost <- if (identical(host, "0.0.0.0"))
         "127.0.0.1"
      else host
      appUrl <- paste("http://", browseHost, ":", port, hash, sep = "")
      if (is.function(launch.browser))
         launch.browser(appUrl)
      else if (launch.browser)
         utils::browseURL(appUrl)
   }
   else {
      appUrl <- NULL
   }
   shiny:::callAppHook("onAppStart", appUrl)
   on.exit({
      shiny:::callAppHook("onAppStop", appUrl)
   }, add = TRUE)
   .globals$retval <- NULL
   .globals$stopped <- FALSE
   shiny:::shinyCallingHandlers(while (!.globals$stopped) {
      shiny:::serviceApp()
      Sys.sleep(0.001)
   })
   return(.globals$retval)
}


## v0.9.1
# myRunApp <- function(appDir = getwd(), port = NULL, launch.browser = getOption("shiny.launch.browser", interactive()), host = getOption("shiny.host", "127.0.0.1"), workerId = "", quiet = FALSE, display.mode = c("auto", "normal", "showcase"), hash="") {
#    if (is.null(host) || is.na(host))
#       host <- "0.0.0.0"
#    ops <- options(warn = 1)
#    on.exit(options(ops))
#    if (nzchar(Sys.getenv("SHINY_PORT"))) {
#       ver <- Sys.getenv("SHINY_SERVER_VERSION")
#       if (compareVersion(ver, .shinyServerMinVersion) < 0) {
#          warning("Shiny Server v", .shinyServerMinVersion,
#          " or later is required; please upgrade!")
#       }
#    }
#    shiny:::setShowcaseDefault(0)
#    if (is.character(appDir)) {
#       desc <- shiny:::file.path.ci(appDir, "DESCRIPTION")
#       if (file.exists(desc)) {
#          settings <- read.dcf(desc)
#          if ("DisplayMode" %in% colnames(settings)) {
#             mode <- settings[1, "DisplayMode"]
#             if (mode == "Showcase") {
#                shiny:::setShowcaseDefault(1)
#             }
#          }
#       }
#    }
#    display.mode <- match.arg(display.mode)
#    if (display.mode == "normal")
#       shiny:::setShowcaseDefault(0)
#    else if (display.mode == "showcase")
#       shiny:::setShowcaseDefault(1)
#
#    require(shiny)
#
#    .globals <- get(".globals", loadNamespace("shiny"))
#
#    if (is.null(port)) {
#       for (i in 1:20) {
#          if (!is.null(.globals$lastPort)) {
#             port <- .globals$lastPort
#             .globals$lastPort <- NULL
#          }
#          else {
#             port <- round(runif(1, min = 3000, max = 8000))
#          }
#          tmp <- try(startServer(host, port, list()), silent = TRUE)
#          if (!is(tmp, "try-error")) {
#             stopServer(tmp)
#             .globals$lastPort <- port
#             break
#          }
#       }
#    }
#    if (is.character(appDir)) {
#       orig.wd <- getwd()
#       setwd(appDir)
#       on.exit(setwd(orig.wd), add = TRUE)
#       server <- shiny:::startAppDir(port = port, host = host, workerId = workerId, quiet = quiet)
#    } else {
#       server <- shiny:::startAppObj(appDir$ui, appDir$server, port = port, host = host, workerId = workerId, quiet = quiet)
#    }
#    on.exit({
#       stopServer(server)
#    }, add = TRUE)
#    if (!is.character(port)) {
#       browseHost <- if (identical(host, "0.0.0.0"))
#          "127.0.0.1"
#       else host
#
#       appUrl <- paste("http://", browseHost, ":", port, hash, sep = "")
#       if (is.function(launch.browser))
#       launch.browser(appUrl)
#       else if (launch.browser)
#       utils::browseURL(appUrl)
#    } else {
#       appUrl <- NULL
#    }
#    shiny:::callAppHook("onAppStart", appUrl)
#    on.exit({
#       shiny:::callAppHook("onAppStop", appUrl)
#    }, add = TRUE)
#    .globals$retval <- NULL
#    .globals$stopped <- FALSE
#    shiny:::shinyCallingHandlers(while (!.globals$stopped) {
#       shiny:::serviceApp()
#       Sys.sleep(0.001)
#    })
#    return(.globals$retval)
# }




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
#         stopServer(server)
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

# for shiny version <= 0.8
# myRunApp <- function(appDir=getwd(), port=8100L, launch.browser=getOption('shiny.launch.browser', interactive()), hash="") {
#
#    workerId <- ""
#
#    # Make warnings print immediately
#    ops <- options(warn = 1)
#    on.exit(options(ops))
#
#    if (nzchar(Sys.getenv('SHINY_PORT'))) {
#       # If SHINY_PORT is set, we're running under Shiny Server. Check the version
#       # to make sure it is compatible. Older versions of Shiny Server don't set
#       # SHINY_SERVER_VERSION, those will return "" which is considered less than
#       # any valid version.
#       ver <- Sys.getenv('SHINY_SERVER_VERSION')
#       if (compareVersion(ver, .shinyServerMinVersion) < 0) {
#          warning('Shiny Server v', .shinyServerMinVersion,
#             ' or later is required; please upgrade!')
#       }
#    }
#
#    require(shiny)
#
#    if (is.character(appDir)) {
#       orig.wd <- getwd()
#       setwd(appDir)
#       on.exit(setwd(orig.wd), add = TRUE)
#       server <- shiny:::startAppDir(port=port, workerId)
#    } else {
#       server <- shiny:::startAppObj(appDir$ui, appDir$server, port=port, workerId)
#    }
#
#    on.exit({
#       stopServer(server)
#    }, add = TRUE)
#
#    if (launch.browser && !is.character(port)) {
#       appUrl <- paste("http://localhost:", port, hash, sep="")
#       utils::browseURL(appUrl)
#    }
#
#    env <- shiny:::.globals
#    assign("retval", NULL, envir=env)
#    assign("stopped", FALSE, envir=env)
#    tryCatch(
#       while (!get("stopped", envir=env)) {
#          shiny:::serviceApp()
#          Sys.sleep(0.001)
#       },
#       finally = {
#          shiny:::timerCallbacks$clear()
#       }
#    )
#
#    return(get("retval", envir=env))
# }

