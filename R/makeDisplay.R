if(getRversion() >= "2.15.1") {
  utils::globalVariables(c("collect"))
}


#' Create a Trelliscope Display
#'
#' Create a trelliscope display and add it to a visualization database (VDB)
#'
#' @param data data of class "ddo" or "ddf" (see \code{\link{ddo}}, \code{\link{ddf}})
#' @param name the name of the display (no spaces or special characters)
#' @param group the group the display belongs to (displays are organized into groups).  Defaults to "common"
#' @param desc a description of the display (used in the viewer and in notebooks)
#' @param height reference dimensions (in pixels) for each panel (panels will be resized based on available space in the viewer)
#' @param width reference dimensions (in pixels) for each panel (panels will be resized based on available space in the viewer)
#' @param panelFn a function that produces a plot and takes one argument, which will be the current split of the data being passed to it.  Useful to test with panelFn(divExample(dat)).  Must return either an object of class "ggplot", "trellis", or "expression" (of base plot commands)
#' @param lims either an object of class "trsLims" as obtained from \code{\link{setLims}} or a list with elements x, y, and prepanelFn, that specify how to apply \code{\link{prepanel}} and \code{\link{setLims}}
#' @param cogFn a function that produces a single row of a data frame where each column is a cognostic feature .  The function should takes one argument, which will be the current split of the data being passed to it.  Useful to test with cogFn(divExample(dat))
#' @param preRender should the panels be pre-rendered and stored (\code{TRUE}), or rendered on-the-fly in the viewer (\code{FALSE}, default)?  Default is recommended unless rendering is very expensive.
#' @param cogConn a connection to store the cognostics data.  By default, this is \code{\link{dfCogConn}()}, but if there are many subsets (millions or more), \code{\link{mongoCogConn}()} is recommended (if MongoDB is an option).
#' @param output how to store the panels and metadata for the display (unnecessary to specify in most cases -- see details)
#' @param conn VDB connection info, typically stored in options("vdbConn") at the beginning of a session, and not necessary to specify here if a valid "vdbConn" object exists
#' @param verbose print status messages?
#' @param params a named list of parameters external to the input data that are needed in the distributed computing (most should be taken care of automatically such that this is rarely necessary to specify)
#' @param control parameters specifying how the backend should handle things (most-likely parameters to \code{rhwatch} in RHIPE) - see \code{\link[datadr]{rhipeControl}} and \code{\link[datadr]{localDiskControl}}
#'
#' @details Many of the parameters are optional or have defaults.  For several examples, see the documentation on github: \url{http://hafen.github.io/trelliscope}
#'
#' Panels by default are not pre-rendered.  Instead, this function creates a display object and computes and stores the cognostics.  Then panels are rendered on the fly.  If a user would like to pre-render the images, then by default these will be stored to a local disk connection (see \code{\link{localDiskConn}}) inside the VDB directory, organized in subdirectories by group and name of the display.  Optionally, the user can specify the \code{output} parameter to be any valid "kvConnection" object, as long as it is one that persists on disk (e.g. \code{\link{hdfsConn}}).
#'
#' @author Ryan Hafen
#'
#' @seealso \code{\link{prepanel}}, \code{\link{setLims}}, \code{\link{divide}}
#'
#' @examples
#' # see docs
#'
#' @export
makeDisplay <- function(
   data,
   name,
   group = "common",
   desc = "",
   height = 800,
   width = 800,
   panelFn = NULL, # function to be applied to each split,
   lims = list(x = "free", y = "free", prepanelFn = NULL),
   cogFn = NULL,
   preRender = FALSE,
   cogConn = dfCogConn(),
   output = NULL,
   conn = getOption("vdbConn"),
   verbose = TRUE,
   params = NULL,
   control = NULL
) {
   # group <- "test"
   # name <- "awesome"
   # desc <- NULL
   # lims <- NULL
   # panelFn <- function(x) {
   #    xyplot(Sepal.Length ~ Petal.Length, data = x)
   # }
   # cogFn <- function(x) {
   #    list(mean = cog(mean(x$Sepal.Length), desc = "Mean sepal length"))
   # }
   # cogConn <- NULL
   # preRender <- TRUE
   # output <- NULL
   # verbose <- TRUE
   # height <- 800
   # width <- 800
   # conn <- vdbConn(file.path(tempdir(), "vdbtest"), autoYes = TRUE)
   # cogStorage <- "local"
   # control <- NULL

   # isSinglePlot <- inherits(data, "trellis") || inherits(data, "ggplot") || inherits(data, "expression")

   validateConn(conn)

   if(!inherits(data, "ddo")) {
      stop("Input data must be an object of class 'ddo'")
   }

   if(!preRender && !hasExtractableKV(data)) {
      if(!inherits(data, "kvLocalDisk"))
         stop("Subsets of this data cannot be extracted by key -- cannot create display using preRender == FALSE.  Try calling makeExtractable() on the data.")
   }

   vdbPrefix <- conn$path

   # get display prefix (and move old display to backup if it already exists)
   displayPrefix <- file.path(vdbPrefix, "displays", group, name)
   checkDisplayPath(displayPrefix, verbose)

   dataConn <- getAttribute(data, "conn")

   # if no cognostics connection was specified, use cogDatConn
   if(is.null(cogConn))
      cogConn <- dfCogConn()

   if(!inherits(cogConn, "cogConn"))
      stop("Argument 'cogConn' not valid")

   # if pre-rendering images and no output is specified
   # then store on disk in the VDB directory
   if(preRender) {
      if(is.null(output)) {
         output <- localDiskConn(file.path(displayPrefix, "panels"), autoYes = TRUE)
      } else if(!inherits(output, "kvConnection")) {
         stop("You are pre-rendering panels, but did not specify a valid 'output' location for these.  It is best to leave output = NULL when pre-rendering.")
      }
   } else {
      if(inherits(data, "kvMemory"))
         data <- convert(data, localDiskConn(file.path(displayPrefix, "panels"), autoYes = TRUE))

      panelDataSource <- data
   }

   if(verbose) message("* Validating 'panelFn'...")
   panelEx <- kvApply(panelFn, kvExample(data))

   cogEx <- validateCogFn(data, cogFn, verbose)

   if(is.null(desc) || is.na(desc))
      desc <- "(no description)"

   lims <- validateLims(lims, data, panelFn, panelEx, verbose)

   # map.keys <- kvExample(ldd)[1]
   # map.values <- kvExample(ldd)[2]

   cogPre(cogConn, conn, group, name)

   ## set up a mr job -- this job simply applies the
   ## cognostics (and panel if preRender = TRUE) to each subset
   map <- expression({
      cogRes <- list()
      for(i in seq_along(map.keys)) {
         # make plot if you need to
         if(preRender) {
            ff <- tempfile()
            makePNG(dat = list(map.keys[[i]], map.values[[i]]),
               panelFn = panelFn, file = ff, width = width,
               height = height, lims = lims)
            collect(map.keys[[i]], encodePNG(ff))
         }
         cogRes[[i]] <- applyCogFn(cogFn, list(map.keys[[i]], map.values[[i]]), dataConn)
         collect("TRS___panelkey", cogRes[[i]]$panelKey) # to build key signature
      }
      cogEmit(cogConn, cogRes, conn, group, name)
   })

   # rbind the results
   reduce <- expression(
      pre = {
         res <- NULL
      },
      reduce = {
         if(reduce.key == "TRS___cog") {
            res <- cogCollect(cogConn, res, reduce.values, conn, group, name)
         } else if(reduce.key == "TRS___panelkey") {
            res <- sort(c(res, do.call(c, reduce.values)))
            res <- c(head(res, 100), tail(res, 100))
         } else {
            collect(reduce.key, reduce.values)
         }
      },
      post = {
         if(reduce.key %in% c("TRS___cog", "TRS___panelkey")) {
            collect(reduce.key, res)
         }
      }
   )

   parList <- list(
      dataConn    = dataConn,
      group       = group,
      name        = name,
      preRender   = preRender,
      cogConn     = cogConn,
      panelFn     = panelFn,
      cogFn       = cogFn,
      lims        = lims,
      conn        = conn,
      height      = height,
      width       = width
   )

   # if the package isn't loaded, need to pass other functions as well
   # (assuming that they are defined in the global environment instead)
   # (debugging and updating the package is a lot easier when just
   # sourcing the files at each change rather than building each time)
   # if(! "package:trelliscope" %in% search()) {
   #    message("* ---- running dev version - sending trelliscope functions to mr job")
      parList <- c(parList, list(
         kvApply = kvApply,
         applyCogFn = applyCogFn,
         getSplitVars = getSplitVars,
         getBsvs = getBsvs,
         makePNG = makePNG,
         encodePNG = encodePNG,
         cogEmit = cogEmit,
         cogEmit.dfCogConn = cogEmit.dfCogConn,
         cogEmit.mongoCogConn = cogEmit.mongoCogConn,
         cogCollect = cogCollect,
         cogCollect.dfCogConn = cogCollect.dfCogConn,
         cogCollect.mongoCogConn = cogCollect.mongoCogConn,
         cog = cog,
         cogScagnostics = cogScagnostics,
         as.cogGeo = as.cogGeo,
         as.cogRel = as.cogRel,
         as.cogHier = as.cogHier,
         as.cogHref = as.cogHref,
         cogMean = cogMean,
         cogRange = cogRange,
         cog2df = cog2df,
         trsCurLim = trsCurLim,
         trsCurXLim = trsCurXLim,
         trsCurYLim = trsCurYLim
      ))

      setup <- expression({
         suppressMessages(require(lattice))
         suppressMessages(require(ggplot2))
         suppressMessages(require(digest))
         suppressMessages(require(base64enc))
         suppressMessages(require(scagnostics))
         suppressMessages(require(data.table))
      })
   # } else {
   #    setup <- expression({
   #       suppressMessages(require(trelliscope))
   #    })
   # }

   # if panelFn uses any data in the environment, pass that on too
   globalVars <- unique(c(findGlobals(panelFn), findGlobals(cogFn)))
   globalVarList <- getGlobalVarList(globalVars, parent.frame())

   if(length(globalVarList) > 0)
      parList <- c(parList, globalVarList)

   if(length(params) > 0)
      parList <- c(parList, params)

   jobRes <- mrExec(data,
      setup   = setup,
      map     = map,
      reduce  = reduce,
      output  = output,
      control = control,
      params  = parList
   )

   if(preRender)
      panelDataSource <- jobRes

   # read in cognostics
   cogDatConn <- cogFinal(cogConn, jobRes, conn, group, name, cogEx)

   # get panelKey "signature"
   keySig <- digest(jobRes[["TRS___panelkey"]][[2]])

   if(verbose)
      message("* Updating displayList...")

   modTime <- Sys.time()

   updateDisplayList(list(
      group = group,
      name = name,
      desc = desc,
      n = getAttribute(data, "nDiv"),
      preRender = preRender,
      dataClass = tail(class(data), 1),
      cogClass = class(cogConn)[1],
      height = height,
      width = width,
      updated = modTime,
      keySig = keySig
   ), conn)

   if(verbose)
      message("* Storing display object...")

   cogInfo <- getCogInfo(cogEx)
   cogDistns <- getCogDistns(cogDatConn, cogInfo)
   
   displayObj <- list(
      name = name,
      group = group,
      desc = desc,
      preRender = preRender,
      panelFn = panelFn,
      panelDataSource = panelDataSource,
      cogFn = cogFn,
      n = getAttribute(data, "nDiv"),
      cogDatConn = cogDatConn,
      cogInfo = cogInfo,
      cogDistns = cogDistns,
      updated = modTime,
      keySig = keySig,
      height = height,
      width = width,
      lims = lims,
      relatedData = globalVarList
   )
   class(displayObj) <- "displayObj"
   
   save(displayObj, file = file.path(displayPrefix, "displayObj.Rdata"))

   # make thumbnail
   message("* Plotting thumbnail...")
   suppressMessages(makePNG(kvExample(data), panelFn = panelFn, file = file.path(displayPrefix, "thumb.png"), width = width, height = height, lims = lims))
   # small thumbnail
   makeThumb(file.path(displayPrefix, "thumb.png"), file.path(displayPrefix, "thumb_small.png"), height = 120, width = 120 * width / height)
      
   return(invisible(displayObj))
}


## remove all _bak directories
cleanupDisplays <- function(conn = getOption("vdbConn")) {
   validateConn(conn)

   ff <- list.files(file.path(conn$path, "displays"), recursive = TRUE, include.dirs = TRUE, pattern = "_bak$", full.names = TRUE)
   for(f in ff) {
      unlink(f, recursive = TRUE)
   }
}

# prefix/group/name/png/0/xxxx.png
# prefix/group/name/cog.Rdata
#   cogDat, cogIndex
# prefix/group/name/input.Rdata
# prefix/group/name/thumb.png
# prefix/group/name/uniqueKeys.Rdata
# prefix/displayList.Rdata
#   df with name, group, description, number of plots, type, width, height



