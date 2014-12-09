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
#' @param state if specified, this tells the viewer the default parameter settings (such as layout, sorting, filtering, etc.) to use when the display is viewed (see \code{\link{validateState}} for details)
#' @param preRender should the panels be pre-rendered and stored (\code{TRUE}), or rendered on-the-fly in the viewer (\code{FALSE}, default)?  Default is recommended unless rendering is very expensive.
#' @param cogConn a connection to store the cognostics data.  By default, this is \code{\link{dfCogConn}()}.
#' @param output how to store the panels and metadata for the display (unnecessary to specify in most cases -- see details)
#' @param conn VDB connection info, typically stored in options("vdbConn") at the beginning of a session, and not necessary to specify here if a valid "vdbConn" object exists
#' @param verbose print status messages?
#' @param params a named list of parameters external to the input data that are needed in the distributed computing (most should be taken care of automatically such that this is rarely necessary to specify)
#' @param packages a vector of R package names that contain functions used in \code{panelFn} or \code{cogFn} (most should be taken care of automatically such that this is rarely necessary to specify)
#' @param control parameters specifying how the backend should handle things (most-likely parameters to \code{rhwatch} in RHIPE) - see \code{\link[datadr]{rhipeControl}} and \code{\link[datadr]{localDiskControl}}
#' 
#' @details Many of the parameters are optional or have defaults.  For several examples, see the documentation at tessera.io: \url{http://tessera.io/docs-trelliscope}
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
   height = 500,
   width = 500,
   panelFn = NULL, # function to be applied to each split,
   lims = list(x = "free", y = "free", prepanelFn = NULL),
   cogFn = NULL,
   state = NULL,
   preRender = FALSE,
   cogConn = dfCogConn(),
   output = NULL,
   conn = getOption("vdbConn"),
   verbose = TRUE,
   params = NULL,
   packages = NULL,
   control = NULL
) {
   validateVdbConn(conn)
   
   # check name and group
   if(grepl("[^a-zA-Z0-9_\\.]", name)) {
      stop("Argument 'name' must contain only numbers, letters, or the symbols '.' or '_'")
   }
   if(grepl("[^a-zA-Z0-9_/\\.]", group)) {
      stop("Argument 'group' must contain only numbers, letters, or the symbols '.', '_', or '/'")
   }

   if(!inherits(data, "ddo")) {
      stop("Input data must be an object of class 'ddo'")
   }
   
   if(!preRender && !hasExtractableKV(data)) {
      if(!inherits(data, "kvLocalDisk"))
         stop("Subsets of this data cannot be extracted by key -- cannot create display using preRender == FALSE.  Try calling makeExtractable() on the data.")
   }
   
   vdbPrefix <- conn$path
   
   # temporary location for display
   tempPrefix <- tempfile()
   dir.create(tempPrefix)

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
         output <- localDiskConn(file.path(tempPrefix, "panels"), autoYes = TRUE)
      } else if(!inherits(output, "kvConnection")) {
         stop("You are pre-rendering panels, but did not specify a valid 'output' location for these.  It is best to leave output = NULL when pre-rendering.")
      }
   } else {
      if(inherits(data, "kvMemory")) {
         # if an in-memory data set is too large we want to put it on disk
         if(object.size(data) > 50 * 1024^2)
            data <- convert(data, localDiskConn(file.path(tempPrefix, "panels"), autoYes = TRUE))
      }
      
      panelDataSource <- data
   }
   
   if(verbose) message("* Validating 'panelFn'...")
   panelEx <- kvApply(panelFn, kvExample(data))
   
   cogEx <- validateCogFn(data, cogFn, verbose)
   
   if(is.null(desc) || is.na(desc))
      desc <- "(no description)"
   
   panelFnType <- getPanelFnType(panelEx)
   class(panelFn) <- c("function", panelFnType)
   lims <- validateLims(lims, data, panelFn, verbose)
   
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
      # cogEmit.mongoCogConn = cogEmit.mongoCogConn,
      cogCollect = cogCollect,
      cogCollect.dfCogConn = cogCollect.dfCogConn,
      # cogCollect.mongoCogConn = cogCollect.mongoCogConn,
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
   
   packages <- c(packages, "lattice", "ggplot2", "digest", "base64enc", "data.table")
   # } else {
   #    packages <- c(packages, "trelliscope")
   # }
   
   panelGlobals <- drGetGlobals(panelFn)
   cogGlobals <- drGetGlobals(cogFn)
   
   packages <- unique(c(packages, panelGlobals$packages, cogGlobals$packages))
   
   globalVarList <- c(panelGlobals$vars, cogGlobals$vars)
   
   if(length(globalVarList) > 0) {
      # don't want duplicates
      nms <- names(globalVarList)
      globalVarList <- globalVarList[which(!duplicated(nms))]
      parList <- c(parList, globalVarList)
      nms <- names(parList)
      parList <- parList[which(!duplicated(nms))]
   }
   
   if(length(params) > 0)
      parList <- c(parList, params)
   
   jobRes <- mrExec(data,
      map      = map,
      reduce   = reduce,
      output   = output,
      control  = control,
      params   = parList,
      packages = packages
   )
   
   tryRes <- try({
   if(preRender)
      panelDataSource <- jobRes
   
   # read in cognostics
   cogDatConn <- cogFinal(cogConn, jobRes, conn, group, name, cogEx)
   
   # get panelKey "signature"
   keySig <- digest(jobRes[["TRS___panelkey"]][[2]])
      
   modTime <- Sys.time()

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
      panelFnType = panelFnType,
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
      relatedData = globalVarList,
      relatedPackages = packages
   )
   class(displayObj) <- "displayObj"
   
   if(!is.null(state))
      displayObj$state <- validateState(name, group, state, displayObj)
   
   save(displayObj, file = file.path(tempPrefix, "displayObj.Rdata"))

   # make thumbnail
   message("* Plotting thumbnail...")
   suppressMessages(makePNG(kvExample(data), panelFn = panelFn, file = file.path(tempPrefix, "thumb.png"), width = width, height = height, lims = lims))
   # small thumbnail
   makeThumb(file.path(tempPrefix, "thumb.png"), file.path(tempPrefix, "thumb_small.png"), height = 120, width = 120 * width / height)      
   })
   
   if(inherits(tryRes, "try-error")) {
      stop("The above error(s) occurred after making the display.", call. = FALSE)
   } else {
      if(verbose)
         message("* Updating displayList...")

      updateDisplayList(list(
         group = group,
         name = name,
         desc = desc,
         n = getAttribute(data, "nDiv"),
         panelFnType = panelFnType,
         preRender = preRender,
         dataClass = tail(class(data), 1),
         cogClass = class(cogConn)[1],
         height = height,
         width = width,
         updated = modTime,
         keySig = keySig
      ), conn)

      # get display prefix (and move old display to backup if it already exists)
      displayPrefix <- file.path(vdbPrefix, "displays", group, name)
      checkDisplayPath(displayPrefix, verbose)
      file.rename(tempPrefix, displayPrefix)
   }

   return(invisible(displayObj))
}


## remove all _bak directories
cleanupDisplays <- function(conn = getOption("vdbConn")) {
   validateVdbConn(conn)
   
   ff <- list.files(file.path(conn$path, "displays"), recursive = TRUE, include.dirs = TRUE, pattern = "_bak$", full.names = TRUE)
   for(f in ff) {
      unlink(f, recursive = TRUE)
   }
}

