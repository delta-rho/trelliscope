# if lims is of class "vdbLims" then update the plot limits
# otherwise, we need to go through and compute limits
# if both x and y are free, 

# TODO: make a vdbPlotTest function that applies to an example of the data

#' Create a vdb Display
#' 
#' Create a vdb display
#' 
#' @param dat data of class "localDiv", "rhData", "trellis", "ggplot", or "expression"
#' @param name the name of the display (no spaces or special characters)
#' @param group the group the display belongs to (displays are organized into groups).  Defaults to "common"
#' @param desc a description of the display (used in the viewer and in notebooks)
#' @param plotDim a list defining aspects of the plot dimension, including height, width, aspect, and res (resolution of raster image).  defaults are 480 (px), 40, "fill", and 150, respectively
#' @param plotFn a function that produces a plot and takes one argument, which will be the current split of the data being passed to it.  Useful to test with plotFn(divExample(dat)).  Must return either an object of class "ggplot", "trellis", or "expression" (of base plot commands)
#' @param lims either an object of class "vdbLims" as obtained from \code{\link{vdbSetLims}} or a list with elements x, y, and preFn, that specify how to apply \code{\link{vdbPrepanel}} and \code{\link{vdbSetLims}}
#' @param cogFn a function that produces a single row of a data frame where each column is a cognostic feature .  The function should takes one argument, which will be the current split of the data being passed to it.  Useful to test with cogFn(divExample(dat))
#' @param cogDesc a vector of descriptions for the cognostic variables specified by cogFn
#' @param inputVars input variables that will allow user input in the viewer, defined by \code{\link{inputVars}}
#' @param conn vdb connection info, typically stored in options("vdbConn") at the beginning of a session, and not necessary to specify here if a valid "vdbConn" object exists
#' @param storage how to store the plots and metadata for the display.  See details
#' @param subDirSize the approximate number plots per subdirectory.  Only used of \code{storage=="local"}.  If number of plots is less, there will not be subdiretories.  If set to 0, there will not be subdirectories.
#' @param verbose print status messages?
#' @param parallel for storage="local" create plots in parallel (currently not working)
#' @param mapred parameters to be passed to the Rhipe mapreduce job (see \code{\link{rhwatch}})
#' @param calledFromRhipe don't mess with this
#' 
#' @details there are a lot of details... see the vignette: \code{browseVignettes("vdb")}
#' 
#' Many of the parameters are optional or have defaults.
#' 
#' Storage options for plots:
#' \describe{
#'    \item{local:}{plots for each of the panels will be stored in the displays directory of the vdb directory}
#'    \item{mongo:}{plots will be stored in mongodb using the mongodb connection as specified by "vdbConn" (see vignette) - this is very experimental}
#'    \item{hdfs:}{plots will be stored in a mapfile on HDFS - can only be done with data of class "rhSplit"}
#'    \item{localData:}{instead of storing plots, data, plotFn, etc. will be stored locally and plotFn will be applied to the data on-the-fly in the viewer}
#' } 
#' There are so many options because there are several tradeoffs, described in the vignette.
#' 
#' @author Ryan Hafen
#' 
#' @seealso \code{\link{vdbPrepanel}}, \code{\link{vdbSetLims}}, \code{\link{inputVars}}, \code{\link{divide}}
#' 
#' @examples
#' # see docs
#' 
#' @export
vdbPlot <- function(
   data,
   name,
   group = "common",
   desc = "",
   plotDim = list(height=NULL, width=NULL, aspect=NULL, res=NULL),
   plotFn = NULL, # function to be applied to each split,
   lims = list(x="free", y="free", preFn=NULL),
   cogFn = NULL,
   cogDesc = NULL,
   inputVars = NULL, 
   conn = getOption("vdbConn"),
   storage = NULL,
   subDirSize = 1000,
   subDirN = 0, # number of subdirectories - this overrides subDirSize
   verbose = TRUE,
   parallel = FALSE,
   rhFail = TRUE,
   mapred = NULL,
   calledFromRhipe = FALSE # don't mess with this
) {
   
   # TODO: make sure plotFn is specified (maybe test it on a subset)
   # TODO: if it's not class localDiv or rhSplit, then error out immediately
   
   isSinglePlot <- inherits(data, "trellis") || inherits(data, "ggplot") || inherits(data, "expression")
   plotEx <- NULL
   
   # validate data (if it is NULL, stop)
   if(inherits(data, "localDiv")) {
      if(length(data) == 0)
         stop("Data is empty")
   }
   
   if(!calledFromRhipe) {
      if(!isSinglePlot) {
         plotEx <- vdbValidatePlotFn(plotFn, data, verbose)         
      }
      
      plotDim <- vdbValidatePlotDim(plotDim, data, plotFn, verbose)
      storage <- vdbValidateStorage(storage, conn, class(data))
      inputVars <- vdbValidateInputs(inputVars)
      
      if(!is.null(cogFn)) {
         cogEx <- vdbValidateCogFn(data, cogFn, verbose)
         cogDesc <- vdbValidateCogDesc(cogEx, cogDesc)         
      }
      vdbPrefix <- vdbValidatePrefix(conn)
      displayPrefix <- vdbGetDisplayPrefix(conn, group, name)   
      
      vdbValidateDisplayPrefix(displayPrefix)
      if(is.null(desc) || is.na(desc))
         desc <- ""
         
      hdfsPrefix <- NULL
      if(inherits(data, "rhData")) {
         hdfsPrefix <- conn$hdfsPrefix
         # TODO: make sure directory exists
         if(is.null(hdfsPrefix)) {
            message("hdfsPrefix not specified... Using current hadoop working directory.")
            hdfsPrefix <- hdfs.getwd()
         }
      }
   } else {
      # we should be here if we have been called from RHIPE
      if(storage=="local") {
         displayPrefix <- vdbGetDisplayPrefix(conn, group, name)         
      } else {
         displayPrefix <- tempdir()         
      }
   }
   
   dataSig <- NA
   
   if(!isSinglePlot && !calledFromRhipe) {
      # if the user specified limits, use them
      # if not, we need to call vdbPrepanel on the data
      if(is.null(lims)) { # 
         if(verbose)
            message("Limits not supplied.  Applying plotFn as is.")
         
         # lims <- list(x=list(type="free"), y=list(type="free"))
         # lims$preFnIsTrellis <- FALSE
         # xLimType <- "free"
         # yLimType <- "free"
      } else if(inherits(lims, "vdbLims")) {
         xLimType <- lims$x$type
         yLimType <- lims$y$type
      } else {
         if(verbose)
            message("Precomputed limits not supplied.  Computing axis limits...")

         # should have x, y, preFn
         xLimType <- lims$x
         yLimType <- lims$y
         if(is.null(xLimType)) xLimType <- "free"
         if(is.null(yLimType)) yLimType <- "free"
         
         lims <- list(x=list(type="free"), y=list(type="free"))         
         
         # # huh? plotEx is plotFn, not preFn
         # if(inherits(plotEx, "trellis")) {
         #    lims$preFnIsTrellis <- TRUE
         # } else {
         #    lims$preFnIsTrellis <- FALSE
         # }
         
         # browser()
         # if both are free, we don't need to do anything
         # this will avoid an extra Rhipe job if data is "rhData"
         if(!(xLimType == "free" && yLimType == "free")) {
            # TODO: checking to make sure things are specified correctly
            # TODO: handle dx and dy
            if(is.null(lims[["preFn"]])) { # cannot do $preFn because of preFnIsTrellis (they both start the same)
               if(inherits(plotEx, "trellis")) {
                  preFn <- plotFn
               } else {
                  stop("'lims' argument does not specify a prepanel function.  This could be ignored if plotFn returns an object of class 'trellis', which can be used to determine axis limits.")
               }
            } else {
               preFn <- lims[["preFn"]]
            }
            pre <- vdbPrepanel(data, preFn=preFn)
            lims <- vdbSetLims(pre, x=xLimType, y=yLimType)
         } else {
            if(verbose)
               message("... skipping this step since both are axes are free ...")
         }
      }
   } else if(calledFromRhipe) {
      xLimType <- lims$x$type
      yLimType <- lims$y$type
   }
   
   # if it's localData, just store the data, plotFn, etc.
   if(storage == "localData" || (storage=="hdfsData" && calledFromRhipe)) {
      if(verbose)
         message("Storing data since storage='", storage, "'.  Plots will be created on-demand in the viewer.")
      
      # this happens in the plotting for the other data, so need to do it here
      splitKeys <- getKeys(data) # sapply(seq_along(data), function(x) data[[x]]$splitKey)
      # TODO: check for non-unique splitKeys
      dataSig <- digest(data)

      if(storage=="localData") {
         localDataPrefix <- file.path(vdbPrefix, "displays", "localData")
         if(!file.exists(localDataPrefix))
            dir.create(localDataPrefix, recursive=TRUE)

         localDataPath <- file.path(localDataPrefix, paste(dataSig, ".Rdata", sep=""))
         if(!file.exists(localDataPath))
            save(data, file=localDataPath)

         # save(localDataExtra, file=file.path(displayPrefix, "localDataExtra.Rdata"))

         nPanels <- length(data)
      }
   } else {
      if(verbose)
         message("Generating plots...")

      if(isSinglePlot) {
         if(verbose)
            message("-- Plotting trellis / ggplot / base R plot object.")
         # browser()
         
         xLimType <- NULL
         yLimType <- NULL
         lims <- NULL
         
         dir.create(file.path(displayPrefix, "png"))
         
         pngPrefix <- file.path(displayPrefix, "png", paste(name, "_%04d", ".png", sep=""))
         
         vdbMakePNG(dat=data, file=pngPrefix, width=plotDim$width, height=plotDim$height, res=plotDim$res, xLimType=xLimType, yLimType=yLimType, lims=lims)
         
         plotLocs <- list.files(file.path(displayPrefix, "png"), full.names=TRUE)
         splitKeys <- list.files(file.path(displayPrefix, "png"))
         splitKeys <- gsub("(.*)\\.png$", "\\1", splitKeys)
         
         nPanels <- length(splitKeys)
         
         if(storage == "mongo") {
            plotRes <- lapply(seq_along(plotLocs), function(x) mongoEncodePlot(plotLocs[x], splitKeys[x]))
            mongo.insert.batch(mongoConn, mongoNS, plotRes)
         }
      }
      
      # if object is of class "localDiv", it is plotted
      # (note: this is called within each rhipe map task if data is of class rhData, unless storage is hdfsData)
      if(inherits(data, "localDiv") && storage != "hdfsData") {
         dataSig <- digest(data)
         
         if(storage == "mongo") {
            mongoConn <- vdbMongoInit(conn)
            mongoNS <- paste(conn$vdbName, name, sep=".")
         }
         nPanels <- length(data)
         
         splitKeys <- getKeys(data) # sapply(seq_along(data), function(x) data[[x]]$splitKey)
         
         # handle subdirectories if necessary:
         # if number of subdirectories (subDirN) wasn't provided
         # then we need to compute it based on subDirSize
         if(subDirN == 0) {
            if(subDirSize > 0 && nPanels > subDirSize) {
               subDirN <- ceiling(nPanels / subDirSize)
            } else {
               subDirN <- 0
            }
         }

         # stop(paste("nPanels:", nPanels, "; subDirN:", subDirN, "; subDirSize:", subDirSize, "; splitKeys:", splitKeys, "\n"))

         if(subDirN > 0) {
            subDirs <- sapply(splitKeys, function(x) keyHash(x, subDirN))
         } else {
            subDirs <- rep("", nPanels)
         }
         
         pngPrefix <- file.path(displayPrefix, "png")
         
         if(storage == "mongo") {
            # remove old plots...
            # TODO: maybe make this more safe?
            if(!calledFromRhipe)
               mongo.remove(mongoConn, mongoNS)
            pngPrefix <- file.path(tempdir(), group, name)
         }
         pngUniquePrefix <- file.path(pngPrefix, unique(subDirs))

         sapply(pngUniquePrefix, function(x) dir.create(x, recursive=TRUE))

         # make list of plot paths
         plotLocs <- file.path(pngPrefix, subDirs, paste(splitKeys, ".png", sep=""))

         # loop through and apply plotFn to each one
         # TODO: make this parallel

         # if(parallel) {
         #    require(parallel)
         #    cl <- makeCluster(detectCores()))
         #    clusterEvalQ(cl, library(lattice))
         #    # maybe use clusterMap

         if(verbose) cat("") # avoid \r below deleting all previous messages
         plotRes <- lapply(seq_len(nPanels), function(i) {
            if(verbose)
               message(paste("\r-- Plotting panel ", i, " of ", nPanels, sep=""), appendLF=FALSE)
            
            vdbMakePNG(dat=data[[i]], plotFn=plotFn, file=plotLocs[i], width=plotDim$width, height=plotDim$height, res=plotDim$res, xLimType=xLimType, yLimType=yLimType, lims=lims)
                        
            if(storage == "mongo") {
               return(mongoEncodePlot(plotLocs[i], names(data)[i]))
            }
            
            if(storage == "hdfs") {
               rhcollect(names(data)[i], encodePNG(plotLocs[i]))
            }
         })
         if(verbose)
            message(" ")

         if(storage == "mongo") {
            mongo.insert.batch(mongoConn, mongoNS, plotRes)
         }
      }

      if(inherits(data, "rhData")) {
         # run a RHIPE job that calls this function on each k/v pair where each is turned into a localDiv with the key being the splitKey and the value being the data
         # unless a different transformation is applied

         # get a hash of the listing of the data
         dataSig <- digest(rhls(data$loc))
         
         if(storage == "mongo") {
            library(digest)
            mongoConn <- vdbMongoInit(conn)
            mongoNS <- paste(conn$vdbName, name, sep=".")         
            # first, clear out the collection:
            mongo.remove(mongoConn, mongoNS)
         }

         # a <- rhread(data$loc)
         # k <- a[[1]][[1]]
         # r <- a[[1]][[2]]
         
         map <- rhmap({
            res <- try({
               d <- list(r)
               names(d) <- k
               class(d) <- c("localDiv", "list")
               attr(d, "divBy") <- data$divBy
               
               # d <- vdb:::vdbRhKeyValTrans(rhKeyTrans(k), rhValTrans(r))

               p <- vdbPlot(
                  name=name,
                  group=group,
                  plotFn=plotFn,
                  data=d,
                  lims=lims,
                  desc=desc,
                  cogFn=cogFn,
                  cogDesc=cogDesc,
                  conn=conn,
                  storage=storage,
                  subDirSize = subDirSize,
                  subDirN = subDirN,
                  plotDim=plotDim,
                  calledFromRhipe=TRUE,
                  rhFail=rhFail,
                  verbose=FALSE
               )
            }, silent=TRUE)
            
            if(inherits(res, "try-error")) {
               if(rhFail) # by default, we want the job to stop (but there are cases where we know some will fail and we want it to keep going)
                  stop(geterrmessage())
               rhcollect("VDB___error", list(key=k, error=res))
            } else {
               rhcollect("VDB___cog", p$cog)
               rhcollect("VDB___count", length(d))
            }
         })
         
         # rbind the results
         reduce <- expression(
            pre = {
               res <- NULL
               count <- 0
               sig <- NULL
               error <- NULL
            },
            reduce = {
               if(reduce.key == "VDB___cog") {
                  res <- rbind(res, do.call(rbind, reduce.values))
               } else if(reduce.key == "VDB___count") {
                  count <- count + sum(do.call(c, reduce.values))
               } else if(reduce.key == "VDB___sig") {
                  sig <- c(sig, do.call(c, reduce.values))
               } else if(reduce.key=="VDB___error") {
                  # TODO: make number of errors to keep configurable
                  if(length(error) < 100)
                     error <- c(reduce.values, error)
               } else {
                  rhcollect(reduce.key, reduce.values)
               }
            },
            post = {
               if(reduce.key == "VDB___cog")
                  rhcollect("VDB___cog", res)
               if(reduce.key == "VDB___count")
                  rhcollect("VDB___count", count)
               if(reduce.key == "VDB___error")
                  rhcollect("VDB___error", error)
            }
         )
         
         # rhmrLocal(map=map, ifolder=irisRhSplit$example)

         # need to remove sourceJobData and mapfile from data (or we get the error: 1(6): already exists in database: rexp.proto)
         data$sourceJobData <- NULL
         data$mapfile <- NULL

         parList <- list(
            name       = name,
            group      = group,
            plotFn     = plotFn,
            data       = data,
            lims       = lims,
            desc       = desc,
            cogFn      = cogFn,
            cogDesc    = cogDesc,
            conn       = conn,
            storage    = storage,
            subDirSize = subDirSize,
            subDirN    = subDirN,
            plotDim    = plotDim,
            # rhKeyTrans = rhKeyTrans,
            # rhValTrans = rhValTrans,
            rhFail     = rhFail
         )
         
         # if the package isn't loaded, need to pass other functions as well
         # (assuming that they are defined in the global environment instead)
         # (debugging and updating the package is a lot easier when just
         # sourcing the files at each change rather than building each time)
         if(! "package:vdb" %in% search()) {
            message("---- running dev version - sending vdb functions to RHIPE")
            parList <- c(parList, list(
               vdbPlot    = vdbPlot,
               vdbCurXLim = vdbCurXLim,
               vdbCurYLim = vdbCurYLim,
               vdbCurLim  = vdbCurLim,
               mongoEncodePlot = mongoEncodePlot,
               vdbMongoInit = vdbMongoInit,
               divide = divide,
               # vdbRhKeyValTrans = vdbRhKeyValTrans,
               encodePNG = encodePNG,
               vdbMakePNG = vdbMakePNG,
               vdbGetDisplayPrefix = vdbGetDisplayPrefix,
               vdbValidatePrefix = vdbValidatePrefix
            ))
         }
         
         if("package:vdb" %in% search()) {
            setup <- expression({
               suppressMessages(require(datadr))
               suppressMessages(require(vdb))
            })
         } else {
            setup <- expression({
               suppressMessages(require(lattice))
               suppressMessages(require(ggplot2))
               suppressMessages(require(digest))
               suppressMessages(require(caTools))
               suppressMessages(require(scagnostics))
               # suppressMessages(require(data.table))
            })
         }
         
         # if plotFn uses any data in the environment, pass that on too
         globalVars <- vdbFindGlobals(plotFn)
         # only look for objects the user has created
         globalVars <- intersect(globalVars, ls(envir=as.environment(-1)))
         if(length(globalVars) > 0) {
            vars <- mget(globalVars, envir=as.environment(-1), ifnotfound="notfound!", inherits=TRUE)
            vars <- vars[!sapply(vars, function(x) length(x) == 1 && x=="notfound!")]
            parList <- c(parList, vars)
         }
         
         # TODO: if 'mongo', can probably leave it as tempfolder (and then rhdel at the end)
         # ofolder <- Rhipe:::mkdHDFSTempFolder(file=name)
         ofolder <- paste(hdfsPrefix, "/", name, sep="")
         
         # a <- rhread(data$loc)
         # k <- a[[1]][[1]]
         # r <- a[[1]][[2]]
         rhoptions(copyObjects=list(auto=FALSE))
         # browser()
         # suppressMessages(capture.output(
         rhJob <- rhwatch(
            setup=setup,
            map=map,
            reduce=reduce,
            input=rhfmt(data$loc, type=data$type),
            output=rhfmt(ofolder, type="map"), #, compression="NONE"),
            mapred=mapred,
            combiner=TRUE,
            parameters=parList,
            # mon.sec=0,
            readback=FALSE
         ) # ))
         
         # id <- gsub(".*jobid=(job_.*)", "\\1", rhJob[[1]]$tracking)
         # rhRes <- vdbRhStatus(id)
         
         if(verbose)
            message(paste("Output written to a map file, ", ofolder, ".  Reading in cognostics output from this file...", sep=""))
         
         if(storage=="hdfs") {
            a <- suppressMessages(rhmapfile(ofolder))
            cogDF <- suppressMessages(a[["VDB___cog"]])
            nPanels <- suppressMessages(a[["VDB___count"]])            
            # error <- suppressMessages(a[["VDB___error"]])
            error <- NULL
         } else { # if plots are not stored in hdfs, we can read in all output
            tmp <- rhread(ofolder, type="map")
            outNames <- sapply(tmp, function(x) x[[1]])
            cogDF <- tmp[[which(outNames=="VDB___cog")]][[2]]
            nPanels <- tmp[[which(outNames=="VDB___count")]][[2]]
            error <- NULL
            if(any(outNames=="error"))
               error <- tmp[[which(outNames=="VDB___error")]][[2]]
         }
         
         # TODO: make easy way to get errors
         if(length(error) > 0)
            warning("There were errors - see...")
         
         # rhdel(ofolder)

         # nPanels <- as.integer(rhRes$counters$`Map-Reduce Framework`["Map input records"]) # not good because further splits can occur
      }
   }
   
   # TODO: automatically add cognostics variables for all conditioning variables
   if(!inherits(data, "rhData")) {
      # generate cogDF
      
      splitVarsDF <- NULL
      cogDF <- NULL
      if(inherits(data, "localDiv")) {
         if(data$divBy$type=="condDiv") {
            splitVarsDF <- do.call(rbind, lapply(data, function(x) {
               attr(x, "split")
            }))
            rownames(splitVarsDF) <- NULL
            
            cogDF <- data.frame(panelKey = splitKeys, splitVarsDF, stringsAsFactors=FALSE)
            extraCols <- c("panel key (file name)", rep("conditining variable", ncol(splitVarsDF)))
         }
      }

      if(is.null(cogDF)) {
         cogDF <- data.frame(panelKey = splitKeys, stringsAsFactors=FALSE)
         extraCols <- "panel key (file name)"   
      }
      
      # if(storage == "local" && nPanels > 1000) {
      #    cogDF$subDir <- subDirs
      #    extraCols <- c(extraCols, "plot subdirectory")
      # }
      
      if(!is.null(cogFn)) {
         if(verbose)
            message("Computing cognostics...")
         tmp <- lapply(data, function(x) data.frame(cogFn(x)))
         tmp <- do.call(rbind, tmp)
         
         # tmp <- data.frame(do.call(rbind, lapply(data, function(x) cogFn(x))), stringsAsFactors=FALSE)
         cogDF <- cbind(cogDF, tmp)
      }
      cogDesc <- c(extraCols, cogDesc)      
   }
   
   # # example of how to query a mongo plot and display it
   # a <- getMongoPlot(mongoConn, mongoNS, data[[1]]$splitKey)
   # aa <- tempfile(fileext=".html")
   # cat(paste("<img src=\"", a, "\">"), file=aa)
   # browseURL(aa)
   
   if(calledFromRhipe) {
      return(list(cog=cogDF))
   } else {
      # write cognostics, inputs, cognostics, and unique keys to disk and update displayList
      
      cogDF <- cogDF[order(cogDF$panelKey),]
      
      # TODO: aspect ratio
      if(verbose)
         message("Updating displayList...")
         
      modTime <- Sys.time()
      keySig <- digest(sort(cogDF$panelKey))
      
      vdbUpdateDisplayList(
         vdbPrefix=vdbPrefix, 
         name=name, 
         group=group, 
         desc=desc, 
         n=nPanels, 
         storage=storage, 
         hdfsPrefix=hdfsPrefix, 
         width=plotDim$width, 
         height=plotDim$height, 
         aspect=plotDim$aspect, 
         updated=modTime, 
         keySig=keySig,
         dataSig=dataSig,
         subDirN=subDirN
      )
      # keySig is a representation of the collection of keys, used to identify other plots with the same set of keys
      
      vdbUpdateDisplayListJson(vdbPrefix)
      
      # # write input variables, if specified
      # if(!is.null(inputVars)) {
      #    message("Writing input variables...")
      #    # browser()
      #    save(inputVars, file=file.path(displayPrefix, "input.Rdata"))
      # }

      if(verbose)
         message("Updating displayList...")
      
      hdfsDataSource <- NULL
      if(storage=="hdfsData")
         hdfsDataSource <- list(loc=data$loc, type=data$type, class=class(data))
      
      displayObj <- list(
         vdbPrefix=vdbPrefix, 
         name=name, 
         group=group, 
         desc=desc, 
         n=nPanels, 
         storage=storage, 
         hdfsPrefix=hdfsPrefix, 
         updated=modTime, 
         keySig=keySig,
         dataSig=dataSig,
         subDirN=subDirN,
         plotFn=plotFn,
         cogFn=cogFn,
         cogDesc=cogDesc,
         inputVars=inputVars,
         plotDim=plotDim, 
         lims=lims,
         xLimType=xLimType,
         yLimType=yLimType,
         storage=storage,
         hdfsDataSource=hdfsDataSource
         # rhKeyTrans=rhKeyTrans,
         # rhValTrans=rhValTrans
      )
      
      save(displayObj, file=file.path(displayPrefix, "object.Rdata"))
      
      # write panel keys
      # (this is used when linking to other displays)
      # (need the plut keys)
      if(verbose)
         message("Writing panel keys...")
      # keyCols <- which(names(cogDF) %in% c("panelKey", "subDir"))
      keys <- cogDF$panelKey
      save(keys, file=file.path(displayPrefix, "panelKeys.Rdata"))
      
      # write cognostics
      # if(!is.null(cogFn)) {
         message("Writing cognostics...")
         cog <- list(
            cogDF=cogDF,
            cogDesc=cogDesc,
            cogIndex=NULL
         )
         # TODO: add indexes
         save(cog, file=file.path(displayPrefix, "cog.Rdata"))
      # }
      
      # make thumbnail
      message("Plotting thumbnail...")
      # thumbHeight <- conn$thumbHeight
      # if(is.null(thumbHeight))
      #    thumbHeight <- 120
      # curWidth <- thumbHeight * plotDim$width / plotDim$height
      
      suppressMessages(vdbMakePNG(dat=divExample(data), plotFn=plotFn, file=file.path(displayPrefix, "thumb.png"), width=plotDim$width, height=plotDim$height, res=plotDim$res, xLimType=xLimType, yLimType=yLimType, lims=lims))
      
      if(storage != "mongo") {
         # browser()
         vdbWriteCogJson(displayPrefix, group, name, cogDF, cogDesc, inputVars, plotDim$height, plotDim$width, nPanels)
      }
      
      return(invisible(displayObj))
   }
}

# prefix/group/name/png/0/xxxx.png
# prefix/group/name/cog.Rdata
#   cogDF, cogIndex
# prefix/group/name/input.Rdata
# prefix/group/name/thumb.png
# prefix/group/name/cog.json (for old viewer)
# prefix/group/name/uniqueKeys.Rdata
# prefix/displayList.Rdata
#   df with name, group, description, number of plots, type, width, height



