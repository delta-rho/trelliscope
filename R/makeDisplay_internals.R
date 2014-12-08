
## internal
validateVdbConn <- function(conn, mustHaveDisplays = FALSE) {
   if(!inherits(conn, "vdbConn"))
      stop("connection must be valid vdb connection", call. = FALSE)

   if(!file.exists(conn$path))
      stop("VDB connection path: ", conn$path, " does not exist.  Initiate a valid VDB connection by calling vdbConn()", call. = FALSE)

   if(mustHaveDisplays) {
      if(!file.exists(file.path(conn$path, "displays", "_displayList.Rdata")))
         stop("Visualization database does not have any displays: ", conn$path, call. = FALSE)
   }
}

## internal
validateCogFn <- function(dat, cogFn, verbose = FALSE) {
   if(verbose)
      message("* Testing cognostics function on a subset ... ", appendLF = FALSE)
   ex <- applyCogFn(cogFn, kvExample(dat), getAttribute(dat, "conn"))

   # if(!is.list(ex))
   #    stop("cogFn should return a list")
   # if(!all(sapply(ex, function(x) inherits(x, "cog"))))
   #    stop("Each cognostic must have class 'cog' - please make sure you are specifying: var = cog(...)")

   exdf <- cog2df(ex)
   if(nrow(exdf) > 1)
      stop("'cogFn' must return something that can be coerced into a 1-row data.frame", call. = FALSE)
   if(verbose)
      message("ok")
   ex
}

getPanelFnType <- function(panelEx) {
   panelFnType <- NULL
   if(is.null(panelEx)) {
      panelFnType <- "rplotFn"
   } else if(inherits(panelEx, "trellis")) {
      panelFnType <- "trellisFn"
   } else if(inherits(panelEx, "ggplot")) {
      panelFnType <- "ggplotFn"
   } else if(inherits(panelEx, "ggvis")) {
      panelFnType <- "ggvisFn"
   } else if(inherits(panelEx, "rCharts")) {
      panelFnType <- "rChartsFn"
   }
   if(is.null(panelFnType))
      stop("Unsupported panel function", call. = FALSE)
   
   panelFnType
}

validateLims <- function(lims, data, panelFn, verbose) {
   # if the user specified limits, no need to compute lims
   # otherwise, we need to call prepanel on the data
   if(is.null(lims)) {
      if(verbose)
         message("* Limits not supplied.  Applying panelFn as is.")
   } else if(!inherits(lims, "trsLims")) {
      if(verbose)
         message("* Precomputed limits not supplied.  Computing axis limits...")

      # should have x, y, prepanelFn
      xLimType <- lims$x
      yLimType <- lims$y
      if(is.null(xLimType))
         xLimType <- "free"
      if(is.null(yLimType))
         yLimType <- "free"

      # lims <- list(x = list(type = "free"), y = list(type = "free"))

      # if both are free, we don't need to do anything
      if(!(xLimType == "free" && yLimType == "free")) {
         # TODO: checking to make sure things are specified correctly
         # TODO: handle dx and dy
         if(is.null(lims[["prepanelFn"]])) { # cannot do $prepanelFn because of prepanelFnIsTrellis (they both start the same)
            if(inherits(panelFn, c("trellisFn", "ggplotFn"))) {
               prepanelFn <- panelFn
            } else {
               stop("'lims' argument does not specify a prepanel function.  This could be ignored if panelFn returns an object of class 'trellis' or 'ggplot', which can be used to determine axis limits.")
            }
         } else {
            prepanelFn <- lims[["prepanelFn"]]
         }
         pre <- prepanel(data, prepanelFn = prepanelFn)
         lims <- setLims(pre, x = xLimType, y = yLimType)
      } else {
         if(verbose)
            message("* ... skipping this step since both axes are free ...")
         lims <- list(x = list(type = "free"), y = list(type = "free"))
      }
   }
   lims
}

## internal
checkDisplayPath <- function(displayPrefix, verbose = TRUE) {
   if(file.exists(displayPrefix)) {
      bakFile <- paste(displayPrefix, "_bak", sep = "")
      message(paste("* Display exists... backing up previous to", bakFile))
      if(file.exists(bakFile)) {
         message("* Removing previous backup plot directory")
         unlink(bakFile, recursive = TRUE)
      }
      file.rename(displayPrefix, bakFile)
   }
   dir.create(displayPrefix, recursive = TRUE)
}

## internal
updateDisplayList <- function(argList, conn) {

   displayListPath <- file.path(conn$path, "displays", "_displayList.Rdata")

   if(!file.exists(displayListPath)) {
      displayList <- list()
   } else {
      load(displayListPath)
   }

   # make sure all other displays still exist
   gps <- do.call(c, lapply(displayList, function(x) x$group))
   nms <- do.call(c, lapply(displayList, function(x) x$name))
   existsInd <- file.exists(
      file.path(conn$path, "displays", gps, nms)
   )
   displayList <- displayList[existsInd]

   displayListNames <- c("uid", "Group", "Name", "Description", "Panels", "Pre-rendered", "Data Class", "Cog Class", "Height (px)", "Width (px)", "Resolution", "Aspect Ratio", "Last Updated", "Key Signature")

   if(!is.null(argList))
      displayList[[paste(argList$group, argList$name, sep = "_")]] <- argList

   displayListDF <- do.call(rbind, lapply(displayList, function(x) as.data.frame(x, stringsAsFactors = FALSE)))
   displayListDF <- displayListDF[order(displayListDF$group, displayListDF$name),]
   displayListDF <- data.frame(uid = seq_len(nrow(displayListDF)), displayListDF, stringsAsFactors = FALSE)

   save(displayList, displayListDF, displayListNames, file = displayListPath)
}

# creates low-resolution thumbnail
makeThumb <- function(inFile, outFile, height, width) {
   img <- png::readPNG(inFile)
   
   png(filename = outFile, height = height, width = width)
      par(mar = c(0,0,0,0), xaxs = "i", yaxs = "i", ann = FALSE)
      plot(1:2, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
      lim <- par()
      rasterImage(img, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
   dev.off()
}

# ## internal
# validateInputs <- function(input) {
#    if(inherits(input, "inputVars")) {
#       input <- list(input)
#    } else if(!all(sapply(input, function(x) inherits(x, "inputVars"))) && !is.null(input)) {
#       stop("cogInput must be either of class 'inputVars' or a list of objects of class 'inputVars'")
#    }
#    input
# }

# # if there is an aspect ratio and layout specified, then
# # set width or height accordingly to get rid of margins around plot
# # assumes this is the same for all objects in the list
# if(!p[[1]]$aspect.fill && !is.null(p[[1]]$layout) && p[[1]]$layout[1] != 0) {
#    if(is.null(width) && is.null(height)) {
#       width <- 7; height <- vdb_getHeight(p[[1]], width)
#    } else if(!is.null(width) && is.null(height)) {
#       height <- vdb_getHeight(p[[1]], width)
#    } else if(is.null(width) && !is.null(height)) {
#       width <- vdb_getWidth(p[[1]], height)
#    }
# }

#' base64 Encoding of a .png File
#'
#' @param plotLoc TODO
#'
#' @export
#' @importFrom base64enc base64encode
encodePNG <- function(plotLoc) {
   bytes <- file.info(plotLoc)$size
   b64 <- base64encode(readBin(plotLoc, "raw", n = bytes))
   paste("data:image/png;base64,", b64, sep = "")
}

