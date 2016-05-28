
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
validateNameGroup <- function(name, group) {

  # If spaces are present in name or group, fill them with underscores
  name <- gsub("\\ ", "_", name)
  group <- gsub("\\ ", "_", group)

  # check name and group
  if(grepl("[^a-zA-Z0-9_\\.]", name)) {
    stop("Argument 'name' must contain only numbers, letters, spaces, or the symbols '.' or '_'")
  }
  if(grepl("[^a-zA-Z0-9_/\\.]", group)) {
    stop("Argument 'group' must contain only numbers, letters, spaces, or the symbols '.', '_', or '/'")
  }

  # Send back the updated name and group into the calling function
  assign("name", name, envir = parent.frame())
  assign("group", group, envir = parent.frame())

}

## internal
validateCogFn <- function(dat, cogFn, verbose = FALSE) {

  if(verbose)
    message("* Testing cognostics function on a subset ... ", appendLF = FALSE)

  ex <- applyCogFn(cogFn, datadr::kvExample(dat),
    datadr::getAttribute(dat, "conn"))

  # if(!is.list(ex))
  #   stop("cogFn should return a list")
  # if(!all(sapply(ex, function(x) inherits(x, "cog"))))
  #   stop("Each cognostic must have class 'cog' - please make sure you are specifying: var = cog(...)")

  exdf <- cog2df(ex)

  if(verbose)
    message("ok")

  return(ex)

}

getPanelFnType <- function(panelEx) {
  panelFnType <- NULL
  if(is.null(panelEx)) {
    panelFnType <- "rplotFn"
  } else if(inherits(panelEx, "trellis")) {
    panelFnType <- "trellisFn"
  } else if(inherits(panelEx, "ggplot")) {
    panelFnType <- "ggplotFn"
  } else if(inherits(panelEx, "htmlwidget")) {
    panelFnType <- "htmlwidgetFn"
  } else if(inherits(panelEx, "base64png")) {
    panelFnType <- "base64pngFn"
  }
  if(is.null(panelFnType))
    stop("Unsupported panel function.  If panel function uses base R commands, be sure to include 'return(NULL)' at the end of the function definition.", call. = FALSE)

  panelFnType
}

validateLims <- function(lims, data, panelFn, params = NULL, packages = NULL, verbose) {
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
      pre <- prepanel(data, prepanelFn = prepanelFn, params = params, packages = packages)
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

    copyVerify <- copy_dir(displayPrefix, bakFile)

    # Verify the file renaming
    if(!copyVerify) {
      warning("Backup files for display were not successfully moved to '", bakFile, "'", call. = FALSE)
    } else {
      unlink(displayPrefix, recursive = TRUE)
    }
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
#' @importFrom png readPNG
#' @importFrom jpeg readJPEG
makeThumb <- function(inFile, outFile, height, width) {
  img <- try(png::readPNG(inFile), silent = TRUE)
  if(inherits(img, "try-error"))
    img <- try(jpeg::readJPEG(inFile), silent = TRUE)
  if(inherits(img, "try-error")) {
    file.copy(inFile, outFile)
  }

  grDevices::png(filename = outFile, height = height, width = width)
    graphics::par(mar = c(0,0,0,0), xaxs = "i", yaxs = "i", ann = FALSE)
    plot(1:2, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
    lim <- graphics::par()
    graphics::rasterImage(img, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
  grDevices::dev.off()

  if(!file.exists(outFile))
    file.copy(system.file("thumb_small.png", package = "trelliscope"), outFile)
}

# ## internal
# validateInputs <- function(input) {
#   if(inherits(input, "inputVars")) {
#     input <- list(input)
#   } else if(!all(sapply(input, function(x) inherits(x, "inputVars"))) && !is.null(input)) {
#     stop("cogInput must be either of class 'inputVars' or a list of objects of class 'inputVars'")
#   }
#   input
# }

# # if there is an aspect ratio and layout specified, then
# # set width or height accordingly to get rid of margins around plot
# # assumes this is the same for all objects in the list
# if(!p[[1]]$aspect.fill && !is.null(p[[1]]$layout) && p[[1]]$layout[1] != 0) {
#   if(is.null(width) && is.null(height)) {
#     width <- 7; height <- vdb_getHeight(p[[1]], width)
#   } else if(!is.null(width) && is.null(height)) {
#     height <- vdb_getHeight(p[[1]], width)
#   } else if(is.null(width) && !is.null(height)) {
#     width <- vdb_getWidth(p[[1]], height)
#   }
# }

#' base64 Encoding of a .png File
#'
#' @param plotLoc location of a png file on disk to encode as a base64 string
#'
#' @examples
#' f <- tempfile(fileext = ".png")
#' png(f)
#' plot(1:10)
#' dev.off()
#' encodePNG(f)
#' @export
#' @importFrom base64enc base64encode
encodePNG <- function(plotLoc) {
  bytes <- file.info(plotLoc)$size
  b64 <- base64encode(readBin(plotLoc, "raw", n = bytes))
  paste("data:image/png;base64,", b64, sep = "")
}

# this seems to be the best cross-platform way to copy directories
# if to exists it will be deleted
copy_dir <- function(from, to) {
  if(file.exists(to))
    unlink(to, recursive = TRUE)
  ff <- list.files(from, recursive = TRUE, full.names = TRUE)
  ff2 <- file.path(to, list.files(from, recursive = TRUE))
  upath <- unique(dirname(ff2))
  for(up in upath)
    suppressWarnings(dir.create(up, recursive = TRUE))
  all(file.copy(ff, ff2, overwrite = TRUE))
}

