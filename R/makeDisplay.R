if(getRversion() >= "2.15.1") {
  utils::globalVariables(c("collect"))
}


#' Create a Trelliscope Display
#'
#' Create a trelliscope display and add it to a visualization database (VDB)
#'
#' @param data data of class "ddo" or "ddf" (see \code{\link{ddo}}, \code{\link{ddf}})
#' @param name the name of the display (no special characters, spaces are converted to underscores)
#' @param group the group the display belongs to, where displays are organized into groups (no special characters, spaces are
#' converted to underscores).  Defaults to "common"
#' @param desc a description of the display (used in the viewer and in notebooks)
#' @param mdDesc an optional longer-form description of the display and data, which can be text or can be a path to a markdown file or file with html snippets.  The description will appear in the "Display Information" panel in the Trelliscope viewer.
#' @param height reference dimensions (in pixels) for each panel (panels will be resized based on available space in the viewer)
#' @param width reference dimensions (in pixels) for each panel (panels will be resized based on available space in the viewer)
#' @param panelFn a function that produces a plot and takes one argument, which will be the current split of the data being passed to it.  It is recommended that you first test \code{panelFn} on a single key-value pair using \code{panelFn(data[[1]][[2]])}. This function must return either an object of class "ggplot", "trellis", or return "NULL" (for base plot commands)
#' @param lims either an object of class "trsLims" as obtained from \code{\link{setLims}} or a list with elements x, y, and prepanelFn, that specify how to apply \code{\link{prepanel}} and \code{\link{setLims}}
#' @param cogFn a function that returns a named list, where each element of the list is a cognostic feature (with length 1). This list must be coerceable to a 1-row data frame. The function should take one argument, which will be the current split of the data being passed to it.  Useful to test with \code{cogFn(divExample(dat))}
#' @param state if specified, this tells the viewer the default parameter settings (such as layout, sorting, filtering, etc.) to use when the display is viewed (see \code{\link{validateState}} for details)
#' @param preRender should the panels be pre-rendered and stored (\code{TRUE}), or rendered on-the-fly (\code{FALSE}, default)?  Default is recommended unless rendering is very expensive.  See Details.
#' @param cogConn a connection to store the cognostics data.  By default, this is \code{\link{dfCogConn}()}.
#' @param output how to store the panels and metadata for the display (unnecessary to specify in most cases -- see details)
#' @param conn VDB connection info, typically stored in options("vdbConn") at the beginning of a session, and not necessary to specify here if a valid "vdbConn" object exists
#' @param verbose print status messages?
#' @param keySig a user-defined key signature (string - see details)
#' @param params a named list of objects external to the input data that are needed in the distributed computing (most should be taken care of automatically such that this is rarely necessary to specify)
#' @param packages a vector of R package names that contain functions used in \code{panelFn} or \code{cogFn} (most should be taken care of automatically such that this is rarely necessary to specify)
#' @param control parameters specifying how the backend should handle things (most-likely parameters to \code{rhwatch} in RHIPE) - see \code{\link[datadr]{rhipeControl}} and \code{\link[datadr]{localDiskControl}}
#'
#' @details Many of the parameters are optional or have defaults.  For several examples, see the documentation at tessera.io: \url{http://tessera.io/docs-trelliscope}
#'
#' Panels by default are not pre-rendered. Instead, this function creates a display object and computes and stores the cognostics.  Panels are then rendered on the fly by the Tessera backend and pushed to the Trelliscope viewer as html with the panel images embedded in the html.  If a user would like to pre-render the images for every subset (using \code{preRender = TRUE}), then by default the image files for the panels will be stored to a local disk connection (see \code{\link{localDiskConn}}) inside the VDB directory, organized in subdirectories by group and name of the display.  Optionally, the user can specify the \code{output} parameter to be any valid "kvConnection" object, as long as it is one that persists on disk (e.g. \code{\link{hdfsConn}}).
#'
#' \code{keySig} does not generally need to be specified.  It is useful to specify when creating multiple displays that you would like to be treated as related displays, so that you can view them side by side.  Two displays are determined to be related when their key signatures, typically computed as a md5 hash of the complete collection of keys, match.  Sometimes two displays will have data where the keys match for a significant portion of subsets, but not all.  Manually specifying the same \code{keySig} for each can ensure that they will be treated as related displays.
#'
#' @author Ryan Hafen
#'
#' @seealso \code{\link{prepanel}}, \code{\link{setLims}}, \code{\link{divide}}
#'
#' @examples
#' # see docs
#'
#' @export
#' @importFrom digest digest
#' @importFrom data.table rbindlist
makeDisplay <- function(
  data,
  name,
  group = "common",
  desc = "",
  mdDesc = NULL,
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
  keySig = NULL,
  params = NULL,
  packages = NULL,
  control = NULL
) {
  validateVdbConn(conn)

  validateNameGroup(name, group)

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

  if(!is.null(mdDesc)) {
    if(grepl("\\.md$|\\.html$", mdDesc)) {
      if(file.exists(mdDesc)) {
        mdDesc <- paste(readLines(mdDesc), collapse = "\n")
      } else {
        warning("Could not find file ", mdDesc)
        mdDesc <- ""
      }
    }
  }

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
  panelEx <- kvApply(kvExample(data), panelFn)$value

  cogEx <- validateCogFn(data, cogFn, verbose)

  if(is.null(desc) || is.na(desc))
    desc <- "(no description)"

  panelFnType <- getPanelFnType(panelEx)
  class(panelFn) <- c("function", panelFnType)
  lims <- validateLims(lims, data, panelFn, params, packages, verbose)

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
    cogEmit(cogConn, cogRes, collect, conn, group, name)
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
    dataConn   = dataConn,
    group     = group,
    name      = name,
    preRender  = preRender,
    cogConn    = cogConn,
    panelFn    = panelFn,
    cogFn     = cogFn,
    lims      = lims,
    conn      = conn,
    height    = height,
    width     = width
  )

  panelGlobals <- drGetGlobals(panelFn)
  cogGlobals <- drGetGlobals(cogFn)

  packages <- c(packages, "trelliscope")
  packages <- unique(c(packages, panelGlobals$packages, cogGlobals$packages))

  globalVarList <- c(panelGlobals$vars, cogGlobals$vars)

  if(length(params) > 0)
    for(pnm in names(params))
      globalVarList[[pnm]] <- params[[pnm]]

  parList <- c(parList, globalVarList)
  nms <- names(parList)
  parList <- parList[which(!duplicated(nms))]

  jobRes <- mrExec(data,
    map    = map,
    reduce  = reduce,
    output  = output,
    control  = control,
    params  = parList,
    packages = packages
  )

  tryRes <- try({
  if(preRender)
    panelDataSource <- jobRes

  # read in cognostics
  cogDatConn <- cogFinal(cogConn, jobRes, conn, group, name, cogEx)

  # get panelKey "signature"
  if(!is.null(keySig)) {
    if(!is.character(keySig)) {
      message("User-defined 'keySig' is not a string - converting it to one...")
      keySig <- digest(keySig)
    }
  } else {
    keySig <- digest(jobRes[["TRS___panelkey"]][[2]])
  }

  modTime <- Sys.time()

  if(verbose)
    message("* Storing display object...")

  cogInfo <- getCogInfo(cogEx)
  cogInfo <- getCogInfo(cogDatConn)
  # if type couldn't be inferred then get it from the whole cog data frame
  ind <- which(is.na(cogInfo$type))
  for(ii in ind)
    cogInfo$type[ii] <- inferCogType(cogDatConn[[ii]])

  cogDistns <- getCogDistns(cogDatConn, cogInfo)

  # some back ends (like RHIPE) might need to store environment variables
  # so that R knows how to talk to the back end
  envs <- NULL

  if(inherits(panelDataSource, "kvHDFS")) {
    envs <- Sys.getenv()
    envs <- as.list(envs[grepl("HADOOP", names(envs))])
  }

  displayObj <- list(
    name = name,
    group = group,
    desc = desc,
    mdDesc = mdDesc,
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
    relatedPackages = packages,
    envs = envs
  )
  class(displayObj) <- "displayObj"

  if(!is.null(state))
    displayObj$state <- validateState(state, name, group, displayObj)

  save(displayObj, file = file.path(tempPrefix, "displayObj.Rdata"))

  # make thumbnail
  message("* Plotting thumbnail...")
  if(inherits(panelEx, "htmlwidget"))
    widgetThumbnail(panelEx, file.path(tempPrefix, "thumb.png"))
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

    # Move newly created vdb files to the display folder
    copyVerify <- file.copy(list.files(tempPrefix, full.names = TRUE), file.path(displayPrefix, list.files(tempPrefix)))

    if(!all(copyVerify)) {
      stop("Files needed for building trelliscope display were not correctly moved to '", displayPrefix, "'", call. = FALSE)
    }

    # Remove the temporary directory that contained the vdb objects
    if(unlink(tempPrefix, recursive = TRUE)) {
      warning("Temporary directory '", tempPrefix, "'\ncontaining trelliscope vdb objects was not removed successfully", call. = FALSE)
    }

  }

  return(invisible(displayObj))
}

updateDisplay <- function(name, group = NULL, conn = getOption("vdbConn"), ...) {
  args <- list(...)
  nms <- names(args)

  updateable <- c("panelFn", "desc", "state", "width", "height", "keySig")

  notup <- setdiff(nms, updateable)
  if(length(notup) > 0) {
    message("note: the following attributes cannot be used to update a display and will be ignored: ", paste(notup, collapse = ", "))
  }

  disp <- getDisplay(name, group, conn)

  noPreRend <- c("panelFn", "width", "height")
  if(disp$preRender) {
    if(nms %in% noPreRend)
      message("note: preRender is TRUE, so the following cannot be set: ",
        paste(noPreRend, collapse = ", "))
    nms <- setdiff(nms, noPreRend)
  }

  for(cur in c("desc", "width", "height", "keySig")) {
    if(cur %in% nms)
      disp[[cur]] <- args[[cur]]
  }

  if("panelFn" %in% nms) {
    # panelGlobals <- drGetGlobals(args$panelFn)
    # cogGlobals <- drGetGlobals(cogFn)
    # packages <- unique(c(packages, panelGlobals$packages, cogGlobals$packages))
    # globalVarList <- c(panelGlobals$vars, cogGlobals$vars)
  }

  displayObj <- disp
  # save(displayObj, file = file.path(tempPrefix, "displayObj.Rdata"))

  updateDisplayList(list(
    group = disp$group,
    name = disp$name,
    desc = disp$desc,
    n = disp$n,
    panelFnType = disp$panelFnType,
    preRender = disp$preRender,
    dataClass = tail(class(disp$panelDataSource), 1),
    cogClass = class(disp$cogDatConn)[1],
    height = disp$height,
    width = disp$width,
    updated = Sys.time(),
    keySig = disp$keySig
  ), conn)
}



## remove all _bak directories
cleanupDisplays <- function(conn = getOption("vdbConn")) {
  validateVdbConn(conn)

  ff <- list.files(file.path(conn$path, "displays"), recursive = TRUE, include.dirs = TRUE, pattern = "_bak$", full.names = TRUE)
  for(f in ff) {
    unlink(f, recursive = TRUE)
  }
}


