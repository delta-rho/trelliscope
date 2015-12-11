if(getRversion() >= "2.15.1") {
  utils::globalVariables(c("displayList", "displayListDF", "displayListNames"))
}



#' S3method print displayObj
#' @param x TODO
#' @param ... TODO
print.displayObj <- function(x, ...) {
  cat("display object...\n")
}

#' Retrieve Display Object from VDB
#'
#' Retrieve a display object from a VDB.
#'
#' @param name the name of the display
#' @param group the group of the display
#' @param conn VDB connection info, typically stored in options("vdbConn") at the beginning of a session, and not necessary to specify here if a valid "vdbConn" object exists
#'
#' @details If a display is uniquely determined by its name, then group is not required.
#'
#' @return a display object
#' @author Ryan Hafen
#'
#' @seealso \code{\link{makeDisplay}}, \code{\link{removeDisplay}}
#' @export
getDisplay <- function(name, group = NULL, conn = getOption("vdbConn")) {

  validateVdbConn(conn, mustHaveDisplays = TRUE)

  load(file.path(conn$path, "displays", "_displayList.Rdata"))

  displayInfo <- findDisplay(name = name, group = group, conn = conn)
  if(is.null(displayInfo))
    return(NULL)
  vdbPrefix <- conn$path

  load(file.path(vdbPrefix, "displays", displayInfo$group, displayInfo$name, "displayObj.Rdata"))

  # if it is a local disk connection, the location can change
  # this happens when we move things to a web server
  if(inherits(displayObj$panelDataSource, "kvLocalDisk")) {
    cn <- datadr::getAttribute(displayObj$panelDataSource, "conn")
    if(!file.exists(cn$loc)) {
      tmp <- file.path(conn$path, "displays", displayObj$group, displayObj$name, "panels")
      if(!file.exists(tmp))
        tmp <- file.path(conn$path, "data", basename(cn$loc))
      if(file.exists(tmp)) {
        if(inherits(displayObj$panelDataSource, "ddf")) {
          displayObj$panelDataSource <- datadr::ddf(datadr::localDiskConn(tmp,
            reset = TRUE, verbose = FALSE), verbose = FALSE)
        } else {
          displayObj$panelDataSource <- datadr::ddo(datadr::localDiskConn(tmp,
            reset = TRUE, verbose = FALSE), verbose = FALSE)
        }
      }
    }
  }

  displayObj
}

#' Remove a Display from a VDB
#'
#' Remove a display from a VDB.
#'
#' @param name the name of the display
#' @param group the group of the display
#' @param conn VDB connection info, typically stored in options("vdbConn") at the beginning of a session, and not necessary to specify here if a valid "vdbConn" object exists
#' @param autoYes should questions to proceed with display removal be automatically answered with "yes"?
#' @param verbose logical - print messages about what is being done
#'
#' @details If a display is uniquely determined by its name, then group is not required.
#'
#' @author Ryan Hafen
#'
#' @seealso \code{\link{makeDisplay}}
#' @export
removeDisplay <- function(name = NULL, group = NULL, conn = getOption("vdbConn"), autoYes = FALSE, verbose = TRUE) {

  validateVdbConn(conn, mustHaveDisplays = TRUE)

  load(file.path(conn$path, "displays", "_displayList.Rdata"))

  displayInfo <- findDisplay(name, group, conn)
  if(is.null(displayInfo))
    return(invisible(NULL))
  vdbPrefix <- conn$path

  fileLoc <- file.path(vdbPrefix, "displays", displayInfo$group, displayInfo$name)

  if(file.exists(fileLoc)) {
    if(autoYes) {
      ans <- "y"
    } else {
      ans <- readline(paste("Are you sure you want to remove ", fileLoc, "? (y = yes) ", sep = ""))
    }
    if(!tolower(substr(ans, 1, 1)) == "y")
      return()
    unlink(fileLoc, recursive = TRUE)
  } else {
    stop("Files associated with display not found: ", fileLoc, call. = FALSE)
  }

  displayList[paste(displayInfo$group, displayInfo$name, sep = "_")] <- NULL

  ind <- which(
    displayListDF$name == displayInfo$name
      & displayListDF$group == displayInfo$group)

  displayListDF <- displayListDF[-ind,]

  # if there are no displays, remove the file
  if(length(displayList) == 0) {
    file.remove(file.path(conn$path, "displays", "_displayList.Rdata"))
  } else {
    save(displayList, displayListDF, displayListNames, file = file.path(conn$path, "displays", "_displayList.Rdata"))
  }

  if(verbose)
    message("* Display removed successfully")
}

## internal
## ensures that a display exists and returns its name and group
# @return a list with the name and group of the display, if found - otherwise NULL
findDisplay <- function(name, group = NULL, conn = getOption("vdbConn")) {
  load(file.path(conn$path, "displays", "_displayList.Rdata"))

  errStr <- ""
  if(is.null(group)) {
    curDisplay <- which(displayListDF$name == name)
  } else {
    curDisplay <- which(displayListDF$name == name & displayListDF$group == group)
    errStr <- paste(" from group \"", group, "\"", sep = "")
  }

  if(length(curDisplay) == 0) {
    message("The display \"", name, "\"", errStr, " wasn't found.", sep = "")
    return(NULL)
  } else if (length(curDisplay) > 1) {
    if(is.null(group)) {
      message("There is more than one display of name \"", name, "\".  Try specifying a group as well.", sep = "")
      return(NULL)
    } else {
      message("There is more than one display of name \"", name, "\" from group \"", group, "\".  This should not be possible")
      return(NULL)
    }
  } else {
    curDisplay <- displayListDF[curDisplay,]
    return(list(name = curDisplay$name, group = curDisplay$group))
  }
}

#' List Displays in a VDB
#'
#' List displays in a VDB.
#'
#' @param conn VDB connection info, typically stored in options("vdbConn") at the beginning of a session, and not necessary to specify here if a valid "vdbConn" object exists
#'
#' @author Ryan Hafen
#'
#' @seealso \code{\link{makeDisplay}}, \code{\link{removeDisplay}}, \code{\link{view}}
#' @export
listDisplays <- function(conn = getOption("vdbConn")) {
  validateVdbConn(conn, mustHaveDisplays = TRUE)

  load(file.path(conn$path, "displays", "_displayList.Rdata"))

  tmp <- as.matrix(displayListDF[,c("name", "group", "desc", "n", "dataClass")])
  rownames(tmp) <- NULL
  # tmp[,"updated"] <- substr(tmp[,"updated"], 1, 16)
  tmp[is.na(tmp[,"dataClass"]),"dataClass"] <- "none (R plot)"
  tmp <- tmp[order(tmp[,"group"], tmp[,"name"]),,drop = FALSE]

  nc <- ncol(tmp)
  sepWidth <- (nc - 1) * 3

  headers <- colnames(tmp)

  colWidths <- apply(tmp, 2, function(x) max(nchar(x)))
  colWidths <- pmax(colWidths, nchar(headers))

  totWidth <- getOption("width")

  excess <- (totWidth - sepWidth) - sum(colWidths)
  # (totWidth - sepWidth) - (sum(colWidths) - colWidths["desc"])
  if(excess < 0) {
    descCut <- excess + colWidths["desc"]
    if(descCut < 3) {
      tmp <- tmp[,which(colnames(tmp) != "desc")]
    } else {
      ell <- ifelse(tmp[,"desc"] == "", "", "...")
      tmp[,"desc"] <- paste(substr(tmp[,"desc"], 1, descCut - 3), ell, sep = "")
    }
  }
  headers <- colnames(tmp)
  colWidths <- apply(tmp, 2, function(x) max(nchar(x)))
  colWidths <- pmax(colWidths, nchar(headers))
  nc <- length(headers)

  fmtStr <- paste(paste("%", colWidths, "s", sep = ""), collapse = " | ")

  cat(paste(c(
    do.call(sprintf, c(list(fmt = fmtStr), as.list(headers))),
    paste(sapply(colWidths, function(x) paste(rep("-", x), collapse = "")), collapse = "-+-"),
    apply(tmp, 1, function(x) {
    do.call(sprintf, c(list(fmt = fmtStr), as.list(x)))
  })), collapse = "\n"))

}

#' Update a Display Object
#'
#' @param name the name of the display
#' @param group the group the display belongs to
#' @param conn VDB connection info, typically stored in options("vdbConn") at the beginning of a session, and not necessary to specify here if a valid "vdbConn" object exists
#' @param \ldots display parameters to update which must be one of "desc", "width", "height", "keySig", "panelFn", "state" - see \code{\link{makeDisplay}} for details on these parameters.
#'
#' @export
updateDisplay <- function(name, ..., group = "common", conn = getOption("vdbConn")) {

  load(file.path(conn$path, "displays", "_displayList.Rdata"))

  displayInfo <- findDisplay(name = name, group = group, conn = conn)
  if(is.null(displayInfo)) {
    message("No display to update")
    return(invisible(NULL))
    # message("Creating new display...")
    # makeDisplay(name = name, group = group, conn = conn, ...)
  } else {
    args <- list(...)
    nms <- names(args)

    updateable <- c("panelFn", "desc", "state", "width", "height", "keySig")

    notup <- setdiff(nms, updateable)
    if(length(notup) > 0) {
      message("note: the following attributes cannot be used to update a display and will be ignored: ", paste(notup, collapse = ", "))
    }

    disp <- getDisplay(name, group, conn)

    noPreRend <- c("panelFn", "width", "height", "lims")
    if(disp$preRender) {
      if(nms %in% noPreRend)
        message("note: preRender is TRUE, so the following cannot be set: ",
          paste(noPreRend, collapse = ", "))
      nms <- setdiff(nms, noPreRend)
    }

    for(cur in setdiff(updateable, c("panelFn", "state"))) {
      # TODO: validate each one
      if(cur %in% nms)
        disp[[cur]] <- args[[cur]]
    }

    if("state" %in% nms) {
      disp$state <- validateState(args$state, name, group, disp)
    }

    if("panelFn" %in% nms) {
      if(is.null(args$detectGlobals)) {
        getGlobals <- TRUE
      } else {
        getGlobals <- as.logical(args$detectGlobals)
      }
      if(getGlobals) {
        panelGlobals <- datadr::drGetGlobals(args$panelFn)
        disp$relatedPackages <- unique(c(args$packages,
          panelGlobals$packages, disp$relatedPackages))
        for(nm in names(c(panelGlobals$vars, args$params))) {
          disp$relatedData[[nm]] <- panelGlobals$vars[[nm]]
        }
      }
      if(!is.null(args$params) &&  inherits(args$params, "list"))
        environment(args$panelFn) <- list2env(args$params)
      panelEx <- datadr::kvApply(datadr::kvExample(disp$panelDataSource),
        args$panelFn)$value
      panelFnType <- getPanelFnType(panelEx)
      class(args$panelFn) <- c("function", panelFnType)
      disp$panelFn <- args$panelFn
      # TODO: should also update thumbnail
    }

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
    vdbPrefix <- conn$path

    displayObj <- disp
    save(displayObj, file = file.path(vdbPrefix, "displays",
      disp$group, disp$name, "displayObj.Rdata"))
  }
}

## remove all _bak directories
cleanupDisplays <- function(conn = getOption("vdbConn")) {
  validateVdbConn(conn)

  ff <- list.files(file.path(conn$path, "displays"), recursive = TRUE, include.dirs = TRUE, pattern = "_bak$", full.names = TRUE)
  for(f in ff) {
    unlink(f, recursive = TRUE)
  }
}

