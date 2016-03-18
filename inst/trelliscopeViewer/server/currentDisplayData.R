
displayListOutputData <- function(dl) {
  # logMsg("Populating display list")

  for(i in seq_along(dl)) {
    dl[[i]]$updated <- as.character(dl[[i]]$updated)
    dl[[i]]$thumb <- paste("<img src =\"", encodePNG(file.path(getOption("vdbConn")$path, "displays", dl[[i]]$group, dl[[i]]$name, "thumb_small.png")), "\" style=\"max-height: 60px; max-width: 80px\">", sep = "")
  }
  names(dl) <- NULL
  dl
}

## every time a new display is selected, these functions
## populate the data that initially fills out each control
## panel

# cdName and cdGroup are added to all outputs
# to make outputs unique when a display is changed
# (we want to trigger everything to change when a display is changed)

displayInformationOutputData <- function(x) {
  if(!is.null(x$cdo)) {
    ds <- x$cdo$panelDataSource

    attrs <- list()
    attrs$name <- list(name = "Group/Name", val = paste(x$cdo$group, "/", x$cdo$name))
    if(!is.null(x$cdo$desc))
      attrs$desc <- list(name = "Description", val = x$cdo$desc)
    attrs$n <- list(name = "Number of panels", val = x$cdo$n)
    attrs$up <- list(name = "Last updated", val = x$cdo$updated)
    attrs$conn <- list(name = "Data source type",
      val = capture.output(print(datadr::getAttribute(ds, "conn"))))

    cogs <- x$cdo$cogInfo[,c("name", "desc")]

    if(inherits(ds, "ddf")) {
      sbst <- capture.output(print(ds[[1]]))
      ddo_ddf <- "distributed data frame (ddf)"
    } else {
      sbst <- capture.output(str(ds[[1]]))
      ddo_ddf <- "distributed data object (ddo)"
    }
    sbst <- paste(sbst, collapse = "\n")
    panelFn <- x$cdo$panelFn
    class(panelFn) <- NULL
    code <- paste(capture.output(dump("panelFn", ""))[-1], collapse="\n")

    panel_pre <- "this function takes the value of each subset key-value pair as its argument"
    if(length(formals(panelFn)) > 1)
      panel_pre <- "this function takes the key and value of each subset key-value pair as its arguments"

    md_desc <- x$cdo$mdDesc
    if(is.null(md_desc))
      md_desc <- ""

    return(list(md_desc = md_desc, attrs = unname(attrs), cogs = cogs, ddo_ddf = ddo_ddf, subset = sbst, panel = code, panel_pre = panel_pre))
  }
}

panelLayoutOutputData <- function(x) {
  if(!is.null(x$cdo)) {
    panelLayout <- x$cdo$state$layout
    nrow <- panelLayout$nrow
    if(is.null(nrow))
      nrow <- 1
    ncol <- panelLayout$ncol
    if(is.null(ncol))
      ncol <- 1

    labelState <- x$cdo$state$labels
    if(is.null(labelState)) {
      nLabels <- length(which(x$cdo$cogInfo$defLabel))
    } else {
      nLabels <- length(labelState)
    }
    list(panel_aspect = x$cdo$height / x$cdo$width,
      n_panel_labels = nLabels,
      nrow = nrow, ncol = ncol, cdName = x$cdo$name, cdGroup = x$cdo$group)
  }
}

# panelFunctionOutputData <- function(x) {
#   if(!is.null(x$cdo)) {
#     panelFn <- x$cdo$panelFn
#     list(code = paste(capture.output(dump("panelFn", "")), collapse="\n"),
#       cdName = x$cdo$name, cdGroup = x$cdo$group)
#   }
# }

panelLabelListOutputData <- function(x) {
  if(!is.null(x$cdo)) {
    cogInfo <- x$cdo$cogInfo
    cogInfo$active <- ""
    cogInfo$active[cogInfo$name %in% x$cdo$state$labels] <- "active"

    ci <- split(cogInfo, cogInfo$group)
    nms <- names(ci)
    # panelKey, condVar, and bsv go first, then sorted
    newNms <- intersect(c("panelKey", "condVar", "bsv"), nms)
    newNms <- c(newNms, setdiff(nms, newNms))
    ci <- lapply(newNms, function(a) {
      list(
        groupName = a,
        data = ci[[a]][, c("name", "desc", "active")]
      )
    })
    list(cog = ci, cdName = x$cdo$name, cdGroup = x$cdo$group)
  }
}

activeCogListOutputData <- function(x) {
  if(!is.null(x$cdo)) {
    cogInfo <- x$cdo$cogInfo
    cogInfo$active <- ""
    cogInfo$active[cogInfo$defActive] <- "active"
    cogInfo$selectable <- "selectable"
    cogInfo$selectable[cogInfo$name == "panelKey"] <- ""

    ci <- split(cogInfo, cogInfo$group)
    nms <- names(ci)
    # panelKey, condVar, and bsv go first, then sorted
    newNms <- intersect(c("panelKey", "condVar", "bsv"), nms)
    newNms <- c(newNms, setdiff(nms, newNms))
    ci <- lapply(newNms, function(a) {
      list(
        groupName = a,
        data = ci[[a]][, c("name", "desc", "active", "selectable")]
      )
    })
    list(cog = ci, cdName = x$cdo$name, cdGroup = x$cdo$group)
  }
}

relatedDisplayListOutputData <- function(x, displayList) {
  if(!is.null(x$cdo)) {
    curKeySig <- x$cdo$keySig
    curName <- x$cdo$name
    curGroup <- x$cdo$group

    # TODO: better logic about what "related" means
    relDisp <- sapply(displayList, function(a)
      !(a$name == curName && a$group == curGroup) && a$keySig == curKeySig)

    res <- lapply(displayList[relDisp], function(x) {
      x$updated <- as.character(x$updated)
      img <- paste("<img src =\"", encodePNG(file.path(getOption("vdbConn")$path, "displays", x$group, x$name, "thumb.png")), "\" style=\"max-height: 60px; max-width: 80px\">", sep = "")
      x$thumb <- img
      x$aspect <- x$height / x$width
      x
    })

    names(res) <- NULL
    list(displays = res, cdName = x$cdo$name, cdGroup = x$cdo$group)
  }
}

cogTableControlsOutputData <- function(x) {
  # render the entire cognostics table (all possible cognostics)
  # then we will dynamically change which are shown with different
  # choices of active cogs and selected columns

  if(!is.null(x$cdo)) {
    # TODO: apply sorting and filtering in filterState, etc.
    ac <- x$cdo$state$activeCog
    cogInfo <- x$cdo$cogInfo
    # cogInfo <- subset(cogInfo, name %in% ac)
    cogInfo$index <- seq_len(nrow(cogInfo)) - 1
    cogInfo$active <- ""
    cogInfo$active[cogInfo$filterable] <- "active"
    # cogInfo$hidden <- "hidden"
    # cogInfo$hidden[cogInfo$name %in% ac] <- ""

    cogDF <- x$cdo$cogDatConn
    n <- cogNrow(cogDF)
    curDF <- getCogData(cogDF, seq_len(min(n, 10)))

    plotDat <- lapply(cogInfo$name, function(nm) {
      getUnivarPlotDat(x, name = nm, maxLevels = 100, calledFromFooter = TRUE)
    })
    list(
      data = cogTableBodyData(curDF),
      filter = cogTableFootFilter(cogDF),
      plotDat = plotDat,
      cogInfo = cogInfo,
      cdName = x$cdo$name, cdGroup = x$cdo$group,
      cogTableInfo = paste("Showing entries 1 - 8 of", n),
      curPage = "1",
      nPages = ceiling(n / 8)
    )
  }
}

cogUniFilterControlsOutputData <- function(x) {
  if(!is.null(x$cdo)) {
    cd <- x$cdo$cogInfo
    cd$type[cd$type == "integer"] <- "numeric"
    # list(cogs = subset(cd, filterable), cdName = x$cdo$name, cdGroup = x$cdo$group)
    list(cogs = subset(cd, type == "numeric"), cdName = x$cdo$name, cdGroup = x$cdo$group)
  }
}

cogBiFilterControlsOutputData <- function(x) {
  if(!is.null(x$cdo)) {
    cd <- x$cdo$cogInfo
    cd$type[cd$type == "integer"] <- "numeric"
    list(cogs = subset(cd, type == "numeric"), cdName = x$cdo$name, cdGroup = x$cdo$group)
  }
}

panelPageNavOutputData <- function(x) {
  if(!is.null(x$cdo)) {
    nrow <- x$cdo$state$layout$nrow
    ncol <- x$cdo$state$layout$ncol
    if(is.null(nrow))
      nrow <- 1
    if(is.null(ncol))
      ncol <- 1

    ppp <- nrow * ncol

    goodIncrements <- c(1, 2, 5, 10, 25, 50, 100, 500, 1000, 10000)
    n <- cogNrow(x$cdo$cogDatConn)
    skips <- goodIncrements[goodIncrements < n / 2]

    list(n = ceiling(n / ppp), skips = skips, cdName = x$cdo$name, cdGroup = x$cdo$group)
  }
}


