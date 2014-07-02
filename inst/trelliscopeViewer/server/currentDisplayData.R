
displayListOutputData <- function(displayList) {
   tmp <- lapply(displayList, function(x) {
      x$updated <- as.character(x$updated)
      img <- paste("<img src =\"", encodePNG(file.path(options()$vdbConn$path, "displays", x$group, x$name, "thumb.png")), "\" style=\"height: 60px\">", sep = "")
      x$thumb <- img
      x
   })
   names(tmp) <- NULL
   tmp
}

## every time a new display is selected, these functions
## populate the data that initially fills out each control
## panel


# cdName and cdGroup are added to all outputs
# to make outputs unique when a display is changed
# (we want to trigger everything to change when a display is changed)

panelLayoutOutputData <- function(x) {
   if(!is.null(x$cdo)) {
      panelLayout <- x$cdo$state$panelLayout
      nrow <- panelLayout$nrow
      if(is.null(nrow))
         nrow <- 1
      ncol <- panelLayout$ncol
      if(is.null(ncol))
         ncol <- 1
      
      list(panel_aspect = x$cdo$height / x$cdo$width, 
         n_panel_labels = length(which(x$cdo$cogInfo$defLabel)),
         nrow = nrow, ncol = ncol, cdName = x$cdo$name, cdGroup = x$cdo$group)
   }
}

panelFunctionOutputData <- function(x) {
   if(!is.null(x$cdo)) {
      panelFn <- x$cdo$panelFn
      list(code = paste(capture.output(dump("panelFn", "")), collapse="\n"), 
         cdName = x$cdo$name, cdGroup = x$cdo$group)
   }
}

panelLabelListOutputData <- function(x) {
   if(!is.null(x$cdo)) {
      cogInfo <- x$cdo$cogInfo
      cogInfo$active <- ""
      cogInfo$active[cogInfo$defLabel] <- "active"
      
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
      
      relDisp <- sapply(displayList, function(a)
         !(a$name == curName && a$group == curGroup) && a$keySig == curKeySig)
      
      res <- lapply(displayList[relDisp], function(x) {
         x$updated <- as.character(x$updated)
         img <- paste("<img src =\"", encodePNG(file.path(options()$vdbConn$path, "displays", x$group, x$name, "thumb.png")), "\" style=\"height: 60px\">", sep = "")
         x$thumb <- img
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
         getUnivarPlotDat(x$cdo, name = nm, maxLevels = 100)
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
      list(cogs = subset(cd, filterable), cdName = x$cdo$name, cdGroup = x$cdo$group)
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
      nrow <- x$cdo$state$panelLayout$nrow
      ncol <- x$cdo$state$panelLayout$ncol
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


