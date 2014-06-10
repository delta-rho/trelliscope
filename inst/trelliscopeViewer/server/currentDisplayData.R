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
         n_visible_cog = max(1, length(which(x$cdo$cogDesc$type == "splitVar"))),
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

visibleCogListOutputData <- function(x) {
   if(!is.null(x$cdo)) {
      cogDesc <- x$cdo$cogDesc
      cogDesc$active <- ""
      ind <- cogDesc$type == "splitVar"
      cogDesc$active[ind] <- "active"
      list(cog = cogDesc, cdName = x$cdo$name, cdGroup = x$cdo$group)
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
   if(!is.null(x$cdo)) {
      # TODO: apply sorting and filtering in filterState, etc.
      cogDF <- x$cdo$cogDatConn
      n <- cogNrow(cogDF)
      curDF <- getCogData(cogDF, seq_len(min(n, 10)))
      cogDesc <- x$cdo$cogDesc
      cogDesc$index <- seq_len(ncol(cogDF)) - 1
      cogDesc$active <- ""
      # cogDesc$active[cogDesc$name %in% names(curDF)] <- "active"
      cogDesc$active[-1] <- "active"
      plotDat <- lapply(names(x$cdo$cogInfo), function(nm) {
         getUnivarPlotDat(x$cdo, name = nm)         
      })
      
      dfInfo <- data.frame(
         name = c("panelKey", sapply(x$cdo$cogInfo, function(a) a$name)), 
         type = c("character", sapply(x$cdo$cogInfo, function(a) a$type)),
         stringsAsFactors = FALSE)
      
      list(
         dfInfo = dfInfo,
         data = cogTableBodyData(curDF),
         filter = cogTableFootFilter(cogDF),
         plotDat = plotDat,
         cogNames = cogDesc,
         cdName = x$cdo$name, cdGroup = x$cdo$group,
         cogTableInfo = paste("Showing entries 1 - 8 of", n),
         curPage = "1",
         nPages = ceiling(n / 8)
      )
   }
}

cogUniFilterControlsOutputData <- function(x) {
   if(!is.null(x$cdo)) {
      cd <- x$cdo$cogDesc
      cd$dataType[cd$dataType == "integer"] <- "numeric"
      list(cogs = subset(cd, type != "panelKey"), cdName = x$cdo$name, cdGroup = x$cdo$group)
   }
}

cogBiFilterControlsOutputData <- function(x) {
   if(!is.null(x$cdo)) {
      cd <- x$cdo$cogDesc
      cd$dataType[cd$dataType == "integer"] <- "numeric"
      list(cogs = subset(cd, dataType == "numeric"), cdName = x$cdo$name, cdGroup = x$cdo$group)
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


