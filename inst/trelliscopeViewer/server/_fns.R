renderDataLite <- function(expr, env = parent.frame(), quoted = FALSE, func = NULL) {
   if (!is.null(func)) {
      shinyDeprecated(msg = "renderTemplate: argument 'func' is deprecated. Please use 'expr' instead.")
   }
   else {
      installExprFunction(expr, "func", env, quoted)
   }
   function() {
      # jsonlite handles data frames the way we want
      res <- func()
      if(!is.null(res))
         return(jsonlite::toJSON(res))
   }
}

renderData <- function(expr, env = parent.frame(), quoted = FALSE, func = NULL) {
   if (!is.null(func)) {
      shinyDeprecated(msg = "renderTemplate: argument 'func' is deprecated. Please use 'expr' instead.")
   }
   else {
      installExprFunction(expr, "func", env, quoted)
   }
   function() {
      # jsonlite handles data frames the way we want
      res <- func()
      if(!is.null(res))
         return(RJSONIO::toJSON(res))
   }
}

cogDataString <- function(data) {
   nc <- ncol(data)
   matrix(sapply(seq_len(nc), function(i) {
      if(inherits(data[[i]], c("integer", "numeric"))) {
         val <- format(data[[i]], big.mark = ",") #, scientific = -3)
      } else {
         val <- as.character(data[[i]])
      }
   }), ncol = nc)
}

cogTableBodyData <- function(data, nr = 10) {
   nc <- ncol(data)
   if(nc == 0) {
      return(NULL)
   } else if(nrow(data) == 0) {
      tDataString <- matrix(nrow = nr, ncol = nc, data = "&nbsp;")
   } else {
      data <- data[1:min(nr, nrow(data)),, drop = FALSE]
      tDataString <- cogDataString(data)
      # if fewer then the number of rows, fill with blank <td>s
      # (so that the height is always fixed)
      if(nrow(tDataString) < nr) {
         rnr <- nrow(tDataString)
         nn <- nr - rnr
         tDataString <- rbind(tDataString, matrix(nrow = nn, ncol = nc))
         tDataString[(rnr + 1):nr, 1:nc] <- "&nbsp;"
      }
   }
   # res <- data.frame(tDataString, stringsAsFactors = FALSE)
   # names(res) <- names(data)
   # res
   tDataString
}

cogTableFootFilter <- function(data) {
   nms <- names(data)
   lapply(seq_along(data), function(i) {
      disabled <- ""
      if(inherits(data[[i]], c("numeric", "integer"))) {
         list(i = i, name = nms[i], numeric = TRUE)
      } else {
         if(nms[i] == "panelKey") {
            levels <- ""
            disabled <- "disabled"
         } else {
            # TODO: deal with very large number of levels
            levels <- sort(unique(as.character(data[[i]])))
            if(length(levels) > 100) {
               levels <- ""
               disabled <- "disabled"
            }
         }
         list(i = i, name = nms[i], levels = levels, disabled = disabled)
      }
   })
}

cogTableFootHist <- function(data) {
   rep("<div class='cog-univar-hist'></div>", ncol(data))
}

# creates data ready for univariate d3 plotting
# either bar chart for character or hist / quantile for numeric (specify with plotType)
getUnivarPlotDat <- function(cd, name, distType = "marginal", plotType = "hist", maxLevels = 100, calledFromFooter = FALSE) {
   if(plotType == "histogram")
      plotType <- "hist"
   if(plotType == "quantile")
      plotType <- "quant"
   
   curInfo <- cd$cdo$cogDistns[[name]]
   
   if(!is.na(curInfo$type)) {
      if(curInfo$type == "numeric") {
         if(distType == "marginal" || cogNrow(cd$cdo$cogDatConn) == nrow(cd$curCogDF)) {
            tmp <- curInfo$marginal[[plotType]]
         } else {
            tmp <- trelliscope:::getCogQuantPlotData(cd$curCogDF, name, plotType)
         }
         if(plotType == "hist") {
            delta <- diff(tmp$xdat[1:2])
            tmp$label <- paste("(", tmp$xdat, ",", tmp$xdat + delta, "]", sep = "")
            return(list(name = name, type = curInfo$type, data = tmp, plotType = plotType, id = ifelse(calledFromFooter, 1, rnorm(1))))
            # add random normal to make sure it triggers even when it 
            # doesn't change - this is to ensure spinner will get replaced
         } else {
            names(tmp)[1:2] <- c("x", "y")
            return(list(name = name, type = curInfo$type, data = tmp, plotType = plotType, id = ifelse(calledFromFooter, 1, rnorm(1))))
         }
      } else { # bar chart
         if(distType == "marginal") {
            tmp <- curInfo$marginal
         } else {
            tmp <- trelliscope:::getCogCatPlotData(cd$curCogDF, name, plotType)$freq
         }
         if(!is.null(tmp)) {
            if(nrow(tmp) > maxLevels)
               return(list(name = name, id = ifelse(calledFromFooter, 1, rnorm(1))))
            tmp <- rbind(tmp, data.frame(label = "", Freq = 0, stringsAsFactors = FALSE))
            names(tmp)[which(names(tmp) == "Freq")] <- "ydat"
            tmp$xdat <- seq_len(nrow(tmp))
            return(list(name = name, type = curInfo$type, data = tmp, plotType = "bar", id = ifelse(calledFromFooter, 1, rnorm(1))))
         }
      }
   }
   return(list(name = name))
}

getCogScatterPlotData <- function(x, ...)
   UseMethod("getCogScatterPlotData", x)

getCogHexbinPlotData <- function(x, ...)
   UseMethod("getCogHexbinPlotData", x)

getCogScatterPlotData.data.frame <- function(cogDF, xVar, yVar) {
   idx <- complete.cases(cogDF[,c(xVar, yVar)])
   list(
      data = data.frame(x = cogDF[idx,xVar], y = cogDF[idx,yVar]),
      plotType = "scatter",
      xlab = xVar,
      ylab = yVar
   )
}

getCogHexbinPlotData.data.frame <- function(cogDF, xVar, yVar = 370 / 515, shape, xbin = 30) {
   require(hexbin)
   dat <- hexbin:::hexbin(cogDF[,xVar], cogDF[,yVar], shape = shape, xbin = xbin)
   style <- "lattice"
   minarea <- 0.05
   maxarea <- 0.8
   mincnt <- 1
   maxcnt <- max(dat@count)
   style <- "lattice"
   trans <- NULL
   
   cnt <- dat@count
   xbins <- dat@xbins
   shape <- dat@shape
   tmp <- hcell2xy(dat)
   good <- mincnt <= cnt & cnt <= maxcnt
   
   xnew <- tmp$x[good]
   ynew <- tmp$y[good]
   cnt <- cnt[good]
   
   sx <- xbins/diff(dat@xbnds)
   sy <- (xbins * shape)/diff(dat@ybnds)
   
   if (is.null(trans)) {
      if (min(cnt, na.rm = TRUE) < 0) {
         pcnt <- cnt + min(cnt)
         rcnt <- {
            if (maxcnt == mincnt) rep.int(1, length(cnt)) else (pcnt - mincnt)/(maxcnt - mincnt)
         }
      } else rcnt <- {
         if (maxcnt == mincnt) rep.int(1, length(cnt)) else (cnt - mincnt)/(maxcnt - mincnt)
      }
   } else {
      rcnt <- (trans(cnt) - trans(mincnt))/(trans(maxcnt) - trans(mincnt))
      if (any(is.na(rcnt))) stop("bad count transformation")
   }
   area <- minarea + rcnt * (maxarea - minarea)
   area <- pmin(area, maxarea)
   radius <- sqrt(area)
   
   inner <- 0.5
   outer <- (2 * inner)/sqrt(3)
   dx <- inner/sx
   dy <- outer/(2 * sy)
   rad <- sqrt(dx^2 + dy^2)
   hexC <- hexcoords(dx, dy, sep = NULL)
   
   list(
      data = data.frame(x = xnew, y = ynew, r = radius),
      plotType = "hexbin",
      xlab = xVar,
      ylab = yVar,
      shape = shape,
      hexx = hexC$x,
      hexy = hexC$y
   )
}

getBivarPlotDat <- function(cd, xVar, yVar, distType = "marginal", plotType = "scatter", shape = 370 / 515, xbin = 50) {
   if(distType == "marginal" || cogNrow(cd$cdo$cogDatConn) == nrow(cd$curCogDF)) {
      if(plotType == "scatter") {
         getCogScatterPlotData(cd$cdo$cogDatConn, xVar, yVar)
      } else {
         getCogHexbinPlotData(cd$cdo$cogDatConn, xVar, yVar, shape, xbin)
      }
   } else {
      if(plotType == "scatter") {
         getCogScatterPlotData(cd$curCogDF, xVar, yVar)
      } else {
         getCogHexbinPlotData(cd$curCogDF, xVar, yVar, shape, xbin)
      }
   }
}

getCogICA <- function(x, ...)
   UseMethod("getCogICA", x)

getCogICA.data.frame <- function(cogDF, vars) {
   require(fastICA)
   # set.seed(4331)
   idx <- which(complete.cases(cogDF[,vars]))
   res <- fastICA(cogDF[idx, vars], n.comp=2)
   data.frame(IC1 = res$S[,1], IC2 = res$S[,2])
}

getMultivarPlotDat <- function(cd, vars, distType = "marginal", plotType = "scatter", shape = 370 / 515, xbin = 50) {
   xVar <- "IC1"
   yVar <- "IC2"
   if(distType == "marginal" || cogNrow(cd$cdo$cogDatConn) == nrow(cd$curCogDF)) {
      icaDat <- getCogICA(cd$cdo$cogDatConn, vars)
   } else {
      icaDat <- getCogICA(cd$curCogDF, vars)      
   }
   if(plotType == "scatter") {
      getCogScatterPlotData(icaDat, xVar, yVar)
   } else {
      getCogHexbinPlotData(icaDat, xVar, yVar, shape, xbin)
   }
}


############################################################################
### panel table functions
############################################################################


dummyPanel <- function(w, h) {
   sprintf("<div style=\"width:%dpx; height:%dpx; background-color: #ddd\"></div>", w, h)
}

dummyCog <- function(labelVars) {
   data.frame(cog_name = labelVars, cog_value = "")
}

getPanels <- function(cdo, width, height, curRows, pixelratio = 2) {
   if(cdo$preRender) {
      pngs <- unlist(lapply(cdo$panelDataSource[curRows$panelKey], "[[", 2))
   } else {
      environment(cdo$panelFn) <- environment()
      
      curDat <- cdo$panelDataSource[curRows$panelKey]
      if(is.null(curDat))
         warning("data for key ", curRows$panelKey, " could not be found.")
      
      panelContent <- lapply(seq_along(curDat), function(i) {
         x <- curDat[[i]]
         res <- try({
            list(
               html = renderPanelHtml(cdo$panelFn, x, width, height, cdo$width, cdo$lims, pixelratio),
               data = list(
                  id = paste("#display-panel-table-", i, sep = ""),
                  spec = renderPanelData(cdo$panelFn, x, width, height, cdo$width, cdo$lims, pixelratio)
               )
            )
         })
         if(inherits(res, "try-error"))
            res <- NULL
         res
      })
   }
   panelContent
}

renderPanelHtml <- function(panelFn, ...)
   UseMethod("renderPanelHtml", panelFn)

renderPanelHtml.rplotFn <- function(panelFn, ...)
   renderPanelHtml.trellisFn(panelFn, ...)

renderPanelHtml.ggplotFn <- function(panelFn, ...)
   renderPanelHtml.trellisFn(panelFn, ...)

renderPanelHtml.trellisFn <- function(panelFn, x, width, height, origWidth, lims, pixelratio) {
   tmpfile <- tempfile()
   on.exit(rm(tmpfile))
   
   makePNG(dat = x, 
      panelFn = panelFn, 
      file = tmpfile, 
      width = width, 
      height = height, 
      origWidth = origWidth,
      # res = 72, # * cdo$state$layout$w / cdo$width, 
      lims = lims,
      pixelratio = pixelratio
   )
   paste("<img src=\"", encodePNG(tmpfile),
      "\" width=\"", width, "px\" height=\"", height, "px\">", sep = "")   
}

renderPanelHtml.ggvisFn <- function(panelFn, x, width, height, origWidth, lims, pixelratio) {
   ""
}

renderPanelHtml.rChartsFn <- function(panelFn, x, width, height, origWidth, lims, pixelratio) {
   
   p <- kvApply(panelFn, x)
   p$set(width = width, height = height)
   # paste(capture.output(p$print()), collapse = '\n')
   
   p <- capture.output(p$show('iframesrc', cdn = TRUE))
   
   ind <- which(grepl("iframe\\.rChart", p))
   if(length(ind) > 0) {
      p[ind[1]] <- "<style>iframe.rChart{ width: 100%; height: 100%;}</style>"
   }
   
   paste(c(
      sprintf("<div style='width:%dpx; height:%dpx'>", as.integer(width), as.integer(height)),
      p,
      "</div>"
   ), collapse = "\n")
}

renderPanelData <- function(panelFn, ...)
   UseMethod("renderPanelData", panelFn)

renderPanelData.rplotFn <- function(panelFn, ...) {
   ""
}

renderPanelData.trellisFn <- function(panelFn, ...) {
   ""
}

renderPanelData.ggplotFn <- function(panelFn, ...) {
   ""
}

renderPanelData.ggvisFn <- function(panelFn, x, width, height, origWidth, lims, pixelratio) {
   # plotXLim <- tmp$x.limits
   # plotYLim <- tmp$y.limits
   # curXLim <- trsCurXLim(lims, x, plotXLim)
   # curYLim <- trsCurYLim(lims, x, plotYLim)
   p <- kvApply(panelFn, x)
   p <- set_options(p, width = width, height = height)
   
   getVegaSpec(p)
}

renderPanelData.rChartsFn <- function(panelFn, x, width, height, origWidth, lims, pixelratio) {
   # TODO: javascript library dependency
   ""
}


getVegaSpec <- function(x) {
   spec <- ggvis:::as.vega(x)
   RJSONIO::toJSON(spec)
}


# makePanel.rGraphics <- function(filename, func, width = 400, height = 400, origWidth = 400, origHeight = 400, pixelratio = 1, res = 72, basePointSize = 12) {
# 
#    if(capabilities("aqua")) {
#       pngfun <- png
#    } else if (suppressWarnings(suppressMessages(require("Cairo")))) {
#       pngfun <- CairoPNG
#    } else {
#       pngfun <- png
#    }
#    
#    pngfun(filename = filename,
#       width = width * pixelratio,
#       height = height * pixelratio,
#       res = res * pixelratio,
#       pointsize = basePointSize * width / origWidth)
# 
#    dv <- dev.cur()
# 
#    tryCatch(func(), finally = dev.off(dv))
# }
# 
# require(fastICA)
#
#
# makeBivarJSON(ic$S[,1], ic$S[,2], xlab="IC 1", ylab="IC 2")

