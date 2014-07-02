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
      data <- data[1:min(nr, nrow(data)),]
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
      if(inherits(data[[i]], c("numeric", "integer"))) {
         list(i = i, name = nms[i], numeric = TRUE)
      } else {
         if(nms[i] == "panelKey") {
            levels <- ""
         } else {
            # TODO: deal with very large number of levels
            levels <- sort(unique(as.character(data[[i]])))
         }
         list(i = i, name = nms[i], levels = levels)
      }
   })
}

cogTableFootHist <- function(data) {
   rep("<div class='cog-univar-hist'></div>", ncol(data))
}

# creates data ready for univariate d3 plotting
# either bar chart for character or hist / quantile for numeric (specify with plotType)
getUnivarPlotDat <- function(cdo, name, distType = "marginal", plotType = "hist") {
   if(plotType == "histogram")
      plotType <- "hist"
   if(plotType == "quantile")
      plotType <- "quant"

   curInfo <- cdo$cogInfo[[name]]

   if(curInfo$type == "numeric") {
      if(distType == "marginal") {
         tmp <- curInfo$marginal[[plotType]]
      } else {
         # call trelliscope:::getCogQuantPlotData...
         return(NULL)
      }
      if(plotType == "hist") {
         delta <- diff(tmp$xdat[1:2])
         tmp$label <- paste("(", tmp$xdat, ",", tmp$xdat + delta, "]", sep = "")
         return(list(name = name, type = curInfo$type, data = tmp, plotType = plotType))
      } else {
         names(tmp)[1:2] <- c("x", "y")
         return(list(name = name, type = curInfo$type, data = tmp, plotType = plotType))
      }
   } else {
      if(distType == "marginal") {
         tmp <- curInfo$marginal
      } else {
         # call trelliscope:::getCogCatPlotData...
         return(NULL)
      }
      tmp <- rbind(tmp, data.frame(label = "", Freq = 0, stringsAsFactors = FALSE))
      # browser()
      tmp$ind <- seq_len(nrow(tmp))
      return(list(name = name, type = curInfo$type, data = tmp, plotType = "bar"))
   }
}

getCogScatterPlotData <- function(x, ...)
   UseMethod("getCogScatterPlotData", x)

getCogHexbinPlotData <- function(x, ...)
   UseMethod("getCogHexbinPlotData", x)

getCogScatterPlotData.data.frame <- function(cogDF, xVar, yVar) {
   list(
      data = data.frame(x = cogDF[,xVar], y = cogDF[,yVar]),
      plotType = "scatter",
      xlab = xVar,
      ylab = yVar
   )
}

getCogHexbinPlotData.data.frame <- function(cogDF, xVar, yVar = 370 / 515, shape, xbin = 30) {
   dat <- hexbin(cogDF[,xVar], cogDF[,yVar], shape = shape, xbin = xbin)
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

getBivarPlotDat <- function(cdo, xVar, yVar, distType = "marginal", plotType = "scatter", shape = 370 / 515, xbin = 50) {
   # TODO: handle marginal...
   if(plotType == "scatter") {
      getCogScatterPlotData(cdo$cogDatConn, xVar, yVar)
   } else {
      getCogHexbinPlotData(cdo$cogDatConn, xVar, yVar, shape, xbin)
   }
}

getCogICA <- function(x, ...)
   UseMethod("getCogICA", x)

getCogICA.data.frame <- function(cogDF, vars) {
   require(fastICA)
   set.seed(4331)
   res <- fastICA(cogDF[,vars], n.comp=2)
   data.frame(IC1 = res$S[,1], IC2 = res$S[,2])
}

getMultivarPlotDat <- function(cdo, vars, distType = "marginal", plotType = "scatter", shape = 370 / 515, xbin = 50) {
   # TODO: handle marginal...
   xVar <- "IC1"
   yVar <- "IC2"
   icaDat <- getCogICA(cdo$cogDatConn, vars)
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

dummyCog <- function(cogVars) {
   data.frame(cog_name = cogVars, cog_value = "")
}

getPanels <- function(cdo, curRows, pixelratio = 2) {
   if(cdo$cdo$preRender) {
      pngs <- unlist(lapply(cdo$cdo$panelDataSource[curRows$panelKey], "[[", 2))
   } else {
      tmpfile <- tempfile()

      # load relatedData
      # rel <- cdo$relatedData
      # for(i in seq_along(rel)) {
      #    assign(names(rel)[i], rel[[i]], environment())
      # }
      environment(cdo$cdo$panelFn) <- environment()

      curDat <- cdo$cdo$panelDataSource[curRows$panelKey]
      if(is.null(curDat))
         warning("data for key ", curRows, " could not be found.")

      pngs <- sapply(curDat, function(x) {
         res <- try({
            makePNG(dat = x,
               panelFn = cdo$cdo$panelFn,
               file = tmpfile,
               width = cdo$cdo$state$panelLayout$w,
               height = cdo$cdo$state$panelLayout$h,
               origWidth = cdo$cdo$width,
               # res = 72, # * cdo$cdo$state$panelLayout$w / cdo$cdo$width,
               lims = cdo$cdo$lims,
               pixelratio = pixelratio
            )
            encodePNG(tmpfile)
         })
         if(inherits(res, "try-error"))
            res <- NULL
         res
      })
   }
   pngs
}

makePanel.rGraphics <- function(filename, func, width = 400, height = 400, origWidth = 400, origHeight = 400, pixelratio = 1, res = 72, basePointSize = 12) {

   if(capabilities("aqua")) {
      pngfun <- png
   } else if (suppressWarnings(suppressMessages(require("Cairo")))) {
      pngfun <- CairoPNG
   } else {
      pngfun <- png
   }

   pngfun(filename = filename,
      width = width * pixelratio,
      height = height * pixelratio,
      res = res * pixelratio,
      pointsize = basePointSize * width / origWidth)

   dv <- dev.cur()

   tryCatch(func(), finally = dev.off(dv))
}

# require(fastICA)
#
#
# makeBivarJSON(ic$S[,1], ic$S[,2], xlab="IC 1", ylab="IC 2")

