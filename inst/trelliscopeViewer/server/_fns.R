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
      return(jsonlite::toJSON(res, na = "string"))
  }
}

# renderDataLite <- function(expr, env = parent.frame(), quoted = FALSE, func = NULL) {
#   if (!is.null(func)) {
#     shinyDeprecated(msg="renderDateLite: argument 'func' is deprecated. Please use 'expr' instead.")
#   } else {
#     installExprFunction(expr, "func", env, quoted)
#   }

#   markRenderFunction(NULL, function() {
#     value <- func()
#     return(value)
#   })
# }

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
      return(shiny:::toJSON(res))
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

cogTitleString <- function(data) {
  nc <- ncol(data)
  matrix(sapply(seq_len(nc), function(i) {
    if(inherits(data[[i]], c("integer", "numeric"))) {
      val <- ""
    } else {
      val <- as.character(data[[i]])
      if(nchar(val) < 30 || grepl("<|>", val))
        val <- ""
    }
    val
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

  if(length(name) == 0)
    return(list(name = name))

  curInfo <- cd$cdo$cogDistns[[name]]

  if(is.null(curInfo))
    return(list(name = name))
  if(is.na(curInfo$type))
    return(list(name = name))

  if(curInfo$type == "numeric") {
    if(distType == "marginal" || cogNrow(cd$cdo$cogDatConn) == nrow(cd$curCogDF)) {
      tmp <- curInfo$marginal[[plotType]]
    } else {
      tmp <- trelliscope:::getCogQuantPlotData(cd$curCogDF, name, plotType, cogInfo = cd$cdo$cogInfo)
    }
    if(plotType == "hist") {
      delta <- diff(tmp$xdat[1:2])
      logPrefix <- ""
      if(!is.null(curInfo$log))
        if(!is.na(curInfo$log))
          logPrefix <- paste0(curInfo$log, "^")
      tmp$label <- paste("(", logPrefix, tmp$xdat, ",", logPrefix, tmp$xdat + delta, "]", sep = "")
      return(list(name = name, type = curInfo$type, data = tmp, plotType = plotType, id = ifelse(calledFromFooter, 1, rnorm(1)), log = curInfo$log))
      # add random normal to make sure it triggers even when it
      # doesn't change - this is to ensure spinner will get replaced
    } else {
      names(tmp)[1:2] <- c("x", "y")
      return(list(name = name, type = curInfo$type, data = tmp, plotType = plotType, id = ifelse(calledFromFooter, 1, rnorm(1)), log = curInfo$log))
    }
  } else { # bar chart
    if(distType == "marginal") {
      tmp <- curInfo$marginal
    } else {
      tmp <- trelliscope:::getCogCatPlotData(cd$curCogDF, name, plotType)$freq
    }
    if(is.data.frame(tmp)) {
      if(nrow(tmp) > maxLevels)
        return(list(name = name, id = ifelse(calledFromFooter, 1, rnorm(1))))
      tmp <- rbind(tmp, data.frame(label = "", Freq = 0, stringsAsFactors = FALSE))
      names(tmp)[which(names(tmp) == "Freq")] <- "ydat"
      tmp$xdat <- seq_len(nrow(tmp))
      return(list(name = name, type = curInfo$type, data = tmp, plotType = "bar", id = ifelse(calledFromFooter, 1, rnorm(1))))
    }
  }
  return(list(name = name))
}

getCogScatterPlotData <- function(x, ...)
  UseMethod("getCogScatterPlotData", x)

getCogHexbinPlotData <- function(x, ...)
  UseMethod("getCogHexbinPlotData", x)

getCogScatterPlotData.data.frame <- function(cogDF, xVar, yVar, xTrans = identity, yTrans = identity, xLog = NULL, yLog = NULL) {
  idx <- complete.cases(cogDF[,c(xVar, yVar)])
  list(
    data = data.frame(x = xTrans(cogDF[idx,xVar]), y = yTrans(cogDF[idx,yVar])),
    plotType = "scatter",
    xlab = xVar,
    ylab = yVar,
    xLog = xLog,
    yLog = yLog
  )
}

getCogHexbinPlotData.data.frame <- function(cogDF, xVar, yVar = 370 / 515, shape, xbin = 30, xTrans = xTrans, yTrans = yTrans, xLog = NULL, yLog = NULL) {
  require(hexbin)
  dd <- data.frame(x = xTrans(cogDF[,xVar]), y = yTrans(cogDF[,yVar]))
  idx <- complete.cases(dd) & !is.infinite(dd$x) & !is.infinite(dd$y)

  dat <- try(hexbin:::hexbin(dd$x[idx], dd$y[idx], shape = shape, xbin = xbin), silent = TRUE)
  if(inherits(dat, "try-error"))
    return(NULL)

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
    hexy = hexC$y,
    xLog = xLog,
    yLog = yLog
  )
}

getBivarPlotDat <- function(cd, xVar, yVar, distType = "marginal", plotType = "scatter", shape = 370 / 515, xbin = 50) {

  # deal with log
  xRow <- cd$cdo$cogInfo[cd$cdo$cogInfo$name == xVar,,drop=FALSE]
  yRow <- cd$cdo$cogInfo[cd$cdo$cogInfo$name == yVar,,drop=FALSE]
  xTrans <- identity; xLog = NA
  if(!is.null(xRow$log)) {
    if(!is.na(xRow$log)) {
      xTrans <- function(x) log(x, xRow$log)
      xLog <- xRow$log
    }
  }
  yTrans <- identity; yLog = NA
  if(!is.null(yRow$log)) {
    if(!is.na(yRow$log)) {
      yTrans <- function(x) log(x, yRow$log)
      yLog <- yRow$log
    }
  }

  if(distType == "marginal" || cogNrow(cd$cdo$cogDatConn) == nrow(cd$curCogDF)) {
    if(plotType == "scatter") {
      getCogScatterPlotData(cd$cdo$cogDatConn, xVar, yVar, xTrans = xTrans, yTrans = yTrans, xLog = xLog, yLog = yLog)
    } else {
      getCogHexbinPlotData(cd$cdo$cogDatConn, xVar, yVar, shape, xbin, xTrans = xTrans, yTrans = yTrans, xLog = xLog, yLog = yLog)
    }
  } else {
    if(plotType == "scatter") {
      getCogScatterPlotData(cd$curCogDF, xVar, yVar, xTrans = xTrans, yTrans = yTrans, xLog = xLog, yLog = yLog)
    } else {
      getCogHexbinPlotData(cd$curCogDF, xVar, yVar, shape, xbin, xTrans = xTrans, yTrans = yTrans, xLog = xLog, yLog = yLog)
    }
  }
}

# getCogICA <- function(x, ...)
#   UseMethod("getCogICA", x)

# getCogICA.data.frame <- function(cogDF, vars) {
#   require(fastICA)
#   # set.seed(4331)
#   idx <- which(complete.cases(cogDF[,vars]))
#   res <- fastICA(cogDF[idx, vars], n.comp=2)
#   data.frame(IC1 = res$S[,1], IC2 = res$S[,2])
# }

# getMultivarPlotDat <- function(cd, vars, distType = "marginal", plotType = "scatter", shape = 370 / 515, xbin = 50) {
#   xVar <- "IC1"
#   yVar <- "IC2"
#   if(distType == "marginal" || cogNrow(cd$cdo$cogDatConn) == nrow(cd$curCogDF)) {
#     icaDat <- getCogICA(cd$cdo$cogDatConn, vars)
#   } else {
#     icaDat <- getCogICA(cd$curCogDF, vars)
#   }
#   if(plotType == "scatter") {
#     getCogScatterPlotData(icaDat, xVar, yVar)
#   } else {
#     getCogHexbinPlotData(icaDat, xVar, yVar, shape, xbin)
#   }
# }


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

    if(!is.null(cdo$relatedData)) {
      environment(cdo$panelFn) <- list2env(cdo$relatedData, parent = .GlobalEnv)
      pfe <- environment(cdo$panelFn)
      vars <- ls(envir = pfe)
      for(vr in vars) {
        if(is.function(pfe[[vr]]))
          environment(pfe[[vr]]) <- pfe
      }
    }

    panelContent <- lapply(seq_along(curDat), function(i) {
      x <- curDat[[i]]
      res <- try({
        getPanelContent(cdo$panelFn, x, width, height, cdo$width, cdo$height, cdo$lims, pixelratio, cdo$name, cdo$group)
      })
      if(inherits(res, "try-error"))
        res <- NULL
      res$id <- paste("#display-panel-table-", i, sep = "")

      res
    })
  }
  panelContent
}

getPanelContent <- function(panelFn, ...)
  UseMethod("getPanelContent", panelFn)

getPanelContent.rplotFn <- function(panelFn, ...)
  getPanelContent.trellisFn(panelFn, ...)

getPanelContent.ggplotFn <- function(panelFn, ...)
  getPanelContent.trellisFn(panelFn, ...)

getPanelContent.trellisFn <- function(panelFn, x, width, height, origWidth, origHeight, lims, pixelratio, name, group) {
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
  html <- paste("<img src=\"", encodePNG(tmpfile),
    "\" width=\"", width, "px\" height=\"", height, "px\">", sep = "")

  list(html = html, class = "raster")
}

getPanelContent.base64pngFn <- function(panelFn, x, width, height, origWidth, origHeight, lims, pixelratio, name, group) {

  html <- paste("<img src=\"", kvApply(x, panelFn)$value,
    "\" width=\"", width, "px\" height=\"", height, "px\">", sep = "")

  list(html = html, class = "raster")
}

getPanelContent.htmlwidgetFn <- function(panelFn, x, width, height, origWidth, origHeight, lims, pixelratio, name, group) {

  p <- datadr::kvApply(x, panelFn)$value

  if(inherits(p, "rbokeh") || !is.null(p$trscope_direct_embed)) {
    scaleUp <- p$scale_up
    scaleDown <- p$scale_down
    if(is.null(scaleUp))
      scaleUp <- FALSE
    # default if not explicitly specified is to scale down
    if(is.null(scaleDown))
      scaleDown <- TRUE

    if(scaleUp && width > origWidth) {
      p$width <- origWidth
      p$height <- origHeight
      scale <- width / origWidth
    } else if(scaleDown && width < origWidth) {
      p$width <- origWidth
      p$height <- origHeight
      scale <- width / origWidth
    } else {
      p$width <- width
      p$height <- height
      scale <- ""
    }

    w <- htmlwidgets:::toHTML(p) #, standalone = TRUE)
    d <- attr(w, "html_dependencies")
    d <- lapply(d, function(el) {
      class(el) <- "list"
      # copy script and css and set $src$href
      # also set a flag so we dont render dependency every time?
      depDir <- paste("widgetassets/", el$name, "-", el$version, sep = "")
      assetDir <- file.path("www", depDir)

      if(!file.exists(assetDir))
        dir.create(assetDir, recursive = TRUE)

      if(!is.null(el$script)) {
        ff1 <- file.path(assetDir, el$script)
        ff2 <- file.path(el$src$file, el$script)
        for(ii in seq_along(ff1)) {
          if(!file.exists(ff1[ii]))
            file.copy(ff2[ii], assetDir)
        }
        el$src$href <- depDir
      } else {
        el$script <- NULL
      }
      if(!is.null(el$stylesheet)) {
        ff1 <- file.path(assetDir, el$stylesheet)
        ff2 <- file.path(el$src$file, el$stylesheet)
        for(ii in seq_along(ff1)) {
          if(!file.exists(ff1[ii]))
            file.copy(ff2[ii], assetDir)
        }
        el$src$href <- depDir
      } else {
        el$stylesheet <- NULL
      }
      el
    })

    return(list(
      html = as.character(w),
      class = "htmlwidget",
      deps = d,
      scale = scale
    ))
  } else {
    ff <- file.path("iframes", paste(group, name, sep = "_"), paste0(digest::digest(x[[1]]), ".html"))
    fb <- file.path(getOption("vdbConn")$path, "www", ff)
    if(!file.exists(dirname(fb)))
      dir.create(dirname(fb), recursive = TRUE)
    p$sizingPolicy$padding <- 0
    p$width <- width
    p$height <- height
    suppressMessages(saveWidget(p, fb, selfcontained = FALSE))
    # <iframe src="data:text/html;base64, ..."></iframe>
    html <- paste0("<iframe width='", width, "' height='", height, "' frameBorder='0' webkitallowfullscreen='' mozallowfullscreen='' allowfullscreen='' scrolling='no' sandbox='allow-forms allow-scripts allow-popups allow-same-origin allow-pointer-lock' src='", ff, "'></iframe>")

    return(list(
      html = html,
      class = "iframe"
    ))
  }
}


