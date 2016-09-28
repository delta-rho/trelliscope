#' Convert a VDB to be usable with the new Trelliscope viewer
#'
#' @param overwrite should existing converted files be overwritten? (not implemented)
#' @param basePath the base directory to place the converted vdb in (doesn't need to exist)
#' @param convertPanels should panels be converted to json for the new viewer? (good to set to FALSE if this has already been done but other aspects of the VDB have changed and need to be re-converted)
#' @param jsonp should jsonp files be created instead of json?
#' @param conn VDB connection info, typically stored in options("vdbConn") at the beginning of a session, and not necessary to specify here if a valid "vdbConn" object exists
#' @param autoYes should questions to proceed with directory creation operations be automatically answered with "yes"?
#' @export
#' @importFrom DistributionUtils skewness
#' @importFrom htmltools as.tags htmlDependencies
#' @importFrom utils str
vdbConvert <- function(overwrite = FALSE, basePath = NULL, convertPanels = TRUE,
  jsonp = TRUE, conn = getOption("vdbConn"), autoYes = FALSE) {

  if (is.null(basePath)) {
    basePath <- file.path(conn$path, "trscope")
  }
  basePath2 <- file.path(basePath, "displays")

  if (!dir.exists(basePath2)) {
    ans <- "y"
    if (!autoYes)
      ans <- readline(paste("The path ", basePath,
        " does not exist.  Should it be created? (y = yes) ", sep = ""))
    if (!tolower(substr(ans, 1, 1)) == "y")
      return()
    if (!dir.create(basePath, recursive = TRUE))
      stop("Could not create directory.\n")
  }

  # step through all displays...
  load(file.path(conn$path, "displays", "_displayList.Rdata"))
  for (ii in seq_len(nrow(displayListDF))) {
    nm <- displayListDF$name[ii]
    gp <- displayListDF$group[ii]

    if (!dir.exists(file.path(basePath2, gp, nm)))
      dir.create(file.path(basePath2, gp, nm), recursive = TRUE)

    cogDatPath <- file.path(basePath2, gp, nm, "cogData.json")
    displayObjPath <- file.path(basePath2, gp, nm, "displayObj.json")
    if (jsonp) {
      cogDatPath <- paste0(cogDatPath, "p")
      displayObjPath <- paste0(displayObjPath, "p")
    }
    # TODO: check for these files and only do stuff if overwrite = TRUE
    message("converting display ", nm, "...")

    a <- getDisplay(nm, gp)

    # fix issue with missing types
    for (jj in seq_len(nrow(a$cogInfo))) {
      type <- a$cogInfo$type[jj]
      if (is.null(type) || is.na(type)) {
        a$cogInfo$type[jj] <- ifelse(is.numeric(a$cogDatConn[[a$cogInfo$name[jj]]]),
          "numeric", "factor")
      }
      if (type == "integer")
        a$cogInfo$type[jj] <- "numeric"
    }
    for (jj in seq_along(a$cogDatConn)) {
      type <- attr(a$cogDatConn[[jj]], "cogAttrs")$type
      if (is.null(type) || is.na(type))
        attr(a$cogDatConn[[jj]], "cogAttrs")$type <- ifelse(is.numeric(x),
          "numeric", "factor")
      if (type == "integer")
        attr(a$cogDatConn[[jj]], "cogAttrs")$type <- "numeric"
    }

    if (!is.null(a$relatedData)) {
      environment(a$panelFn) <- list2env(a$relatedData, parent = .GlobalEnv)
      pfe <- environment(a$panelFn)
      vars <- ls(envir = pfe)
      for (vr in vars) {
        if (is.function(pfe[[vr]]))
          environment(pfe[[vr]]) <- pfe
      }
    }

    # get rid of cognostics that are all NA
    naCogs <- which(sapply(a$cogDatConn, function(x) all(is.na(x))))
    if (length(naCogs) > 0) {
      a$cogDatConn[naCogs] <- NULL
      a$cogInfo <- subset(a$cogInfo, ! (name %in% names(naCogs)))
    }

    message("updating cog distributions, info, and state...")
    a$state <- updateCogState(a$state)
    if (length(a$state$labels) == 0)
      a$state$labels <- I(a$cogInfo$name[a$cogInfo$defLabel])
    if (length(a$state$sort) == 0) {
      condNames <- a$cogInfo$name[a$cogInfo$group == "condVar"]
      a$state$sort <- lapply(seq_along(condNames), function(kk)
        list(order = kk, dir = "asc", name = condNames[kk]))
    }
    a$cogDistns <- getCogDistnsConvert(a$cogDatConn)
    a$cogInfo <- updateCogInfo(a)

    if (jsonp) {
      cat(paste0("__loadCogData__(", jsonlite::toJSON(a$cogDatConn), ")"),
        file = cogDatPath)
    } else {
      cat(jsonlite::toJSON(a$cogDatConn), file = cogDatPath)
    }
    a$cogDatConn <- NULL

    a$cogInterface <- list(name = a$name, group = a$group, type = "JSON")

    a$panelInterface <- list(
      type = ifelse(a$panelFnType == "htmlwidgetFn", "htmlwidget", "image"),
      deps = writeWidgetDeps(a, basePath)
    )

    if (convertPanels) {
      message("converting panels to json...")
      panelPath <- file.path(basePath2, gp, nm, "json")
      if (jsonp)
        panelPath <- file.path(basePath2, gp, nm, "jsonp")
      if (!dir.exists(panelPath))
        dir.create(panelPath, recursive = TRUE)

      panel2json(a, conn, panelPath = panelPath, jsonp = jsonp)
    }

    message("writing displayObj...")
    a$example <- paste(capture.output(utils::str(a[[1]]))[-1], collapse = "\n")
    a$panelDataSource <- NULL
    a$panelFn <- fn2text(a$panelFn)
    a$cogFn <- fn2text(a$cogFn)
    if (is.null(a$mdDesc))
      a$mdDesc <- ""
    class(a) <- "list"
    if (jsonp) {
      cat(paste0("__loadDisplayObj__(",
        jsonlite::toJSON(a, pretty = TRUE, auto_unbox = TRUE), ")"),
        file = displayObjPath)
    } else {
      cat(jsonlite::toJSON(a, pretty = TRUE, auto_unbox = TRUE),
        file = displayObjPath)
    }

    message("copying thumbnail...")
    file.copy(file.path(conn$path, "displays", gp, nm, "thumb.png"),
      file.path(basePath2, gp, nm), overwrite = overwrite)
  }

  load(file.path(conn$path, "displays", "_displayList.Rdata"))
  if (jsonp) {
    cat(paste0("__loadDisplayList__(",
      jsonlite::toJSON(displayListDF, pretty = TRUE), ")"),
      file = file.path(basePath2, "displayList.jsonp"))
  } else {
    cat(jsonlite::toJSON(displayListDF, pretty = TRUE),
      file = file.path(basePath2, "displayList.json"))
  }

  # download latest js
  dir.create(file.path(basePath, "static/fonts/IcoMoon/fonts"), recursive = TRUE)
  dir.create(file.path(basePath, "static/fonts/OpenSans"), recursive = TRUE)

  toCopy <- c(
    "bundle.js",
    "bundle.js.map",
    "index.html",
    "favicon.ico",
    "static/fonts/IcoMoon/style.css",
    "static/fonts/IcoMoon/fonts/icomoon.eot",
    "static/fonts/IcoMoon/fonts/icomoon.svg",
    "static/fonts/IcoMoon/fonts/icomoon.ttf",
    "static/fonts/IcoMoon/fonts/icomoon.woff",
    "static/fonts/OpenSans/opensans-light-webfont.woff",
    "static/fonts/OpenSans/opensans-light-webfont.woff2",
    "static/fonts/OpenSans/opensans-regular-webfont.woff",
    "static/fonts/OpenSans/opensans-regular-webfont.woff2",
    "static/fonts/OpenSans/stylesheet.css"
  )

  urlBase <- "https://raw.githubusercontent.com/hafen/trelliscopejs-demo/gh-pages/"
  for (ff in toCopy) {
    curl::curl_download(
      paste0(urlBase, ff),
      file.path(basePath, ff))
  }

  # make config
  cfg <- as.character(jsonlite::toJSON(
    list(
      display_base = "displays",
      data_type = ifelse(jsonp, "jsonp", "json"),
      cog_server = list(
        type = ifelse(jsonp, "jsonp", "json"),
        info = list(base = "displays")
      )
    ),
    pretty = TRUE,
    auto_unbox = TRUE
  ))
  if (jsonp)
    cfg <- paste0("__loadTrscopeConfig__(", cfg, ")")
  cat(cfg, file = file.path(basePath,
    paste0("config", ifelse(jsonp, ".jsonp", ".json"))))

  invisible(TRUE)
}

panel2json <- function(a, conn, panelPath, jsonp) {
  get_jsonp_text <- function(key, jsonp) {
    if (jsonp) {
      list(
        st = paste0("__panel__._", key, "("),
        nd = ")"
      )
    } else {
      return(list(st = "", nd = ""))
    }
  }

  if (a$preRender || a$panelFnType == "base64pngFn") {
    # convert ddo to png files
    ff <- list.files(file.path(conn$path, "displays", a$group, a$name, "panels"),
      "Rdata", full.names = TRUE)
    ff2 <- file.path(panelPath,
      paste0(gsub("\\.Rdata", "", basename(ff)), ifelse(jsonp, ".jsonp", ".json")))
    for (jj in seq_along(ff)) {
      key <- gsub("\\.Rdata", "", basename(ff[jj]))
      txt <- get_jsonp_text(key, jsonp)
      load(ff[jj])
      cat(paste0(txt$st, "\"", obj[[1]][[2]][[1]], "\"", txt$nd), file = ff2[jj])
    }
  } else if (a$panelFnType %in% c("rplotFn", "trellisFn", "ggplotFn")) {
    keys <- getAttribute(a$panelDataSource, "keyHashes")
    for (key in keys) {
      ff <- tempfile()
      makePNG(dat = a$panelDataSource[[key]],
        panelFn = a$panelFn, file = ff, width = a$width,
        height = a$height, lims = a$lims)
      dat <- paste0("\"", encodePNG(ff), "\"")
      txt <- get_jsonp_text(key, jsonp)
      cat(paste0(txt$st, dat, txt$nd),
        file = file.path(panelPath, paste0(key, ifelse(jsonp, ".jsonp", ".json"))))
    }
  } else if (a$panelFnType == "htmlwidgetFn") {
    keys <- getAttribute(a$panelDataSource, "keyHashes")
    for (key in keys) {
      p <- datadr::kvApply(a$panelDataSource[[key]],
        a$panelFn)$value
      p2 <- htmltools::as.tags(p)
      txt <- get_jsonp_text(key, jsonp)
      cat(paste0(txt$st, p2[[2]]$children[[1]], txt$nd),
        file = file.path(panelPath,
          paste0(key, ifelse(jsonp, ".jsonp", ".json"))))
    }
  }
}

writeWidgetDeps <- function(a, dir) {
  if (a$panelFnType == "htmlwidgetFn") {
    panelEx <- datadr::kvApply(datadr::kvExample(a$panelDataSource),
      a$panelFn)$value

    pt <- htmltools::as.tags(panelEx)
    deps <- htmltools::htmlDependencies(pt)

    libdir <- "lib"
    dir.create(dir, showWarnings = FALSE)
    oldwd <- getwd()
    on.exit(setwd(oldwd), add = TRUE)
    setwd(dir)
    dir.create(libdir, showWarnings = FALSE)

    deps2 <- do.call(c, lapply(deps, function(x) {
      x <- htmltools::copyDependencyToDir(x, libdir, FALSE)
      x <- htmltools::makeDependencyRelative(x, dir, FALSE)
      res <- list()
      if (!is.null(x$script)) {
        res[[length(res) + 1]] <- list(type = "script",
          url = paste(x$src, x$script, sep = "/"))
      }
      if (!is.null(x$stylesheet)) {
        res[[length(res) + 1]] <- list(type = "stylesheet",
          url = paste(x$src, x$stylesheet, sep = "/"))
      }
      res
    }))

    list(name = class(panelEx)[1], assets = deps2)
  }
}

fn2text <- function(fn) {
  class(fn) <- NULL
  paste(capture.output(dump("fn", ""))[-1], collapse = "\n")
}

updateCogState <- function(state) {
  state$group <- NULL
  nms <- names(state$sort)
  for (ii in seq_along(state$sort)) {
    state$sort[[ii]]$name <- nms[ii]
  }
  nms <- names(state$filter)
  for (ii in seq_along(state$filter)) {
    if ("regex" %in% names(state$filter[[ii]])) {
      ulvls <- unique(a$cogDatConn[[nms[ii]]])
      state$filter[[ii]] <- list(name = nms[ii], type = "regex", varType = "factor",
        regex = state$filter[[ii]]$regex,
        value = ulvls[grepl(state$filter[[ii]]$regex, ulvls, ignore.case = TRUE)])
    } else if ("select" %in% names(state$filter[[ii]])) {
      state$filter[[ii]] <- list(name = nms[ii], type = "select", varType = "factor",
        value = I(state$filter[[ii]]$select))
    } else if ("from" %in% names(state$filter[[ii]])) {
      state$filter[[ii]] <- list(name = nms[ii], type = "range", varType = "numeric",
        value = state$filter[[ii]])
    }
  }
  class(state) <- "list"
  state$labels <- I(state$labels)
  names(state$sort) <- NULL
  # names(state$filter) <- NULL
  state
}

updateCogInfo <- function(a) {
  tmp <- lapply(seq_len(nrow(a$cogInfo)), function(i) {
    res <- as.list(a$cogInfo[i, ])
    if (res$type == "factor" && length(unique(a$cogDatConn[[res$name]])) <= 5000) {
      res$levels <- unique(a$cogDatConn[[res$name]])
      res$levels[is.na(res$levels)] <- "NA"
    }
    # can add range for numeric, etc.
    if (res$type %in% c("numeric", "integer")) {
      res$range <- range(a$cogDatConn[[res$name]], na.rm = TRUE)
      res$nnna <- length(which(!is.na(a$cogDatConn[[res$name]])))
      res$breaks <- pretty(res$range, log2(res$nnna) + 1)
      res$delta <- diff(res$breaks[1:2])
      res$type <- "numeric"
    }
    # don't want to enable filter on singleton cogs
    if (length(unique(a$cogDatConn[[res$name]])) == 1) {
      res$filterable <- FALSE
    }
    res
  })
  names(tmp) <- sapply(tmp, function(x) x$name)
  tmp
}

#' @importFrom grDevices nclass.Sturges
getCogDistnsConvert <- function(cogDat) {
  cogDistns <- lapply(cogDat, function(x) {
    type <- attr(x, "cogAttrs")$type
    if (is.null(type) || is.na(type))
      type <- ifelse(is.numeric(x), "numeric", "factor")
    res <- list(
      type = type,
      dist = NULL
    )

    if (type == "factor") {
      res$has_dist <- FALSE
      if (length(unique(x)) <= 5000) {
        x[is.na(x)] <- "NA"
        tmp <- table(x)
        tmp <- tmp[order(tmp, decreasing = TRUE)]
        res$dist <- as.list(tmp)
        res$has_dist <- TRUE
        res$max <- as.numeric(max(tmp))
      }
    } else if (type %in% c("numeric", "integer")) {
      type <- "numeric"
      n0 <- length(which(x == 0))
      skw <- DistributionUtils::skewness(x, na.rm = TRUE)
      log <- FALSE

      hst <- hist(x, plot = FALSE)
      res <- list(
        type = type,
        dist = list(
          raw = list(
            breaks = hst$breaks,
            freq = hst$counts
          )
        )
      )
      res$log_default <- FALSE
      if (!is.nan(skw) && skw > 1.5 && all(x >= 0, na.rm = TRUE)) {
        log <- TRUE
        x <- x[x > 0]
        x2 <- log10(x)
        rng <- range(x2, na.rm = TRUE)
        brks <- 10 ^ seq(rng[1], rng[2], length = grDevices::nclass.Sturges(x))
        lhst <- hist(x, breaks = brks, plot = FALSE)
        res$dist$log <- list(
          breaks = hst$breaks,
          freq = hst$counts
        )
        res$log_default <- TRUE
      }
    }
    res
  })

  names(cogDistns) <- names(cogDat)
  cogDistns
}
