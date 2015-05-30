if(getRversion() >= "2.15.1") {
  utils::globalVariables(c("collect", "logMsg"))
}


############################################################################
### dfCogConn constructor / methods
############################################################################

#' Initiate Data Frame Cognostics Connection
#'
#' Initiate data frame cognostics connection
#'
#' @return "cogConn" object of class "dfCogConn"
#'
#' @note This should never need to be called explicitly.  It is the default mechanism for storing cognostics in \code{\link{makeDisplay}}.
#'
#' @author Ryan Hafen
#' @seealso \code{\link{makeDisplay}}
#' @export
dfCogConn <- function() {
  structure(list(), class = c("dfCogConn", "cogConn"))
}

#' @export
print.dfCogConn <- function(x, ...) {
  cat("dfCogConn object\n")
}

#' @export
cogPre.dfCogConn <- function(cogConn, conn, group, name, ...) {
  # do nothing
  NULL
}

#' @export
cogEmit.dfCogConn <- function(cogConn, data, collect, ...) {
  collect("TRS___cog", do.call(rbind, lapply(data, cog2df)))
}

#' @export
cogCollect.dfCogConn <- function(cogConn, res, newValues, ...) {
  # rbind things
  rbind(res, data.frame(rbindlist(newValues)))
}

#' @export
cogFinal.dfCogConn <- function(cogConn, jobRes, ...) {
  # grab cognostics from mr job result
  jobRes[["TRS___cog"]][[2]]
}

############################################################################
### dfCogDatConn (data.frame) constructor / methods
############################################################################

#' @export
cogNcol.data.frame <- function(x, ...) {
  ncol(x)
}

#' @export
cogNrow.data.frame <- function(x, ...) {
  nrow(x)
}

#' @export
cogNames.data.frame <- function(x, ...) {
  names(x)
}

#' @export
getCogData.data.frame <- function(x, rowIdx, colIdx, ...) {
  x[rowIdx, colIdx, drop = FALSE]
}


#' @export
getCogQuantPlotData.data.frame <- function(cogDF, name, type = "hist", filter = NULL, cogInfo = NULL) {
  # TODO: add logic about number of breaks
  # TODO: make number of quantiles configurable
  dat <- cogDF[[name]]
  dat <- dat[!is.na(dat)]
  # handle log if specified
  if(!is.null(cogInfo$log)) {
    lg <- cogInfo[cogInfo$name == name, "log"]
    if(!is.na(lg))
      dat <- log(dat, base = lg)
  }

  res <- list()

  if("hist" %in% type) {
    if(length(dat) == 0) {
      res[["hist"]] <- data.frame(xdat = c(0, 1), ydat = c(0, 0))
    } else {
      hst <- hist(dat, plot = FALSE)
      res[["hist"]] <- data.frame(xdat = hst$breaks, ydat = c(hst$counts, 0))
    }
  }

  if("quant" %in% type) {
    n <- length(dat)
    if(length(dat) == 0) {
      res[["quant"]] <- data.frame(f = c(0, 1), q = c(0, 0))
    } else {
      # get quantiles
      if(n <= 200) {
        qnt <- data.frame(f = seq(0, 1, length = n), q = sort(dat))
      } else {
        sq <- seq(0, 1, length = 200)
        qnt <- data.frame(f = sq, q = quantile(dat, sq))
      }
      res[["quant"]] <- qnt
    }
  }

  if(length(type) == 1) {
    res[[1]]
  } else {
    res
  }
}

#' @export
getCogCatPlotData.data.frame <- function(cogDF, name, filter = NULL) {
  # TODO: make number of levels configurable
  dat <- as.character(cogDF[[name]])
  dat[is.na(dat)] <- "--missing--"
  n <- length(unique(dat))
  freq <- NULL
  if(n <= 1000) {
    freq <- data.frame(xtabs(~ dat))
    names(freq)[1] <- "label"
    freq <- freq[order(freq$Freq, freq$label),]
  }
  list(n = n, freq = freq)
}

