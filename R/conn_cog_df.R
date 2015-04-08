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
oldGetCurCogDat.data.frame <- function(cogDF, flt, ordering, colIndex, verbose = FALSE, ...) {
   filterIndex <- seq_len(cogNrow(cogDF))

   if(!is.null(flt)) {
      logMsg("Updating cognostic filter index", verbose = verbose)
      flt <- processFilterInput(flt)

      for(i in seq_along(flt)) {
         cur <- flt[[i]]
         if(cur[1] == "from") {
            newIndex <- which(cogDF[,colIndex][[as.integer(cur[2])]] >= as.numeric(cur[3]))
         } else if(cur[1] == "to") {
            newIndex <- which(cogDF[,colIndex][[as.integer(cur[2])]] <= as.numeric(cur[3]))
         } else if(cur[1] == "regex") {
            # browser()
            newIndex <- try(which(grepl(cur[3], cogDF[,colIndex][[as.integer(cur[2])]])))
            if(inherits(newIndex, "try-error"))
               newIndex <- seq_len(cogNrow(cogDF))
         }
         filterIndex <- intersect(filterIndex, newIndex)
      }
   }

   # before ordering, perform any filters
   logMsg("Updating cognostic sort index", verbose = verbose)
   cogDF <- cogDF[filterIndex,, drop = FALSE]
   orderIndex <- seq_len(cogNrow(cogDF))
   # browser()
   # need to know which columns are visible so we are sorting the right column
   # get sort order and sort the table
   if(!is.null(ordering)) {
      if(any(ordering != 0)) {
         # if we are only sorting by one column
         # TODO: use data.table here for faster sorting
         if(sum(abs(ordering)) == 1) {
            ind <- which(abs(ordering) == 1)
            orderIndex <- order(cogDF[,colIndex[ind],drop = FALSE], decreasing = ifelse(ordering[ind] < 0, TRUE, FALSE))
            # if(ordering[ind] < 0) {
            #    orderIndex <- rev(cogDFOrd[,ind])
            # } else {
            #    orderIndex <- cogDFOrd[,ind]
            # }
         } else {
            nonZero <- which(ordering != 0)
            colOrder <- ordering[nonZero]
            orderCols <- nonZero[order(abs(colOrder))]
            orderSign <- sign(ordering)[orderCols]
            orderCols <- lapply(seq_along(orderCols), function(i) {
               if(orderSign[i] < 0) {
                  return(-xtfrm(cogDF[,colIndex[orderCols[i]], drop = FALSE]))
               } else {
                  return(cogDF[,colIndex[orderCols[i]], drop = FALSE])
               }
            })
            orderIndex <- do.call(order, orderCols)
         }
      }
   }
   # orderIndex <- filterIndex[orderIndex]
   logMsg("Retrieving sorted and filtered cognostics data", verbose = verbose)
   return(cogDF[orderIndex,,drop = FALSE])
}


#' @export
getCogQuantPlotData.data.frame <- function(cogDF, name, type = "hist", filter = NULL) {
   # TODO: add logic about number of breaks
   # TODO: make number of quantiles configurable
   dat <- cogDF[[name]]
   dat <- dat[!is.na(dat)]

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
         if(length(n) <= 1000) {
            qnt <- data.frame(f = seq(0, 1, length = n), q = sort(dat))
         } else {
            sq <- seq(0, 1, length = 1000)
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
   if(n <= 5000) {
      freq <- data.frame(xtabs(~ dat))
      names(freq)[1] <- "label"
      freq <- freq[order(freq$Freq, freq$label),]
   }
   list(n = n, freq = freq)
}

