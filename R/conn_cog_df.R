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
   structure(list(), class=c("dfCogConn", "cogConn"))
}

#' @S3method print dfCogConn
print.dfCogConn <- function(x, ...) {
   cat("dfCogConn object")
}

#' @S3method cogPre dfCogConn
cogPre.dfCogConn <- function(cogConn, ...) {
   # do nothing
   NULL
}

#' @S3method cogEmit dfCogConn
cogEmit.dfCogConn <- function(cogConn, data, ...) {
   # add to mongo collection
   collect("TRS___cog", do.call(rbind, lapply(data, cog2df)))
}

#' @S3method cogCollect dfCogConn
cogCollect.dfCogConn <- function(cogConn, res, newValues, ...) {
   # rbind things
   rbind(res, data.frame(rbindlist(newValues)))
}

#' @S3method cogFinal dfCogConn
cogFinal.dfCogConn <- function(cogConn, jobRes, ...) {
   # grab cognostics from mr job result
   jobRes[["TRS___cog"]][[2]]
}

############################################################################
### dfCogDatConn (data.frame) constructor / methods
############################################################################

#' @S3method cogNcol data.frame
cogNcol.data.frame <- function(x) {
   ncol(x)
}

#' @S3method cogNrow data.frame
cogNrow.data.frame <- function(x) {
   nrow(x)
}

#' @S3method cogNames data.frame
cogNames.data.frame <- function(x) {
   names(x)            
}

#' @S3method getCogData data.frame
getCogData.data.frame <- function(x, rowIdx, colIdx) {
   x[rowIdx, colIdx, drop=FALSE]
}

#' @S3method getCurCogDat data.frame
getCurCogDat.data.frame <- function(cogDF, flt, ordering, colIndex, verbose=FALSE) {
   filterIndex <- seq_len(cogNrow(cogDF))
   
   if(!is.null(flt)) {
      logMsg("Updating cognostic filter index", verbose=verbose)
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
   logMsg("Updating cognostic sort index", verbose=verbose)
   cogDF <- cogDF[filterIndex,, drop=FALSE]
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
            orderIndex <- order(cogDF[,colIndex[ind],drop=FALSE], decreasing=ifelse(ordering[ind] < 0, TRUE, FALSE))
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
                                          return(-xtfrm(cogDF[,colIndex[orderCols[i]], drop=FALSE]))
               } else {
                  return(cogDF[,colIndex[orderCols[i]], drop=FALSE])
               }
            })
            orderIndex <- do.call(order, orderCols)               
         }
      }
   }
   # orderIndex <- filterIndex[orderIndex]
   logMsg("Retrieving sorted and filtered cognostics data", verbose=verbose)
   return(cogDF[orderIndex,,drop=FALSE])
}


