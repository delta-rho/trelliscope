############################################################################
### cogConn methods
############################################################################

# executed prior to mr job
#' Methods Used in MapReduce for makeDisplay
#'
#' @param obj object
#' @export
#' @rdname mr-methods
cogPre <- function(obj, ...)
   UseMethod("cogPre")

# in the map, how to emit records
#' Methods Used in MapReduce for makeDisplay
#'
#' @param obj object
#' @export
#' @rdname mr-methods
cogEmit <- function(obj, ...)
   UseMethod("cogEmit")

# in the reduce, how to collate results
#' Methods Used in MapReduce for makeDisplay
#'
#' @param obj object
#' @export
#' @rdname mr-methods
cogCollect <- function(obj, ...)
   UseMethod("cogCollect")

# after mr job, final steps
#' Methods Used in MapReduce for makeDisplay
#'
#' @param obj object
#' @export
#' @rdname mr-methods
cogFinal <- function(obj, ...)
   UseMethod("cogFinal")

############################################################################
### cogDatConn methods
############################################################################

#' Methods for Cognostics Connections
#' 
#' @param x object
#' @note These methods are used mainly by the trelliscope viewer and therefore must be exported.  They should never need to be used by an analyst.
#' @export
#' @rdname cogConn-methods
cogNcol <- function(x, ...)
   UseMethod("cogNcol")

#' @export
#' @rdname cogConn-methods
cogNrow <- function(x, ...)
   UseMethod("cogNrow")

#' @export
#' @rdname cogConn-methods
cogNames <- function(x, ...)
   UseMethod("cogNames")

#' @export
#' @rdname cogConn-methods
getCogData <- function(x, ...)
   UseMethod("getCogData")

#' @export
#' @rdname cogConn-methods
getCurCogDat <- function(x, ...)
   UseMethod("getCurCogDat")

############################################################################
### misc functions used inside cog conn methods
############################################################################

processFilterInput <- function(flt) {
   # flt is a vector of 3-tuples - (filter type, filter column, filter value)
   # see getColumFilterInputs in table.js
   
   n <- length(flt)
   # get index for filters that are NULL
   ind <- which(!sapply(flt[seq(3, n, by=3)], function(x) is.null(x) | x==""))
   # remove those ones
   if(length(ind) == 0) {
      flt <- NULL
   } else {
      flt <- lapply(ind, function(x) flt[((x - 1)*3 + 1):(x*3)])
   }
   flt
}
