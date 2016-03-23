############################################################################
### cogConn methods
############################################################################

# executed prior to mr job
#' Methods Used in MapReduce for makeDisplay
#'
#' @param cogConn,conn,group,name,\ldots TODO
#' @export
#' @examples
#' # used internally when calling makeDisplay
#' @rdname mr-methods
cogPre <- function(cogConn, conn, group, name, ...)
  UseMethod("cogPre")

# in the map, how to emit records
#' Methods Used in MapReduce for makeDisplay
#'
#' @export
#' @rdname mr-methods
cogEmit <- function(cogConn, ...)
  UseMethod("cogEmit")

# in the reduce, how to collate results
#' Methods Used in MapReduce for makeDisplay
#'
#' @export
#' @rdname mr-methods
cogCollect <- function(cogConn, ...)
  UseMethod("cogCollect")

# after mr job, final steps
#' Methods Used in MapReduce for makeDisplay
#'
#' @export
#' @rdname mr-methods
cogFinal <- function(cogConn, ...)
  UseMethod("cogFinal")

############################################################################
### cogDatConn methods
############################################################################

#' Methods for Cognostics Connections
#'
#' @param x cognostics connection object
#' @param ... other objects passed onto generic methods
#' @param rowIdx index of rows to be retrieved from the cognostics connection
#' @param colIdx index of columns to be retrieved from the cognostics connection
#' @note These methods are used mainly by the trelliscope viewer and therefore must be exported.  Their purpose is to provide a general interface for a cognostics store.  Currently just data frames are used for cognostics, but in previous versions systems like MongoDB were used.  These methods should never need to be used by an analyst.
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
getCogData <- function(x, rowIdx, colIdx, ...)
  UseMethod("getCogData")

### for cogDistns

getCogQuantPlotData <- function(x, ...) {
  UseMethod("getCogQuantPlotData", x)
}

getCogCatPlotData <- function(x, ...) {
  UseMethod("getCogCatPlotData", x)
}


############################################################################
### misc functions used inside cog conn methods
############################################################################

processFilterInput <- function(flt) {
  # flt is a vector of 3-tuples - (filter type, filter column, filter value)
  # see getColumFilterInputs in table.js

  n <- length(flt)
  if(n == 0 || !((n %% 3) == 0)) {
    ind <- NULL
  } else {
    # get index for filters that are NULL
    ind <- which(!sapply(flt[seq(3, n, by = 3)], function(x) is.null(x) | x == ""))
  }
  # remove those ones
  if(length(ind) == 0) {
    flt <- NULL
  } else {
    flt <- lapply(ind, function(x) flt[((x - 1)*3 + 1):(x*3)])
  }
  flt
}
