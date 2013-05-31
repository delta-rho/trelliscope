

cogNcol <- function(x) {
   if(inherits(x, "mongoCogConn")) {
      return(x$ncol)
   } else {
      return(ncol(x))
   }
}

cogNrow <- function(x) {
   if(inherits(x, "mongoCogConn")) {
      return(x$nr)
   } else {
      return(nrow(x))
   }
}

cogNames <- function(x) {
   if(inherits(x, "mongoCogConn")) {
      names(x$ex)
   } else {
      names(x)            
   }
}

getCogData <- function(x, rowIdx, colIdx) {
   if(inherits(x, "mongoCogConn")) {
      crs <- mongo.find(x$mongoConn, x$NS, skip=as.integer(rowIdx[1] - 1), sort=x$srt, query=x$qry)
      res <- list()
      for(i in seq_along(rowIdx)) {
         mongo.cursor.next(crs)
         tmp <- mongoCog2DF(mongo.bson.to.list(mongo.cursor.value(crs))[-1])
         res[[i]] <-
as.data.frame(tmp, stringsAsFactors=FALSE)
      }

      do.call(rbind, res)[, colIdx, drop=FALSE]
   } else {
      x[rowIdx, colIdx, drop=FALSE]
   }
}

mongoCogEx <- function(mongoConn, NS) {
   
}

mongoCog2DF <- function(x) {
   lns <- sapply(x, length)
   lns <- which(lns > 1)
   for(j in lns) {
      x[[j]] <- as.list(x[[j]])
   }
   x
}


