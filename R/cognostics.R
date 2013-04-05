
#' @export
cogLoessRMSE <- function(x, y, ...) {
   tmp <- try(loess(y ~ x, ...))
   if(inherits(tmp, "try-error"))
      return(NA)
   tmp$s
}

#' @export
cogRange <- function(x) {
   res <- suppressWarnings(diff(range(x, na.rm=TRUE)))
   if(is.infinite(res))
      res <- NA
   res
}

#' @export
cogMean <- function(x) {
   res <- suppressWarnings(mean(x, na.rm=TRUE))
   if(is.infinite(res))
      res <- NA
   res
}

#' @export
cogScagnostics <- function(x, y) {
   require(scagnostics)
   tmp <- try(scagnostics(x, y))
   if(inherits(tmp, "try-error")) {
      # make a data.frame of NA
      res <- scagnostics(1:10, 1:10)
      res <- data.frame(t(as.matrix(res)))
      res[1,] <- NA
      res$cor <- NA
   } else {
      res <- data.frame(t(as.matrix(tmp)), cor=cor(x, y, use="complete.obs"))
   }
   names(res)[9] <- "Monoton" # so it's not too wide in cog table
   res
}


