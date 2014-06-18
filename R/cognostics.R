
#' Compute RMSE of Loess Fit Cognostic
#'
#' Compute RMSE of loess fit as a cognostic to be used as cognostics in a trelliscope display.
#'
#' @param \ldots arguments to be passed to \code{link{loess}}, such as the formula, data, smoothing parameters, etc.
#' @param desc description of cognostic
#'
#' @author Ryan Hafen
#' @seealso \code{\link{cog}}
#' @examples
#' cogLoessRMSE(dist ~ speed, span = 0.5, data = cars)
#' @export
cogLoessRMSE <- function(..., desc = "RMSE of residuals from loess fit") {
   suppressWarnings(tmp <- try(loess(...), silent = TRUE))
   if(inherits(tmp, "try-error"))
      return(NA)
   cog(tmp$s, desc = desc, type = "num")
}

#' Compute Range Cognostic
#'
#' Compute range to be used as cognostics in a trelliscope display.
#'
#' @param x numeric vector from which to compute the range
#' @param desc description of cognostic
#'
#' @author Ryan Hafen
#' @seealso \code{\link{cog}}
#' @examples
#' cogRange(rnorm(100))
#' @export
cogRange <- function(x, desc = "range (max - min)") {
   res <- suppressWarnings(diff(range(x, na.rm = TRUE)))
   if(is.infinite(res))
      res <- NA
   cog(res, desc = desc, type = "num")
}

#' Compute Mean Cognostic
#'
#' Compute mean to be used as cognostics in a trelliscope display.
#'
#' @param x numeric vector from which to compute the mean
#' @param desc description of cognostic
#'
#' @author Ryan Hafen
#' @seealso \code{\link{cog}}
#' @examples
#' cogMean(rnorm(100))
#' @export
cogMean <- function(x, desc = "mean") {
   res <- suppressWarnings(mean(x, na.rm = TRUE))
   if(is.infinite(res))
      res <- NA
   cog(res, desc = desc, type = "num")
}

#' Compute Scagnostics
#'
#' Compute list of scagnostics (see \code{\link[scagnostics]{scagnostics}}) to be used as cognostics in a trelliscope display.
#'
#' @param x,y
#' @param desc description of cognostic
#'
#' @author Ryan Hafen
#' @seealso \code{\link{cog}}
#' @examples
#' cogScagnostics(cars$dist, cars$speed)
#' @export
cogScagnostics <- function(x, y) {
   suppressMessages(require(scagnostics))
   tmp <- try(scagnostics(x, y), silent = TRUE)
   if(inherits(tmp, "try-error")) {
      # make a data.frame of NA
      res <- scagnostics(1:10, 1:10)
      res <- as.data.frame(t(as.matrix(res)))
      res[1,] <- NA
      res$cor <- NA
   } else {
      res <- data.frame(t(as.matrix(tmp)))
   }
   names(res)[9] <- "Monoton" # so it's not too wide in cog table
   list(
      outly   = cog(res[1] , type = "num",
         desc = "Proportion of the total edge length due to extremely long edges connected to points of single degree"),
      skew     = cog(res[2] , type = "num",
         desc  = "Ratio of quantiles of edge lengths"),
      clumpy   = cog(res[3] , type = "num",
         desc  = "A runt-based measure that emphasizes clusters with small intra-cluster distances relative to the length of their connecting edge"),
      sparse   = cog(res[4] , type = "num",
         desc  = "Measures whether points in a 2D scatterplot are confined to a lattice or a small number of locations on the plane"),
      striated = cog(res[5] , type = "num",
         desc  = "Measure of coherence"),
      convex   = cog(res[6] , type = "num",
         desc  = "Ratio of the area of the alpha hull and the area of the convex hull"),
      skinny   = cog(res[7] , type = "num",
         desc  = "Ratio of perimeter to area of a polygon -- roughly, how skinny it is. A circle yields a value of 0, a square yields 0.12 and a skinny polygon yields a value near one."),
      stringy  = cog(res[8] , type = "num",
         desc  = "A stringy shape is a skinny shape with no branches"),
      monoton  = cog(res[9] , type = "num",
         desc  = "Squared Spearman correlation coefficient")
   )
}


# TODO: document example
#' Create a Cognostics Object
#'
#' Create a cognostics object.  To be used inside of the function passed to the \code{cogFn} argument of \code{\link{makeDisplay}} for each cognostics value to be computed for each subset.
#'
#' @param val a scalar value (numeric, characer, date, etc.)
#' @param desc a description for this cognostic value
#' @param the desired type of cognostic you would like to compute (see details)
#'
#' @return object of class "cog"
#'
#' @details Different types of cognostics can be specified through the \code{type} argument that will effect how the user is able to interact with those cognostics in the viewer.  This can usually be ignored because it will be inferred from the implicit data type of \code{val}.  But there are special types of cognostics, such as geographic coordinates and relations (not implemented) that can be specified as well.  Current possibilities for \code{type} are "key", "int", "num", "fac", "date", "time", "geo", "rel", "hier".
#'
#' @author Ryan Hafen
#'
#' @seealso \code{\link{makeDisplay}}, \code{\link{cogRange}}, \code{\link{cogMean}}, \code{\link{cogScagnostics}}, \code{\link{cogLoessRMSE}}
#'
#' @export
cog <- function(val = NULL, desc = "", type = NULL) {
   cogTypes <- list(
      key  = as.character,
      int  = as.integer  ,
      num  = as.numeric  ,
      fac  = as.character,
      date = as.Date     ,
      time = as.POSIXct  ,
      geo  = as.cogGeo   ,
      rel  = as.cogRel   ,
      hier = as.cogHier  ,
      href = as.cogHref
   )

   types <- names(cogTypes)

   if(!is.null(type)) {
      if(!type %in% types)
         stop("Invalid cognostics type: ", type)

      val <- try(cogTypes[[type]](val))
      if(inherits(val, "try-error"))
         val <- NA
   } else { # try to infer type
      if(is.factor(val))
         val <- as.character(val)

      if(!(is.character(val) || is.numeric(val) || inherits(val, "Date") || inherits(val, "POSIXct")))
         val <- NA
   }

   attr(val, "desc") <- desc
   class(val) <- c("cog", class(val))
   val
}

#' @export
print.cog <- function(x, ...) {
   attr(x, "desc") <- NULL
   class(x) <- setdiff(class(x), "cog")
   print(x)
}

#' Apply Cognostics Function to a Key-Value Pair
#'
#' Apply cognostics function to a key-value pair
#'
#' @param cogFn cognostics function
#' @param kvSubset key-value pair
#'
#' @author Ryan Hafen
#' @seealso \code{\link{cog}}, \code{\link{makeDisplay}}
#' @export
applyCogFn <- function(cogFn, kvSubset, conn) {
   if(inherits(conn, "localDiskConn")) {
      panelKey <- conn$fileHashFn(list(kvSubset[[1]]), conn)
   } else {
      panelKey <- digest(kvSubset[[1]])
   }
   res <- list(
      panelKey = cog(panelKey, desc = "panel key", type = "key")
   )
   splitVars <- getSplitVars(kvSubset)
   if(!is.null(splitVars)) {
      res$splitVars <- splitVars
   }
   bsvs <- getBsvs(kvSubset)
   if(!is.null(bsvs)) {
      res$bsv <- bsvs
   }
   if(!is.null(cogFn))
      res$cog <- kvApply(cogFn, kvSubset)

   res
}

## internal

## some special cognostics, such as relations, need to be concatenated to a comma-separated string if we are storing them as a data.frame
cog2df <- function(x) {
   # TODO: when class(x[[i]]) == "cogRel", first concatenate
   # TODO: make sure it is 1 row
   data.frame(as.list(c(panelKey = x$panelKey, x$splitVars, x$bsv, x$cog)), stringsAsFactors = FALSE)
}

as.cogGeo <- function(x) {
   x <- x[1:2]
   names(x) <- c("lat", "lon")
   class(x) <- c("cogGeo", "list")
   x
}

as.cogRel <- function(x) {
   class(x) <- c("cogRel")
   x
}

as.cogHref <- function(x) {
   paste("<a href=\"", x$href, "\">", x$label, ">")
}

as.cogHier <- function(x) {
   stop("not implemented...")
}

cogFlatten <- function(x) {
   if(inherits(x, "cogRel"))
      return(paste(x, collapse = ","))
   x
}

getCogDesc <- function(x, df = TRUE) {
   getDesc <- function(a) {
      tmp <- attr(a, "desc")
      ifelse(is.null(tmp), "", tmp)
   }
   getType <- function(a) {
      lapply(a, function(x) setdiff(class(x), "cog"))
   }

   pk <- data.frame(type = "panelKey", name = "panelKey", desc = "panel key", dataType = "panelKey", stringsAsFactors = FALSE)
   sv <- if(!is.null(x$splitVars)) {
      tmp <- lapply(x$splitVars, function(x) "conditioning variable")
      data.frame(type = "splitVar", name = names(tmp), desc = unlist(tmp), dataType = "character", stringsAsFactors = FALSE)
   } else {
      NULL
   }
   bsvs <- if(!is.null(x$bsv)) {
      descs <- lapply(x$bsv, getDesc)
      types <- getType(x$bsv)
      data.frame(type = "bsv", name = names(descs), desc = unlist(descs), dataType = unlist(types), stringsAsFactors = FALSE)
   } else {
      NULL
   }
   cogs <- if(!is.null(x$cog)) {
      descs <- lapply(x$cog, getDesc)
      types <- getType(x$cog)
      data.frame(type = "cog", name = names(descs), desc = unlist(descs), dataType = unlist(types), stringsAsFactors = FALSE)
   } else {
      NULL
   }

   res <- rbind(
      pk, sv, bsvs, cogs
   )
   rownames(res) <- NULL
   class(res) <- c("cogDesc", "data.frame")
   res
}

getCognostics <- function(data, cogFn, splitKeys = NULL) {
   if(is.null(splitKeys))
      splitKeys <- names(data)

   isCondDiv <- data$divBy$type == "condDiv"
   lapply(seq_along(data), function(ii) {
      getCognosticsSub(data[[ii]], cogFn, isCondDiv, splitKeys[[ii]])
   })
}

getCogInfo <- function(x, cogDesc) {
   cogDesc <- subset(cogDesc, type != "panelKey")
   res <- lapply(seq_len(nrow(cogDesc)), function(i) {
      curRow <- cogDesc[i,]

      if(curRow$dataType %in% c("character", "factor")) {
         res <- getCogCatPlotData(x, curRow$name)
         return(list(
            name = curRow$name,
            type = "character",
            n = res$n,
            marginal = res$freq
         ))
      } else {
         return(list(
            name = curRow$name,
            type = "numeric",
            marginal = getCogQuantPlotData(x, curRow$name, type = c("hist", "quant"))
         ))
      }
   })
   names(res) <- sapply(res, function(x) x$name)
   class(res) <- c("cogInfo", "list")
   res
}

