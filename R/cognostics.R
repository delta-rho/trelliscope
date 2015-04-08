if(getRversion() >= "2.15.1") {
  utils::globalVariables(c("type"))
}

#' Compute RMSE of Loess Fit Cognostic
#'
#' Compute RMSE of loess fit as a cognostic to be used as cognostics in a trelliscope display.
#'
#' @param \ldots arguments to be passed to \code{link{loess}}, such as the formula, data, smoothing parameters, etc.
#' @param desc,group,defLabel,defActive,filterable arguments passed to \code{\link{cog}}
#'
#' @author Ryan Hafen
#' @seealso \code{\link{cog}}
#' @examples
#' cogLoessRMSE(dist ~ speed, span = 0.5, data = cars)
#' @export
cogLoessRMSE <- function(..., desc = "RMSE of residuals from loess fit", group = "common", defLabel = FALSE, defActive = TRUE, filterable = TRUE) {
   suppressWarnings(tmp <- try(loess(...), silent = TRUE))
   if(inherits(tmp, "try-error"))
      return(NA)
   cog(tmp$s, desc = desc, type = "numeric", group = group, defLabel = defLabel, defActive = defActive, filterable = filterable)
}

#' Compute Range Cognostic
#'
#' Compute range to be used as cognostics in a trelliscope display.
#'
#' @param x numeric vector from which to compute the range
#' @param desc,group,defLabel,defActive,filterable arguments passed to \code{\link{cog}}
#'
#' @author Ryan Hafen
#' @seealso \code{\link{cog}}
#' @examples
#' cogRange(rnorm(100))
#' @export
cogRange <- function(x, desc = "range (max - min)", group = "common", defLabel = FALSE, defActive = TRUE, filterable = TRUE) {
   res <- suppressWarnings(diff(range(x, na.rm = TRUE)))
   if(is.infinite(res))
      res <- NA
   cog(res, desc = desc, type = "numeric", group = group, defLabel = defLabel, defActive = defActive, filterable = filterable)
}

#' Compute Mean Cognostic
#'
#' Compute mean to be used as cognostics in a trelliscope display.
#'
#' @param x numeric vector from which to compute the mean
#' @param desc,group,defLabel,defActive,filterable arguments passed to \code{\link{cog}}
#' @author Ryan Hafen
#' @seealso \code{\link{cog}}
#' @examples
#' cogMean(rnorm(100))
#' @export
cogMean <- function(x, desc = "mean", group = "common", defLabel = FALSE, defActive = TRUE, filterable = TRUE) {
   res <- suppressWarnings(mean(x, na.rm = TRUE))
   if(is.infinite(res))
      res <- NA
   cog(res, desc = desc, type = "numeric", group = group, defLabel = defLabel, defActive = defActive, filterable = filterable)
}

#' Href Cognostic
#'
#' Create href to be used as cognostics in a trelliscope display.
#'
#' @param x URL to link to
#' @param label label of the href
#' @param target value to be used for the \code{target} attribute of the \code{a} html tag - default is "_blank" which will open the link in a new window
#' @param desc,group,defLabel,defActive,filterable arguments passed to \code{\link{cog}}
#'
#' @author Ryan Hafen
#' @seealso \code{\link{cog}}
#' @examples
#' cogHref("www.google.com")
#' @export
cogHref <- function(x, label = "link", desc = "link", group = "common", target = "_blank", defLabel = FALSE, defActive = FALSE, filterable = FALSE) {
   cog(paste("<a href=\"", x, "\" target=\"", target, "\">", label, "</a>", sep = ""), type = "href", desc = desc, group = group, defLabel = defLabel, defActive = defActive, filterable = filterable)
}


#' DisplayHref Cognostic
#'
#' Create href that points to another trelliscope display with optional state
#'
#' @param displayName the name of the display
#' @param displayGroup the group the display belongs to
#' @param state if specified, this tells the viewer the default parameter settings (such as layout, sorting, filtering, etc.) to use when the display is viewed (see \code{\link{validateState}} for details)
#' @param label label of the href
#' @param target value to be used for the \code{target} attribute of the \code{a} html tag - default is "_blank" which will open the link in a new window
#' @param desc,group,defLabel,defActive,filterable arguments passed to \code{\link{cog}}
#'
#' @return a hash string
#'
#' @author Ryan Hafen
#'
#' @seealso \code{\link{validateState}}, \code{\link{cogHref}}
#' @export
cogDisplayHref <- function(displayName, displayGroup = "common", state = NULL, label = "link", desc = "display link", group = "common", target = "_blank", defLabel = FALSE, defActive = FALSE, filterable = FALSE) {

   state <- validateState(state, displayName, displayGroup)
   x <- makeStateHash(state, displayName, displayGroup)

   cog(paste("<a href=\"#", x, "\" target=\"", target, "\">", label, "</a>", sep = ""), type = "href", desc = desc, group = group, defLabel = defLabel, defActive = defActive, filterable = filterable)
}


#' Compute Scagnostics
#'
#' Compute list of scagnostics (see \code{\link[scagnostics]{scagnostics}}) to be used as cognostics in a trelliscope display.
#'
#' @param x vector of the x-axis data for a scatterplot
#' @param y vector of the y-axis data for a scatterplot
#' @param group,defLabel,defActive,filterable arguments passed to \code{\link{cog}}
#'
#' @author Ryan Hafen
#' @seealso \code{\link{cog}}
#' @examples
#' cogScagnostics(iris$Sepal.Length, iris$Sepal.Width)
#' @export
cogScagnostics <- function(x, y, group = "scagnostics", defLabel = FALSE, defActive = TRUE, filterable = TRUE) {

   if (!requireNamespace("scagnostics", quietly = TRUE)) {
      stop("Package 'scagnostics' is needed for this function to work. Please install it.",
      call. = FALSE)
   }

   tmp <- try(scagnostics::scagnostics.default(x, y), silent = TRUE)
   if(inherits(tmp, "try-error")) {
      # make a data.frame of NA
      res <- scagnostics::scagnostics.default(1:10, 1:10)
      res <- as.data.frame(t(as.matrix(res)))
      res[1,] <- NA
      res$cor <- NA
   } else {
      res <- data.frame(t(as.matrix(tmp)))
   }
   names(res)[9] <- "Monoton" # so it's not too wide in cog table
   list(
      outly   = cog(res[1] , type = "numeric",
         desc = "Proportion of the total edge length due to extremely long edges connected to points of single degree", group = group, defLabel = defLabel, defActive = defActive, filterable = filterable),
      skew     = cog(res[2] , type = "numeric",
         desc  = "Ratio of quantiles of edge lengths", group = group, defLabel = defLabel, defActive = defActive, filterable = filterable),
      clumpy   = cog(res[3] , type = "numeric",
         desc  = "A runt-based measure that emphasizes clusters with small intra-cluster distances relative to the length of their connecting edge", group = group, defLabel = defLabel, defActive = defActive, filterable = filterable),
      sparse   = cog(res[4] , type = "numeric",
         desc  = "Measures whether points in a 2D scatterplot are confined to a lattice or a small number of locations on the plane", group = group, defLabel = defLabel, defActive = defActive, filterable = filterable),
      striated = cog(res[5] , type = "numeric",
         desc  = "Measure of coherence", group = group, defLabel = defLabel, defActive = defActive, filterable = filterable),
      convex   = cog(res[6] , type = "numeric",
         desc  = "Ratio of the area of the alpha hull and the area of the convex hull", group = group, defLabel = defLabel, defActive = defActive, filterable = filterable),
      skinny   = cog(res[7] , type = "numeric",
         desc  = "Ratio of perimeter to area of a polygon -- roughly, how skinny it is. A circle yields a value of 0, a square yields 0.12 and a skinny polygon yields a value near one.", group = group, defLabel = defLabel, defActive = defActive, filterable = filterable),
      stringy  = cog(res[8] , type = "numeric",
         desc  = "A stringy shape is a skinny shape with no branches", group = group, defLabel = defLabel, defActive = defActive, filterable = filterable),
      monoton  = cog(res[9] , type = "numeric",
         desc  = "Squared Spearman correlation coefficient", group = group, defLabel = defLabel, defActive = defActive, filterable = filterable)
   )
}

#' Create a Cognostics Object
#'
#' Create a cognostics object.  To be used inside of the function passed to the \code{cogFn} argument of \code{\link{makeDisplay}} for each cognostics value to be computed for each subset.
#'
#' @param val a scalar value (numeric, characer, date, etc.)
#' @param desc a description for this cognostic value
#' @param group optional categorization of the cognostic for organizational purposes
#' @param type the desired type of cognostic you would like to compute (see details)
#' @param defLabel should this cognostic be used as a panel label in the viewer by default?
#' @param defActive should this cognostic be active (available for sort / filter / sample) by default?
#' @param filterable should this cognostic be filterable?  Default is \code{TRUE}.  It can be useful to set this to \code{FALSE} if the cognostic is categorical with many unique values and is only desired to be used as a panel label.
#'
#' @return object of class "cog"
#'
#' @details Different types of cognostics can be specified through the \code{type} argument that will affect how the user is able to interact with those cognostics in the viewer.  This can usually be ignored because it will be inferred from the implicit data type of \code{val}.  But there are special types of cognostics, such as geographic coordinates and relations (not implemented) that can be specified as well.  Current possibilities for \code{type} are "key", "integer", "numeric", "factor", "date", "time", "geo", "rel", "hier", "href".
#'
#' @author Ryan Hafen
#'
#' @seealso \code{\link{makeDisplay}}, \code{\link{cogRange}}, \code{\link{cogMean}}, \code{\link{cogScagnostics}}, \code{\link{cogLoessRMSE}}
#'
#' @export
cog <- function(val = NULL, desc = "", group = "common", type = NULL, defLabel = FALSE, defActive = TRUE, filterable = TRUE) {

   cogTypes <- list(
      key      = as.character,
      integer  = as.integer  ,
      numeric  = as.numeric  ,
      factor   = as.character,
      date     = as.Date     ,
      time     = as.POSIXct  ,
      geo      = as.cogGeo   ,
      rel      = as.cogRel   ,
      hier     = as.cogHier  ,
      href     = as.cogHref
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

      if(is.character(val)) {
         type <- "factor"
      } else if(is.numeric(val)) {
         type <- "numeric"
      } else if(inherits(val, "Date")) {
         type <- "date"
      } else if(inherits(val, "POSIXct")) {
         type <- "time"
      } else {
         val <- NA
         type <- NA
      }
   }

   cogAttrs <- list(
      desc = desc,
      type = type,
      group = group,
      defLabel = defLabel,
      defActive = defActive,
      filterable = filterable
   )
   attr(val, "cogAttrs") <- cogAttrs

   class(val) <- c("cog", class(val))
   val
}

#' @export
print.cog <- function(x, ...) {
   attr(x, "cogAttrs") <- NULL
   class(x) <- setdiff(class(x), "cog")
   print(x)
}

#' Apply Cognostics Function to a Key-Value Pair
#'
#' Apply cognostics function to a key-value pair
#'
#' @param cogFn cognostics function
#' @param kvSubset key-value pair
#' @param conn TODO
#'
#' @author Ryan Hafen
#' @seealso \code{\link{cog}}, \code{\link{makeDisplay}}
#' @export
applyCogFn <- function(cogFn, kvSubset, conn) {
   res <- list()
   if(inherits(conn, "localDiskConn")) {
      panelKey <- conn$fileHashFn(list(kvSubset[[1]]), conn)
   } else {
      panelKey <- digest(kvSubset[[1]])
   }
   res$panelKey <- cog(panelKey, desc = "panel key", type = "key", group = "panelKey", defActive = TRUE, filterable = FALSE)
   splitVars <- getSplitVars(kvSubset)
   if(!is.null(splitVars)) {
      nms <- names(splitVars)
      for(i in seq_along(splitVars)) {
         res[[nms[i]]] <- cog(splitVars[[i]], desc = "conditioning variable", type = "factor", group="condVar", defLabel = TRUE)
      }
   }
   bsvs <- getBsvs(kvSubset)
   if(!is.null(bsvs)) {
      nms <- names(bsvs)
      # TODO: get bsvInfo so we can get bsv description
      for(i in seq_along(splitVars)) {
         res[[nms[i]]] <- cog(bsvs[[i]], desc = "bsv")
      }
   }
   if(!is.null(cogFn))
      res <- c(res, kvApply(cogFn, kvSubset))

   res
}

## internal

## some special cognostics, such as relations, need to be concatenated to a comma-separated string if we are storing them as a data.frame
cog2df <- function(x) {

   # TODO: when class(x[[i]]) == "cogRel", first concatenate
   # data.frame(as.list(c(panelKey = x$panelKey, x$splitVars, x$bsv, x$cog)), stringsAsFactors = FALSE)

   # Try to coerce to data frame.
   out <- try(data.frame(x, stringsAsFactors = FALSE), silent = TRUE)

   # Assume it doesn't fail
   fail <- FALSE

   # If the try() doesn't fail, test the number of rows
   if(!inherits(out, "try-error")) {
      if(nrow(out) != 1) {
         fail <- TRUE
      }
   # Otherwise the try() failed
   } else {
     fail <- TRUE
   }

   # If failure has occured:
   if(fail) {

      # Construct the error message
      e <- simpleError("'Could not coerce the following object generated by 'cogFn' to a 1-row data frame:")
      final <- "Note that each each cognostic should be a scalar value (of length 1).\n"

      # This trick allows us to print the 'x' after the stop is issued
      tryCatch(stop(e), finally = eval(expression({print(x); cat(final)})))

   }

   return(out)

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
   if(is.character(x)) {
      if(grepl("^<a href=", tolower(x)))
         return(x)
      x <- list(href = x)
   }
   if(is.null(x$label))
      x$label <- "link"
   if(is.null(x$target))
      x$target <- "_blank"
   paste("<a href=\"", x$href, "\" target=\"", x$target, "\">", x$label, "</a>", sep = "")
}

as.cogHier <- function(x) {
   stop("not implemented...")
}

cogFlatten <- function(x) {
   if(inherits(x, "cogRel"))
      return(paste(x, collapse = ","))
   x
}

getCogInfo <- function(x, df = TRUE) {
   nms <- names(x)
   res <- do.call(rbind, lapply(seq_along(x), function(i) {
      if(!inherits(x[[i]], "cog"))
         x[[i]] <- cog(x[[i]])
      tmp <- attr(x[[i]], "cogAttrs")
      data.frame(name = nms[i], tmp, stringsAsFactors = FALSE)
   }))

   class(res) <- c("cogInfo", "data.frame")
   res
}

# gets distribution of
getCogDistns <- function(x, cogInfo) {
   cogInfo <- subset(cogInfo, type != "panelKey")
   res <- lapply(seq_len(nrow(cogInfo)), function(i) {
      curRow <- cogInfo[i,]

      if(curRow$type %in% c("character", "factor")) {
         res <- getCogCatPlotData(x, curRow$name)
         return(list(
            name = curRow$name,
            type = "character",
            n = res$n,
            marginal = res$freq
         ))
      } else if(curRow$type %in% c("numeric", "integer")) {
         return(list(
            name = curRow$name,
            type = "numeric",
            marginal = getCogQuantPlotData(x, curRow$name, type = c("hist", "quant"))
         ))
      } else {
         return(list(
            name = curRow$name,
            type = NA,
            marginal = NA
         ))
      }
   })
   names(res) <- sapply(res, function(x) x$name)
   class(res) <- c("cogDistns", "list")
   res
}

