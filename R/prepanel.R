#' Prepanel Function for Trelliscope Displays
#'
#' Apply a prepanel function to objects of class "ddo" or "ddf" to determine ranges of x and y axis limits prior to creating a trelliscope display (\code{\link{makeDisplay}}).  Useful in conjunction with \code{\link{setLims}}.
#'
#' @param dat an object of class "localDiv" or "rhData"
#' @param prepanelFn a prepanel function that returns a list specifying \code{xlim} and \code{ylim} for determining axis limits, and optionally \code{dx} and \code{dy} for determining aspect ratio (used to define slopes of line segments used for banking computations).  prepanelFn can also be a panelFn (see \code{\link{makeDisplay}}) that returns either an object of class "trellis" or "ggplot", since xlim and ylim can be determined from these.
#' @param verbose print status messages?
#' @param control parameters specifying how the backend should handle things (most-likely parameters to \code{\link{rhwatch}} in RHIPE) - see \code{\link{rhipeControl}} and \code{\link{localDiskControl}}
#'
#' @return object of class "trsPre".  This is a list of the x and y axis ranges for each split, along with the aspect ratio banking value if \code{dx} and \code{dy} are supplied in \code{prepanelFn}.  Can be used with \code{\link{plot.trsPre}} and \code{\link{setLims}}.
#'
#' @details
#' The plot method plots the sorted axis ranges for the x and y axis for the case of "same" (all axis limits share the same range) and "sliced" (all axis limits share the) and can be useful in helping determine how to ultimately set the limits.
#'
#' You do not need to use \code{prepanel()} to ultimately create a display with \code{\link{makeDisplay()}}, but if you bypass, you will either need to specify your own limits in your plot command, or do nothing, in which case each individual plot will have limits based on the data in the split being plotted (the axes will be "free").
#'
#' Axis limits are very important.  What makes viewing groups of plots of subsets of data ("small multiples") so powerful is being able to make meaningful visual comparisons across plots.  This is much easier to do if scales for each plot are commensurate.
#'
#' This function is also useful for identifying subsets with very large outlying values, and in conjunction with \code{\link{setLims}}, allows you to account for that prior to the expensive process of creating all of the plots.
#'
#' @author Ryan Hafen
#'
#' @seealso \code{\link{x}}
#' \code{\link{plot.trsPre}}, \code{\link{setLims}}, \code{\link{makeDisplay}}
#'
#' @examples
#' \dontrun{
#' irisSplit <- divide(iris, "Species")
#' irisPreFn <- function(x) {
#'    list(
#'       xlim = range(x$Sepal.Length),
#'       ylim = range(x$Sepal.Width)
#'    )
#' }
#' irisPre <- prepanel(irisSplit, prepanelFn=irisPreFn)
#' plot(irisPre)#' }
#'
#' @export
prepanel <- function(data,
   prepanelFn = NULL,
   control = NULL,
   verbose = TRUE
) {
   banking <- function(dx, dy) {
      if (length(dx)!=length(dy)) return(NA)
      id <- dx!=0 & dy!=0 & !is.na(dx) & !is.na(dy)
      if (any(id)) {
         r  <- abs(dx[id]/dy[id])
         median(r)
      }
      else 1
   }
   prepanelFnIsTrellis <- FALSE
   prepanelFnIsGgplot <- FALSE
   doBanking <- TRUE

   if(verbose)
      message("Testing 'prepanelFn' on a subset...")
   p <- kvApply(prepanelFn, kvExample(data))
   if(inherits(p, "trellis")) {
      prepanelFnIsTrellis <- TRUE
      if(verbose)
         message("Using 'trellis' panelFn to determine limits... dx and dy will not be computed.")
      doBanking <- FALSE
   } else if(inherits(p, "ggplot")) {
      prepanelFnIsGgplot <- TRUE
      if(verbose)
         message("Using 'ggplot' panelFn to determine limits... dx and dy will not be computed.")
      doBanking <- FALSE
   } else {
      if(is.null(p$xlim) || is.null(p$ylim))
         stop("'prepanelFn' must either return an object of class 'trellis' or 'ggplot' or return a list with elements 'xlim' and 'ylim'.")
      if(is.null(p$dx) || is.null(p$dy)) {
         if(verbose)
            message("dx or dy (or both) were not specified - not computing banking.")
         doBanking <- FALSE
      }
   }

   map <- expression({
      for(i in seq_along(map.keys)) {
         k <- map.keys[[i]]
         r <- map.values[[i]]
         bnk <- NA

         if(prepanelFnIsTrellis) {
            # temporarily remove axis padding
            curOption <- lattice.getOption("axis.padding")$numeric
            lattice.options(axis.padding=list(numeric=0))
            p <- kvApply(prepanelFn, list(k, r))
            if(all(is.na(p$panel.args[[1]]$x)) || all(is.na(p$panel.args[[1]]$y))) {
               xr <- c(NA, NA)
               yr <- c(NA, NA)
            } else {
               if(is.list(p$x.limits) || is.list(p$y.limits))
                  stop("Either x or y scales for lattice prepanel function is 'sliced' or 'fill' - currently can't compute limits when these are set")
               xr <- p$x.limits
               yr <- p$y.limits
            }

            lattice.options(axis.padding=list(numeric=curOption))
            # # TODO: for ggplot:
            # a <- print(p) # need to not make it actually plot
            # a$panel$ranges[[1]]$x.range
            # a$panel$ranges[[1]]$y.range
         } else if(prepanelFnIsGgplot) {
            p <- kvApply(prepanelFn, list(k, r))
            gglims <- try(ggplot_build(p)$panel$ranges, silent=TRUE)

            if(length(gglims) == 1 && !inherits(gglims, "try-error")) {
               xr <- gglims[[1]]$x.range
               yr <- gglims[[1]]$y.range
            } else {
               xr <- c(NA, NA)
               yr <- c(NA, NA)
            }
         } else {
            pre <- kvApply(prepanelFn, list(k, r))
            xr <- pre$xlim
            yr <- pre$ylim

            if(doBanking) {
               dx <- pre$dx
               dy <- pre$dy
               bnk <- banking(dx, dy)
            }
         }

         collect("x", data.frame(
            key=digest(k),
            min=xr[1],
            max=xr[2],
            # med=median(x, na.rm=TRUE),
            bnk=bnk,
            stringsAsFactors=FALSE
         ))

         collect("y", data.frame(
            key=digest(k),
            min=yr[1],
            max=yr[2],
            # med=median(y, na.rm=TRUE),
            bnk=bnk,
            stringsAsFactors=FALSE
         ))
      }
   })

   # rbind the results
   reduce <- expression(
      pre = {
         res <- NULL
      },
      reduce = {
         res <- rbind(res, data.frame(rbindlist(reduce.values)))
      },
      post = {
         collect(reduce.key, res)
      }
   )

   parList <- list(
      prepanelFn = prepanelFn,
      prepanelFnIsTrellis = prepanelFnIsTrellis,
      prepanelFnIsGgplot = prepanelFnIsGgplot,
      doBanking = doBanking
   )

   if(! "package:trelliscope" %in% search()) {
      # parList <- c(parList, list(
      # ))

      setup <- expression({
         suppressMessages(require(lattice))
         suppressMessages(require(ggplot2))
         suppressMessages(require(data.table))
      })
   } else {
      setup <- expression({
         suppressMessages(require(lattice))
         suppressMessages(require(ggplot2))
         suppressMessages(require(data.table))
         suppressMessages(require(trelliscope))
         suppressMessages(require(datadr))
      })
   }

   # suppressMessages(capture.output(
   jobRes <- mrExec(
      data,
      setup=setup,
      map=map,
      reduce=reduce,
      control=control,
      params=parList,
   )

   res <- list(
      x = jobRes[["x"]][[2]],
      y = jobRes[["y"]][[2]],
      prepanelFnIsTrellis = prepanelFnIsTrellis,
      prepanelFnIsGgplot = prepanelFnIsGgplot,
      prepanelFn = prepanelFn
   )
   class(res) <- c("trsPre", "list")
   return(res)
}

#' Plot results form prepanel
#'
#' Plot results form prepanel
#'
#' @param x object of class "trsPre" created by \code{\link{prepanel}}
#' @param layout, as.table, strip, strip.left, between, xlab, ylab, \ldots parameters for the lattice plot that is output (these are defaults - can ignore unless you want fine control)
#'
#' @return object of class "trellis" (plotted by default)
#'
#' @details This function plots the sorted axis ranges for the x and y axis for the case of "same" (all axis limits share the same range) and "sliced" (all axis limits share the) and can be useful in helping determine how to ultimately set the limits.
#'
#' @author Ryan Hafen
#'
#' @seealso \code{\link{prepanel}}, \code{\link{makeDisplay}}
#'
#' @examples
#' \dontrun{
#' irisSplit <- divide(iris, "Species")
#' irisPreFn <- function(x) {
#'    list(
#'       xlim = range(x$Sepal.Length),
#'       ylim = range(x$Sepal.Width)
#'    )
#' }
#' irisPre <- prepanel(irisSplit, prepanelFn=irisPreFn)
#' plot(irisPre)
#' }
#'
#' @method plot trsPre
#' @export
plot.trsPre <- function(x, layout=c(2, 2), as.table=TRUE, strip=FALSE, strip.left=TRUE, between=list(y=0.25), xlab="Rank", ylab="Panel Limits", ...
) {
   # TODO: what about dx and dy for aspect ratio?
   lims <- x

   alreadyWarned <- FALSE

   adjust <- function(var, type) {
      dat <- lims[[var]]
      if(!is.numeric(dat$max)) {
         if(!alreadyWarned) {
            message("At least one of the variables is not numeric.  Casting as numeric for plotting purposes.")
            alreadyWarned <<- TRUE
         }
         dat$max <- as.numeric(dat$max)
         dat$min <- as.numeric(dat$min)
      }

      if(type == "sliced") {
         dat[,2:4] <- dat[,2:4] - (dat$max + dat$min) / 2
         dat <- dat[order(dat$max - dat$min, decreasing=FALSE),]
      } else {
         dat <- dat[order((dat$max + dat$min) / 2, decreasing=FALSE),]
      }
      dat$rank <- seq_len(nrow(dat))
      dat$which <- paste(var, " (", type, ")", sep="")
      dat
   }

   lims2 <- rbind(
      adjust("x", "same"),
      adjust("x", "sliced"),
      adjust("y", "same"),
      adjust("y", "sliced")
   )

   p <- xyplot(min + max ~ rank | which, data=lims2,
      panel=function(x, y, ..., subscripts) {
         curlims <- lims2[subscripts,]
         panel.abline(v=pretty(curlims$rank), col="#e6e6e6")
         panel.abline(h=pretty(c(curlims$min, curlims$max)), col="#e6e6e6")
         panel.segments(curlims$rank, curlims$min, curlims$rank, curlims$max)
         # panel.lines(curlims$rank, curlims$med, col="red", pch=".")
         panel.lines(curlims$rank, (curlims$max + curlims$min) / 2, col="red", pch=".")
      },
      scales=list(y=list(relation="free")),
      layout=layout,
      as.table=as.table,
      strip=strip,
      strip.left=strip.left,
      between=between,
      xlab=xlab,
      ylab=ylab,
      ...
   )

   p
}

#' Specify Rules for x and y Limits for a Display
#'
#' Based on results from \code{\link{prepanel}}, specify rules that will determine x and y axis limits to be passed as the \code{lims} argument when calling \code{\link{makeDisplay}}.
#'
#' @param lims object of class "trsPre"
#' @param x x-axis limits rule (either "same", "sliced", or "free" - see details)
#' @param y y-axis limits rule (either "same", "sliced", or "free" - see details)
#' @param xQuant lower and upper quantiles at which to cut off x-axis limits, in the case of outliers.  Used when x="same".
#' @param yQuant same as xQuant but for y-axis
#' @param xQuantRange a single upper quantile at which to cut off the x-axis range, used when x="sliced", used in the case of a few splits having abnormally high range, which are wished to be excluded
#' @param yQuantRange same as xQuantRange but for y-axis
#'
#' @return object of class "trsLims", which can be used in a call to \code{\link{makeDisplay}}
#'
#' @details
#' This function reduces the list of axis limits computed for each split of a data set to an overall axis limit rule for the plot.
#'
#' About "x" and "y" parameters: This is the same as in lattice.  From lattice documentation:
#' A character string that determines how axis limits are calculated for each panel. Possible values are "same" (default), "free" and "sliced". For relation="same", the same limits, usually large enough to encompass all the data, are used for all the panels. For relation="free", limits for each panel is determined by just the points in that panel. Behavior for relation="sliced" is similar, except that the length (max - min) of the scales are constrained to remain the same across panels.
#'
#' @author Ryan Hafen
#'
#' @seealso \code{\link{prepanel}}, \code{\link{makeDisplay}}
#'
#' @examples
#' irisSplit <- divide(iris, "Species")
#' irisPreFn <- function(x) {
#'    list(
#'       xlim = range(x$Sepal.Length),
#'       ylim = range(x$Sepal.Width)
#'    )
#' }
#' irisPre <- prepanel(irisSplit, prepanelFn=irisPreFn)
#' irisLims <- setLims(irisPre, x="same", y="sliced")
#'
#' @export
setLims <- function(lims, x="same", y="same", xQuant=c(0,1), yQuant=c(0,1), xRangeQuant=1, yRangeQuant=1, prop=0.07) {

   alreadyWarned <- FALSE
   # xQuant=c(0,1); yQuant=c(0,1); xRangeQuant=1; yRangeQuant=1
   getLims <- function(var, type, quant, rangeQuant) {
      dat <- lims[[var]]
      datClass <- class(dat$max)[1]
      if(!is.numeric(dat$max)) {
         if(datClass != "character") {
            if(!alreadyWarned) {
               message("At least one of the variables is not numeric.  Casting as numeric for quantile calculation purposes.")
               alreadyWarned <<- TRUE
            }
            dat$max <- as.numeric(dat$max)
            dat$min <- as.numeric(dat$min)
         }
      }

      # TODO: if character and not "free" then set limits
      # to all levels of the variable, if known
      if(type == "sliced" && datClass != "character") {
         tmp <- as.numeric(quantile(dat$max - dat$min, rangeQuant, na.rm=TRUE))
         tmp <- tmp + 2 * prop * tmp
         res <- list(type="sliced", lim=NULL, range=tmp)
      } else if(type == "same" && datClass != "character") {
         tmp <- as.numeric(c(quantile(dat$min, quant[1], na.rm=TRUE), quantile(dat$max, quant[2], na.rm=TRUE)))
         tmp <- tmp + c(-1, 1) * diff(tmp) * prop
         res <- list(type="same", lim=tmp, range=NULL)
      } else {
         res <- list(type="free", lim=NULL, range=NULL)
         return(res)
      }
      if(datClass=="Date")
         res$lim <- as.Date(res$lim, origin="1970-01-01")
      if(datClass=="POSIXct")
         res$lim <- as.POSIXct(res$lim, origin="1970-01-01")
      # TODO: time zone checking
      res
   }

   res <- list(
      x = getLims("x", x, xQuant, xRangeQuant),
      y = getLims("y", y, yQuant, yRangeQuant),
      prepanelFnIsTrellis = lims$prepanelFnIsTrellis,
      prepanelFnIsGgplot = lims$prepanelFnIsGgplot,
      prepanelFn = lims$prepanelFn,
      prop = prop,
      n = nrow(lims$x)
   )
   class(res) <- c("trsLims", "list")
   res
}
