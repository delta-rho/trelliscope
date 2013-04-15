#' Prepanel Function for vdb Plots
#'
#' Prepanel function for vdb plots
#' 
#' Apply a prepanel function to objects of class "localDiv" (obtained from splitDF()) or "rhData" to determine ranges of x and y axis limits.  Useful in conjunction with \code{\link{vdbSetLims}}.
#'
#' @param dat an object of class "localDiv" or "rhData"
#' @param preFn a prepanel function that returns a list specifying \code{xlim} and \code{ylim} for determining axis limits, and optionally \code{dx} and \code{dy} for determining aspect ratio (used to define slopes of line segments used for banking computations).  preFn can also be a plotFn (see \code{\link{vdbPlot}}) that returns either an object of class "trellis" or "ggplot", since xlim and ylim can be determined from these.
#' @param mapred a list of parameters to be sent to \code{\link{rhwatch}} if dat is of class "rhData"
#' @param verbose print status messages?
#' @param calledFromRhipe ignore this parameter (don't use it)
#' 
#' @return object of class "vdbPre".  This is a list of the x and y axis ranges for each split, along with the aspect ratio banking value if \code{dx} and \code{dy} are supplied in \code{preFn}.  Can be used with \code{\link{plot.vdbPre}} and \code{\link{vdbSetLims}}. 
#' 
#' @details
#' The plot method plots the sorted axis ranges for the x and y axis for the case of "same" (all axis limits share the same range) and "sliced" (all axis limits share the) and can be useful in helping determine how to ultimately set the limits.  
#' 
#' You don not need to use \code{vdbPrepanel} to ultimately create a \code{\link{vdbPlot}}, but if you bypass, you will either need to specify your own limits in your plot command, or do nothing, in which case each individual plot will have limits based on the data in the split being plotted (the axes will be "free").
#' 
#' Axis limits are very important.  What makes viewing groups of plots of subsets of data ("small multiples") so powerful is being able to make meaningful visual comparisons across plots.  This is much easier to do if scales for each plot are commensurate.
#' 
#' This function is also useful for identifying subsets with very large outlying values, and in conjunction with \code{\link{vdbSetLims}}, allows you to account for that prior to the expensive process of creating all of the plots.
#' 
#' In the future, there will be helper functions for special types of plots, such as histograms, etc. to help the user more easily provide the \code{xlim} and \code{ylim} components of the prepanel function.
#' 
#' @author Ryan Hafen
#' 
#' @seealso \code{\link{x}}
#' \code{\link{plot.vdbPre}}, \code{\link{vdbSetLims}}, \code{\link{vdbPlot}}, \code{\link{localDiv}}, \code{\link{rhData}}
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
#' irisPre <- vdbPrepanel(irisSplit, preFn=irisPreFn)
#' plot(irisPre)#' }
#' 
#' @export
vdbPrepanel <- function(data, preFn=NULL,
   mapred=NULL, 
   verbose=TRUE,
   calledFromRhipe = FALSE # don't mess with this
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
   preFnIsTrellis <- FALSE
   doBanking <- TRUE
   
   if(verbose)
      message("Testing 'preFn' on a subset...")
   p <- preFn(divExample(data))
   if(inherits(p, "trellis")) {
      preFnIsTrellis <- TRUE
      if(verbose)
         message("Using lattice plot to determine limits... dx and dy will not be computed.")
      doBanking <- FALSE
   } else {
      if(is.null(p$xlim) || is.null(p$ylim))
         stop("'preFn' must either return an object of class 'trellis' or return a list with elements 'xlim' and 'ylim'.")
      if(is.null(p$dx) || is.null(p$dy)) {
         if(verbose)
            message("dx or dy (or both) were not specified - not computing banking.")
         doBanking <- FALSE
      }
   }
   
   splitKeys <- getKeys(data)
   
   if(inherits(data, "localDiv")) {
      res <- lapply(seq_along(data), function(ii) {
         dd <- data[[ii]]
         curSplitKey <- splitKeys[[ii]]
         bnk <- NA
         
         if(preFnIsTrellis) {
            # temporarily remove axis padding
            curOption <- lattice.getOption("axis.padding")$numeric
            lattice.options(axis.padding=list(numeric=0))
            p <- preFn(dd)
            xr <- p$x.limits
            yr <- p$y.limits

            lattice.options(axis.padding=list(numeric=curOption))
            # # TODO: for ggplot:
            # a <- print(p) # need to not make it actually plot
            # a$panel$ranges[[1]]$x.range
            # a$panel$ranges[[1]]$y.range
         } else {
            pre <- preFn(dd)
            xr <- pre$xlim
            yr <- pre$ylim

            if(doBanking) {
               dx <- pre$dx
               dy <- pre$dy
               bnk <- banking(dx, dy)
            }
         }
         
         list(
            data.frame(
               key=as.character(curSplitKey), 
               min=xr[1],
               max=xr[2],
               # med=median(x, na.rm=TRUE),
               bnk=bnk
            ),
            data.frame(
               key=as.character(curSplitKey), 
               min=yr[1],
               max=yr[2],
               # med=median(y, na.rm=TRUE),
               bnk=bnk
            )
         )
      })
      res <- list(
         x = do.call(rbind, lapply(res, function(x) x[[1]])),
         y = do.call(rbind, lapply(res, function(x) x[[2]]))
      )
   } else if(inherits(data, "rhData")) {

      map <- expression({
         for(i in seq_along(map.keys)) {
            curKey <- map.keys[[i]]
            curVal <- map.values[[i]]
            data <- list(curVal)
            names(data) <- curKey
            class(data) <- "localDiv"
            
            tmp <- vdbPrepanel(
               data=data,
               preFn=preFn,
               verbose=FALSE,
               calledFromRhipe=TRUE
            )
            rhcollect("1", tmp)
         }
      })
      
      # rbind the results
      reduce <- expression(
         pre = {
            x <- NULL
            y <- NULL
         },
         reduce = {
            x <- rbind(x, do.call(rbind, lapply(reduce.values, function(a) a$x)))
            y <- rbind(y, do.call(rbind, lapply(reduce.values, function(a) a$y)))
         },
         post = {
            rhcollect("1", list(x=x, y=y))
         }
      )

      parList <- list(
         preFn = preFn
      )
      
      if(! "package:vdb" %in% search()) {
         parList <- c(parList, list(
            vdbPrepanel = vdbPrepanel, # remove when packaged 
            divExample = divExample
         ))
      }
      
      if("package:vdb" %in% search()) {
         setup <- expression({
            suppressMessages(require(lattice))
            suppressMessages(require(ggplot2))
            # suppressMessages(require(data.table))
            suppressMessages(require(vdb))
            suppressMessages(require(datadr))
         })
      } else {
         setup <- expression({
            suppressMessages(require(lattice))
            suppressMessages(require(ggplot2))
            # suppressMessages(require(data.table))
            suppressMessages(require(datadr))
         })
      }
      
      ofolder <- Rhipe:::mkdHDFSTempFolder(file="prepanel")
      
      rhoptions(copyObjects=list(auto=FALSE))
      # suppressMessages(capture.output(
      rhJob <- rhwatch(
         setup=setup,
         map=map,
         reduce=reduce,
         input=rhfmt(data$loc, type=data$type),
         output=ofolder,
         mapred=mapred,
         combiner=TRUE,
         param=parList,
         # mon.sec=0,
         readback=FALSE
      ) # ))
      
      # id <- gsub(".*jobid=(job_.*)", "\\1", rhJob[[1]]$tracking)
      # rhRes <- vdbRhStatus(id)
      
      res <- rhread(ofolder)[[1]][[2]]
      rhdel(ofolder)
   }
   
   if(calledFromRhipe) {
      return(res)
   } else {
      res <- list(
         x = res$x,
         y = res$y,
         preFnIsTrellis = preFnIsTrellis,
         preFn = preFn
      )
      class(res) <- c("vdbPre", "list")
      return(res)
   }
}

#' Plot results form vdbPrepanel
#'
#' Plot results form vdbPrepanel
#'
#' @param lims object of class "vdbPre" created by \code{\link{vdbPrepanel}}
#' @param layout, as.table, strip, strip.left, between, xlab, ylab, \ldots parameters for the lattice plot that is output (these are defaults - can ignore unless you want fine control)
#'
#' @return object of class "trellis" (plotted by default)
#'
#' @details This function plots the sorted axis ranges for the x and y axis for the case of "same" (all axis limits share the same range) and "sliced" (all axis limits share the) and can be useful in helping determine how to ultimately set the limits.  
#' 
#' @author Ryan Hafen
#' 
#' @seealso \code{\link{vdbPrepanel}}, \code{\link{vdbPlot}}
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
#' irisPre <- vdbPrepanel(irisSplit, preFn=irisPreFn)
#' plot(irisPre)
#' }
#' 
#' @export
plot.vdbPre <- function(lims, layout=c(2, 2), as.table=TRUE, strip=FALSE, strip.left=TRUE, between=list(y=0.25), xlab="Rank", ylab="Panel Limits", ...
) {
   # TODO: what about dx and dy for aspect ratio?

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



   # # metrics:
   # lapply(lims, function(a) {
   #    data.frame(
   #       minRange = min(a$max - a$min),
   #       maxRange = max(a$max - a$min),
   #       medRange = median(a$max - a$min),
   #       totRange = max(a$max) - min(a$min)
   #    )
   # })
   
   p
}

#' Specify rules for x and y limits for a vdbPlot
#'
#' Based on results from \code{\link{vdbPrepanel}}, specify rules that will determine x and y axis limits.
#'
#' @param lims object of class "vdbPre"
#' @param x x-axis limits rule (either "same", "sliced", or "free" - see details)
#' @param y y-axis limits rule (either "same", "sliced", or "free" - see details)
#' @param xQuant lower and upper quantiles at which to cut off x-axis limits, in the case of outliers.  Used when x="same".
#' @param yQuant same as xQuant but for y-axis
#' @param xQuantRange a single upper quantile at which to cut off the x-axis range, used when x="sliced", used in the case of a few splits having abnormally high range, which are wished to be excluded
#' @param yQuantRange same as xQuantRange but for y-axis
#'
#' @return object of class "vdbLims", which can be used in a call to \code{\link{vdbPlot}}
#'
#' @details
#' This function reduces the list of axis limits computed for each split of a data set to an overall axis limit rule for the plot.  
#' 
#' About "x" and "y" parameters: This is the same as in lattice.  From lattice documentation:
#' A character string that determines how axis limits are calculated for each panel. Possible values are "same" (default), "free" and "sliced". For relation="same", the same limits, usually large enough to encompass all the data, are used for all the panels. For relation="free", limits for each panel is determined by just the points in that panel. Behavior for relation="sliced" is similar, except that the length (max - min) of the scales are constrained to remain the same across panels.
#'
#' @author Ryan Hafen
#'
#' @seealso \code{\link{vdbPrepanel}}, \code{\link{vdbPlot}}
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
#' irisPre <- vdbPrepanel(irisSplit, preFn=irisPreFn)
#' irisLims <- vdbSetLims(irisPre, x="same", y="sliced")
#' }
#' 
#' @export
vdbSetLims <- function(lims, x="same", y="same", xQuant=c(0,1), yQuant=c(0,1), xRangeQuant=1, yRangeQuant=1, prop=0.07) {
   
   alreadyWarned <- FALSE
   # xQuant=c(0,1); yQuant=c(0,1); xRangeQuant=1; yRangeQuant=1
   getLims <- function(var, type, quant, rangeQuant) {
      dat <- lims[[var]]
      datclass <- class(dat$max)[1]
      if(!is.numeric(dat$max)) {
         if(!alreadyWarned) {
            message("At least one of the variables is not numeric.  Casting as numeric for quantile calculation purposes.")
            alreadyWarned <<- TRUE            
         }
         dat$max <- as.numeric(dat$max)
         dat$min <- as.numeric(dat$min)
      }
      
      if(type == "sliced") {
         tmp <- as.numeric(quantile(dat$max - dat$min, rangeQuant))
         tmp <- tmp + 2 * prop * tmp
         res <- list(type="sliced", lim=NULL, range=tmp)
      } else if(type == "same") {
         tmp <- as.numeric(c(quantile(dat$min, quant[1]), quantile(dat$max, quant[2])))
         tmp <- tmp + c(-1, 1) * diff(tmp) * prop
         res <- list(type="same", lim=tmp, range=NULL)
      } else {
         res <- list(type="free", lim=NULL, range=NULL)
      }
      if(datclass=="Date")
         res$lim <- as.Date(res$lim, origin="1970-01-01")
      if(datclass=="POSIXct")
         res$lim <- as.POSIXct(res$lim, origin="1970-01-01")
      # TODO: need to make sure the origin is correct
      res
   }
   
   res <- list(
      x=getLims("x", x, xQuant, xRangeQuant),
      y=getLims("y", y, yQuant, yRangeQuant),
      preFnIsTrellis = lims$preFnIsTrellis,
      preFn = lims$preFn,
      prop = prop, 
      n=nrow(lims$x)
   )
   class(res) <- c("vdbLims", "list")
   res
}


