#' Default Plot Function for vdbSplod
#'
#' Default plot function for vdbSplod
#' 
#' @param df a subset of data created by \code{\link{makeSplodData}}
#'
#' @return a trellis plot object of a scatterplot for the given subset
#'
#' @references
#' Wilkinson's scagnostics paper.
#'
#' @author Ryan Hafen
#' 
#' @seealso \code{\link{splodCogFn}}, \code{\link{vdbSplod}}, \code{\link{makeSplodData}}
#'
#' @export
splodPlotFn <- function(df) {
   xyplot(y ~ x, data=df,
      xlab=attr(df, "split")$xVar,
      ylab=attr(df, "split")$yVar
   ) 
} 

#' Default Cognostics Function for vdbSplod
#'
#' Default cognostics function for vdbSplod
#' 
#' @param df a subset of data created by \code{\link{makeSplodData}}
#'
#' @return a data.frame of scagnostics for the given subset
#'
#' @references
#' Wilkinson's scagnostics paper.
#'
#' @author Ryan Hafen
#' 
#' @seealso \code{\link{splodPlotFn}}, \code{\link{vdbSplod}}, \code{\link{makeSplodData}}
#'
#' @export
splodCogFn <- function(df) {
   cogScagnostics(df$x, df$y)
}

#' Create Data Plottable by vdbSplod
#'
#' Create pairwise scatterplot data plottable by vdbSplod
#' 
#' @param data a data.frame
#' @param id.vars variables to ignore when computing all pairs of variables
#'
#' @return an object of class 'localDiv' and 'splodDat' that can be passed to \code{\link{vdbSplod}}
#'
#' @references
#' Wilkinson's scagnostics paper.
#'
#' @author Ryan Hafen
#' 
#' @seealso \code{\link{vdbSplod}}, \code{\link{vdbCogFn}}, \code{\link{splodPlotFn}}
#'
#' @export
makeSplodData <- function(data, id.vars=NULL) {
   nCol <- ncol(data)
   dataNames <- names(data)
   
   nonSplodVars <- which(dataNames %in% id.vars)
   tmp <- which(!sapply(data, is.numeric))
   nonSplodVars <- sort(union(nonSplodVars, tmp))
   
   splodVars <- setdiff(1:nCol, nonSplodVars)
   nSplodVars <- length(splodVars)
   
   combs <- combn(nSplodVars, 2)
   
   newData <- do.call(rbind, lapply(1:ncol(combs), function(j) {
      yInd <- splodVars[combs[1,j]]
      xInd <- splodVars[combs[2,j]]
      
      data.frame(x=data[,xInd], y=data[,yInd], xVar=dataNames[xInd], yVar=dataNames[yInd], data[nonSplodVars], stringsAsFactors=FALSE)
   }))
   
   data <- divide(newData, c("xVar", "yVar"))
   class(data) <- c(class(data), "splodDat")
   data
}

# scatterplot display
# data is matrix or data.frame
# plotFn - 

#' Create a Scatterplot Display
#'
#' Create a scatterplot display (splod)
#' 
#' @param data a data.frame or an object of class "splodDat"
#' @param id.vars variables to ignore when computing all pairs of variables
#' @param name, desc, cogFn, plotFn, verbose, \ldots parameters passed to \code{\link{vdbPlot}}
#'
#' @return an object of class 'localDiv' that can be passed to \code{\link{vdbSplod}}
#'
#' @references
#' Wilkinson's scagnostics paper.
#'
#' @author Ryan Hafen
#' 
#' @seealso \code{\link{vdbPlot}}, \code{\link{makeSplodDat}}, \code{\link{vdbCogFn}}, \code{\link{splodPlotFn}}
#'
#' @export
vdbSplod <- function(
   data,
   id.vars=NULL,
   name=NULL,
   desc=NULL,
   cogFn = splodCogFn,   
   plotFn = splodPlotFn,
   verbose=TRUE,
   ...
) {
   
   if(is.null(name))
      name <- paste(deparse(substitute(data)), "_splod", sep="")
   
   if(is.null(desc))
      desc <- paste("Scatterplot display")
   
   if(!inherits(data, "splodDat")) {
      if(verbose)
         message("Transforming data into splodDat format...")
      data <- makeSplodData(data, id.vars=id.vars)
   }
   
   vdbPlot(data, name=name, cogFn=cogFn, plotFn=plotFn, verbose=verbose, ...)
}



