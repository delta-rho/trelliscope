#' Make a png for a Subset
#' 
#' Make a png for a subset of an object of class "ddo" or "ddf".  The user should never need to call this directly, but the viewer needs it, so it is exported with this documentation.
#'
#' @param dat a key-value pair
#' @param panelFn panel function
#' @param file file name for png
#' @param width,height,res width, height, and resolution
#' @param lims axis limits
#'
#' @author Ryan Hafen
#' @seealso \code{\link{makeDisplay}}
#' @export
makePNG <- function(dat, panelFn=NULL, file, width, height, res, lims=NULL) {
   
   a <- suppressWarnings(try({
      png(file=file, width=width, height=height, res=res)
   }, silent=TRUE))
   if(inherits(a, "try-error")) {
      suppressMessages(require(Cairo))
      CairoPNG(file=file, width=width, height=height, dpi=res, pointsize=12*res/72)
   }
   
   if(inherits(dat, "trellis") || inherits(dat, "ggplot")) { # single panel plot
      print(dat)
   } else if (inherits(dat, "expression")) { # expression (can't change limits)
      eval(dat)
   } else { # plot objects such as trellis or lattice
      tmp <- kvApply(panelFn, dat)
      # browser()
      
      if(!is.null(lims)) {
         if(inherits(tmp, "trellis")) {
            # if there are multiple panels inside of one plot, we can't do this
            if(!(inherits(tmp$x.limits, "list") || inherits(tmp$y.limits, "list"))) {
               plotXLim <- tmp$x.limits
               plotYLim <- tmp$y.limits               
               curXLim <- trsCurXLim(lims, dat, plotXLim)
               curYLim <- trsCurYLim(lims, dat, plotYLim)         
               
               if(lims$x$type != "free")
                  tmp$x.limits <- curXLim
               if(lims$y$type != "free")
                  tmp$y.limits <- curYLim
            }
         } else if(inherits(tmp, "ggplot")) {
            # this is ugly now - make more robust, etc.
            ggbuild <- ggplot_build(tmp)
            gglims <- ggbuild$panel$ranges
            if(length(gglims) == 1) {
               plotXLim <- gglims[[1]]$x.range
               plotYLim <- gglims[[1]]$y.range
               curXLim <- trsCurXLim(lims, dat, plotXLim)
               curYLim <- trsCurYLim(lims, dat, plotYLim)
               
               if(lims$x$type != "free") {
                  if(ggbuild$panel$x_scales[[1]]$scale_name == "datetime") {
                     tmp <- tmp + scale_x_datetime(limits=as.POSIXct(curXLim, origin="1970-01-01"))                  
                  } else {
                     tmp <- tmp + scale_x_continuous(limits=curXLim)
                  }
               }
               if(lims$y$type != "free") {
                  tmp <- tmp + scale_y_continuous(limits=curYLim)                  
               }
            }
         }
      }
      
      if(inherits(tmp, c("trellis"))) {
         print(tmp)
      }
      if(inherits(tmp, c("ggplot"))) {
         print(tmp)
      }
      if(inherits(tmp, "expression")) {
         eval(tmp)
      }
   }
   dev.off()
}