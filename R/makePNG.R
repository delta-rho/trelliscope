#' Make a png for a Subset
#'
#' Make a png for a subset of an object of class "ddo" or "ddf".  The user should never need to call this directly, but the viewer needs it, so it is exported with this documentation.
#'
#' @param dat a key-value pair
#' @param panelFn panel function
#' @param file file name for png
#' @param width,height,res width, height, and resolution
#' @param origWidth TODO
#' @param basePointSize TODO
#' @param lims axis limits
#' @param pixelratio TODO
#'
#' @author Ryan Hafen
#' @seealso \code{\link{makeDisplay}}
#' @export
#' @import lattice
makePNG <- function(dat, panelFn = NULL, file, width, height, origWidth = width, res = 72, basePointSize = 12, lims = NULL, pixelratio = 2) {

   if(capabilities("aqua")) {
      pngfun <- png
   } else {
      pkg <- "Cairo"
      if(suppressWarnings(suppressMessages(require(pkg, character.only = TRUE)))) {
         pngfun <- CairoPNG
      } else {
         pngfun <- png
      }
   }

   pointsize <- basePointSize * width / origWidth * pixelratio

   pngfun(filename = file,
      width = width * pixelratio,
      height = height * pixelratio,
      # res = res * pixelratio,
      pointsize = pointsize)

   dv <- dev.cur()
   tryCatch({
      if(inherits(dat, "trellis")) {
         # single panel plot
         dat$par.settings$fontsize <- list(text = pointsize, points = pointsize * 2 / 3)
         print(dat)
      } else if(inherits(dat, "ggplot")) {
         # single panel plot
         print(dat)
      } else if (inherits(dat, "expression")) {
         # expression (can't change limits)
         eval(dat)
      } else {
         # plot objects such as trellis or lattice
         tmp <- kvApply(panelFn, dat)

         if(!is.null(lims)) {
            if(inherits(tmp, "trellis")) {
               # set pointsize
               tmp$par.settings$fontsize <- list(text = pointsize, points = pointsize * 2 / 3)

               # if there are multiple panels inside of one plot, we can't do this
               if(!(inherits(tmp$x.limits, "list") || inherits(tmp$y.limits, "list"))) {
                  plotXLim <- tmp$x.limits
                  if(is.numeric(plotXLim) || inherits(plotXLim, "Date")) {
                     curXLim <- trsCurXLim(lims, dat, plotXLim)
                     if(lims$x$type != "free")
                        tmp$x.limits <- curXLim
                  }

                  plotYLim <- tmp$y.limits
                  if(is.numeric(plotYLim)) {
                     curYLim <- trsCurYLim(lims, dat, plotYLim)
                     if(lims$y$type != "free")
                        tmp$y.limits <- curYLim
                  }
               }
            } else if(inherits(tmp, "ggplot")) {
               theme_set(theme_grey(pointsize))
               # tmp <- tmp + opts(pointsize = pointsize)
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
                        tmp <- tmp + scale_x_datetime(limits = as.POSIXct(curXLim, origin = "1970-01-01"))
                     } else {
                        tmp <- tmp + scale_x_continuous(limits = curXLim)
                     }
                  }
                  if(lims$y$type != "free") {
                     tmp <- tmp + scale_y_continuous(limits = curYLim)
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
   }, finally = dev.off(dv))

   # if panel function didn't plot anything then make a blank panel
   if(!file.exists(file)) {
      pngfun(filename = file,
         width = width * pixelratio,
         height = height * pixelratio,
         # res = res * pixelratio,
         pointsize = pointsize)

      print(xyplot(NA ~ NA, xlab = "", ylab = "", scales = list(draw = FALSE), panel = function(x, y, ...) panel.text(0.5, 0.5, "no panel")))

      dev.off()
   }
}

