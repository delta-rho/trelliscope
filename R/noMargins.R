#' Remove Margins from Trellis/Lattice Plot
#'
#' Removes whitespace surrounding a trellis plot.  Ideal for \code{\link{makeDisplay}} because when tiling multiple panels, a lot of space is wasted with the margins.
#'
#' @param \ldots a list of other parameters to be passed to par.settings
#' @param topkey should extra space be added for a top key?
#' @param rightkey should extra space be added for a right key?
#' 
#' @author Ryan Hafen
#' 
#' @seealso \code{\link{xyplot}}, \code{\link{makeDisplay}}
#'
#' @examples
#' \dontrun{
#' xyplot(c(1:10) ~ c(1:10) | sample(letters[1:2], 10, replace=TRUE),
#'    par.settings=noMargins()
#' )
#' 
#' # with additional par.settings...
#' xyplot(c(1:10) ~ c(1:10) | sample(letters[1:2], 10, replace=TRUE),
#'    par.settings=noMargins(
#'       list(plot.symbol=list(col="black"))
#'    )
#' )
#' }
#' 
#' @export
noMargins <- function(..., topkey=FALSE, rightkey=FALSE) {

   nmlist <- list(
      layout.heights = list(
         top.padding = 0, 
         main.key.padding = 0, 
         key.axis.padding = 0, 
         axis.xlab.padding = 0, 
         xlab.key.padding = 0, 
         key.sub.padding = 0, 
         bottom.padding = 0.5
      ), 
      layout.widths = list(
         left.padding = 0, 
         key.ylab.padding = 0, 
         ylab.axis.padding = 0, 
         axis.key.padding = 0, 
         right.padding = 0.6
      ),
      axis.components=list(
         top=list(pad1 = ifelse(topkey, 2, 1), pad2 = ifelse(topkey, 2, 0)), # padding above top axis 
         right=list(pad1 = 0, pad2 = 0)
      )
   )
   # TODO: allow "..." to override any elements in "nmlist"
   c(nmlist, ...)
}



