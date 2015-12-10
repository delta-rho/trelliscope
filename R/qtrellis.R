# qtrellis can take either a panel function, a ddo/ddf, a grouped_df, or a data frame

#' Quick trelliscope display for data frame-like inputs
#'
#' @param x either a data frame
#' @param panel a function taking one argument (which will be a subset of the input data frame) and returning a plot
#' @param cog an optional cognostics funtion to be applied to each subset
#' @param by if the input is a data frame, a character vector of column names to split the data by
#' @param layout a vector indicating the number of rows and columns to arrange the panels in by default
#' @param conn VDB connection info, typically stored in options("vdbConn") at the beginning of a session, and not necessary to specify here if a valid "vdbConn" object exists or if you would like to use a temporary one for this display
#' @param \ldots parameters passed to \code{\link{makeDisplay}} - most importantly \code{name}, \code{group} (see note below), \code{width}, and \code{height}
#'
#' @note If you don't have a vdb connection set up (see \code{\link{vdbConn}}), a temporary one will be created and used, and you can think of the plot as disposable.  If you would like the plot to persist, set up a vdb connection.  Likewise, if you don't supply \code{name} and \code{group}, the plot will be stored under defaults "qtrellis_plot" and "__qtrellis", and a subsequent call will cause the previous display with this name and group to be replaced.  Therefore, if you want your display to persist, make sure a vdb connection has been set up prior to calling this function, and give it a unique name.
#'
#' @examples
#' \dontrun{
#' panel <- function(x)
#'   xyplot(Sepal.Width ~ Sepal.Length, data = x)
#'
#' p <- datadr::divide(iris, by = "Species") %>%
#'   qtrellis(panel, layout = c(1, 3))
#' p
#'
#' # data frame input (need to specify 'by')
#' iris %>% qtrellis(panel, by = "Species")
#'
#' # dplyr grouped tbl input
#' library(dplyr)
#' p <- iris %>%
#'   group_by(Species) %>%
#'   qtrellis(panel, layout = c(1, 3))
#' p
#' }
#' @export
qtrellis <- function(x, panel = NULL, cog = NULL, by = NULL, layout = c(1, 1), conn = getOption("vdbConn"), ...)
  UseMethod("qtrellis")

#' @export
qtrellis.grouped_df <- function(x, panel = NULL, cog = NULL, by = NULL, layout = c(1, 1), conn = getOption("vdbConn"), ...) {
  x <- x %>% datadr::to_ddf()
  qtrellis_(x, panel, cog, by, layout, conn, ...)
}

#' @export
qtrellis.ddo <- function(x, panel = NULL, cog = NULL, by = NULL, layout = c(1, 1), conn = getOption("vdbConn"), ...) {
  qtrellis_(x, panel, cog, by, layout, conn, ...)
}

#' @export
qtrellis.data.frame <- function(x, panel = NULL, cog = NULL, by = NULL, layout = c(1, 1), conn = getOption("vdbConn"), ...) {
  x <- datadr::divide(x, by = by)
  qtrellis_(x, panel, cog, by, layout, conn, ...)
}

# qtrellis.function <- function(x, panel = NULL, cog = NULL, by = NULL, layout = c(1, 1), data = NULL, conn = getOption("vdbConn"), ...) {
#   qtrellis_(data, panel, cog, by, layout, conn, ...)
# }

qtrellis_ <- function(x, panel = NULL, cog = NULL, by = NULL, layout = c(1, 1), conn = getOption("vdbConn"), ...) {

  if(is.null(conn))
    conn <- vdbConn(file.path(tempdir(), "vdb"), autoYes = TRUE)

  args <- list(...)
  if(is.null(args$name))
    args$name <- "qtrellis_plot"
  if(is.null(args$group))
    args$group <- "__qtrellis"

  args$data <- x
  args$panelFn <- panel
  args$cogFn <- cog

  if(!is.null(layout)) {
    if(!is.null(args$state)) {
      if(is.null(args$state$layout))
        args$state$layout <- list(nrow = layout[1], ncol = layout[2])
    } else {
      args$state <- list(layout = list(nrow = layout[1], ncol = layout[2]))
    }
  }

  res <- do.call(makeDisplay, args)
  res$vdbConn <- vdbConn # keep this so we can view it if the vdb connection changes
  class(res) <- c(class(res), "qtrellis")
  res
}

#' @export
print.qtrellis <- function(x, ...) {
  view(name = x$name, group = x$group, state = x$state)
}

# library(rbokeh)
# library(dplyr)
# panel <- function(x)
#   figure() %>% ly_points(Sepal.Width, Petal.Width, data = x)
#
# a <- housing %>%
#   group_by(county, state) %>%
#   qtrellis(function(x)
#     figure() %>%
#       ly_points(time, medListPriceSqft, data = x, hover = medListPriceSqft))


