#' Set State Parameters
#'
#' @param name the name of the display
#' @param group the group of the display
#' @param labels a vector of names of cognostics to be shown as labels underneath each panel.  If not specified, the default is to show labels for any of the splitting variables that created the partition of the data being plotted.
#' @param  layout a list with optional elements \code{nrow}, \code{ncol}, and \code{arrange}.  \code{nrow} and \code{ncol} specify the arrangement of the panels into rows and columns (\code{nrow = 1} and \code{ncol = 1} are defaults), and \code{arrange} can be either "row" or "col" and specified whether to sort the panels by row or by column ("row" is default)
#' @param sort a named list where each name corresponds to a cognostic name and the value is either "asc" or "desc" for sorting in ascending or descending order.  The order in which sorting is applied to each variable is according to the order of the variables specified.
#' @param filter a named list where each name corresponds to a cognostic name and the value is a specification of either "regex" or "select" for categorical variables, or a range, "from" and "to", for quantitative variables.  For a "regex", a simple regular expression string is specified, and the filter finds all matches for the regular expression against the variable.  For "select" a vector of strings is specified, and all exact matches are returned.  For the range filter, all values of the specified variable within the range "from" and "to" are returned.  If either "from" or "to" are omitted, they are treated as \code{-Inf} and \code{Inf} respectively.
#' @details Trelliscope allows you to specify either a default state in \code{\link{makeDisplay}} or specify the state of the display when you call \code{\link{view}}.
#' @example man-roxygen/ex-state.R
#' @export
stateSpec <- function(name = NULL, group = "common", labels = NULL, layout = NULL,
  sort = NULL, filter = NULL) {

  state <- structure(list(), class = c("list", "cogState"))
  state$name   <- name
  state$group  <- group
  state$sort   <- sort
  state$filter <- filter
  state$labels <- labels
  state$layout <- layout

  state
}

#' Validate State Parameters
#'
#' Validate state parameters for a Trelliscope display
#'
#' @param x a list of state parameter settings (such as layout, sorting, filtering, etc.) to use when the display is viewed (see details)
#' @param displayObj a display object to validate against (if not provided and checkDisplay is TRUE, it will be fetched based on \code{x})
#' @param checkDisplay should the state be checked against a display (to make sure fields match, etc.) or should it simply be checked for structure?
#'
#' @return a modified state parameter list that is valid, an object of class "cogState"
#'
#' @note See \code{\link{stateSpec}} for details on how to specify state.
#'
#' @seealso \code{\link{view}}, \code{\link{cogDisplayHref}}
#'
#' @example man-roxygen/ex-state.R
#' @export
validateState <- function(x, displayObj = NULL,
  checkDisplay = TRUE) {
  nms <- names(x)

  if(!inherits(x, "cogState"))
    stop("Use stateSpec() to get a proper state object.")

  name <- x$name
  group <- x$group

  if(checkDisplay) {
    if(is.null(displayObj))
      displayObj <- getDisplay(name = x$name, group = x$group)
    if(is.null(displayObj))
      stop("Could not find display '", x$name,"' when validating state.  See listDisplays() for a list of available displays, or call validateState with checkDisplay = FALSE.", call. = FALSE)
    ci <- displayObj$cogInfo
    name <- displayObj$name
    group <- displayObj$group
  }

  implStates <- c("name", "group", "sort", "filter", "layout", "labels")

  pnms <- nms[nms %in% implStates]

  extra <- setdiff(nms, pnms)
  if(length(extra) > 0) {
    warning("Specified viewer parameters ", paste(extra, collapse = ", "), " have been ignored.")
  }

  if("sort" %in% pnms) {
    if(checkDisplay) {
      snms <- setdiff(names(x$sort), ci$name)
      if(length(snms) > 0)
        stop("In the 'sort' parameter, variable(s) ", paste(snms, collapse = ","), " were specified but are not present in this display. Available variables are ", paste(ci$name, collapse = ","), ".")
    }

    dirs <- sapply(x$sort, function(a)
      a %in% c("asc", "desc"))

    if(!all(dirs))
      stop("In the 'sort' parameter, all elements must be either 'asc' or 'desc'.")

    for(ii in seq_along(x$sort)) {
      x$sort[[ii]] <- list(order = ii, dir = x$sort[[ii]])
    }

    class(x$sort) <- c("list", "sortState")
  }

  if("layout" %in% pnms) {
    if(!is.null(x$layout$nrow)) {
      x$layout$nrow <- as.integer(x$layout$nrow)
      if(x$layout$nrow < 1)
        stop("In the 'layout' parameter, 'nrow' must be a positive integer")
    } else {
      x$layout$nrow <- 1
    }

    if(!is.null(x$layout$ncol)) {
      x$layout$ncol <- as.integer(x$layout$ncol)
      if(x$layout$ncol < 1)
        stop("In the 'layout' parameter, 'ncol' must be a positive integer")
    } else {
      x$layout$ncol <- 1
    }

    if(!is.null(x$layout$arrange)) {
      if(!x$layout$arrange %in% c("row", "col"))
        stop("In the 'layout' parameter, 'arrange' must be either 'row' or 'col'")
    } else {
      x$layout$arrange <- "row"
    }
    class(x$layout) <- c("list", "layoutState")
  }

  if("labels" %in% pnms) {
    tmp <- try(as.character(x$labels), silent = TRUE)
    if(inherits(tmp, "try-error")) {
      stop("Labels parameter must be a character vector.")
    } else {
      x$labels <- tmp
    }

    if(checkDisplay) {
      # default is conditioning variables
      curLabels <- sort(x$labels)
      defaultLabels <- sort(as.character(ci$name[ci$defLabel]))
      if(!identical(curLabels, defaultLabels)) {
        lnms <- setdiff(x$labels, ci$name)
        if(length(lnms) > 0)
          stop("In the 'labels' parameter, variable(s) ", paste(lnms, collapse = ","), " were specified but are not present in this display. Available variables are ", paste(ci$name, collapse = ","), ".")
      } else {
        x$labels <- NULL
      }
    }

    if(!is.null(x$labels))
      class(x$labels) <- c("character", "labelsState")
  }

  if("filter" %in% pnms) {
    if(checkDisplay) {
      fnms <- setdiff(names(x$filter), ci$name)
      if(length(fnms) > 0)
        stop("In the 'filter' parameter, variable(s) ", paste(fnms, collapse = ","), " were specified but are not present in this display. Available variables are ", paste(ci$name, collapse = ","), ".")
    }

    fltnms <- names(x$filter)

    for(ii in seq_along(x$filter)) {
      flt <- x$filter[[ii]]
      if(checkDisplay)
        curType <- ci$type[which(ci$name == fltnms[[ii]])]

      if(names(flt)[1] == "regex") {
        if(checkDisplay) {
          if(!curType %in% c("factor", "key"))
            stop("Cannot use 'regex' filter for variable '", fltnms[ii], "' because it is not a categorical variable.")
        }
        flt[[1]] <- as.character(flt[[1]])
        if(length(flt[[1]]) != 1)
          stop("A 'regex' filter must be a single string.")
      } else if(names(flt)[1] == "select") {
        flt[[1]] <- as.character(flt[[1]])
        if(checkDisplay) {
          if(!curType %in% c("factor", "key"))
            stop("Cannot use 'select' filter for variable '", fltnms[ii], "' because it is not a categorical variable.")
        }
        if(!is.character(flt[[1]]))
          stop("A 'select' filter must be a single string or vector of strings")
        x$filter[[ii]] <- as.list(x$filter[[ii]])
      } else if(names(flt)[1] %in% c("from", "to")) {
        if(checkDisplay) {
          if(curType != "numeric")
            stop("Cannot use a range filter with 'from' and/or 'to' for variable '", fltnms[ii], "' because it is not a numeric variable.")
        }
        if(!is.null(flt$from))
          if(!is.numeric(flt$from))
            stop("Range filter for variable '", fltnms[ii], "' must be numeric.")
        if(!is.null(flt$to))
          if(!is.numeric(flt$to))
            stop("Range filter for variable '", fltnms[ii], "' must be numeric.")
        if(!is.null(flt$from) && !is.null(flt$to))
          if(flt$from > flt$to)
            stop("In range filter for variable '", fltnms[ii], "', 'from' must be less than 'to'.")
      } else {
        stop("Invalid filter for variable '", fltnms[ii], "'. Must be either a 'regex', 'select' for categorical variables, or numeric range with 'from' and/or 'to' specified for quantitiative variables.")
      }
    }
    class(x$filter) <- c("list", "filterState")
  }
  structure(x, class = "cogState")
}

#' Make a URL hash out of state information
#'
#' Make a URL hash out of cognostics state information
#'
#' @param x a list of cognostics state parameters
#'
#' @return a URL hash
#'
#' @seealso \code{\link{validateState}}
#'
#' @example man-roxygen/ex-state.R
#' @export
makeStateHash <- function(x) {
  if(!inherits(x, "cogState"))
    stop("Use stateSpec() to get a proper state object.")

  res <- list()
  res$name <- x$name
  res$group <- x$group
  res$layout <- toHash(x$layout)
  res$sort <- toHash(x$sort)
  res$filter <- toHash(x$filter)
  res$labels <- toHash(x$labels)

  # TODO: add curPage()

  res <- unlist(res)
  if(!is.null(res))
    res <- paste(names(res), res, collapse = "&", sep = "=")

  res
}

#' Methods for dealing with state and hashes
#'
#' @note Used in \code{\link{makeStateHash}}.
#' @param x state or hash object
#' @example man-roxygen/ex-state.R
#' @export
#' @rdname hash-methods
toHash <- function(x) {
  UseMethod("toHash")
}

#' @export
#' @rdname hash-methods
fromHash <- function(x) {
  UseMethod("fromHash")
}

#' @export
toHash.NULL <- function(x)
  NULL

#' @export
fromHash.NULL <- function(x)
  NULL

#' @export
toHash.filterState <- function(x) {
  res <- sapply(x, function(a) {
    if(names(a)[1] == "regex") {
      res <- paste("(regex:", a, ")", sep = "")
    } else if(names(a)[1] == "select") {
      res <- paste("(select:", paste(unlist(a), collapse = ";", sep = ""), ")", sep = "")
    } else if(names(a)[1] %in% c("from", "to")) {
      res <- paste("(", paste(paste(names(a), ":", sep = ""), a, collapse = ";", sep = ""), ")", sep = "")
    }
    res
  })
  paste(names(res), res, collapse = ",", sep = ":")
}

#' @export
fromHash.filterHash <- function(x) {
  res <- strsplit(x, ",")[[1]]
  res <- strsplit(res, ":")
  keys <- lapply(res, "[", 1)
  values <- lapply(res, function(a) {
    val <- gsub("\\(|\\)", "", a[-1])
    if(val[1] == "regex") {
      val <- list(regex = val[2])
    } else if(val[1] == "select") {
      val <- list(select = as.list(strsplit(val[2], ";")[[1]]))
    } else if(val[1] %in% c("from", "to")) {
      tmp <- do.call(c, strsplit(val, ";"))
      idx <- seq(1, length(tmp), 2)
      val <- as.list(as.numeric(tmp[idx + 1]))
      names(val) <- tmp[idx]
    }
    val
  })
  res <- stats::setNames(values, keys)
  class(res) <- c(class(res), "filterState")
  res
}

#' @export
toHash.sortState <- function(x) {
  orders <- as.integer(sapply(x, function(a) a$order))
  res <- sapply(x[order(orders)], function(x) x$dir)
  paste(names(res), res, collapse = ",", sep = ":")
}

#' @export
fromHash.sortHash <- function(x) {
  res <- strsplit(x, ",")[[1]]
  res <- strsplit(res, ":")
  keys <- sapply(res, "[", 1)
  values <- lapply(seq_along(res), function(i) {
    list(dir = res[[i]][2], order = i)
  })
  res <- stats::setNames(as.list(values), keys)
  class(res) <- c(class(res), "sortState")
  res
}

#' @export
toHash.labelsState <- function(x) {
  paste(x, collapse = ",", sep = "")
}

#' @export
fromHash.labelsHash <- function(x) {
  res <- strsplit(x, ",")[[1]]
  class(res) <- c(class(res), "labelsState")
  res
}

#' @export
toHash.layoutState <- function(x) {
  x$w <- x$h <- NULL # don't store height / width state
  # if defaults, don't include in string
  if(!is.null(x$nrow))
    if(x$nrow == 1)
      x$nrow <- NULL
  if(!is.null(x$ncol))
    if(x$ncol == 1)
      x$ncol <- NULL
  if(!is.null(x$arrange))
    if(x$arrange == "row")
      x$arrange <- NULL

  x <- unlist(x)
  if(length(x) > 0) {
    paste(names(x), x, collapse = ",", sep = ":")
  } else {
    NULL
  }
}

#' @export
fromHash.layoutHash <- function(x) {
  res <- strsplit(x, ",")[[1]]
  res <- strsplit(res, ":")
  keys <- sapply(res, "[", 1)
  values <- sapply(res, "[", 2)
  res <- stats::setNames(as.list(values), keys)

  if(is.null(res$nrow)) {
    res$nrow <- 1
  } else {
    res$nrow <- as.integer(res$nrow)
  }

  if(is.null(res$ncol)) {
    res$ncol <- 1
  } else {
    res$ncol <- as.integer(res$ncol)
  }

  if(is.null(res$arrange)) {
    res$arrange <- "row"
  }

  class(res) <- c(class(res), "layoutState")
  res
}

