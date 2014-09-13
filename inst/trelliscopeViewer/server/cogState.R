
# Data for these inputs are set independently within control panels
# - sortStateInput (from cogColumnSortInput)
# - filterStateInput (from cogColumnFilterInput, univarFilterState, or bivarFilterState)
# - panelLayoutStateInput
# - panelLabelStateInput

# When a user hits the "Apply" button, an associated *ApplyButton function is executed which sets input (e.g. filterStateInput) and triggers change on that input

# A collection of all of these inputs is gathered and sent to output as data in an empty div called exposedStateDataOutput -- this output is dependent on all of the above inputs and builds up a list to be thought of as the current state exposed to the panel arrangement

# every time exposedStateDataOutput is updated, we want to force all control panels to update their content
# we do this with a function updateControlsExposedState()
# this calls methods for each individual control

filterState <- reactive({
   fs <- input$filterStateInput
   if(is.null(fs)) {
      fs <- currentDisplay()$cdo$state$filter
   } else {
      logMsg("- filter state changed")
      class(fs) <- c(class(fs), "filterState")
   }
   fs
})

sortState <- reactive({
   ss <- input$sortStateInput
   if(is.null(ss)) {
      ss <- currentDisplay()$cdo$state$sort
   } else {
      logMsg("- panel sort state changed")
      class(ss) <- c(class(ss), "sortState")
   }
   ss
})

panelLabelState <- reactive({
   pl <- input$panelLabelStateInput
   if(is.null(pl)) {
      pl <- currentDisplay()$cdo$state$labels
   } else {
      logMsg("- panel label state changed: ", paste(pl, collapse = ","))
      class(pl) <- c(class(pl), "labelState")
      if(pl[1] == "__none__")
         pl <- NULL
   }
   pl
})

panelLayoutState <- reactive({
   pls <- input$panelLayoutStateInput
   if(is.null(pls)) {
      pls <- currentDisplay()$cdo$state$layout      
   } else {
      logMsg("- panel layout state changed: nrow: ", pls$nrow, ", ncol: ", pls$ncol, ", w: ", pls$w, " h: ", pls$h, " arrange: ", pls$arrange)
      class(pls) <- c(class(pls), "layoutState")
   }
   pls
})

activeCogState <- reactive({
   ac <- input$activeCogStateInput
   if(!is.null(ac)) {
      class(ac) <- c(class(ac, "activeCogState"))
      logMsg("- active cog state changed: ", paste(ac, collapse=","))
   }
   ac
})

relatedDisplayState <- reactive({
   rd <- input$relatedDisplayStateInput
   if(length(rd) > 0) {
      logMsg("- related display state changed")      
   } else {
      rd <- NULL
   }
   rd
})

cdoExposedCogState <- reactive({
   cdo <- currentDisplay()$cdo
   
   if(!is.null(cdo)) {
      cdo$state$filter <- filterState()
      cdo$state$sort <- sortState()
      cdo$state$labels <- panelLabelState()
      cdo$state$layout <- panelLayoutState()
      cdo$state$activeCog <- activeCogState()
      cdo$state$relatedDisplays <- relatedDisplayState()
      
      relatedDisplayObjects <- list()
      if(length(cdo$state$relatedDisplays) > 0) {
         # load the additional displays
         rd <- cdo$state$relatedDisplays
         for(i in seq_along(rd)) {
            curName <- rd[[i]]$name
            curGroup <- rd[[i]]$group
            dispKey <- paste(curGroup, curName, sep = "___")
            if(curName == cdo$name && curGroup == cdo$group) {
               relatedDisplayObjects[[dispKey]] <- NULL
            } else {
               tmp <- getDisplay(name = curName, group = curGroup)
               
               # load packages and data
               if(!is.null(tmp$relatedPackages)) {
                  logMsg("Loading packages: ", paste(tmp$relatedPackages, collapse = ", "))
                  for(pkg in tmp$relatedPackages)
                     suppressMessages(require(pkg, character.only = TRUE))
               }
               
               # load any related data into global environment
               # note: these should not be put in global environment
               # but a display-specific environment - need to update
               if(!is.null(tmp$relatedData)) {
                  for(nm in names(tmp$relatedData))
                     .GlobalEnv[[nm]] <- tmp$relatedData[[nm]]
               }
               
               relatedDisplayObjects[[dispKey]] <- tmp
            }
         }
      }
      cdo$relatedDisplayObjects <- relatedDisplayObjects      
   }
   
   # cdo$state$sample <- 
   # cdo$state$multivar <- 
   
   cdo
})

output$exposedStateDataOutput <- renderData({
   cdo <- cdoExposedCogState()
   # if(!is.null(cdo$state))
   #    browser()
   cdo$state
})

output$cogBreadcrumbOutput <- renderDataLite({
   cdo <- cdoExposedCogState()
   if(length(cdo$state$filter) > 0 || length(cdo$state$sort) > 0) {
      # btnLookup <- list(sort = "btn-primary", filter = "btn-success", multivar = "btn-warning", sample = "btn-danger")
      # iconLookup <- list(sort = list(numeric = list(asc = "icon-sort-numeric-asc", desc = "icon-sort-numeric-desc"), categ = list(asc = "icon-sort-alpha-asc", desc = "icon-sort-alpha-desc")), filter = "icon-filter", multivar = "icon-multivar", sample = "icon-sample")
      state <- cdo$state
      
      filterNm <- names(state$filter)
      filters <- lapply(filterNm, function(nm) {
         list(name = nm, class = "btn-success filter-breadcrumb", icon = "icon-filter", type = "filter")
      })
      
      iconLookup <- list(
         "asc" = list(
            "numeric" = "icon-sort-numeric-asc",
            "factor" = "icon-sort-alpha-asc"
         ),
         "desc" = list(
            "numeric" = "icon-sort-numeric-desc",
            "factor" = "icon-sort-alpha-desc"
         )
      )
      
      sortNm <- names(state$sort)
      sorts <- lapply(sortNm, function(nm) {
         cur <- state$sort[[nm]]
         type <- cdo$cogInfo$type[cdo$cogInfo$name == nm]
         bcIcon <- iconLookup[[cur$dir]][[type]]
         list(name = nm, class = "btn-primary sort-breadcrumb", icon = bcIcon, order = cur$order, type = "sort")
      })
      
      if(length(sorts) > 0) {
         ord <- sapply(sorts, function(x) x$order)
         sorts <- sorts[order(ord)]
      }
      
      return(list(buttons = c(filters, sorts)))
   }
})

output$appHashOutput <- renderText({
   cdo <- cdoExposedCogState()
   
   res <- list()
   
   res$name <- cdo$name
   res$group <- cdo$group
   
   if(!is.null(cdo$state$layout))
      res$layout <- toHash(cdo$state$layout)
   
   if(length(cdo$state$sort) > 0)
      res$sort <- toHash(cdo$state$sort)
   
   if(length(cdo$state$filter) > 0)
      res$filter <- toHash(cdo$state$filter)
   
   if(length(cdo$state$labels) > 0) {
      curLabels <- sort(as.character(cdo$state$labels))
      defaultLabels <- sort(as.character(cdo$cogInfo$name[cdo$cogInfo$defLabel]))
      if(!identical(curLabels, defaultLabels))
         res$labels <- toHash(cdo$state$labels)
   }
   
   # TODO: add curPage()
   
   res <- unlist(res)
   if(!is.null(res))
      paste(names(res), res, collapse = "&", sep = "=")
})

# $layout
# [1] "ncol:2"
# $sort
# [1] "state:asc,slope:desc"
# $filter
# [1] "county:(regex:a),state:(select:AL;AR),slope:(from:0;to:1),meanList:(from:50)"
# $labels
# [1] "county,state,slope"

toHash <- function(x) {
   UseMethod("toHash")
}

fromHash <- function(x) {
   UseMethod("fromHash")
}

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
   res <- setNames(values, keys)
   class(res) <- c(class(res), "filterState")
   res
}

toHash.sortState <- function(x) {
   orders <- as.integer(sapply(x, function(a) a$order))
   res <- sapply(x[order(orders)], function(x) x$dir)
   paste(names(res), res, collapse = ",", sep = ":")
}

fromHash.sortHash <- function(x) {
   res <- strsplit(x, ",")[[1]]
   res <- strsplit(res, ":")
   keys <- sapply(res, "[", 1)
   values <- lapply(seq_along(res), function(i) {
      list(dir = res[[i]][2], order = i)
   })
   res <- setNames(as.list(values), keys)
   class(res) <- c(class(res), "sortState")
   res
}

toHash.labelState <- function(x) {
   paste(x, collapse = ",", sep = "")
}

fromHash.labelsHash <- function(x) {
   res <- strsplit(x, ",")[[1]]
   class(res) <- c(class(res), "labelState")
   res
}

toHash.layoutState <- function(x) {
   x$w <- x$h <- NULL # don't store height / width state
   # if defaults, don't include in string
   if(x$nrow == 1)
      x$nrow <- NULL
   if(x$ncol == 1)
      x$ncol <- NULL
   if(x$arrange == "row")
      x$arrange <- NULL
   
   x <- unlist(x)
   if(length(x) > 0) {
      paste(names(x), x, collapse = ",", sep = ":")      
   } else {
      NULL
   }
}

fromHash.layoutHash <- function(x) {
   res <- strsplit(x, ",")[[1]]
   res <- strsplit(res, ":")
   keys <- sapply(res, "[", 1)
   values <- sapply(res, "[", 2)
   res <- setNames(as.list(values), keys)
   
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



