
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
    class(fs) <- c(class(fs), "filterState")
  }
  logMsg("- filter state changed: ", toHash(fs))
  fs
})

sortState <- reactive({
  ss <- input$sortStateInput
  if(is.null(ss)) {
    ss <- currentDisplay()$cdo$state$sort
  } else {
    class(ss) <- c(class(ss), "sortState")
  }
  logMsg("- panel sort state changed: ", toHash(ss))
  ss
})

panelLabelState <- reactive({
  cur <- currentDisplay()$cdo$state$labels
  pl <- input$panelLabelStateInput
  if(is.null(pl)) {
    pl <- cur
  } else {
    class(pl) <- c(class(pl), "labelsState")
    if(pl[1] == "__none__")
      pl <- NULL
  }
  logMsg("- panel label state changed: ", toHash(pl))
  pl
})

panelLayoutState <- reactive({
  pls <- input$panelLayoutStateInput
  if(is.null(pls)) {
    pls <- currentDisplay()$cdo$state$layout
  } else {
    class(pls) <- c(class(pls), "layoutState")
  }
  logMsg("- panel layout state changed: nrow: ", pls$nrow, ", ncol: ", pls$ncol, ", w: ", pls$w, " h: ", pls$h, " arrange: ", pls$arrange)
  pls
})

activeCogState <- reactive({
  ac <- input$activeCogStateInput
  if(!is.null(ac)) {
    class(ac) <- c(class(ac), "activeCogState")
  }
  logMsg("- active cog state changed: ", paste(ac, collapse=","))
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
    cdo$state <- stateSpec(
      name = cdo$name,
      group = cdo$group)

    cdo$state$filter = filterState()
    cdo$state$sort = sortState()
    cdo$state$labels = panelLabelState()
    cdo$state$layout = panelLayoutState()
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

          # don't need cog data
          tmp$cogDatConn <- NULL

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

## want scalars as json scalars so use renderData instead of renderDataLite
output$exposedStateDataOutput <- renderData({
  cdo <- cdoExposedCogState()
  # if(!is.null(cdo$state))
  #   browser()
  c(list(name = cdo$name, group = cdo$group), cdo$state)
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

  if(!is.null(cdo))
    makeStateHash(cdo$state)
})


