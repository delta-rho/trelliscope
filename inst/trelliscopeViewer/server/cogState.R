
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

cdoExposedCogState <- reactive({
   cdo <- currentDisplay()$cdo
   filterState <- input$filterStateInput
   if(!is.null(filterState)) {
      logMsg("- filter state changed")
      cdo$state$filter <- filterState      
   }
   
   sortState <- input$sortStateInput
   if(!is.null(sortState)) {
      logMsg("- panel sort state changed")
      cdo$state$sort <- sortState      
   }
   
   panelLabelState <- input$panelLabelStateInput
   if(!is.null(panelLabelState)) {
      logMsg("- panel label state changed: ", paste(panelLabelState, collapse=","))
      if(panelLabelState == "__none__")
         panelLabelState <- NULL
      cdo$state$panelLabel <- panelLabelState      
   }
   
   pls <- input$panelLayoutStateInput
   if(!is.null(pls)) {
      logMsg("- panel layout state changed: nrow: ", pls$nrow, ", ncol: ", pls$ncol, ", w: ", pls$w, " h: ", pls$h, " arrange: ", pls$arrange)
      cdo$state$panelLayout <- pls
   }
   
   activeCogState <- input$activeCogStateInput
   if(!is.null(activeCogState)) {
      logMsg("- active cog state changed: ", paste(activeCogState, collapse=","))
      cdo$state$activeCog <- activeCogState
   }
   relatedDisplayState <- input$relatedDisplayStateInput
   if(length(relatedDisplayState) > 0) {
      # load the additional displays
      logMsg("- related display state changed")
      relatedDisplayObjects <- list()
      for(i in seq_along(relatedDisplayState)) {
         curName <- relatedDisplayState[[i]]$name
         curGroup <- relatedDisplayState[[i]]$group
         dispKey <- paste(curGroup, curName, sep = "___")
         if(curName == cdo$name && curGroup == cdo$group) {
            relatedDisplayObjects[[dispKey]] <- NULL
         } else {
            relatedDisplayObjects[[dispKey]] <- getDisplay(name = curName, group = curGroup)
         }
      }
      cdo$relatedDisplayObjects <- relatedDisplayObjects
      cdo$state$relatedDisplays <- relatedDisplayState
   }
   
   # cdo$state$sample <- 
   # cdo$state$multivar <- 
   
   cdo
})

# is this really necessary? could just set it in the browser
# before triggering each state input
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
      
      sortNm <- names(state$sort)
      sorts <- lapply(sortNm, function(nm) {
         cur <- state$sort[[nm]]
         list(name = nm, class = "btn-primary sort-breadcrumb", icon = cur$bcIcon, order = cur$order, type = "sort")
      })
      
      if(length(sorts) > 0) {
         ord <- sapply(sorts, function(x) x$order)
         sorts <- sorts[order(ord)]
      }
      
      return(list(buttons = c(filters, sorts)))
   }
})



