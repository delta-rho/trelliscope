
# Data for these inputs are set independently within control panels
# - sortStateInput (from cogColumnSortInput)
# - filterStateInput (from cogColumnFilterInput, univarFilterState, or bivarFilterState)
# - panelLayoutStateInput
# - visibleCogStateInput

# When a user hits the "Apply" button, an associated *ApplyButton function is executed which sets input (e.g. filterStateInput) and triggers change on that input

# A collection of all of these inputs is gathered and sent to output as data in an empty div called exposedStateDataOutput -- this output is dependent on all of the above inputs and builds up a list to be thought of as the current state exposed to the panel arrangement

# every time exposedStateDataOutput is updated, we want to force all control panels to update their content
# we do this with a function updateControlsExposedState()
# this calls methods for each individual control

cdoExposedCogState <- reactive({
   cdo <- currentDisplay()$cdo
   filterState <- input$filterStateInput
   if(!is.null(filterState))
      cdo$state$filter <- filterState
   
   sortState <- input$sortStateInput
   if(!is.null(sortState))
      cdo$state$sort <- sortState
   
   visibleCogState <- input$visibleCogStateInput
   if(!is.null(visibleCogState))
      cdo$state$visibleCog <- visibleCogState
   # browser()
   panelLayoutState <- input$panelLayoutStateInput
   if(!is.null(panelLayoutState))
      cdo$state$panelLayout <- panelLayoutState
   
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
         list(name = nm, class = "btn-success filter-breadcrumb", icon = "icon-filter")
      })
      
      sortNm <- names(state$sort)
      sorts <- lapply(sortNm, function(nm) {
         cur <- state$sort[[nm]]
         list(name = nm, class = "btn-primary sort-breadcrumb", icon = cur$bcIcon, order = cur$order)
      })
      
      if(length(sorts) > 0) {
         ord <- sapply(sorts, function(x) x$order)
         sorts <- sorts[order(ord)]
      }
      
      return(list(buttons = c(filters, sorts)))
   }
})



