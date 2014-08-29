## functions that get template data in proper form
source("server/currentDisplayData.R")

## list of displays to select from for current vdb
output$displayListOutput <- renderDataLite({
   displayListOutputData(displayList)
})
outputOptions(output, "displayListOutput", suspendWhenHidden = FALSE)

## holds name and group of currently selected display
selectedDisplay <- reactive({
   # TODO: appHash stuff here...
   input$displaySelectInput
})

## name of the display printed in the header
output$headerDisplayNameOutput <- renderText({
   sld <- selectedDisplay()
   if(!is.null(sld))
      paste(sld$group, "/", sld$name)
})

## holds the currently selected displayObject
currentDisplay <- reactive({
   sld <- selectedDisplay()
   
   if(!is.null(sld)) {
      logMsg("Loading display: ", paste(sld$group, "/", sld$name))
      cdo <- do.call(getDisplay, sld)
      logMsg("Display loaded")
      
      # load required packages
      if(!is.null(cdo$relatedPackages)) {
         logMsg("Loading packages: ", paste(cdo$relatedPackages, collapse = ", "))
         for(pkg in cdo$relatedPackages)
            suppressMessages(require(pkg, character.only = TRUE))
      }
      
      # set up an environment for this display to be evaluated in
      # dispEnv <- new.env(parent = .GlobalEnv)
      # cdo$envir <- .GlobalEnv
      
      # load any related data into global environment
      # note: these should not be put in global environment
      # but a display-specific environment - need to update
      if(!is.null(cdo$relatedData)) {
         for(nm in names(cdo$relatedData))
            .GlobalEnv[[nm]] <- cdo$relatedData[[nm]]
      }
      
      if(is.null(cdo$cogDistns))
         cdo$cogDistns <- trelliscope:::getCogDistns(cdo$cogDatConn)
      
      logMsg("Getting default state...")
      # default state values if not specified
      if(is.null(cdo$state$panelLabelState)) {
         defaultLabels <- cdo$cogInfo$name[cdo$cogInfo$defLabel]
         if(length(defaultLabels) == 0)
            defaultLabels <- NULL
         cdo$state$panelLabel <- defaultLabels
      }

      if(is.null(cdo$state$activeCog)) {
         defaultActive <- cdo$cogInfo$name[cdo$cogInfo$defActive]
         if(length(defaultActive) == 0)
            defaultActive <- NULL
         cdo$state$activeCog <- defaultActive
      }
      
      if(is.null(cdo$state$panelLayout))
         cdo$state$panelLayout <- list(nrow = 1, ncol = 1)
      
      # TODO: add related
      list(cdo = cdo)
   }
})

## initial UI template outputs dependent on selected display

output$panelLayoutOutput <- renderDataLite({
   panelLayoutOutputData(currentDisplay())
})

output$panelFunctionOutput <- renderDataLite({
   panelFunctionOutputData(currentDisplay())
})

output$panelLabelListOutput <- renderDataLite({
   panelLabelListOutputData(currentDisplay())
})

output$relatedDisplayListOutput <- renderDataLite({
   relatedDisplayListOutputData(currentDisplay(), displayList)
})

output$activeCogListOutput <- renderDataLite({
   activeCogListOutputData(currentDisplay())
})

output$cogTableControlsOutput <- renderDataLite({
   cogTableControlsOutputData(currentDisplay())
})

output$cogUniFilterControlsOutput <- renderDataLite({
   cogUniFilterControlsOutputData(currentDisplay())
})

output$cogBiFilterControlsOutput <- renderDataLite({
   cogBiFilterControlsOutputData(currentDisplay())
})

output$cogMultiFilterControlsOutput <- renderDataLite({
   cogBiFilterControlsOutputData(currentDisplay())
})

output$cogSampleControlsOutput <- renderDataLite({
   list()
})

output$panelPageNavOutput <- renderDataLite({
   panelPageNavOutputData(currentDisplay())
})

output$cogMapOutput <- renderDataLite({
   cogBiFilterControlsOutputData(currentDisplay())
})

cdoCogState <- reactive({
   cdo <- currentDisplay()$cdo
   cdo
})
