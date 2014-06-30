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
      cdo <- do.call(getDisplay, sld)
      if(is.null(cdo$cogInfo))
         cdo$cogInfo <- trelliscope:::getCogInfo(cdo$cogDatConn)
      
      # default state values if not specified
      if(is.null(cdo$state$panelLabelState))
         cdo$state$panelLabel <- cdo$cogDesc$name[cdo$cogDesc$type == "splitVar"]
      
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
