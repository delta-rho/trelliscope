

output$univarFilterPlot <- renderDataLite({
  selectVals <- input$univarFilterSelect
  cdo <- exposedCogDF()
  if(!is.null(selectVals) && !is.null(cdo)) {
    getUnivarPlotDat(cdo, selectVals$varName, distType = selectVals$distType, plotType = selectVals$plotType, maxLevels = 10000)
  }
})

output$bivarFilterPlot <- renderDataLite({
  selectVals <- input$bivarFilterSelect
  cdo <- exposedCogDF()

  if(!is.null(selectVals$xName) && !is.null(selectVals$yName) && !is.null(selectVals$distType) && !is.null(selectVals$plotType) && !is.null(cdo)) {
    getBivarPlotDat(cdo, selectVals$xName, selectVals$yName, distType = selectVals$distType, plotType = selectVals$plotType)
  }
})

# output$multivarFilterPlot <- renderDataLite({
#   selectVals <- input$multivarFilterSelect
#   cdo <- exposedCogDF()

#   if(!is.null(selectVals) && !is.null(cdo)) {
#     getMultivarPlotDat(cdo, unlist(selectVals$varNames), distType = selectVals$distType, plotType = selectVals$plotType)
#   }
# })


