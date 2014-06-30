
# contains the sorted / filtered cognostics data frame
# reflective of the current state of sorting / filtering
exposedCogDF <- reactive({
   cdo <- cdoExposedCogState()
   if(!is.null(cdo)) {
      cogDF <- getCurCogDat(cdo$cogDatConn, cdo$state)

      list(cdo = cdo, curCogDF = cogDF)      
   }
})

output$panelPageTotOutput <- renderText({
   cdo <- exposedCogDF()
   
   if(!is.null(cdo)) {
      totPanels <- cogNrow(cdo$curCogDF)
      ppp <- cdo$cdo$state$panelLayout$nrow * cdo$cdo$state$panelLayout$ncol
      
      ceiling(totPanels / ppp)      
   }
})

output$panelTableContentOutput <- renderDataLite({
   cdo <- exposedCogDF()
   curPage <- input$curPanelPageInput
   if(!is.null(cdo) && !is.null(curPage)) {
      curPage <- as.integer(curPage)
      
      labelVars <- cdo$cdo$state$panelLabel
      w <- cdo$cdo$state$panelLayout$w
      h <- cdo$cdo$state$panelLayout$h
      nr <- cdo$cdo$state$panelLayout$nrow
      nc <- cdo$cdo$state$panelLayout$ncol
      ppp <- nr * nc
      
      idxStart <- (curPage - 1) * ppp + 1
      idxEnd <- min(cogNrow(cdo$curCogDF), curPage * ppp)
      
      curRows <- cdo$curCogDF[idxStart:idxEnd, , drop = FALSE]
      
      # create list where each element is a vector of indices for a row of panels
      idxMat <- matrix(seq_len(nr * nc), nrow = nr, ncol = nc, byrow = TRUE)
      idxList <- lapply(seq_len(nr), function(x) idxMat[x,])
      
      panelContent <- paste("<img src=\"", 
         getPanels(cdo, curRows, 
            pixelratio = session$clientData$pixelratio), 
         "\" width=\"", w, "\" height=\"", h, "\">", sep = "")      
      
      lapply(idxList, function(rw) {
         lapply(rw, function(i) {
            if(i + idxStart - 1 > idxEnd) {
               curPanelContent <- dummyPanel(w, h)
               cogData <- dummyCog(labelVars)
            } else {
               curPanelContent <- panelContent[i]
               tmp <- cogDataString(curRows[i, labelVars, drop = FALSE])
               cogData <- data.frame(cog_name = labelVars, cog_value = tmp[1,])
            }
            list(
               i = i,
               panel_content = curPanelContent,
               cogs = cogData
            )
         })
      })
   }
})

