
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
   curPage <- suppressWarnings(as.integer(input$curPanelPageInput))
   if(length(curPage) == 0)
      curPage <- NULL
   if(!is.null(curPage)) {
      if(is.na(curPage))
         curPage <- NULL
   }
   
   if(!is.null(cdo) && !is.null(curPage)) {
      labelVars <- cdo$cdo$state$panelLabel
      
      if(is.null(cdo$cdo$state$relatedDisplays)) {
         # we are in regular plotting mode
         w <- cdo$cdo$state$panelLayout$w
         h <- cdo$cdo$state$panelLayout$h
         nr <- cdo$cdo$state$panelLayout$nrow
         nc <- cdo$cdo$state$panelLayout$ncol
         arrange <- cdo$cdo$state$panelLayout$arrange
         byRow <- TRUE
         if(arrange == "col")
            byRow <- FALSE
         ppp <- nr * nc

         idxStart <- (curPage - 1) * ppp + 1
         if(idxStart > cogNrow(cdo$curCogDF))
            return(NULL)
         idxEnd <- min(cogNrow(cdo$curCogDF), curPage * ppp)
         
         curRows <- cdo$curCogDF[idxStart:idxEnd, , drop = FALSE]
         
         # create list where each element is a vector of indices for a row of panels
         idxMat <- matrix(seq_len(nr * nc), nrow = nr, ncol = nc, byrow = byRow)
         idxList <- lapply(seq_len(nr), function(x) idxMat[x,])
         
         logMsg("Rendering panel for keys ", paste(curRows$panelKey, collapse = ","))
         
         # panelContent should be a list:
         # - html
         # - vega
         
         panelContent <- getPanels(cdo$cdo, 
            width = w, 
            height = h, 
            curRows = curRows, 
            pixelratio = session$clientData$pixelratio)
         # browser()
         lapply(idxList, function(rw) {
            lapply(rw, function(i) {
               if(i + idxStart - 1 > idxEnd) {
                  curPanelContent <- dummyPanel(w, h)
                  cogData <- dummyCog(labelVars)
               } else {
                  curPanelContent <- panelContent[[i]]
                  # browser()
                  tmp <- cogDataString(curRows[i, labelVars, drop = FALSE])
                  if(is.null(labelVars)) {
                     cogData <- NULL
                  } else {
                     cogData <- data.frame(cog_name = labelVars, cog_value = tmp[1,])
                  }
               }
               # browser()
               list(
                  i = i,
                  html_wrap_start = "",
                  panel_content = list(curPanelContent),
                  html_wrap_end = "",
                  cogs = cogData
               )
            })
         })
      } else {
         # we are showing related displays
         curRows <- cdo$curCogDF[curPage, , drop = FALSE]
         
         # div with each related display rendered with relative position
         tmp <- cogDataString(curRows[1, labelVars, drop = FALSE])
         if(is.null(labelVars)) {
            cogData <- NULL
         } else {
            cogData <- data.frame(cog_name = labelVars, cog_value = tmp[1,])
         }
         
         pageHeight <- cdo$cdo$state$relatedDisplays[[1]]$pageHeight
         pageWidth <- cdo$cdo$state$relatedDisplays[[1]]$pageWidth
         
         curPanelContent <- lapply(seq_along(cdo$cdo$state$relatedDisplays), function(i) {
            a <- cdo$cdo$state$relatedDisplays[[i]]
            dispKey <- paste(a$group, a$name, sep = "___")
            if(is.null(cdo$cdo$relatedDisplayObjects[[dispKey]])) {
               curDisp <- cdo$cdo
            } else {
               curDisp <- cdo$cdo$relatedDisplayObjects[[dispKey]]
            }
            
            tmp <- getPanels(curDisp, 
               width = a$width, # + 4
               height = a$height, # - 4
               curRows = curRows, 
               pixelratio = session$clientData$pixelratio)[[1]]
            
            tmp$data$id <- paste("#rel-disp-div-", i, sep = "")
            tmp$html <- paste("<div style=\"position: absolute; top: ", a$top, "px; left: ", a$left, "px;\" id=\"rel-disp-div-", i, "\">", tmp$html, "</div>", sep = "")
            tmp
         })
         
         names(curPanelContent) <- NULL
         # browser()
         list(list(list(
            i = 1,
            html_wrap_start = paste("<div style=\"width: ", 
               pageWidth, "px; height: ", 
               pageHeight, "px;\">", sep = ""),
            panel_content = curPanelContent,
            html_wrap_end = "</div>",
            cogs = cogData
         )))
      }
   }
})


