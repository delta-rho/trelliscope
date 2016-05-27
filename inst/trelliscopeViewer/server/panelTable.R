
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
  cd <- exposedCogDF()

  if(!is.null(cd)) {
    totPanels <- cogNrow(cd$curCogDF)
    ppp <- cd$cdo$state$layout$nrow * cd$cdo$state$layout$ncol

    ceiling(totPanels / ppp)
  }
})

curPage <- reactive({
  curPage <- suppressWarnings(as.integer(input$curPanelPageInput))
  if(length(curPage) == 0)
    curPage <- NULL
  if(!is.null(curPage)) {
    if(is.na(curPage))
      curPage <- NULL
  }
  curPage
})

output$panelTableContentOutput <- renderDataLite({
  cd <- exposedCogDF()

  curPage <- curPage()

  if(!is.null(cd) && !is.null(curPage)) {
    labelVars <- cd$cdo$state$labels

    if(is.null(cd$cdo$state$relatedDisplays)) {
      w <- cd$cdo$state$layout$w
      h <- cd$cdo$state$layout$h
      if(!is.null(h) && !is.null(w)) {
        # we are in regular plotting mode
        nr <- cd$cdo$state$layout$nrow
        nc <- cd$cdo$state$layout$ncol
        arrange <- cd$cdo$state$layout$arrange
        byRow <- TRUE
        if(arrange == "col")
          byRow <- FALSE
        ppp <- nr * nc

        idxStart <- (curPage - 1) * ppp + 1
        if(idxStart > cogNrow(cd$curCogDF))
          return(NULL)
        idxEnd <- min(cogNrow(cd$curCogDF), curPage * ppp)

        curRows <- cd$curCogDF[idxStart:idxEnd, , drop = FALSE]

        # create list where each element is a vector of indices for a row of panels
        idxMat <- matrix(seq_len(nr * nc), nrow = nr, ncol = nc, byrow = byRow)
        idxList <- lapply(seq_len(nr), function(x) idxMat[x,])

        logMsg("Rendering panel for keys ", paste(curRows$panelKey, collapse = ","))

        panelContent <- getPanels(cd$cdo,
          width = w,
          height = h,
          curRows = curRows,
          pixelratio = session$clientData$pixelratio)

        lapply(idxList, function(rw) {
          lapply(rw, function(i) {
            if(i + idxStart - 1 > idxEnd) {
              curPanelContent <- list(html = dummyPanel(w, h), class = "raster")
              cogData <- dummyCog(labelVars)
            } else {
              curPanelContent <- panelContent[[i]]
              tmp <- cogDataString(curRows[i, labelVars, drop = FALSE])
              tmp2 <- cogTitleString(curRows[i, labelVars, drop = FALSE])
              if(is.null(labelVars)) {
                cogData <- NULL
              } else {
                cogData <- data.frame(cog_name = labelVars,
                  cog_value = tmp[1,], cog_title = tmp2[1,])
              }
            }

            list(
              i = i,
              html_wrap_start = "",
              panel_content = list(curPanelContent),
              html_wrap_end = "",
              cogs = cogData
            )
          })
        })
      }
    } else {
      # we are showing related displays
      curRows <- cd$curCogDF[curPage, , drop = FALSE]

      # div with each related display rendered with relative position
      tmp <- cogDataString(curRows[1, labelVars, drop = FALSE])
      tmp2 <- cogTitleString(curRows[1, labelVars, drop = FALSE])

      if(is.null(labelVars)) {
        cogData <- NULL
      } else {
        cogData <- data.frame(cog_name = labelVars,
          cog_value = tmp[1,], cog_title = tmp2[1,])
      }

      pageHeight <- cd$cdo$state$relatedDisplays[[1]]$pageHeight
      pageWidth <- cd$cdo$state$relatedDisplays[[1]]$pageWidth

      curPanelContent <- lapply(seq_along(cd$cdo$state$relatedDisplays), function(i) {
        a <- cd$cdo$state$relatedDisplays[[i]]
        dispKey <- paste(a$group, a$name, sep = "___")
        if(is.null(cd$cdo$relatedDisplayObjects[[dispKey]])) {
          curDisp <- cd$cdo
        } else {
          curDisp <- cd$cdo$relatedDisplayObjects[[dispKey]]
        }

        tmp <- try(getPanels(curDisp,
          width = a$width, # + 4
          height = a$height, # - 4
          curRows = curRows,
          pixelratio = session$clientData$pixelratio)[[1]],
        silent = TRUE)
        if(inherits(tmp, "try-error"))
          tmp <- list(html = paste0("<div style='background-color: #efefef; width: ", a$width, "px; height: ", a$height, "px'></div>"))

        tmp$data$id <- paste("#rel-disp-div-", i, sep = "")
        tmp$html <- paste("<div style=\"position: absolute; top: ", a$top, "px; left: ", a$left, "px;\" id=\"rel-disp-div-", i, "\">", tmp$html, "</div>", sep = "")
        tmp
      })

      names(curPanelContent) <- NULL

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
