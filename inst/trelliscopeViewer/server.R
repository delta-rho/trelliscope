library(shiny)
library(base64enc)

# if(file.exists("conn.Rdata")) { # for glimmer
if(file.exists("../conn.Rdata")) { # it is on a web server
   library(trelliscope)
   # vdbConn <- vdbConn(getwd()) # for glimmer
   vdbConn <- vdbConn(normalizePath(file.path(getwd(), "..")))
   vdbPrefix <- vdbConn$path
} else {
   vdbConn <- getOption("vdbConn")
   vdbPrefix <- vdbConn$path
}
message("vdbPrefix is ", vdbPrefix)

options(vdbShinyPrefix = vdbPrefix)

sapply(list.files("_serverHelpers", full.names=TRUE), source)

verbose <- FALSE
if(is.null(verbose))
   verbose <- TRUE

load(file.path(vdbPrefix, "displays/_displayList.Rdata"))
ind <- which(is.na(displayListDF$dataClass))
if(length(ind) > 0) {
   displayList <- displayList[-ind]
   displayListDF <- displayListDF[-ind,]
}

shinyServer(function(input, output) {
   
   conn <- getOption("vdbConn")
   
   # NOTE: reactive objects starting with "cd" is shorthand for "current display", meaning that it corresponds with the display currently being viewed
   
   ### get the current display name, either from a URL hash or the input
   cdName <- reactive({
      uid <- as.integer(input$displayListTable)
      appHash <- input$appHash
      # TODO: add check for whether the plot exists and give it a good error message
      getCdName(uid, appHash, displayListDF, verbose)
   })
   
   getPanelRows <- reactive({
      # pr <- as.integer(getFromAppHash(input$appHash, "nrow")$nrow)
      # if(is.null(pr))
         pr <- input$panelRows
      pr
   })
   
   getPanelCols <- reactive({
      # pc <- as.integer(getFromAppHash(input$appHash, "ncol")$ncol)
      # browser()
      # if(is.null(pc))
         pc <- input$panelCols
      pc
   })
   
   ## displays the current display group / name in the page header
   output$displayNameHeader <- renderUI({
      cdn <- cdName()
      txt <- ""
      if(!is.null(cdn)) {
         logMsg("Updating header text", verbose=verbose)
         txt <- paste(cdn, collapse=" / ")
      }
      tags$a(class='brand', txt)
   })
   outputOptions(output, 'displayNameHeader', suspendWhenHidden=FALSE)
   
   ### holds the data in "object.Rdata" for the current display
   cdDisplayObj <- reactive({
      cdn <- cdName()
      if(is.null(cdn)) {
         return()
      } else {
         cdo <- getDisplay(group=cdn[1], name=cdn[2])
         logMsg("Retrieving display metadata", verbose=verbose)
         return(cdo)
      }
   })
   
   cdCogDesc <- reactive({
      cdDisplayObj()$cogDesc
   })
   
   ########################################################
   ### header stuff
   ########################################################
   
   nPages <- reactive({
      cogDF <- cdCogDF()
      if(is.null(cogDF))
         return(1)
         
      ceiling(cogNrow(cogDF) / (getPanelRows() * getPanelCols()))
   })
   
   output$nPages <- renderText({
      HTML(nPages())
   })
   outputOptions(output, 'nPages', suspendWhenHidden=FALSE)
   
   output$currentPageText <- reactive({
      HTML(paste(input$currentPage, "/", nPages()))
   })
   outputOptions(output, 'currentPageText', suspendWhenHidden=FALSE)
   
   ########################################################
   ### cognostics data
   ########################################################
   
   ### holds the data in cogDatConn object for the current display
   cdCogObj <- reactive({
      cdn <- cdName()
      if(is.null(cdn))
         return()

      cdo <- cdDisplayObj()
      logMsg("Retrieving cognostics data for ", cdn[1], " / ", cdn[2], verbose=verbose)
      
      cdo$cogDatConn
   })
   
   ### holds the cognostics data frame for the current display, sorted and filtered
   cdCogDF <- reactive({
      cogDF <- cdCogObj()
      if(is.null(cogDF))
         return()
      
      getCurCogDat(cogDF, 
         flt = input$cogColumnFilterInput,
         ordering = input$cogColumnSortInput,
         colIndex = cogTableColVisibility()
      )
   })
   
   cdCogLength <- reactive({
      cogDF <- cdCogDF()
      if(!is.null(cogDF)) {
         cogNrow(cogDF)
      }
   })
   
   ########################################################
   ### cognostics modal table components
   ########################################################
   
   ### data to be shown in the table view
   ### (reflects current sort and filter state, as well as pagination value)
   cogTableCurrentData <- reactive({
      # cat("get current data\n")
      cogDF <- cdCogDF()
      if(is.null(cogDF))
         return()
      
      logMsg("Retrieving data to be shown in cognostics modal", verbose=verbose)
      n <- cogNrow(cogDF)
      
      colIndex <- cogTableColVisibility()
      
      pageNum <- input$cogTablePagination
      # pageLen <- as.integer(input$cogTablePageLength)
      pageLen <- 8
      if(n == 0) {
         idx <- integer(0)
      } else {
         idx <- ((pageNum - 1)*pageLen + 1):min(pageNum*pageLen, n)         
      }
      
      getCogData(cogDF, idx, colIndex)
   })
   
   ### returns the columns that are to be displayed in the cognostics table
   cogTableColVisibility <- reactive({
      # basically get the column names
      cogDF <- cdCogObj()
      colIndex <- NULL
      if(!is.null(cogDF)) {
         colNames <- input$selectedCogTableVar
         
         if(colNames == "" || is.null(colNames)) {
            colIndex <- seq_len(cogNcol(cogDF))[-1] # don't show panelKey by default
         } else {
            colIndex <- which(cogNames(cogDF) %in% strsplit(colNames, ",")[[1]])
         }
      }
      colIndex
   })
   
   ### hidden field that I don't think is used...
   output$cogTableNrow <- reactive({
      HTML(cdCogLength())
   })
   outputOptions(output, 'cogTableNrow', suspendWhenHidden=FALSE)
   
   ### hidden field
   output$cogTablePageLengthOut <- reactive({
      # HTML(input$cogTablePageLength)
      HTML(8)
   })
   outputOptions(output, 'cogTablePageLengthOut', suspendWhenHidden=FALSE)
   
   ### text indicating pagination info for cognostics modal table
   output$cogTablePaginateText <- renderText({
      n <- cdCogLength()
      # n2 <- as.integer(input$cogTablePageLength)
      n2 <- 8

      HTML(
         input$cogTablePagination, 
         "of", ceiling(n / n2)
      )
   })
   
   output$cogTableHead <- reactive({
      # cat("table head")
      cogDF <- cogTableCurrentData()
      if(is.null(cogDF)) {
         return()
      } else if(cogNcol(cogDF) > 0) {
         cogTableHead(cogDF)
      }
   })
   outputOptions(output, 'cogTableHead', suspendWhenHidden=FALSE)
   
   output$cogTableBody <- reactive({
      cogDF <- cogTableCurrentData()
      
      if(!is.null(cogDF)) {
         # pageLen <- as.integer(input$cogTablePageLength)
         pageLen <- 8
         cogTableBodyHTML(cogDF, pageLen)
      }
   })
   outputOptions(output, 'cogTableBody', suspendWhenHidden=FALSE)
   
   output$cogTableFoot <- renderUI({
      # cat("table foot\n")
      colIndex <- cogTableColVisibility()
      cogDF <- cdCogObj()
      if(is.null(cogDF)) {
         return()
      } else if(cogNcol(cogDF) > 0) {
         cogDF <- getCogData(cogDF, 1, colIndex)
         HTML(paste(
            cogTableFootFilter(cogDF),
            cogTableFootUnivar(cogDF),
            cogTableFootBivar(cogDF)
         ))
      }
   })
   outputOptions(output, 'cogTableFoot', suspendWhenHidden=FALSE)
   
   output$cogTableInfo <- renderText({
      # cat("table info\n")
      n <- cdCogLength()
      pageNum <- input$cogTablePagination
      # pageLen <- as.integer(input$cogTablePageLength)
      pageLen <- 8
      
      HTML(paste(
         "Showing entries", 
         (pageNum - 1)*pageLen + 1, 
         "-", 
         min(pageNum*pageLen, n),
         "of",
         n
      ))
   })
   
   ### create histogram data outputs for d3 histogram filter
   ### this creates a list of json and the custom output binding puts it 
   ### where it belongs
   ### it might not be a good practice to store json in a div, but this is 
   ### small data and it is better than a reactive being called every time
   output$d3histData <- renderData({
      # these are marginal distributions - need whole cogDF
      colIndex <- cogTableColVisibility()
      cogDF <- cdCogObj()
      if(length(colIndex) > 1 && !is.null(cogDF)) {
         # browser()
         cogDF <- getCogData(cogDF, 1:cogNrow(cogDF), colIndex)
         cdfNames <- cogNames(cogDF)
         res <- lapply(cdfNames, function(nm) {
            getD3HistData(cogDF, nm)
         })
         names(res) <- as.character(1:length(colIndex))
         return(res)
      } else {
         return()
      }
   })
   outputOptions(output, 'd3histData', suspendWhenHidden=FALSE)
   
   output$d3bivarPlotDat <- renderText({
      curCols <- input$bivarColumns
      # cat(curCols)
      cogDF <- cdCogObj()
      colIndex <- cogTableColVisibility()
      if(length(colIndex) > 1 && !is.null(cogDF) && curCols != "") {
         # TODO: make this a method for different cog types
         #e.g. for mongodb, use aggregation framework
         cogDF <- getCogData(cogDF, 1:cogNrow(cogDF), colIndex)
         
         curCols <- as.integer(strsplit(curCols, ",")[[1]])
         
         if(length(curCols) == 2) {
            # browser()
            # curCols[1] <- which(names(cogDF) == curCols[1])
            # curCols[2] <- which(names(cogDF) == curCols[2])
            curCols <- as.integer(curCols)
            
            x <- cogDF[,curCols[1]]
            y <- cogDF[,curCols[2]]
            
            makeBivarJSON(x, y, xlab=names(cogDF)[curCols[1]], ylab=names(cogDF)[curCols[2]])            
         } else {
            # do ICA
            require(fastICA)
            ic <- fastICA(cogDF[,curCols, drop=FALSE], n.comp=2)
   
            makeBivarJSON(ic$S[,1], ic$S[,2], xlab="IC 1", ylab="IC 2")
         }
      } else {
         return()
      }
   })
   outputOptions(output, 'd3bivarPlotDat', suspendWhenHidden=FALSE)
   
   ########################################################
   ### panel display
   ########################################################
   
   ### each time nRow, nCol, relatedDisplays changes, we need to update
   ### the layout skeleton that the images will go in
   output$panelLayout <- renderUI({
      cdo <- cdDisplayObj()
      
      if(!is.null(cdo)) {
         logMsg("Updating panel layout", verbose=verbose)
         relList <- getRelatedDisplays()
         nRow <- getPanelRows()
         nCol <- getPanelCols()
         # plotWidth <- input$plotWidth
         # plotHeight <- input$plotHeight
         plotTabSkeleton(nRow, nCol, relList, cdo)
      }
   })
   
   # this is for the plots to be displayed
   curPageCogDF <- reactive({
      cogDF <- cdCogDF()
      if(!is.null(cogDF)) {
         n <- cogNrow(cogDF)
         if(n==0) {
            idx <- integer(0)
         } else {
            cp <- input$currentPage
            nr <- getPanelRows()
            nc <- getPanelCols()
            idx <- ((cp - 1) * nr * nc + 1):min((cp * nr * nc), n)
         }
         res <- getCogData(cogDF, idx, seq_len(cogNcol(cogDF)))
         if(nrow(res) == 0) {
            return()
         } else {
            return(res)            
         }         
      } else {
         return()
      }
   })
   
   output$panelLayoutCogInfo <- renderData({
      cogDF <- curPageCogDF()
      if(!is.null(cogDF)) {
         if(verbose)
            logMsg("Updating cog info in panel layout")
         relList <- getRelatedDisplays() # not used but need to react to it
         
         totPanels <- getPanelRows() * getPanelCols()
         colNames <- input$selectedPlotVar
         
         idx <- which(names(cogDF) %in% strsplit(colNames, ",")[[1]])
         res <- lapply(seq_len(cogNrow(cogDF)), function(i) {
            names <- paste("<strong>", names(cogDF)[idx], "</strong>", sep="")
            
            vals <- sapply(cogDF[i,idx, drop=FALSE], as.character)
            # browser()
            paste("<table class='table-condensed table-bordered table-striped' style='width:100%'><tbody>",
            paste("<tr><td>", apply(matrix(c(names, vals), ncol=2), 1, function(x) paste(x, collapse="</td><td>", sep="")), "</td></tr>", collapse="", sep=""),
            "</tbody></table>")
         })
         if(length(res) < totPanels)
         res <- c(res, rep("", totPanels - length(res)))
         names(res) <- paste("plotTableCog_panel_", seq_along(res), sep="")
         res
      }
   })
   outputOptions(output, 'panelLayoutCogInfo', suspendWhenHidden=FALSE)
   
   output$panelLayoutPlots <- renderData({
      cogDF <- curPageCogDF()
      
      if(!is.null(cogDF)) {
         totPanels <- getPanelRows() * getPanelCols()
         cdo <- cdDisplayObj()
         # localData <- getLocalData()
         # hdfsData <- getHDFSdata()
         
         if(!cdo$preRender) {
            a <- input$panelFnInput
            if(a != "") {
               eval(parse(text=a))
               cdo$panelFn <- panelFn
            }
         }
         
         # a <- system.time({
         res <- getPNGs(cogDF, cdo, vdbPrefix, conn)
         # })
         # cat(a, "\n")
         if(length(res) < totPanels)
         res <- c(res, rep("", totPanels - length(res)))
         names(res) <- paste("plotTable_panel_", seq_along(res), sep="")
         
         relList <- getRelatedDisplays()
         if(!is.null(relList)) {
            res2 <- do.call(c, lapply(seq_along(relList), function(i) {
               ld <- NULL
               tmp <- getPNGs(cogDF, relList[[i]], vdbPrefix, conn)
               if(length(tmp) < totPanels)
                  tmp <- c(tmp, rep("", totPanels - length(tmp)))
               names(tmp) <- paste("plotTable_panel_", seq_len(totPanels), "_rel_", i, sep="")
               tmp
            }))
            res <- c(res, res2)
         }
         
         res
      }
   })
   outputOptions(output, 'panelLayoutPlots', suspendWhenHidden=FALSE)
   
   ########################################################
   ### related displays
   ########################################################
   
   ### add logic for related selected displays
   getRelatedDisplayList <- reactive({
      cdo <- cdDisplayObj()
      if(is.null(cdo)) {
         return()
      } else {
         logMsg("Getting list of displays related to current display")
         curDisplayInd <- which(displayListDF$group==cdo$group & displayListDF$name==cdo$name)
         curDisplaySig <- cdo$keySig
         relatedDisplays <- subset(displayListDF[-curDisplayInd,], keySig==curDisplaySig)
         if(nrow(relatedDisplays) > 0) {
            return(relatedDisplays)
         } else {
            return()
         }
      }
   })
   
   output$relatedDisplayListOutput <- renderText({
      relatedDisplayTable(getRelatedDisplayList())
   })
   
   output$relatedDisplays <- renderText({
      paste(getRelatedDisplayList(), collapse=", ")
   })
      
   getRelatedDisplays <- reactive({
      relUID <- input$relatedDisplayUID # this is the 
      if(!is.null(relUID)) {
         if(relUID != "") {
            logMsg("Getting user selection of related displays")
            relUID <- as.integer(strsplit(relUID, ",")[[1]])
            res <- subset(displayListDF, uid %in% relUID)
            lapply(seq_len(nrow(res)), function(x) {
               getDisplay(group=res[x,]$group, name=res[x,]$name)
            })
         }
      }
   })
   
   ########################################################
   ### hidden values used in javascript
   ########################################################
   
   ### used in updateTableDims() (aspect ratio of panels in cd)
   output$panelAspect <- renderText({
      HTML(cdDisplayObj()$panelDim$aspect)
   })
   outputOptions(output, 'panelAspect', suspendWhenHidden=FALSE)
   
   output$variableCogSelectInput <- renderUI({
      cogDF <- cdCogObj()
      if(is.null(cogDF))
         return()
      
      desc <- cdCogDesc()
      
      makeVariableSelectTable(desc, "Cog")
   })
   
   output$variablePlotSelectInput <- renderUI({
      cogDF <- cdCogObj()
      if(is.null(cogDF))
         return()

      vars <- cogNames(cogDF)
      desc <- cdCogDesc()
      # browser()
      makeVariableSelectTable(desc, "Plot")
   })
   outputOptions(output, 'variablePlotSelectInput', suspendWhenHidden=FALSE)
   
   output$editor <- reactive({
      cdo <- cdDisplayObj()
      panelFn <- cdo$panelFn
      # or could try deparse?
      paste(capture.output(dump("panelFn", "")), collapse="\n")
   })
   outputOptions(output, 'editor', suspendWhenHidden=FALSE)
   
   # precompute the orders?
   # cogDFOrders <- reactive({
   #    cogDF <- cdCogObj()
   #    apply(cogDF, 2, order)
   # })
   
   # output$testOutput <- reactive({
   #    cdo <- cdDisplayObj()
   #    HTML(paste("ppp: ", pppInput(), "; aspect: ", cdo$panelDim$aspect, "; nRow: ", getPanelRows(), "; nCol: ", getPanelCols(), "; plotHeight: ", input$plotHeight, "; plotWidth: ", input$plotWidth, "; storage: ", cdo$storage, sep=""))
   # })
})



# http://stackoverflow.com/questions/1296646/how-to-sort-a-dataframe-by-columns-in-r
# http://stackoverflow.com/questions/8509595/how-do-i-select-rows-by-two-criteria-in-data-table-in-r?rq=1

