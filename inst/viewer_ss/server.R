library(shiny)
# library(vdb)
library(fastICA)
library(base64enc) # this one is much faster than caTools!!
# library(Rhipe)
# rhinit()

vdbPrefix <- getOption("vdbShinyPrefix")
sapply(list.files("serverHelpers", full.names=TRUE), source)

verbose <- getOption("vdbLogViewer", default=FALSE)

load(file.path(vdbPrefix, "displays/_displayList.Rdata"))

shinyServer(function(input, output) {
   
   # NOTE: reactive objects starting with "cd" is shorthand for "current display", meaning that it corresponds with the display currently being viewed
   
   ### get the current display name, either from a URL hash or the input
   cdName <- reactive({
      uid <- as.integer(input$displayListTable)
      appHash <- input$appHash
      # TODO: add check for whether the plot exists and give it a good error message
      getCdName(uid, appHash, displayList, verbose)
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
         return(NULL)
      } else {
         cdo <- getDisplay(group=cdn[1], name=cdn[2])
         logMsg("Retrieving display metadata", verbose=verbose)
         # need to load Rhipe only if necessary
         if(cdo$storage %in% c("hdfs", "hdfsData")) {
            if(! "package:Rhipe" %in% search()) {
               library(Rhipe)
               rhinit()
               # TODO: need to provide error message if this doesn't load
               # if it is "hdfs", then initialize a mapfile and append it to cdo
            }
            if(cdo$storage=="hdfs") {
               cdo$mapfile <- rhmapfile(paste(cdo$hdfsPrefix, "/", cdo$name, sep=""))
            }
         }
         return(cdo)
      }
   })
   
   ########################################################
   ### header stuff
   ########################################################
   
   nPages <- reactive({
      ceiling(nrow(cdCogDF()) / (input$nRow * input$nCol))
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

   ### holds the data in "cog.Rdata" for the current display
   cdCogObj <- reactive({
      cdn <- cdName()
      if(is.null(cdn)) {
         return(NULL)
      } else {
         logMsg("Retrieving cognostics data for ", cdn[1], " / ", cdn[2], verbose=verbose)
         load(file.path(vdbPrefix, "displays/", cdn[1], cdn[2], "cog.Rdata"))
         return(cog)
      }
   })
   
   ### holds the cognostics data frame for the current display, sorted and filtered
   cdCogDF <- reactive({
      cogDF <- cdCogObj()$cogDF
      if(!is.null(cogDF)) {
         orderIndex <- cdCogIndex()
         logMsg("Retrieving sorted and filtered cognostics data", verbose=verbose)
         return(cogDF[orderIndex,,drop=FALSE])
      } else {
         return(NULL)
      }
   })
   
   ### index of filtered state of cognostics data frame for current display
   cdCogFilterIndex <- reactive({
      cogDF <- cdCogObj()$cogDF
      if(!is.null(cogDF)) {
         filterIndex <- seq_len(nrow(cogDF))
         # browser()
         # flt is a vector of 3-tuples - (filter type, filter column, filter value)
         # see getColumFilterInputs in table.js
         flt <- input$cogColumnFilterInput
         if(!is.null(flt)) {
            logMsg("Updating cognostic filter index", verbose=verbose)
            n <- length(flt)
            # get index for filters that are NULL
            ind <- which(!sapply(flt[seq(3, n, by=3)], function(x) is.null(x) | x==""))
            # remove those ones
            if(length(ind) == 0) {
               flt <- NULL
            } else {
               flt <- lapply(ind, function(x) flt[((x - 1)*3 + 1):(x*3)])
            }
            for(i in seq_along(flt)) {
               cur <- flt[[i]]
               if(cur[1] == "from") {
                  newIndex <- which(cogDF[[as.integer(cur[2])]] >= as.numeric(cur[3]))               
               } else if(cur[1] == "to") {
                  newIndex <- which(cogDF[[as.integer(cur[2])]] <= as.numeric(cur[3]))
               } else if(cur[1] == "regex") {
                  # browser()
                  newIndex <- try(which(grepl(cur[3], cogDF[[as.integer(cur[2])]])))
                  if(inherits(newIndex, "try-error"))
                     newIndex <- seq_len(nrow(cogDF))
               }
               filterIndex <- intersect(filterIndex, newIndex)
            }
         }
         filterIndex         
      }
   })
   
   ### index of sorted and filtered state of cognostics data frame for current display
   cdCogIndex <- reactive({
      cogDF <- cdCogObj()$cogDF
      if(!is.null(cogDF)) {
         # before ordering, perform any filters
         filterIndex <- cdCogFilterIndex()
         logMsg("Updating cognostic sort index", verbose=verbose)
         cogDF <- cogDF[filterIndex,, drop=FALSE]
         # cogDFOrd <- cogDFOrders()
         ordering <- input$cogColumnSortInput
         # this is a vector of -1 (desc), 0 (no sort), 1 (asc)
         orderIndex <- seq_len(nrow(cogDF))
         
         # get sort order and sort the table
         if(!is.null(ordering)) {
            if(any(ordering != 0)) {
               # if we are only sorting by one column
               # TODO: use data.table here for faster sorting
               if(sum(abs(ordering)) == 1) {
                  ind <- which(abs(ordering) == 1)
                  orderIndex <- order(cogDF[,ind,drop=FALSE], decreasing=ifelse(ordering[ind] < 0, TRUE, FALSE))
                  # if(ordering[ind] < 0) {
                  #    orderIndex <- rev(cogDFOrd[,ind])
                  # } else {
                  #    orderIndex <- cogDFOrd[,ind]
                  # }
               } else {
                  nonZero <- which(ordering != 0)
                  colOrder <- ordering[nonZero]
                  orderCols <- nonZero[order(abs(colOrder))]
                  orderSign <- sign(ordering)[orderCols]
                  orderCols <- lapply(seq_along(orderCols), function(i) {
                     if(orderSign[i] < 0) {
                        return(-xtfrm(cogDF[,orderCols[i], drop=FALSE]))
                     } else {
                        return(cogDF[,orderCols[i], drop=FALSE])
                     }
                  })
                  orderIndex <- do.call(order, orderCols)               
               }
            }
         }
         filterIndex[orderIndex]         
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
      
      colIndex <- cogTableColVisibility()
      
      if(!is.null(cogDF)) {
         logMsg("Retrieving data to be shown in cognostics modal", verbose=verbose)
         n <- nrow(cogDF)
         pageNum <- input$cogTablePagination
         pageLen <- as.integer(input$cogTablePageLength)
         if(n == 0) {
            idx <- integer(0)
         } else {
            idx <- ((pageNum - 1)*pageLen + 1):min(pageNum*pageLen, n)         
         }
         return(cogDF[idx, colIndex, drop=FALSE])
      } else {
         return(NULL)
      }
   })
   
   ### returns the columns that are to be displayed in the cognostics table
   cogTableColVisibility <- reactive({
      # basically get the column names
      cogDF <- head(cdCogObj()$cogDF, 2)
      colIndex <- NULL
      if(!is.null(cogDF)) {
         colNames <- input$selectedCogTableVar
         if(colNames == "" || is.null(colNames)) {
            colIndex <- seq_len(ncol(cogDF))
         } else {
            colIndex <- which(names(cogDF) %in% strsplit(colNames, ",")[[1]])
         }
      }
      # browser()
      colIndex
   })
   
   ### hidden field that I don't think is used...
   output$cogTableNrow <- reactive({
      HTML(length(cdCogIndex()))
   })
   outputOptions(output, 'cogTableNrow', suspendWhenHidden=FALSE)
   
   ### hidden field that I don't think is used...
   output$cogTablePageLengthOut <- reactive({
      HTML(input$cogTablePageLength)
   })
   outputOptions(output, 'cogTablePageLengthOut', suspendWhenHidden=FALSE)
   
   ### text indicating pagination info for cognostics modal table
   output$cogTablePaginateText <- renderText({
      n <- length(cdCogIndex())
      n2 <- as.integer(input$cogTablePageLength)
      HTML(
         input$cogTablePagination, 
         "of", ceiling(n / n2)
      )
   })
   
   output$cogTableHead <- reactive({
      # cat("table head")
      cogDF <- head(cogTableCurrentData(), n = 1)
      if(is.null(cogDF)) {
         return(NULL)
      } else if(ncol(cogDF) > 0) {
         cogTableHead(cogDF)
      }
   })
   outputOptions(output, 'cogTableHead', suspendWhenHidden=FALSE)
   
   output$cogTableBody <- reactive({
      cogDF <- cogTableCurrentData()
      
      if(!is.null(cogDF)) {
         pageLen <- as.integer(input$cogTablePageLength)
         cogTableBodyHTML(cogDF, pageLen)
      }
   })
   outputOptions(output, 'cogTableBody', suspendWhenHidden=FALSE)
   
   output$cogTableFoot <- renderUI({
      # cat("table foot\n")
      colIndex <- cogTableColVisibility()
      cogDF <- head(cdCogObj()$cogDF, 2)
      
      if(is.null(cogDF)) {
         return(NULL)
      } else if(ncol(cogDF) > 0) {
         HTML(paste(
            cogTableFootFilter(cogDF[,colIndex,drop=FALSE]),
            cogTableFootUnivar(cogDF[,colIndex,drop=FALSE]),
            cogTableFootBivar(cogDF[,colIndex,drop=FALSE])
         ))
      }
   })
   outputOptions(output, 'cogTableFoot', suspendWhenHidden=FALSE)
   
   output$cogTableInfo <- renderText({
      # cat("table info\n")
      n <- length(cdCogIndex())
      pageNum <- input$cogTablePagination
      pageLen <- as.integer(input$cogTablePageLength)
      
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
      if(length(colIndex) > 1) {
         cdfNames <- names(cdCogObj()$cogDF)
         res <- lapply(colIndex, function(i) {
            vals <- cdCogObj()$cogDF[,i]
            if(inherits(vals, c("numeric", "integer"))) {
               getD3HistData(vals, cdfNames[i])
            } else {
               ""
            }
         })
         names(res) <- as.character(colIndex)
         return(res)
      } else {
         return(NULL)
      }
   })
   outputOptions(output, 'd3histData', suspendWhenHidden=FALSE)
   
   ### create histograms to be displayed in cognostics table
   ### TODO: migrate this to d3!!
   output$cogHistPlots <- renderData({
      # this can only fire if the element exists
      colIndex <- cogTableColVisibility()
      # cat(isLoaded, "\n")
      if(length(colIndex) > 1) {
         res <- lapply(colIndex, function(i) {
            a <- cdCogObj()$cogDesc
            p <- xyplot(1:10 ~ 1:10)
            myRenderPlot({
               plot(p)
            }, width=93, height=93)
         })
         names(res) <- paste("cogTable_univarPlot_", colIndex, sep="")
         res
      }
   })
   outputOptions(output, 'cogHistPlots', suspendWhenHidden=FALSE)
   
   output$d3bivarPlotDat <- renderText({
      curCols <- input$bivarColumns
      # cat(curCols)
      cogDF <- cdCogObj()$cogDF[,cogTableColVisibility(), drop=FALSE]
      
      if(curCols != "") {
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
            ic <- fastICA(cogDF[,curCols, drop=FALSE], n.comp=2)
   
            makeBivarJSON(ic$S[,1], ic$S[,2], xlab="IC 1", ylab="IC 2")
         }
      } else {
         return(NULL)
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
         nRow <- input$nRow
         nCol <- input$nCol
         # plotWidth <- input$plotWidth
         # plotHeight <- input$plotHeight
         plotTabSkeleton(nRow, nCol, relList, cdo)
      }
   })
   
   # this is for the plots to be displayed
   curPageCogDF <- reactive({
      cogDF <- cdCogDF()
      n <- nrow(cogDF)
      if(!is.null(cogDF)) {
         if(n==0) {
            idx <- integer(0)
         } else {
            cp <- input$currentPage
            nr <- input$nRow
            nc <- input$nCol
            idx <- ((cp - 1) * nr * nc + 1):min((cp * nr * nc), n)
         }
         res <- cogDF[idx,, drop=FALSE]
         if(nrow(res) == 0) {
            return(NULL)
         } else {
            return(res)            
         }         
      } else {
         return(NULL)
      }
   })
   
   output$panelLayoutCogInfo <- renderData({
      cogDF <- curPageCogDF()
      if(!is.null(cogDF)) {
         if(verbose)
            logMsg("Updating cog info in panel layout")
         relList <- getRelatedDisplays() # not used but need to react to it
         totPanels <- input$nRow * input$nCol
         colNames <- input$selectedPlotVar
         idx <- which(names(cogDF) %in% strsplit(colNames, ",")[[1]])
         res <- lapply(seq_len(nrow(cogDF)), function(i) {
            names <- paste("<strong>", names(cogDF)[idx], "</strong>", sep="")
            vals <- cogDF[i,idx, drop=FALSE]
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
         totPanels <- input$nRow * input$nCol
         cdo <- cdDisplayObj()
         localData <- getLocalData()
         hdfsData <- getHDFSdata()
         if(!is.null(localData) || !is.null(hdfsData)) {
            a <- input$plotFnInput
            if(a != "") {
               eval(parse(text=a))
               cdo$plotFn <- plotFn
            }
         }
         # a <- system.time({
         res <- getPNGs(cogDF, cdo, localData, hdfsData, vdbPrefix)
         # })
         # cat(a, "\n")
         if(length(res) < totPanels)
         res <- c(res, rep("", totPanels - length(res)))
         names(res) <- paste("plotTable_panel_", seq_along(res), sep="")
         
         relList <- getRelatedDisplays()
         if(!is.null(relList)) {
            # browser()
   
            res2 <- do.call(c, lapply(seq_along(relList), function(i) {
               tmp <- getPNGs(cogDF, relList[[i]], NULL, NULL, vdbPrefix)
               names(tmp) <- 
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
   
   getLocalData <- reactive({
      cdo <- cdDisplayObj()
      if(!is.null(cdo)) {
         if(cdo$storage=="localData") {
            load(file.path(vdbPrefix, "displays", "localData", paste(cdo$dataSig, ".Rdata", sep="")))
            return(data)
         } else {
            return(NULL)
         }
      }
   })

   getHDFSdata <- reactive({
      cdo <- cdDisplayObj()
      if(!is.null(cdo)) {
         if(cdo$storage=="hdfsData") {
            data <- rhData(loc=cdo$hdfsDataSource$loc, type=cdo$hdfsDataSource$type)
            message("got data!!!")
            return(data)
         } else {
            return(NULL)
         }
      }
   })
   
   # TODO: if related displays have localData, get that too (and warn if it's going to be too big!!)
   # getRelLocalData <- reactive({
   #    
   # })
   
   ########################################################
   ### related displays
   ########################################################
   
   ### add logic for related selected displays
   getRelatedDisplayList <- reactive({
      cdo <- cdDisplayObj()
      if(is.null(cdo)) {
         return(NULL)
      } else {
         logMsg("Getting list of displays related to current display")
         curDisplayInd <- which(displayList$group==cdo$group & displayList$name==cdo$name)
         curDisplaySig <- cdo$keySig
         relatedDisplays <- subset(displayList[-curDisplayInd,], keySig==curDisplaySig)
         if(nrow(relatedDisplays) > 0) {
            return(relatedDisplays)
         } else {
            return(NULL)
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
            res <- subset(displayList, uid %in% relUID)
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
      HTML(cdDisplayObj()$plotDim$aspect)
   })
   outputOptions(output, 'panelAspect', suspendWhenHidden=FALSE)
      
   output$variableCogSelectInput <- renderUI({
      vars <- names(cdCogObj()$cogDF)
      desc <- cdCogObj()$cogDesc
      
      makeVariableSelectTable(vars, desc, "Cog")
   })
   
   output$variablePlotSelectInput <- renderUI({
      vars <- names(cdCogObj()$cogDF)
      desc <- cdCogObj()$cogDesc
      
      makeVariableSelectTable(vars, desc, "Plot")
   })
   outputOptions(output, 'variablePlotSelectInput', suspendWhenHidden=FALSE)
   
   output$editor <- reactive({
      cdo <- cdDisplayObj()
      plotFn <- cdo$plotFn
      # or could try deparse?
      paste(capture.output(dump("plotFn", "")), collapse="\n")
   })
   outputOptions(output, 'editor', suspendWhenHidden=FALSE)
   
   # precompute the orders?
   # cogDFOrders <- reactive({
   #    cogDF <- cdCogObj()$cogDF
   #    apply(cogDF, 2, order)
   # })

   # output$testOutput <- reactive({
   #    cdo <- cdDisplayObj()
   #    HTML(paste("ppp: ", pppInput(), "; aspect: ", cdo$plotDim$aspect, "; nRow: ", input$nRow, "; nCol: ", input$nCol, "; plotHeight: ", input$plotHeight, "; plotWidth: ", input$plotWidth, "; storage: ", cdo$storage, sep=""))
   # })
})



# http://stackoverflow.com/questions/1296646/how-to-sort-a-dataframe-by-columns-in-r
# http://stackoverflow.com/questions/8509595/how-do-i-select-rows-by-two-criteria-in-data-table-in-r?rq=1

