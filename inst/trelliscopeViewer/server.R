library(shiny)
library(trelliscope)
library(base64enc) # this one is much faster than caTools!!

if(file.exists("../conn.R")) { # it is on a web server
   source("../conn.R")
   vdbConn <- options()$vdbConn
   vdbPrefix <- file.path(vdbConn$webConn$appDir, vdbConn$vdbName)
} else {
   vdbConn <- options()$vdbConn
   vdbPrefix <- vdbConn$vdbPrefix
}
message("vdbPrefix is ", vdbPrefix)

options(vdbShinyPrefix = vdbPrefix)

if(!is.null(vdbConn$hadoopConn)) {
   Sys.setenv(HADOOP_CONF_DIR = vdbConn$hadoopConn$HADOOP_CONF_DIR)
   Sys.setenv(HADOOP_HOME = vdbConn$hadoopConn$HADOOP_HOME)
   Sys.setenv(HADOOP_LIBS = vdbConn$hadoopConn$HADOOP_LIBS)
   message("loading RHIPE...")
   library(Rhipe)
   rhinit()
}

sapply(list.files("_serverHelpers", full.names=TRUE), source)

# # try to load options (thinking it is shiny-server mode)
# ld <- try(load("vdbConn.Rdata"))
# if(inherits(ld), "try-error") {
#    # must not be shiny-server mode
#    vdbConn <- options()$vdbConn
# } else {
#    # since we are on the web server, we need to point to the files there...
#    vdbConn$vdbPrefix <- vdbConn$webServerVdbPrefix
#    # TODO: let the vdb server know how to connect to mongodb, hdfs, etc.
# }

verbose <- vdbConn$viewerLog
if(is.null(verbose))
   verbose <- FALSE

load(file.path(vdbPrefix, "displays/_displayList.Rdata"))

shinyServer(function(input, output) {
   
   conn <- getOption("vdbConn")
   
   # NOTE: reactive objects starting with "cd" is shorthand for "current display", meaning that it corresponds with the display currently being viewed
   
   ### get the current display name, either from a URL hash or the input
   cdName <- reactive({
      uid <- as.integer(input$displayListTable)
      appHash <- input$appHash
      # TODO: add check for whether the plot exists and give it a good error message
      getCdName(uid, appHash, displayList, verbose)
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
   
   cdCogDesc <- reactive({
      cdDisplayObj()$cogDesc
   })
   
   ########################################################
   ### header stuff
   ########################################################
   
   nPages <- reactive({
      ceiling(cogNrow(cdCogDF()) / (getPanelRows() * getPanelCols()))
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
         cdo <- cdDisplayObj()
         logMsg("Retrieving cognostics data for ", cdn[1], " / ", cdn[2], verbose=verbose)
         if(cdo$cogStorage=="local") {
            load(file.path(vdbPrefix, "displays/", cdn[1], cdn[2], "cog.Rdata"))
         } else { # cognostics are in mongodb
            mongoConn <- vdbMongoInit(conn)
            NS <- mongoCollName(conn$vdbName, cdn[1], cdn[2], "cog")
            
            ex <- mongoCog2DF(mongo.bson.to.list(mongo.find.one(mongoConn, NS))[-1])
            ex <- data.frame(ex)
            qry <- mongo.bson.empty()
            srt <- mongo.bson.empty()
            
            nr <- mongo.count(mongoConn, NS, query=qry)
            
            cog <- list(
               mongoConn=mongoConn,
               NS=NS,
               qry=qry,
               srt=srt,
               ncol=ncol(ex),
               nrow=nr,
               ex=ex
            )
            
            class(cog) <- c("mongoCogConn", "list")
         }
         return(cog)
      }
   })
   
   ### holds the cognostics data frame for the current display, sorted and filtered
   cdCogDF <- reactive({
      cogDF <- cdCogObj()
      if(!is.null(cogDF)) {
         if(inherits(cogDF, "mongoCogConn")) {
            qryStr <- cdCogMongoQryStr()
            
            cogDF$qry <- qryStr$qry
            cogDF$srt <- qryStr$srt
            
            nr <- mongo.count(cogDF$mongoConn, cogDF$NS, query=cogDF$qry)
            cogDF$nr <- nr
            
            return(cogDF)
         } else {
            orderIndex <- cdCogIndex()
            logMsg("Retrieving sorted and filtered cognostics data", verbose=verbose)
            return(cogDF[orderIndex,,drop=FALSE])            
         }
      } else {
         return(NULL)
      }
   })
   
   cdCogLength <- reactive({
      cogDF <- cdCogDF()
      if(!is.null(cogDF)) {
         cogNrow(cogDF)
      }
   })
   
   ### index of filtered state of cognostics data frame for current display
   cdCogFilterIndex <- reactive({
      cogDF <- cdCogObj()
      colIndex <- cogTableColVisibility()

      if(!is.null(cogDF)) {
         cogDF <- getCogData(cogDF, 1:cogNrow(cogDF), colIndex)
         
         filterIndex <- seq_len(cogNrow(cogDF))
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
                     newIndex <- seq_len(cogNrow(cogDF))
               }
               filterIndex <- intersect(filterIndex, newIndex)
            }
         }
         filterIndex         
      }
   })
   
   cdCogMongoQryStr <- reactive({
      cogDF <- cdCogObj()
      if(!is.null(cogDF)) {
         ex <- cogDF$ex
         exNames <- names(ex)

         # build ordering part of query
         ordering <- input$cogColumnSortInput
         srt <- mongo.bson.empty()
         if(!is.null(ordering)) {
            # need to know which columns are visible so we are sorting the right column
            colIndex <- cogTableColVisibility()

            ordIdx <- which(ordering != 0)
            if(length(ordIdx) > 0) {
               ordIdx <- ordIdx[order(ordering[ordIdx])]
               srt <- list()
               for(i in ordIdx) {
                  srt[[exNames[colIndex[i]]]] <- as.integer(sign(ordering[i]))
               }
            }
         }

         # now build filtering part
         qry <- mongo.bson.empty()

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

            if(length(flt) > 0) {
               qry <- list()

               for(i in seq_along(flt)) {
                  cur <- flt[[i]]
                  curName <- exNames[as.integer(cur[2])]
                  if(cur[1] == "from") {
                     if(is.null(qry[[curName]])) {
                        qry[[curName]] <- list("$gte"=as.numeric(cur[3]))
                     } else {
                        qry[[curName]][["$gte"]] <- as.numeric(cur[3])
                     }
                  } else if(cur[1] == "to") {
                     if(is.null(qry[[curName]])) {
                        qry[[curName]] <- list("$lte"=as.numeric(cur[3]))
                     } else {
                        qry[[curName]][["$lte"]] <- as.numeric(cur[3])
                     }
                  } else if(cur[1] == "regex") {
                     qry[[exNames[as.integer(cur[2])]]] <-  list("$regex"=cur[3])
                  }
               }
            }
         }

         # crs <- mongo.find(mongoConn, NS, query=qry) #, sort=srt)
         # mongo.cursor.next(crs)
         # mongo.cursor.value(crs)
         list(srt=srt, qry=qry)
      }

   })
   
   ### index of sorted and filtered state of cognostics data frame for current display
   cdCogIndex <- reactive({
      cogDF <- cdCogObj()
      if(!is.null(cogDF)) {
         # before ordering, perform any filters
         filterIndex <- cdCogFilterIndex()
         logMsg("Updating cognostic sort index", verbose=verbose)
         cogDF <- cogDF[filterIndex,, drop=FALSE]
         # cogDFOrd <- cogDFOrders()
         ordering <- input$cogColumnSortInput
         # this is a vector of -1 (desc), 0 (no sort), 1 (asc)
         orderIndex <- seq_len(cogNrow(cogDF))
         # browser()
         # need to know which columns are visible so we are sorting the right column
         colIndex <- cogTableColVisibility()
         # get sort order and sort the table
         if(!is.null(ordering)) {
            if(any(ordering != 0)) {
               # if we are only sorting by one column
               # TODO: use data.table here for faster sorting
               if(sum(abs(ordering)) == 1) {
                  ind <- which(abs(ordering) == 1)
                  orderIndex <- order(cogDF[,colIndex[ind],drop=FALSE], decreasing=ifelse(ordering[ind] < 0, TRUE, FALSE))
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
                                                return(-xtfrm(cogDF[,colIndex[orderCols[i]], drop=FALSE]))
                     } else {
                        return(cogDF[,colIndex[orderCols[i]], drop=FALSE])
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

      if(!is.null(cogDF)) {
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
         
         return(getCogData(cogDF, idx, colIndex))
      } else {
         return(NULL)
      }
   })
   
   ### returns the columns that are to be displayed in the cognostics table
   cogTableColVisibility <- reactive({
      # basically get the column names
      cogDF <- cdCogObj()
      colIndex <- NULL
      if(!is.null(cogDF)) {
         colNames <- input$selectedCogTableVar
         if(colNames == "" || is.null(colNames)) {
            colIndex <- seq_len(cogNcol(cogDF))
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
         return(NULL)
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
         return(NULL)
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
      cogDF <- cdCogObj()
      
      if(is.data.frame(cogDF))
         cogDF <- cogDF[,cogTableColVisibility(), drop=FALSE]
      
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
            require(fastICA)
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
      n <- cogNrow(cogDF)
      if(!is.null(cogDF)) {
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
         res <- getPNGs(cogDF, cdo, localData, hdfsData, vdbPrefix, conn)
         # })
         # cat(a, "\n")
         if(length(res) < totPanels)
         res <- c(res, rep("", totPanels - length(res)))
         names(res) <- paste("plotTable_panel_", seq_along(res), sep="")
         
         relList <- getRelatedDisplays()
         if(!is.null(relList)) {
            # browser()
   
            res2 <- do.call(c, lapply(seq_along(relList), function(i) {
               tmp <- getPNGs(cogDF, relList[[i]], NULL, NULL, vdbPrefix, conn)
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
      vars <- cogNames(cdCogObj())
      desc <- cdCogDesc()

      makeVariableSelectTable(vars, desc, "Cog")
   })
   
   output$variablePlotSelectInput <- renderUI({
      vars <- cogNames(cdCogObj())
      desc <- cdCogDesc()
      # browser()
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
   #    cogDF <- cdCogObj()
   #    apply(cogDF, 2, order)
   # })
   
   # output$testOutput <- reactive({
   #    cdo <- cdDisplayObj()
   #    HTML(paste("ppp: ", pppInput(), "; aspect: ", cdo$plotDim$aspect, "; nRow: ", getPanelRows(), "; nCol: ", getPanelCols(), "; plotHeight: ", input$plotHeight, "; plotWidth: ", input$plotWidth, "; storage: ", cdo$storage, sep=""))
   # })
})



# http://stackoverflow.com/questions/1296646/how-to-sort-a-dataframe-by-columns-in-r
# http://stackoverflow.com/questions/8509595/how-do-i-select-rows-by-two-criteria-in-data-table-in-r?rq=1

