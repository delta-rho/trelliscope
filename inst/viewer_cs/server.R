library(shiny)
library(caTools)
library(fastICA)
# library(Rhipe)
# rhinit()

vdbPrefix <- getOption("vdbShinyPrefix")
myVdbPrefix <- getOption("vdbShinyPrefix")

source("cogTable.R")

MAXPANELS <- 10
MAXPANELINPUT <- 10
MAXCOGVARS <- 20

shinyServer(function(input, output) {
   
   load(file.path(vdbPrefix, "displays/_displayList.Rdata"))
   
   plotName <- reactive(function() {
      # input can come either from plot list or hash tag
      uid <- as.integer(input$displayListTable)
      tmp2 <- input$appHash
      # tmp2 <- "#group=global&plot=irisRhTest1"

      # TODO: add check for whether the plot exists and give it a good error message
      if(!is.na(uid)) {
         if(uid!="") {
            # browser()
            tmp1 <- displayList[displayList$uid == uid,]
            return(c(tmp1$group, tmp1$name))
         } else if(tmp2!="") {
            tmp2 <- strsplit(tmp2, "&")[[1]]
            tmp2 <- gsub(".*=(.*)", "\\1", tmp2)         
            return(tmp2)
         }         
      }
      return(NULL)
   })
   
   pppInput <- reactive(function() {
      tmp <- input$pppInput
      if(tmp=="") 
         tmp <- 1
      tmp
   })
   
   getPlotInfo <- reactive(function() {
      curPlotName <- plotName()
      if(is.null(curPlotName)) {
         return(NULL)
      } else {
         return(subset(displayList, group==curPlotName[1] & name==curPlotName[2]))
      }
   })
   
   output$plotAspect <- reactive(function() {
      HTML(getPlotInfo()$aspect)
   })
   
   nPages <- reactive(function() {
      ceiling(nrow(curCogDF()) / (input$nRow * input$nCol))
   })
   
   output$nPages <- reactive(function() {
      HTML(nPages())
   })
   
   curCogDF <- reactive(function() {
      cogDF <- cogListInput()$cogDF
      if(!is.null(cogDF)) {
         orderIndex <- getOrderIndex()
         return(cogDF[orderIndex,,drop=FALSE])
      } else {
         return(NULL)
      }
   })
   
   getLocalData <- reactive(function() {
      plotInfo <- getPlotInfo()
      if(!is.null(plotInfo)) {
         if(plotInfo$storage=="localData") {
            load(file.path(vdbPrefix, "displays/", plotInfo$group, plotInfo$name, "localData.Rdata"))
            return(dat)
         } else {
            return(NULL)
         }
      }
   })

   getLocalDataExtra <- reactive(function() {
      plotInfo <- getPlotInfo()
      if(!is.null(plotInfo)) {
         if(plotInfo$storage=="localData") {
            load(file.path(vdbPrefix, "displays/", plotInfo$group, plotInfo$name, "localDataExtra.Rdata"))
            return(localDataExtra)
         } else {
            return(NULL)
         }
      }
   })
   
   output$currentPageText <- reactive(function() {
      HTML(paste(input$currentPage, "/", nPages()))
   })
   
   output$testOutput <- reactive(function() {
      plotInfo <- getPlotInfo()
      HTML(paste("ppp: ", pppInput(), "; aspect: ", plotInfo$aspect, "; nRow: ", input$nRow, "; nCol: ", input$nCol, "; plotHeight: ", input$plotHeight, "; plotWidth: ", input$plotWidth, "; storage: ", plotInfo$storage, sep=""))
   })
   
   # this is for the plots to be displayed
   curPageCogDF <- reactive(function() {
      # cat("cur page\n")
      cogDF <- curCogDF()
      n <- nrow(cogDF)
      if(!is.null(cogDF)) {
         if(n==0) {
            idx <- integer(0)
         } else {
            idx <- ((input$currentPage - 1) * input$nRow * input$nCol + 1):min((input$currentPage * input$nRow * input$nCol), n)
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

   # this is for the cognostics table
   cogTableCurrentData <- reactive(function() {
      # cat("get current data\n")
      cogDF <- cogListInput()$cogDF

      colIndex <- getColIndex()
      orderIndex <- getOrderIndex()
      
      n <- length(orderIndex)
      pageNum <- input$cogTable_pagination
      pageLen <- as.integer(input$cogTable_length)
      if(n == 0) {
         idx <- integer(0)
      } else {
         idx <- ((pageNum - 1)*pageLen + 1):min(pageNum*pageLen, n)         
      }
      cogDF[orderIndex[idx], colIndex, drop=FALSE]
   })
   
   output$plotOutput <- reactiveUI(function() {
      plotInfo <- getPlotInfo()
      nRow <- input$nRow
      nCol <- input$nCol
      # plotWidth <- input$plotWidth
      # plotHeight <- input$plotHeight
      # cogDF <- curPageCogDF()
      
      if(!is.null(plotInfo)) { # && !is.null(cogDF)) {
         # getPlotString(cogDF, plotInfo, nRow, nCol, plotWidth, plotHeight, plotInfo$storage)
         plotTabSkeleton(nRow, nCol)
      }
   })
   
   output$plotName <- reactive(function() {
      plotName <- plotName()
      if(is.null(plotName)) {
         txt <- ""
      } else {
         txt <- paste(plotName, collapse=" / ")
      }
      HTML(txt)
   })
   
   cogListInput <- reactive(function() {
      plotName <- plotName()
      if(is.null(plotName)) {
         return(NULL)
      } else {
         load(file.path(vdbPrefix, "displays/", plotName[1], plotName[2], "cog.Rdata"))
         return(cog)
      }
   })
      
   # add logic for related selected displays
   getRelatedDisplays <- reactive(function() {
      plotName <- plotName()
      if(is.null(plotName)) {
         return(NULL)
      } else {
         curPlotInd <- which(displayList$group==plotName[1] & displayList$name==plotName[2])
         curPlotSig <- displayList[curPlotInd, "keySig"]
         otherPlots <- subset(displayList[-curPlotInd,], keySig==curPlotSig)
         if(nrow(otherPlots) > 0) {
            return(otherPlots)
         } else {
            return(NULL)
         }
      }
   })

   output$relatedDisplayListOutput <- reactive(function() {
      relatedDisplayTable(getRelatedDisplays())
   })
   
   selectedRelatedDisplays <- reactive(function() {
      rel <- input$relatedDisplayVar
      if(!is.null(rel)) {
         if(rel != "") {
            rel <- as.integer(strsplit(rel, ",")[[1]])
            res <- subset(displayList, uid %in% rel)

            # need to attach localData if there are any
            # this could be bad if there lot of related 
            # displays or just a few with large data sets
            # browser()

            res <- lapply(seq_len(nrow(res)), function(x) {
               dat <- NULL
               localDataExtra <- NULL
               if(res[x,"storage"]=="localData") {
                  load(file.path(vdbPrefix, "displays/", res[x,]$group, res[x,]$name, "localData.Rdata"))
                  load(file.path(vdbPrefix, "displays/", res[x,]$group, res[x,]$name, "localDataExtra.Rdata"))
               }
               list(
                  plotInfo=res[x,],
                  localData=dat,
                  localDataExtra=localDataExtra
               )
            })

            res
         }
      }
   })
   
   getOrderIndex <- reactive(function() {
      cogDF <- cogListInput()$cogDF
      if(!is.null(cogDF)) {
         # browser()
         # before ordering, perform any filters
         filterIndex <- getFilterIndex()

         cogDF <- cogDF[filterIndex,, drop=FALSE]

         # cogDFOrd <- cogDFOrders()
         orderIndex <- seq_len(nrow(cogDF))
         ordering <- input$cogTable_sorting

         # get sort order and sort the table
         if(!is.null(ordering)) {
            if(any(ordering != 0)) {
               # if we are only sorting by one column
               # TODO: use data.table here for faster sorting
               if(sum(abs(ordering)) == 1) {
                  ind <- which(abs(ordering) == 1)
                  orderIndex <- order(cogDF[,ind], decreasing=ifelse(ordering[ind] < 0, TRUE, FALSE))
                  # if(ordering[ind] < 0) {
                  #    orderIndex <- rev(cogDFOrd[,ind])
                  # } else {
                  #    orderIndex <- cogDFOrd[,ind]
                  # }
               } else {
                  # cogDF <- rock
                  # ordering <- c(0, 0, -2, 1)
                  nonZero <- which(ordering != 0)
                  colOrder <- ordering[nonZero]
                  orderCols <- nonZero[order(abs(colOrder))]
                  orderSign <- sign(ordering)[orderCols]
                  orderCols <- lapply(seq_along(orderCols), function(i) {
                     if(orderSign[i] < 0) {
                        return(-xtfrm(cogDF[,orderCols[i]]))
                     } else {
                        return(cogDF[,orderCols[i]])
                     }
                  })
                  orderIndex <- do.call(order, orderCols)               
               }
            }
         }
         filterIndex[orderIndex]         
      }
   })
   
   # precompute the orders?
   # cogDFOrders <- reactive(function() {
   #    cogDF <- cogListInput()$cogDF
   #    apply(cogDF, 2, order)
   # })

   output$cogTable <- reactive(function() {
      HTML(length(getOrderIndex()))
   })

   output$cogTable_nrow <- reactive(function() {
      HTML(length(getOrderIndex()))
   })

   output$cogTable_nrow_length <- reactive(function() {
      HTML(input$cogTable_length)
   })

   output$cogTable_paginate_text <- reactive(function() {
      n <- length(getOrderIndex())
      n2 <- as.integer(input$cogTable_length)
      paste(
         input$cogTable_pagination, 
         "of", ceiling(n / n2)
      )
   })
   
   getColIndex <- reactive(function() {
      cogDF <- head(cogListInput()$cogDF, 2)
      
      colIndex <- NULL
      if(!is.null(cogDF)) {
         colNames <- input$selectedCogVar
         if(colNames == "" || is.null(colNames)) {
            # cat(ncol(cogDF), "\n")
            colIndex <- seq_len(ncol(cogDF))
         } else {
            colIndex <- which(names(cogDF) %in% strsplit(colNames, ",")[[1]])
         }
      }
      # cat(paste(colIndex, collapse=","), "\n")
      # browser()
      colIndex
   })
   
   output$relatedDisplays <- reactiveText(function() {
      paste(getRelatedDisplays(), collapse=", ")
   })
      
   
   output$cogTable_head <- reactive(function() {
      # cat("table head")
      cogDF <- head(cogTableCurrentData(), n = 1)
      if(is.null(cogDF)) {
         return(NULL)
      } else if(ncol(cogDF) > 0) {
         cogTableHead(cogDF)
      }
   })
   
   # create reactives for each univariate plot
   columnFilterIndividualInput <- lapply(1:MAXCOGVARS, function(i) {
      name <- paste("cogTable_uniplot_", i, sep="")
      reactive(function() {
         vals <- input[['impl']]$get(name)
         vals
      })
   })
   
   # create plot outputs for univariate
   lapply(1:MAXCOGVARS, function(i) {
      output[['impl']]$defineOutput(
         paste("cogTable_univarPlot_", i, sep=""),
         reactivePlot(function() {
            # cat("hist plot output\n")
            colIndex <- getColIndex()
            cogDF <- cogListInput()$cogDF
            vals <- NULL
            if(i <= length(colIndex))
               vals <- columnFilterIndividualInput[[colIndex[i]]]()
            # browser()
            if(is.null(vals)) {
               return(NULL)
            } else {
               # browser()
               if(vals[[1]][1]=="from") {
                  lower <- vals[[3]][1]
                  upper <- vals[[6]][1]
                  plot(makeFootHist(cogDF[,colIndex[i]], lower, upper))
               } else {
                  # browser()
                  filt <- vals[[3]][1]
                  if(filt != "")
                     filt <- as.character(unique(cogDF[grepl(filt, cogDF[,colIndex[i]]),i]))
                  plot(makeFootBar(cogDF[,colIndex[i]], filt))
               }
            }
         }, width=95, height=50)
      )
   })
   
   # create histogram data outputs for d3 histogram
   lapply(1:MAXCOGVARS, function(i) {
      output[['impl']]$defineOutput(
         paste("cogTable_univarPlotDat_", i, sep=""),
         reactiveText(function() {
            # cat("hist output\n")
            colIndex <- getColIndex()
            cogDF <- cogListInput()$cogDF
            vals <- NULL
            if(i <= length(colIndex))
               vals <- columnFilterIndividualInput[[colIndex[i]]]()
            # browser()
            if(is.null(vals)) {
               return(NULL)
            } else {
               # browser()
               if(vals[[1]][1]=="from") {
                  getD3HistData(cogDF[,i], names(cogDF)[i])
               } else {
                  NULL
               }
            }
         })
      )
   })
   
   output$cogTable_foot <- reactive(function() {
      # cat("table foot\n")
      colIndex <- getColIndex()
      cogDF <- cogListInput()$cogDF
      
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
   
   output$cogTable_info <- reactive(function() {
      # cat("table info\n")
      n <- length(getOrderIndex())
      pageNum <- input$cogTable_pagination
      pageLen <- as.integer(input$cogTable_length)
      
      HTML(paste(
         "Showing entries", 
         (pageNum - 1)*pageLen + 1, 
         "-", 
         min(pageNum*pageLen, n),
         "of",
         n
      ))
   })
   
   output$d3bivarPlotDat <- reactive(function() {
      curCols <- input$bivarColumns
      # cat(curCols)
      cogDF <- cogListInput()$cogDF[,getColIndex()]
      
      if(curCols != "") {
         curCols <- strsplit(curCols, ",")[[1]]
         
         if(length(curCols) == 2) {
            x <- cogDF[,curCols[1]]
            y <- cogDF[,curCols[2]]
            # browser()
            makeBivarJSON(x, y, xlab=curCols[1], ylab=curCols[2])            
         } else {
            # do ICA
            ic <- fastICA(cogDF[,curCols], n.comp=2)

            makeBivarJSON(ic$S[,1], ic$S[,2], xlab="IC 1", ylab="IC 2")
         }
      } else {
         return(NULL)
      }
   })
   
   getFilterIndex <- reactive(function() {
      cogDF <- cogListInput()$cogDF
      filterIndex <- seq_len(nrow(cogDF))
      
      flt <- input$cogTable_filter
      if(!is.null(flt)) {
         n <- length(flt)
         # get index for filters that are NULL
         ind <- which(!sapply(flt[seq(3, n, by=3)], is.null))
         if(length(ind) == 0) {
            flt <- NULL
         } else {
            flt <- lapply(ind, function(x) flt[((x - 1)*3 + 1):(x*3)])
         }
         for(i in seq_along(flt)) {
            cur <- flt[[i]]
            if(cur[[1]] == "from") {
               newIndex <- which(cogDF[[cur[[2]]]] >= cur[[3]])               
            } else if(cur[[1]] == "to") {
               newIndex <- which(cogDF[[cur[[2]]]] <= cur[[3]])               
            } else if(cur[[1]] == "regex") {
               newIndex <- try(which(grepl(cur[[3]], cogDF[[cur[[2]]]])))
               if(inherits(newIndex, "try-error"))
                  newIndex <- seq_len(nrow(cogDF))
            }
            filterIndex <- intersect(filterIndex, newIndex)
         }
      }
      # browser()
      filterIndex
   })
   
   output$cogTable_body <- reactive(function() {
      cogDF <- cogTableCurrentData()
      
      if(!is.null(cogDF)) {
         pageLen <- as.integer(input$cogTable_length)
         cogTableData(cogDF, pageLen)
      }
   })
   
   output$variableCogSelectInput <- reactiveUI(function() {
      vars <- names(cogListInput()$cogDF)
      desc <- cogListInput()$cogDesc
      
      makeVariableSelectTable(vars, desc, "Cog")
   })

   output$variablePlotSelectInput <- reactiveUI(function() {
      vars <- names(cogListInput()$cogDF)
      desc <- cogListInput()$cogDesc
      
      makeVariableSelectTable(vars, desc, "Plot")
   })

   output$editor <- reactive(function() {
      localDataExtra <- getLocalDataExtra()
      plotFn <- localDataExtra$plotFn
      # or could try deparse?
      paste(capture.output(dump("plotFn", "")), collapse="\n")
   })
   
   # create plot outputs for each panel
   lapply(1:MAXPANELS, function(i) {
      output[['impl']]$defineOutput(
         paste("plotTable_panel_", i, sep=""),
         reactive(function() {
            cogDF <- curPageCogDF()
            relList <- selectedRelatedDisplays()
            # cat(i, "\n")
            if(!is.null(cogDF)) {
               # if(i==1) browser()
               plotInfo <- getPlotInfo()
               if(plotInfo$storage == "localData") {
                  localData <- getLocalData()
                  localDataExtra <- getLocalDataExtra()
                  a <- input$plotFnInput
                  if(a != "") {
                     eval(parse(text=a))
                     localDataExtra$plotFn <- plotFn
                  }
               } else {
                  localData <- NULL
                  localDataExtra <- NULL
               }
               nRow <- input$nRow
               nCol <- input$nCol
               plotWidth <- input$plotWidth
               plotHeight <- input$plotHeight
               if(!is.na(cogDF[i,]$plotKey)) {
                  png <- getPNGs(cogDF[i,], plotInfo, plotInfo$storage, localData, localDataExtra, vdbPrefix)
                  if(!is.null(relList)) {
                     # do some aspect ratio calculations...
                     nDisp <- length(relList) + 1
                     aspects <- c(cogDF[i,"aspect"], sapply(relList, function(x) x$plotInfo$aspect))
                     innerNcol <- ceiling(sqrt(nDisp))
                     innerNrow <- ceiling(nDisp / innerNcol)
                     
                     pngs <- c(png, sapply(relList, function(x) {
                        getPNGs(cogDF[i,], x$plotInfo, x$plotInfo$storage, x$localData, x$localDataExtra, vdbPrefix)
                     }))
                     # browser()

                     groups <- sapply(relList, function(x) x$plotInfo$group)
                     names <- sapply(relList, function(x) x$plotInfo$name)

                     pngs[1] <- paste("<img src='", pngs[1], "' class='png_img' alt=''/>", sep="")
                     pngs[-1] <- paste("<div><img src='", pngs[-1], "' class='png_img' alt='not loaded'/><span class='relatedInfo'>", paste(groups, names, sep=" / "), "</span></div>", sep="")

                     pngs <- matrix(data=pngs, ncol=innerNcol, byrow=TRUE)

                     return(
                     paste("
                  <table class='table-borderless' cellpadding='0' cellspacing='0' align='center' valign='center'>
                  <tbody>",
                        paste("<tr>",
                        apply(pngs, 1, function(x) paste("<td>", x, "</td>", collapse="", sep="")),
                  "</tr>",
                        sep="", collapse=""
                        ),
                        "</tbody></table>",
                        sep="", collapse="")
                     )

                  } else {
                     return(
                        paste("<a href='#' class='plotThumbnail'><img src='", png, "' class='png_img' alt=''/></a>", collapse="", sep="")
                     )
                  }
               }
            }
         })
      )
   })

   # create cognostic outputs for each panel
   lapply(1:MAXPANELS, function(i) {
      output[['impl']]$defineOutput(
         paste("plotTableCog_panel_", i, sep=""),
         reactive(function() {
            cogDF <- curPageCogDF()
            if(!is.null(cogDF)) {
               if(!is.na(cogDF[i,]$plotKey)) {
                  colNames <- input$selectedPlotVar
                  if(colNames != "") {
                     idx <- which(names(cogDF) %in% strsplit(colNames, ",")[[1]])
                     names <- paste("<strong>", names(cogDF)[idx], "</strong>", sep="")
                     vals <- cogDF[i,idx]
                     # browser()
                     HTML(paste("<table class='table-condensed table-bordered table-striped' style='width:100%'><tbody>",
                     paste("<tr><td>", apply(matrix(c(names, vals), ncol=2), 1, function(x) paste(x, collapse="</td><td>", sep="")), "</td></tr>", collapse="", sep=""),
                     "</tbody></table>"))
                  }
               }
            }
         })
      )
   })
})

# http://stackoverflow.com/questions/1296646/how-to-sort-a-dataframe-by-columns-in-r
# http://stackoverflow.com/questions/8509595/how-do-i-select-rows-by-two-criteria-in-data-table-in-r?rq=1

