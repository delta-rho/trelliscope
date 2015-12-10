

# number of pages available for cog table
cogTableNpages <- reactive({
  n <- nrow(cogTableCurrentData())

  if(!is.null(n))
    ceiling(n / 10)
})


# outputs total number of pages available for cog table
output$cogTableNpagesOutput <- renderText({
  cogTableNpages()
})

output$cogNrow <- renderDataLite({
  nrow(cogTableCurrentData())
})

# outputs current page number for cognostics table
output$cogTableCurPageOutput <- renderText({
  pg <- input$cogTablePaginationInput
  if(!is.null(pg)) {
    return(pg)
  } else {
    return(1)
  }
})



# outputs text indicating index of records currently being viewed in cog table
output$cogTableInfoOutput <- renderText({
  n <- nrow(cogTableCurrentData())
  if(inherits(n, "try-error"))
    n <- 0

  pg <- input$cogTablePaginationInput
  if(!is.null(n)) {
    if(!is.null(pg)) {
      if(n == 0) {
        txt <- "0"
      } else {
        txt <- paste((pg - 1) * 10 + 1, "-", min(pg * 10, n))
      }
    } else {
      txt <- paste("1 - 10")
    }

    paste("Showing entries", txt, "of", n)
  }
})

# data to be shown in the cognostics table sort/filter view
# (reflects current sort and filter state
# as specified in cognostics table sort/filter panel)
cogTableCurrentData <- reactive({
  cogDF <- currentDisplay()$cdo$cogDatConn
  if(!is.null(cogDF)) {
    state <- list(
      filter = input$cogColumnFilterInput,
      sort = input$cogColumnSortInput,
      activeCog = input$activeCogStateInput
    )
    getCurCogDat(cogDF, state)
  }
})

# output current page of data to be shown in cognostics table
output$cogTableContentOutput <- renderDataLite({
  cogDF <- cogTableCurrentData()
  if(!is.null(cogDF)) {
    logMsg("Updating cog table data")
    n <- cogNrow(cogDF)

    pg <- input$cogTablePaginationInput
    if(is.null(pg)) {
      pg <- 1
    }
    pageLen <- 10

    if(n == 0) {
      idx <- integer(0)
    } else {
      idx <- ((pg - 1) * pageLen + 1):min(pg * pageLen, n)
    }

    cogTableBodyData(getCogData(cogDF, idx))
  }
})

# takes a displayObj object and applies "state" to cognostics and returns result
getCurCogDat <- function(x, state) {
  filterIndex <- seq_len(cogNrow(x))

  if(length(state$activeCog) > 0) {
    x <- x[,state$activeCog, drop = FALSE]
  }

  if(length(state$filter) > 0) {
    # browser()
    flt <- state$filter
    if(any(unlist(lapply(flt, function(x) x$empty)))) {
      filterIndex <- logical(0)
    } else {
      fltNm <- names(flt)
      for(nm in fltNm) {
        cur <- flt[[nm]]
        if(any(names(cur) %in% c("from", "to"))) {
          if(is.null(cur$from)) {
            filterIndex <- intersect(filterIndex, which(x[[nm]] <= cur$to))
          } else if(is.null(cur$to)) {
            filterIndex <- intersect(filterIndex, which(x[[nm]] >= cur$from))
          } else {
            filterIndex <- intersect(filterIndex, which(x[[nm]] <= cur$to & x[[nm]] >= cur$from))
          }
        } else {
          # if(!is.null(cur$regex))
          #   filterIndex <- intersect(filterIndex, which(grepl(cur$regex, x[[nm]])))
          if(!is.null(cur$select)) {
            filterIndex <- intersect(filterIndex, which(x[[nm]] %in% unlist(cur$select)))
          } else if(!is.null(cur$regex)) {
            filterIndex <- intersect(filterIndex, which(grepl(cur$regex, x[[nm]])))
          }
        }
      }
    }
  }

  x <- x[filterIndex,, drop = FALSE]
  if(length(filterIndex) == 0)
    return(x[filterIndex,,drop = FALSE])

  orderIndex <- seq_len(cogNrow(x))

  if(length(state$sort) > 0) {
    srt <- state$sort
    srtNm <- names(srt)

    srtNm <- intersect(srtNm, names(x))
    # TODO: warning if some srtNm are not in names of x?
    varOrder <- unname(sapply(srt, function(x) x$order))
    varDesc <- sapply(srt[order(varOrder)], function(x) x$dir == "desc")
    varOrderStr <- paste0(ifelse(varDesc, "desc(", ""), names(varDesc), ifelse(varDesc, ")", ""))

    orderIndex <- (x %>%
      dplyr::mutate(trs_idx = 1:n()) %>%
      dplyr::arrange_(.dots = varOrderStr) %>%
      dplyr::select(trs_idx))[[1]]

    # if(length(srt) == 1) {
    #   orderIndex <- order(x[, srtNm, drop = FALSE], decreasing = srt[[1]]$dir == "desc")
    # } else {
    #   orderCols <- lapply(srtNm, function(a) {
    #     if(srt[[a]]$dir == "desc") {
    #       return(-xtfrm(x[,a]))
    #     } else {
    #       return(x[,a])
    #     }
    #   })
    #   orders <- order(sapply(srt, function(x) x$order))
    #   orderIndex <- do.call(order, orderCols[orders])
    # }
  }

  return(x[orderIndex,,drop = FALSE])
}


