library(lattice)

cogTableHeadOutput <- function(outputId) {
   tags$thead(id=outputId, class="shiny-html-output")
}

cogTableFootOutput <- function(outputId) {
   tags$tfoot(id=outputId, class="shiny-html-output")
}

cogTableBodyOutput <- function(outputId) {
   tags$tbody(id=outputId, class="shiny-html-output")
}

cogTableLength <- function() {
   si <- selectInput("cogTable_length", "", c(10, 25, 50, 100), selected=10)
   # si[[2]]$attribs$class <- "btn btn-mini"
   # si[[2]]$attribs$size <- "1"
   si <- tags$label("Show", si, " entries", class="cogTable_inputLabel")
   div(class="cogTable_length pull-left",
      si
   )
}

cogTableFilter <- function() {
   div(class="cogTable_filter pull-right",
      tags$button(class="btn ", href="#", id="cogTable_varSelect", "Variables"),
      div(class="btn-group",
         tags$a(class='btn  dropdown-toggle', "data-toggle"="dropdown", href="#", "Filter State", tags$span(class="caret")),
         tags$ul(class="dropdown-menu", id="cogFilterMenu",
            tags$li(tags$a(id="cogTable_loadFilter", tags$i(class="icon-folder-open"), "Load")),
            tags$li(tags$a(id="cogTable_saveFilter", tags$i(class="icon-bookmark"), "Save")),
            tags$li(tags$a(id="cogTable_resetFilter", tags$i(class="icon-repeat"), "Reset"))
         )
      )
      # tags$label("Cog State: ",
      #    # tags$input(id="cogTable_filter_all", type="text", value=""),
      #    tags$button(class="btn btn-primary", href="#", id="cogTable_resetFilters", "Load"),
      #    tags$button(class="btn btn-success", href="#", id="cogTable_resetFilters", "Save"),
      #    tags$button(class="btn btn-danger", href="#", id="cogTable_resetFilters", "Reset")
      # )
   )
}

cogTableTable <- function(width) {
   # div(class="row-fluid cogTableWrap1",
   #    div(class="cogTableWrap2",
   #       div(class="cogTableWrap3",
            div(class="cogTableWrap", #, style=paste("max-width: ", width, "px;", sep=""),
               tagList(
                  singleton(tags$head(tags$link(href = "css/viewer_ss.css", rel="stylesheet"))),
                  singleton(tags$head(tags$script(src = "js/table.js"))),
                  tags$table(
                     cellpadding='0', 
                     cellspacing='0', 
                     border='0', 
                     class='data table table-condensed table-striped table-bordered cogTable', 
                     id="cogTable",
                     cogTableHeadOutput("cogTable_head"),
                     cogTableFootOutput("cogTable_foot"),
                     cogTableBodyOutput("cogTable_body")
                  )
               )
            )
   #       )
   #    )
   # )
}

cogTableInfo <- function() {
   div(
      class="cogTable_info", 
      id="cogTable_info",
      htmlOutput("cogTable_info")
   )
}

cogTablePaginate <- function() {
   div(
      class="cogTable_paginate pagination",
      id="cogTable_pagination",
      tags$ul(
         tags$li(tags$a(href="#", tags$i(class="icon-step-backward")), class="tableNavFirst"),
         tags$li(tags$a(href="#", tags$i(class="icon-chevron-left")), class="tableNavPrev"),
         tags$li(
            tags$a(href="#", id="cogTable_paginate_text", class="shiny-html-output"),
            class="disabled"
         ),
         tags$li(tags$a(href="#", tags$i(class="icon-chevron-right")), class="tableNavNext"),
         tags$li(tags$a(href="#", tags$i(class="icon-step-forward")), class="tableNavLast"),
         tags$li(style="visibility:hidden", class="paginationValue", 1),
         div(id="cogTable_nrow", class = "shiny-html-output paginationNrow", style="visibility:hidden"),
         div(id="cogTable_nrow_length", class = "shiny-html-output paginationNrowLength", style="visibility:hidden"),
         class="tableNav"
      )
   )
}

cogTableOutput <- function(data, width=500, ...) {
   # width must be at least 450

   div(id="cogTable_wrapper", class="cogTable_wrapper form-inline", role="grid", style=paste("width: ", width, "px;", sep=""),
      div(class="row-fluid", style=paste("width: 100%; padding-bottom: 5px", sep=""),
         cogTableLength(),
         cogTableFilter(),
         div(id="cogTable_processing", class="cogTable_processing", style="visibility: hidden;", "Processing...")
      ),
      cogTableTable(width),
      div(class="row-fluid",
         cogTableInfo(),
         cogTablePaginate()
      )
   )
}

cogTableData <- function(data, nr) {
   # browser()
   nc <- ncol(data)
   if(nc == 0) {
      return(NULL)      
   } else if(nrow(data) == 0) {
      tDataString <- matrix(nrow=nr, ncol=nc, data="<td>&nbsp;</td>")   
   } else {
      tDataString <- matrix(sapply(seq_len(nc), function(i) {
         if(class(data[[i]]) %in% c("integer", "numeric")) {
            val <- format(data[[i]], big.mark=",") #, scientific=-3)
         } else {
            val <- data[[i]]
         }
         paste("<td>", val, "</td>", sep="")
      }), ncol=nc)
      # if fewer then the number of rows, fill with blank <td>s
      # (so that the height is always fixed)
      if(nrow(tDataString) < nr) {
         rnr <- nrow(tDataString)
         nn <- nr - rnr
         tDataString <- rbind(tDataString, matrix(nrow=nn, ncol=nc))
         tDataString[(rnr + 1):nr, 1:nc] <- "<td>&nbsp;</td>"
      }      
   }
   tDataString <- paste(paste(t(cbind("<tr>", tDataString, "</tr>"))), collapse="")
   HTML(tDataString)
}

cogTableHead <- function(data) {
   dnames <- names(data)
   dindex <- seq_along(dnames)
   
   tHeadString <- paste(
      "<tr role='row' id='cogTable_sorting' class='columnSortInput'>",
      paste(
         "<th align='center' class='sorting' role='columnheader' 'tabindex='", 
         dindex - 1, 
         "' aria-controls='cogTable' rowspan='1' colspan='1' aria-label=', ", 
         dnames, 
         ": activate to sort'>", 
         dnames, 
         "<i class='icon-minus'></th>", 
         sep="", collapse=""),
      "</tr>", 
      sep=""
   )
   HTML(tHeadString)
}
# https://github.com/FortAwesome/Font-Awesome/issues/25

cogTableFootFilter <- function(data) {
   # browser()
   nm <- names(data)
   tFoot <- paste("<tr role='row' id=cogTable_filter class='columnFilters'>",
      paste(sapply(seq_along(data), function(i) {
         if(class(data[,i]) %in% c("numeric", "integer")) {
            curInput <- sprintf(
               "<td id='cogTable_uniplot_%d' class='columnUniPlot'><input style='text-align:right; width: 83px' id='lower_column_%s' name='%s' type='text' pattern=\"\\d+(\\.\\d*)?\" placeholder='From' class='noShinyDefaultBinding columnFilter columnFilterFrom'/><br/>
               <input style='text-align:right; width: 83px' id='upper_column_%s' name='%d' type='text' pattern=\"\\d+(\\.\\d*)?\" placeholder='To' class='noShinyDefaultBinding columnFilter columnFilterTo'/></td>", i, nm[i], nm[i], nm[i], i)
         } else {
            curInput <- sprintf("<td id='cogTable_uniplot_%d' class='columnUniPlot'><input style='text-align:right; width: 83px' id='text_column_%s' type='text' name='%s' placeholder='regex' class='noShinyDefaultBinding columnFilter columnFilterRegex'/></td>", i, nm[i], nm[i])
         }
         curInput
      }), collapse="\n", sep=""),
      "</tr>", sep=""
   )
   tFoot
}

footPlotParList <- list(axis.line = list(col = 0), layout.heights = list(top.padding = 0, main.key.padding = 0, key.axis.padding = 0, axis.xlab.padding = 0, xlab.key.padding = 0, key.sub.padding = 0, bottom.padding = 0), layout.widths = list(left.padding = 0, key.ylab.padding = 0, ylab.axis.padding = 0, axis.key.padding = 0, right.padding = 0), axis.components = list(top = list(pad1 = 2, 0, pad2 = 0), right = list(pad1 = 0, pad2 = 0)))

getD3HistData <- function(dat, xlab) {
   # TODO: add logic about number of breaks
   hst <- hist(dat, plot=FALSE)
   HTML(paste("{\"xlab\": \"", xlab, "\", \"data\": [", paste(apply(matrix(data=c(
      hst$breaks,
      c(hst$counts, 0)
   ), ncol=2), 1, function(x) paste("{\"xdat\":", x[1], ",\"ydat\":", x[2], "}", sep="")), collapse=","), "]}", sep=""))
}

makeFootHist <- function(x, rmin=NULL, rmax=NULL) {
   hst  <- hist(x, plot=FALSE)
   delta <- diff(hst$mids[1:2]) / 2

   rng <- diff(range(x, na.rm=TRUE))

   if(is.null(rmax) && is.null(rmin)) {
      rmin <- max(x, na.rm=TRUE) + rng
      rmax <- min(x, na.rm=TRUE) - rng
      sMin <- sMax <- numeric(0)
   } else {
      if(is.null(rmax))
         rmax <- max(x, na.rm=TRUE) + rng
      if(is.null(rmin))
         rmin <- min(x, na.rm=TRUE) - rng

      sMin <- which(hst$mids + delta > rmin & hst$mids - delta < rmin)
      sMax <- which(hst$mids + delta > rmax & hst$mids - delta < rmax)      
   }

   xyplot(hst$density ~ hst$mids,
      panel=function(x, y, ...) {
         for(i in setdiff(seq_along(hst$mids), c(sMin, sMax))) {
            panel.rect(x[i] - delta, 0, x[i] + delta, y[i], border="white", col=ifelse(x[i] + delta < rmin || x[i] - delta > rmax, "darkgray", "#7080D7"))
         }
         if(length(sMin) > 0) {
            panel.rect(x[sMin] - delta, 0, rmin, y[sMin], col="darkgray", border=NA)
            panel.rect(rmin, 0, x[sMin] + delta, y[sMin], col="#7080D7", border=NA)
            panel.rect(x[sMin] - delta, 0, x[sMin] + delta, y[sMin], col=NA, border="white")            
         }
         if(length(sMax) > 0) {
            panel.rect(x[sMax] - delta, 0, rmax, y[sMax], col="#7080D7", border=NA)
            panel.rect(rmax, 0, x[sMax] + delta, y[sMax], col="darkgray", border=NA)
            panel.rect(x[sMax] - delta, 0, x[sMax] + delta, y[sMax], col=NA, border="white")            
         }
      },
      ylim=c(0, max(hst$density)),
      xlim=range(hst$mids) + c(-1, 1) * delta,
      scales=list(draw=FALSE),
      xlab="",
      ylab="",
      par.settings=footPlotParList
   )
}

makeFootBar <- function(x, selected=NULL) {
   dat <- data.frame(table(x))
   dat$x <- as.character(dat$x)
   # if(is.null(selected)) {
   #    selected <- unique(dat$x)
   # }
   dat$order <- order(dat$Freq)
   dat$selected <- dat$x %in% selected
   delta <- 0.5
   xyplot(order ~ Freq,
      data=dat, 
      panel=function(x, y, ...) {
         for(i in seq_along(x)) {
            panel.rect(0, y[i] - delta, x[i], y[i] + delta, border="white", col=ifelse(dat$selected[i], "#7080D7", "darkgray"))
         }
      },
      ylim=range(dat$order) + c(-1, 1) * delta,
      xlim=c(0, max(dat$Freq)),
      scales=list(draw=FALSE),
      xlab="",
      ylab="",
      par.settings=footPlotParList
   )
}

# makeFootHist(iris[,1])
# makeFootHist(iris[,1], 5.2, 7.3)
# makeFootHist(iris[,1], 4.7, NULL)
# makeFootHist(iris[,1], NULL, 6.8)
# 
# set.seed(234)
# x <- iris[sample(1:150, 100, replace=TRUE),5]
# makeFootBar(x)
# makeFootBar(x, "virginica")
# makeFootBar(x, c("virginica", "setosa"))

# Univariate (marginal) distribution filter:
cogTableFootUnivar <- function(data) {
   nm <- names(data)
   tFoot <- paste("<tr><td colspan='", ncol(data), "' style='text-align:left; background-color: #777777; color:white'><strong>Uni Filter </strong><i class='icon-info-sign icon-white'></i></td></tr><tr role='row' id=cogTable_univar class='columnUnivar'>",
      paste(sapply(seq_along(data), function(i) {
         curClass <- ifelse(is.numeric(data[1,i]), "univarPlotHist", "")
         paste(
            "<td><div class='", curClass, "' title='", nm[i], "'><div id='cogTable_univarPlot_", i, "' class='shiny-plot-output' style='width: 95px; height: 53px;'></div><div id='cogTable_univarPlotDat_", i, "' class='shiny-d3histDat-output' style='display:none;'></div></div></td>", sep="")
      }), collapse="", sep=""),
      "</tr>", sep=""
   )
   tFoot
}

# (select 2+ boxes below and click <span class='bivarPlotScatter'>here</span>)
cogTableFootBivar <- function(data) {
   tFoot <- paste("<tr><td colspan='", ncol(data), "' style='text-align:left; background-color: #777777; color:white' class='bivarPlotScatter'><strong>Multi Filter </strong><i class='icon-info-sign icon-white'></i></td></tr><tr role='row' id=cogTable_bivar_select>",
      paste(sapply(seq_along(data), function(i) {
         curClass <- ifelse(is.numeric(data[1,i]), "", "not-numeric")
         paste(
            "<td class='", curClass, "' name='", names(data)[i], "'>&nbsp;</td>", sep="")
      }), collapse="", sep=""),
      "</tr>",
      tdSelectString("#cogTable_bivar_select td"),
      sep=""
   )
   tFoot
}

tdSelectString <- function(selector) {
   paste("<script>
   $(function () {
      var isMouseDown = false,
      isHighlighted;
      $('", selector, "')
      .mousedown(function () {
         isMouseDown = true;
         if(!$(this).hasClass('not-numeric')) {
            $(this).toggleClass('highlighted');
            isHighlighted = $(this).hasClass('highlighted');         
         } else {
            // alert('hi');
         }
         return false; // prevent text selection
      })
      .mouseover(function () {
         if (isMouseDown && !$(this).hasClass('not-numeric')) {
            $(this).toggleClass('highlighted', isHighlighted);
         }
      })
      .bind('selectstart', function () {
         return false;
      })
      $(document)
      .mouseup(function () {
         isMouseDown = false;
      });
   });
   </script>", sep="")
}

viewerBootstrapPage <- function (...) {
   importBootstrap <- function(min = TRUE, responsive = TRUE) {
      ext <- function(ext) {
         ifelse(min, paste(".min", ext, sep = ""), ext)
      }
      cssExt <- ext(".css")
      jsExt = ext(".js")
      bs <- ""
      result <- tags$head(tags$link(rel = "stylesheet", type = "text/css", href = paste(bs, "css/bootstrap", cssExt, sep = "")), tags$script(src = paste(bs, "js/bootstrap", jsExt, sep = "")))
      if (responsive) {
         result <- tagAppendChild(result, tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"))
         result <- tagAppendChild(result, tags$link(rel = "stylesheet", type = "text/css", href = paste(bs, "css/bootstrap-responsive", cssExt, sep = "")))
      }
      result
   }
   tagList(importBootstrap(), list(...))
}

viewerPage <- function (preHeader, headerPanel, mainPanel) 
{
    viewerBootstrapPage(preHeader, div(class = "container-fluid", div(class = "row-fluid", 
        headerPanel), div(class = "row-fluid", 
        mainPanel)))
}

viewerHeaderPanel <- function() {
   div(class='navbar navbar-fixed-top',
   div(class='navbar-inner',
   div(class='container-fluid',
      div(class='span3',
         tags$a(class='brand', id='plotName', htmlOutput("plotName"))
      ),
      HTML("<div class='span1'>
            <ul class='nav'>
               <li><a data-toggle='modal' href='#aboutModal' class='active'>About</a></li>
            </ul>
         </div>
         <div class='span2'>
            <form class='navbar-search' id='pppForm'>
               <input type='text' class='input-medium search-query' placeholder='panels per screen' id='pppInput'>
            </form>
         </div>"
      ),
      div(class='span3', id='navbuttons',
         div(class='btn-group no_selection',
            HTML("<a class='btn' id='btnBeg'>&laquo;</a>               
               <a class='btn' id='btnPrev'>&larr;</a>"
            ),
            tags$a(class='btn', id='btnCurrentPage', htmlOutput("currentPageText")),
            HTML("<a class='btn' id='btnNext'>&rarr;</a>
               <a class='btn' id='btnEnd'>&raquo;</i></a>"
            )
         )
      ),
      HTML("<div class='span3'>
               <a data-toggle='modal' href='#viewOptionsModal' class='btn btn-primary btn-small' id='variables_plot_button'>
               <!--<i class='icon-eye-open icon-white'></i> --> View</a>
               <a data-toggle='modal' href='#cogModal' class='btn btn-primary btn-small' id='cognostics_button'>
               <i class='icon-cog icon-white'></i> Cog</a>
               <div class='btn-group' id='cognostics_button'>
                 <a class='btn dropdown-toggle btn-primary btn-small' data-toggle='dropdown' href='#'>
                   <!--<i class='icon-picture icon-white'></i> -->Display
                   <span class='caret'></span>
                 </a>
                 <ul class='dropdown-menu'>
                   <li><a data-toggle='modal' href='#displayListModal'><i class='icon-list'></i> Change Display</a></li>
                   <li><a data-toggle='modal' href='#relatedDisplayModal'><i class='icon-th'></i> Add Related</a></li>
                 </ul>
               </div>
            </div>"
      )
   )))
}

cogModal <- function() {
   div(id='cogModal', class='modal hide fade',
      HTML("<div class='modal-header'>
         <a class='close' data-dismiss='modal' >&times;</a>
         <h3 style='display: inline'>Cognostics</h3>&nbsp;&nbsp;&nbsp;<a id='cogTipPopover' rel='popover' title='View, Sort, Filter Cognostics' data-content=\"<p>Each row in the data table below represents one panel in the tiled plot display.  The column values are called <em>cognostics</em>, and by filtering or sorting on these cognostics, the user can highlight plots with interesting features.</p><br /><p>This pane allows the user to specify subsets or orderings of the plots based on different cognostic metrics.  It is not expected for large (many-panel) displays that the user will look at all plots, but use this pane to call certain plots to attention.</p><br /> <ul><li>Click on the arrows in the column headers to reorder the plots</li><li>Shift click for multi-column sorting</li><li>Filters for each variable are available at the bottom of the table</li><li>In free text filter fields, regular expressions can be used, e.g. 'a|b' or '^val$', etc.</li><li>The 'reset' button clears all sorting and filtering.</li><li>To revert to sort and filter settings prior to opening the pane, click the 'x' button in the upper-right corner of this pane.</li></ul>\"><i class='icon-info-sign'></i> </a>
      </div>"
      ),
      div(class='modalCog-body',
         cogTableOutput("cogTable", width=600)
      ),
      HTML("
      <div class='modal-footer'>
         <a href='#' class='btn' data-dismiss='modal' >Close</a>
         <a class='btn btn-primary' data-dismiss='modal' id='btnUpdatePlots'>Update</a>
      </div>"
      )
   )
}

displayListModal <- function() {
   div(id='displayListModal', class='modal hide fade',
      HTML("
      <div class='modal-header'>
         <a class='close' data-dismiss='modal' >&times;</a>
         <h3 style='display: inline'>Plot List</h3>
      </div>"
      ),
      div(class='modal-body',
         displayListOutput()
      ),
      HTML("<div class='modal-footer'>
      </div>")
   )
}


relatedDisplaySelectModal <- function() {
   tagList(
   div(
      textInput("relatedDisplayVar", label="", value=""),
      style="display:none"
   ),
   div(id='relatedDisplayModal', class='modal hide fade',
      HTML("
      <div class='modal-header'>
      <a class='close' data-dismiss='modal' >&times;</a>
      <h3 style='display: inline'>Related Displays</h3>&nbsp;&nbsp;&nbsp;<a id='varTipPopover' rel='popover' title='Related Displays' data-content=\"<p>The displays listed below have the same keys as the display currently being viewed.  Selecting from the list below will link the panels of the current display to those of the selected display(s) by key and display them side-by-side in the viewer.  To revert to settings prior to opening the pane, click the 'x' button in the upper-right corner of this pane.\"><i class='icon-info-sign'></i></a>
      </div>"
      ),
      div(class='modal-body',
         htmlOutput("relatedDisplayListOutput")
      ),
      div(class="modal-footer",
         tags$a(class="btn", "data-dismiss"="modal", "Close"),
         tags$a(class="btn btn-primary", "data-dismiss"="modal", id="relatedDisplaySelectBtn", "Update")
      )
   ))
}

d3histModal <- function() {
   div(id='d3histModal', class='modalFilter hide fade',
      HTML("
      <div class='modal-header'>
         <a class='close' data-dismiss='modal' >&times;</a>
         <h3 style='display: inline'>Distribution Range Filter</h3>
      </div>"
      ),
      singleton(
         tags$head(
            tags$link(href = "css/d3hist.css", rel="stylesheet"),
            tags$script(src = "js/d3v2.min.js"),
            tags$script(src = "js/d3hist.js")
            # tags$script(src = "js/onload.js")
         )
      ),
      # where the histogram will go
      div(class='modalFilter-body',
         div(id="d3hist")
      ),
      # button to update selected range
      # hidden div to hold json for histogram
      # div(id="d3histData", class="shiny-d3histDat-output", style="display:none"),
      div(
         numericInput("histMin", "Lower selection", 0),
         numericInput("histMax", "Upper selection", 0),
         style="display:none"
      ),
      textOutput("histRange"),
      div(class="modal-footer", 
         div(id="d3histRange"),
         tags$p(span(class="label label-info", "Note:"), " clicking 'Update' will reset all other filters.    ", 
            tags$a(class="btn", "data-dismiss"="modal", "Close"),
            tags$a(class="btn btn-primary", id="d3histSubmit", "Update")
         )
      )
   )
}

d3bivarModal <- function() {
   div(id='d3bivarModal', class='modalFilter hide fade',
      HTML("
      <div class='modal-header'>
         <a class='close' data-dismiss='modal' >&times;</a>
         <h3 style='display: inline'>Multivariate Range Filter</h3>
      </div>"
      ),
      singleton(
         tags$head(
            # tags$link(href = "css/d3hist.css", rel="stylesheet"),
            # tags$script(src = "js/d3v2.min.js"),
            tags$script(src = "js/d3bivar.js")
         )
      ),
      # where the plot will go
      div(class='modalFilter-body',
         div(id="d3bivar")
      ),
      # button to update selected range
      # hidden div to hold json for plot
      div(id="d3bivarPlotDat", class="shiny-d3bivarDat-output", style="display:none"),
      # hidden input for selected columns and 
      div(
         textInput("bivarColumns", "columns", ""),
         numericInput("bivarMinX", "Lower x", 0),
         numericInput("bivarMaxX", "Upper x", 0),
         numericInput("bivarMinY", "Lower y", 0),
         numericInput("bivarMaxY", "Upper y", 0),
         style="display:none"
      ),
      textOutput("bivarRange"),
      div(class="modal-footer", 
         div(id="d3bivarRange"),
         tags$p(span(class="label label-info", "Note:"), " clicking 'Update' will reset all other filters.    ", 
            tags$a(class="btn", "data-dismiss"="modal", "Close"),
            tags$a(class="btn btn-primary", id="d3bivarSubmit", "Update")
         )
      )
   )
}

relatedDisplayTable <- function(relatedDisplays) {
   if(is.null(relatedDisplays)) {
      HTML("No related displays")
   } else {
      header <- paste("<th>", c("Group", "Name", "Description", "Select"), "</th>", sep="", collapse="")
      
      c1 <- paste("<td>", relatedDisplays$group, "</td>", sep="")
      c2 <- paste("<td>", relatedDisplays$name, "</td>", sep="")
      c3 <- paste("<td>", relatedDisplays$desc, "</td>", sep="")
      c4 <- paste("<td class='selectableDisplayVar' name='", relatedDisplays$uid, "'></td>", sep="")

      HTML(paste("<table id='relatedDisplaySelectTable' class='table table-condensed table-bordered'><thead><tr>", header, "</tr></thead><tbody>",
      paste("<tr>",
         apply(cbind(c1, c2, c3, c4), 1, function(x) paste(x, collapse="")),
         "</tr>",
         collapse=""
      ),
      "</tbody></table>",
         tdSelectString("#relatedDisplaySelectTable td.selectableDisplayVar"),
         sep=""
      ))

      
   }
}

displayListOutput <- function() {
   vdbPrefix <- getOption("vdbShinyPrefix")
   load(file.path(vdbPrefix, "displays/_displayList.Rdata"))
   
   HTML(paste("<table class='table table-condensed table-striped table-bordered table-hover' id='displayListTable' style='width:525px'><thead><tr>",
   paste("<th>", names(displayList), "</th>", collapse="", sep=""),
   "</tr></thead><tbody>",
   paste("<tr class='displayList'>", apply(displayList, 1, function(x) paste("<td>", x, "</td>", sep="", collapse="")), "</tr>", sep="", collapse="\n"),
   "</tbody></table>",
   sep="")
   )
}

hiddenDimInputs <- function() {
   div(style="display: none",
      numericInput("nRow", "", 1),
      numericInput("nCol", "", 1),
      numericInput("plotWidth", "", 0),
      numericInput("plotHeight", "", 0),
      numericInput("currentPage", "", 1),
      # numericInput("plotAspect", "", 1),
      htmlOutput("plotAspect"),
      htmlOutput("nPages")
   )
}

makeBivarJSON <- function(x, y, xlab="x", ylab="y", shape=460/660, xbin=50) {
   dat <- hexbin(x, y, shape=shape, xbin=xbin)
   
   # make a scatterplot of less than 2500 points
   type <- ifelse(length(x) < 2500, "scatter", "hex")

   if(type=="hex") {
      style <- "lattice"
      minarea <- 0.05
      maxarea <- 0.8
      mincnt <- 1
      maxcnt <- max(dat@count)
      style <- "lattice"
      trans <- NULL
      
      cnt <- dat@count
      xbins <- dat@xbins
      shape <- dat@shape
      tmp <- hcell2xy(dat)
      good <- mincnt <= cnt & cnt <= maxcnt

      xnew <- tmp$x[good]
      ynew <- tmp$y[good]
      cnt <- cnt[good]

      sx <- xbins/diff(dat@xbnds)
      sy <- (xbins * shape)/diff(dat@ybnds)

      if (is.null(trans)) {
         if (min(cnt, na.rm = TRUE) < 0) {
            pcnt <- cnt + min(cnt)
            rcnt <- {
               if (maxcnt == mincnt) rep.int(1, length(cnt)) else (pcnt - mincnt)/(maxcnt - mincnt)
            }
         } else rcnt <- {
            if (maxcnt == mincnt) rep.int(1, length(cnt)) else (cnt - mincnt)/(maxcnt - mincnt)
         }
      } else {
         rcnt <- (trans(cnt) - trans(mincnt))/(trans(maxcnt) - trans(mincnt))
         if (any(is.na(rcnt))) stop("bad count transformation")
      }
      area <- minarea + rcnt * (maxarea - minarea)
      area <- pmin(area, maxarea)
      radius <- sqrt(area)

      inner <- 0.5
      outer <- (2 * inner)/sqrt(3)
      dx <- inner/sx
      dy <- outer/(2 * sy)
      rad <- sqrt(dx^2 + dy^2)
      hexC <- hexcoords(dx, dy, sep = NULL)

      dd <- data.frame(xdat=xnew, ydat=ynew, r=radius)

      dataStr <- paste("\"data\": [", paste(apply(as.matrix(dd), 1, function(x) paste("{\"x\":", x[1], ",\"y\":", x[2], ",\"r\":", x[3], "}", sep="")), collapse=","), "]", sep="")

      paste("{\"type\":\"", type, "\", \"shape\":", shape, ", \"xlab\":\"", xlab, "\", \"ylab\":\"", ylab, "\", \"hexx\": [", 
         paste(hexC$x, collapse=","), 
         "], \"hexy\":[",
         paste(hexC$y, collapse=","), 
         "], ", dataStr, "}", sep=""
      )
   } else {
      dd <- data.frame(x=x, y=y)
      dataStr <- paste("\"data\": [", paste(apply(as.matrix(dd), 1, function(x) paste("{\"x\":", x[1], ",\"y\":", x[2], "}", sep="")), collapse=","), "]", sep="")
      
      paste("{\"type\":\"", type, "\", \"xlab\":\"", xlab, "\", \"ylab\":\"", ylab, "\", ", dataStr, "}", sep=""
      )
   }
}


## TODO: make a row of the table selectable tds for which you can view the distribution or projections
# http://stackoverflow.com/questions/2013902/select-cells-on-a-table-by-dragging


# TODO: remove when vdb is available - this exists in vdb
encodePNG <- function(plotLoc) {
   bytes <- file.info(plotLoc)$size
   b64 <- base64encode(readBin(plotLoc, "raw", n = bytes))
   paste("data:image/png;base64,", b64, sep = "")   
}

getPNGs <- function(cogDF, plotInfo, storage, localData=NULL, localDataExtra=NULL, vdbPrefix) {
   if(storage=="local") {
      hasSubDir <- with(plotInfo, subDirSize != 0 && n > subDirSize)
      if(hasSubDir) {
         cogDF$subDir <- sapply(cogDF$plotKey, function(x) keyHash(x, ceiling(plotInfo$n / plotInfo$subDirSize)))
      } else {
         cogDF$subDir <- ""
      }
      # browser()

      ff <- file.path(vdbPrefix, "displays", plotInfo$group, plotInfo$name, "png", cogDF$subDir, paste(cogDF$plotKey, ".png", sep=""))
      pngs <- sapply(ff, encodePNG) # pngs <- ff
   } else if(storage=="hdfs") {
      pngs <- sapply(rhgetkey(as.list(cogDF$plotKey), plotInfo$hdfs), function(x) x[[2]][[1]][[1]])
   } else if(storage=="mongo") {
      mongoConn <- vdbMongoInit(conn)
      mongoNS <- paste(conn$vdbName, plotInfo$name, sep=".")
      pngs <- sapply(cogDF$plotKey, function(x) getMongoPlot(mongoConn, mongoNS, x))
   } else if(storage=="localData") {
      # browser()
      keys <- sapply(localData, function(x) x$splitKey)
      ind <- which(keys %in% cogDF$plotKey)
      tmpfile <- tempfile()
      pngs <- sapply(localData[ind], function(x) {
         vdbMakePNG(dat=x, plotFn=localDataExtra$plotFn, file=tmpfile, width=localDataExtra$plotDim$width, height=localDataExtra$plotDim$height, res=localDataExtra$plotDim$res, xLimType=localDataExtra$xLimType, yLimType=localDataExtra$yLimType, lims=localDataExtra$lims)
         encodePNG(tmpfile)
      })
   }
   pngs
}

plotTabSkeleton <- function(nRow, nCol) {

   innerTables <- matrix(
      paste("
         <table class='table-condensed table-bordered'>
         <tbody>
         <tr>
         <td align='center' class='img_td' name='", seq_len(nRow*nCol), "'>
         <div id='plotTable_panel_", seq_len(nRow*nCol), "' class='shiny-html-output'></div>
         </td>
         </tr>
         <tr>
         <td>
         <div id='plotTableCog_panel_", seq_len(nRow*nCol), "' class='shiny-html-output'></div>         
         </td>
         </tr>
         </tbody>
         </table>", 
         sep=""
      ),
      nrow=nRow, ncol=nCol, byrow=TRUE
   )
   
   outerTable <- paste("
<table border='0' cellpadding='0' cellspacing='0' align='center' id='plotTable' valign='center'>
<tbody>",
      paste("<tr>",
      apply(innerTables, 1, function(x) paste("<td>", x, "</td>", collapse="", sep="")),
"</tr>",
      sep="", collapse=""
      ),
      "</tbody></table>"
   )
   outerTable
}

# getPlotString <- function(cogDF, plotInfo, nRow, nCol, plotWidth, plotHeight, storage) {
#    pngs <- getPNGs(cogDF, plotInfo, storage, vdbPrefix)
#    n <- nrow(cogDF)
# 
#    extra <- nRow*nCol - n
#    if(n > 0)
#       pngs <- c(pngs, rep("", extra))
# 
#    innerTables <- paste("
# <table cellpadding='0' cellspacing='0'>
# <tbody>
# <tr>
#    <td align='center' class='img_td' name='0'><a href='#' class='plotThumbnail'><img src='", pngs, "' class='png_img' alt=''/></a>
#    </td>
# </tr>
# </tbody>
# </table>", 
#       sep=""
#    )
#    innerTables <- matrix(innerTables, nrow=nRow, ncol=nCol, byrow=TRUE)
#    
#    outerTable <- paste("
# <table border='0' cellpadding='0' cellspacing='0' align='center' id='plotTable' valign='center'>
# <tbody>",
#       paste("<tr>",
#       apply(innerTables, 1, function(x) paste("<td>", x, "</td>", collapse="", sep="")),
# "</tr>",
#          sep="", collapse=""
#       ),
#       "</tbody></table>"
#    )
#    outerTable
# }

# type should be "Cog" or "Plot"
makeVariableSelectTable <- function(vars, desc, type) {
   c1 <- paste("<td>", vars, "</td>", sep="")
   c2 <- paste("<td>", desc, "</td>", sep="")

   if(type=="Cog") {
      c3 <- paste("<td class='selectableCogVar highlighted' name='", vars, "'></td>", sep="")      
   } else {
      c3 <- paste("<td class='selectablePlotVar' name='", vars, "'></td>", sep="")
   }
   
   header <- paste("<th>", c("Variable", "Description", "Show"), "</th>", sep="", collapse="")
   
   HTML(paste("<table id='variable", type, "SelectTable' class='table table-condensed table-bordered'><thead><tr>", header, "</tr></thead><tbody>",
   paste("<tr>",
      apply(cbind(c1, c2, c3), 1, function(x) paste(x, collapse="")),
      "</tr>",
      collapse=""
   ),
   "</tbody></table>",
      tdSelectString(paste("#variable", type, "SelectTable td.selectable", type, "Var", sep="")),
      sep=""
   ))
}

# modal to determine what variables to show under each plot
variablePlotSelectModal <- function() {
   tagList(
   div(
      textInput("selectedPlotVar", label="", value=""),
      style="display:none"
   ),
   div(id='viewOptionsModal', class='modal hide fade',
      HTML("
      <div class='modal-header'>
      <a class='close' data-dismiss='modal' >&times;</a>
      <h3 style='display: inline'>View Options</h3>
      </div>"
      ),
      div(class="modal-body",
         tabsetPanel(
            tabPanel("Vars", 
               tags$h4("Display variables under panels"),
               # tags$a(id='varTipPopover', rel='popover', title='Update Variable Visibility', 'data-content'="<p>Here you can choose which variables should be visible under each panel.  To revert to settings prior to opening the pane, click the 'x' button in the upper-right corner of this pane.",
               #    tags$i(class='icon-info-sign')
               # ),
               htmlOutput("variablePlotSelectInput")
            ), 
            tabPanel("plotFn", 
               tags$h4("Update plot function"),
               div(id="editorWrapper", 
                  class="shiny-plotFn-output",
                  div(
                     id="editor", 
                     class="shiny-plotFn-output"
                  ),
                  div(
                     tags$textarea(id="plotFnInput"),
                     style="display:none"
                  ),
                  HTML("<script src='' type='text/javascript' charset='utf-8'></script>
         <script>
            var editor = ace.edit('editor');
            editor.setTheme('ace/theme/tomorrow'); 
            editor.getSession().setTabSize(3);
            editor.getSession().setUseSoftTabs(true);                  
            editor.getSession().setMode('ace/mode/r');
         </script>")
               )
            )
         )
      ),
      div(class="modal-footer",
         tags$a(class="btn", "data-dismiss"="modal", id="d3bivarSubmit", "Close"),
         tags$a(class="btn btn-primary", "data-dismiss"="modal", id="viewOptionsBtn", "Update")
      )
   ))
}

variableCogSelectModal <- function() {
   tagList(
   div(
      textInput("selectedCogVar", label="", value=""),
      style="display:none"
   ),
   div(id='varCogModal', class='modal hide fade',
      HTML("
      <div class='modal-header'>
      <a class='close' data-dismiss='modal' >&times;</a>
      <h3 style='display: inline'>Variables in Cognostics Table</h3>&nbsp;&nbsp;&nbsp;<a id='varTipPopover' rel='popover' title='Update Variable Visibility' data-content=\"<p>Here you can choose which variables should be visible when viewing the cognostics table.  To revert to settings prior to opening the pane, click the 'x' button in the upper-right corner of this pane.  Note that removing variables from cognostics will remove any filtering that has been done on those variables</p>\"><i class='icon-info-sign'></i></a>
      </div>"
      ),
      div(class='modal-body',
         htmlOutput("variableCogSelectInput")
      ),
      div(class="modal-footer",
         tags$p(span(class="label label-info", "Note:"), " clicking 'Update' will reset all other filters.    ", 
            tags$a(class="btn", "data-dismiss"="modal", "Close"),
            tags$a(class="btn btn-primary", "data-dismiss"="modal", id="variableCogSelectBtn", "Update")
         )
      )
   ))
}