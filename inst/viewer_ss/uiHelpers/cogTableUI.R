cogModal <- function() {
   div(id='cogModal', class='modal hide fade',
      HTML("<div class='modal-header'>
         <a class='close' data-dismiss='modal' >&times;</a>
         <h3 style='display: inline'>Cognostics</h3>&nbsp;&nbsp;&nbsp;<a id='cogTipPopover' rel='popover' title='View, Sort, Filter Cognostics' data-title data-content=\"<p>This pane allows the user to specify subsets or orderings of the plots based on different cognostic metrics.  It is not expected for large (many-panel) displays that the user will look at all plots, but use this pane to call certain plots to attention.</p><br /><p>Each row in the data table below represents one panel in the tiled plot display.  The column values are called <em>cognostics</em>, and by filtering or sorting on these cognostics, the user can highlight plots with interesting features.</p><br /><ul><li>Click on the arrows in the column headers to reorder the plots</li><li>Shift click for multi-column sorting</li><li>Filters for each variable are available at the bottom of the table</li><li>In free text filter fields, regular expressions can be used, e.g. 'a|b' or '^val$', etc.</li><li>Click the 'Variables' button  to choose which variables you want to have displayed in the table (if there are too many).</li><li>Multivariate filters of more than 2 variables and loading / saving / reseting the filter state are currently not implemented.</li></ul>\"><i class='icon-info-sign'></i> </a>
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

cogTableOutput <- function(data, width=500, ...) {
   # width must be at least 450

   div(id="cogTable_wrapper", 
      class="cogTable_wrapper form-inline", 
      role="grid", style=paste("width: ", width, "px;", sep=""),
      div(class="row-fluid", 
         style=paste("width: 100%; padding-bottom: 5px", sep=""),
         cogTableLength(),
         cogTableFilter(),
         div(id="cogTable_processing", 
            class="cogTable_processing", 
            style="visibility: hidden;", "Processing...")
      ),
      cogTableTable(width),
      div(class="row-fluid", id="paginateRow",
         cogTableInfo(),
         cogTablePaginate()
      )
   )
}

cogTableLength <- function() {
   si <- selectInput("cogTablePageLength", "", c(10, 25, 50, 100), selected=10)
   # si[[2]]$attribs$class <- "btn btn-mini"
   # si[[2]]$attribs$size <- "1"
   si <- tags$label("Show", si, " entries", class="cogTable_inputLabel")
   div(class="cogTablePageLength pull-left",
      ""
      # si
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
                     class='data table table-condensed table-striped table-bordered cogTable nowrap', 
                     id="cogTable",
                     cogTableHeadOutput("cogTableHead"),
                     cogTableFootOutput("cogTableFoot"),
                     cogTableBodyOutput("cogTableBody")
                  )
               )
            )
   #       )
   #    )
   # )
}

cogTableFilter <- function() {
   div(class="cogColumnFilterInput pull-right",
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
      #    # tags$input(id="cogColumnFilterInput_all", type="text", value=""),
      #    tags$button(class="btn btn-primary", href="#", id="cogTable_resetFilters", "Load"),
      #    tags$button(class="btn btn-success", href="#", id="cogTable_resetFilters", "Save"),
      #    tags$button(class="btn btn-danger", href="#", id="cogTable_resetFilters", "Reset")
      # )
   )
}

cogTableHeadOutput <- function(outputId) {
   tags$thead(id=outputId, class="shiny-html-output")
}

# need to have special output binding because other outputs depend on this one being fully rendered (need to take care of that in .js)
cogTableFootOutput <- function(outputId) {
   tags$tfoot(id=outputId, class="shiny-html-output")
}

cogTableBodyOutput <- function(outputId) {
   tags$tbody(id=outputId, class="shiny-html-output")
}

cogTableInfo <- function() {
   div(id = "cogTableInfo", class = "shiny-html-output pull-left")
}

cogTablePaginate <- function() {
   div(
      class="cogTablePaginate pagination",
      id="cogTablePagination",
      tags$ul(
         tags$li(tags$a(href="#", tags$i(class="icon-step-backward")), class="tableNavFirst"),
         tags$li(tags$a(href="#", tags$i(class="icon-chevron-left")), class="tableNavPrev"),
         tags$li(
            tags$a(href="#", id="cogTablePaginateText", class="shiny-html-output"),
            class="disabled"
         ),
         tags$li(tags$a(href="#", tags$i(class="icon-chevron-right")), class="tableNavNext"),
         tags$li(tags$a(href="#", tags$i(class="icon-step-forward")), class="tableNavLast"),
         tags$li(style="visibility:hidden", class="paginationValue", 1),
         div(id="cogTableNrow", class = "shiny-html-output paginationNrow", style="visibility:hidden"),
         div(id="cogTablePageLengthOut", class = "shiny-html-output paginationNrowLength", style="visibility:hidden"),
         class="tableNav"
      )
   )
}