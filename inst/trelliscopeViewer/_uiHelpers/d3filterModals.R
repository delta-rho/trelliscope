
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
            tags$script(src = "js/ioComponents/d3univariateFilter.js"),
            tags$script(src = "js/ioComponents/d3univariateCogFootPlot.js")
            # tags$script(src = "js/onload.js")
         )
      ),
      # where the histogram will go
      div(class='modalFilter-body',
         div(id="d3hist")
      ),
      # button to update selected range
      # hidden div to handle json for histogram
      div(id="d3histData", class="shiny-d3histdat-output", style="display:none"),
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
            tags$script(src = "js/ioComponents/d3multivariateFilter.js")
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
