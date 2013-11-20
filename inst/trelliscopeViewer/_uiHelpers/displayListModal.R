displayListModal <- function() {
   div(id='displayListModal', class='modal hide fade',
      singleton(
         tags$head(
            tags$script(src = "js/ioComponents/displayList.js")
         )
      ),
      div(class='modal-header',
         tags$a(class='close', 'data-dismiss'='modal', HTML("&times;")),
         tags$h3(style='display: inline', "Select Display to View")
      ),
      div(class='modal-body',
         renderDisplayListTable()
      ),
      div(class='modal-footer')
   )
}

renderDisplayListTable <- function() {
   vdbPrefix <- getOption("vdbShinyPrefix")
   load(file.path(vdbPrefix, "displays", "_displayList.Rdata"))
   displayListDF <- subset(displayListDF, !is.na(dataClass))
   
   HTML(paste("<table class='table table-condensed table-striped table-bordered table-hover' id='displayListTable'><thead><tr>",
   paste("<th>", displayListNames, "</th>", collapse="", sep=""),
   "</tr></thead><tbody>",
   paste("<tr class='displayList'>", apply(displayListDF, 1, function(x) paste("<td>", x, "</td>", sep="", collapse="")), "</tr>", sep="", collapse="\n"),
   "</tbody></table>",
   sep="")
   )
}
