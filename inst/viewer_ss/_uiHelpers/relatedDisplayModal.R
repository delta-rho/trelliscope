
relatedDisplaySelectModal <- function() {
   tagList(
   div(
      textInput("relatedDisplayUID", label="", value=""),
      style="display:none"
   ),
   div(id='relatedDisplayModal', class='modal hide fade',
      HTML("
      <div class='modal-header'>
      <a class='close' data-dismiss='modal' >&times;</a>
      <h3 style='display: inline'>Related Displays</h3>&nbsp;&nbsp;&nbsp;<a id='varRelatedTipPopover' rel='popover' title='Related Displays' data-content=\"<p>The displays listed below have the same keys as the display currently being viewed.  Selecting from the list below will link the panels of the current display to those of the selected display(s) by key and display them side-by-side in the viewer.  To revert to settings prior to opening the pane, click the 'x' button in the upper-right corner of this pane.\"><i class='icon-info-sign'></i></a>
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
