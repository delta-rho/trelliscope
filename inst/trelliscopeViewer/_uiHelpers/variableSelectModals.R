
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
            tabPanel("Layout", 
               tags$h4("Panel Layout", style="display:inline; line-height:25px"),
               tags$a(id='panelLayoutTipPopover', rel='popover', 
                  title='Panel Layout', 
                  'data-content'="<p>Choose either the approximate number of panels per screen you would like to view or specify number of rows and columns.  If you select panels per screen, the number of rows and columns will be chosen based on the aspect ratio of the panels.</p>",
                  tags$i(class='icon-info-sign')
               ),
               tags$hr(),
               tags$form(class="form-horizontal",
                  div(class="control-group",
                  tags$label("Panels per screen", class="control-label"),
                  div(class="controls",
                     tags$input(id="pppInput", type="number", value=1, class="input-small")
                  )),
                  div(class="control-group",
                  tags$label("Rows", class="control-label"),
                  div(class="controls",
                     tags$input(id="panelRows", type="number", value=1, class="input-small")
                  )),
                  div(class="control-group",
                  tags$label("Columns", class="control-label"),
                  div(class="controls",
                     tags$input(id="panelCols", type="number", value=1, class="input-small")
                  ))
               )
            ), 
            tabPanel("Vars", 
               tags$h4("Display variables under panels", style="display:inline; line-height:25px"), 
               tags$a(id='varPanelTipPopover', rel='popover', 
                  title='Update Variable Visibility', 
                  'data-content'="<p>Here you can choose which variables should be visible under each panel.  Click and drag on the 'Show' column to highlight which variables to show.</p>",
                  tags$i(class='icon-info-sign')
               ), 
               tags$hr(),
               htmlOutput("variablePlotSelectInput")
            ), 
            tabPanel("panelFn", 
            tags$h4("Update plot function", style="display:inline; line-height:25px"), 
            tags$a(id='panelFnUpdateTipPopover', rel='popover', 
               title='Update panelFn', 
               'data-content'="<p>Here you can update the plot function to be applied to each subset - only works with storage method of 'localData'.  Please be careful here!", 
               tags$i(class='icon-info-sign')
            ), 
            div(id="editorWrapper", 
               class="shiny-panelFn-output", 
               div(
                  id="editor", 
                  class="shiny-panelFn-output"
               ), 
               div(
                  tags$textarea(id="panelFnInput"), 
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
      textInput("selectedCogTableVar", label="", value=""),
      style="display:none"
   ),
   div(id='varCogModal', class='modal hide fade',
      HTML("
      <div class='modal-header'>
      <a class='close' data-dismiss='modal' >&times;</a>
      <h3 style='display: inline'>Variables in Cognostics Table</h3>&nbsp;&nbsp;&nbsp;<a id='varCogTipPopover' rel='popover' title='Update Variable Visibility' data-content=\"<p>Here you can choose which variables should be visible when viewing the cognostics table.  To revert to settings prior to opening the pane, click the 'x' button in the upper-right corner of this pane.  Note that removing variables from cognostics will remove any filtering that has been done on those variables</p>\"><i class='icon-info-sign'></i></a>
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


# type should be "Cog" or "Plot"
makeVariableSelectTable <- function(desc, type) {
   c1 <- paste("<td>", desc$name, "</td>", sep="")
   c2 <- paste("<td>", desc$desc, "</td>", sep="")
   
   if(type=="Cog") {
      highlighted <- rep(" highlighted", nrow(desc))
      highlighted[desc$type=="panelKey"] <- ""
      c3 <- paste("<td class='selectableCogVar", highlighted, "' name='", desc$name, "'></td>", sep="")
   } else {
      highlighted <- rep("", nrow(desc))
      highlighted[desc$type=="splitVar"] <- " highlighted"
      c3 <- paste("<td class='selectablePlotVar", highlighted, "' name='", desc$name, "'></td>", sep="")
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
