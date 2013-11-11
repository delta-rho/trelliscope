library(shiny)
library(hexbin)
# library(trelliscope)

sapply(list.files("_uiHelpers", full.names=TRUE), source)

shinyUI(
   viewerPage(
      tagList(
         variableCogSelectModal(),
         variablePlotSelectModal(),
         relatedDisplaySelectModal(),
         cogModal(),
         displayListModal(),
         d3histModal(),
         d3bivarModal()
      ),
      viewerHeaderPanel(),
      div(class="span12",
         tags$input(id="appHash", type="text", style="display:none;"),
         hiddenDimInputs(),
         # htmlOutput('displayNameHeader'),
         # div(htmlOutput("panelAspect2"), style="display:none;"),
         div(htmlOutput("panelAspect2")),
         # htmlOutput("relatedDisplays"),
         # htmlOutput("selectedPlotVarOut"),
         # div(id="d3histData", class="shiny-html-output"),
         # htmlOutput("d3histData"),
         # div(id="d3histData", class="shiny-d3histDat-output", style="display:none"),
         div(id="panelLayoutPlots", class="shiny-plotgroup-output", style="display:none"),
         div(id="panelLayoutCogInfo", class="shiny-htmlgroup-output", style="display:none"),
         div(id="panelLayout", class="shiny-panelLayout-output"),
         tagList(
            singleton(tags$head(tags$script(src = "js/ace/ace.js")))
         )
      )
   )
)


# http://d1n0x3qji82z53.cloudfront.net/src-min-noconflict/ace.js

