library(shiny)
library(caTools)
library(hexbin)
source("cogTable.R")

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
         # htmlOutput("relatedDisplays"),
         # htmlOutput("selectedPlotVarOut"),
         # htmlOutput("testOutput"),
         # htmlOutput("plotOutput")
         div(id="plotOutput", class="shiny-plotMat-output"),
         tagList(
            singleton(tags$head(tags$script(src = "js/ace/ace.js")))
         )
      )
   )
)


# http://d1n0x3qji82z53.cloudfront.net/src-min-noconflict/ace.js