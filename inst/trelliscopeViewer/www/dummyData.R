
library(jsonlite)

load("vdb/displays/_displayList.Rdata")
load("vdb/displays/common/variety_vs_yield/displayObj.Rdata")

source("server/_fns.R")
source("server/currentDisplayData.R")

currentDisplay <- list(cdo = displayObj)

# cogState <- list(
#    filter =
#    sort =
# )

cogState <- list(
   list(name = "var1", action = "sort", type = "numeric", dir = "asc"),
   list(name = "var2", action = "sort", type = "numeric", dir = "desc"),
   list(name = "var3", action = "sort", type = "categ", dir = "desc"),
   list(name = "var4", action = "sort", type = "categ", dir = "asc"),
   list(name = "var1", action = "filter"),
   list(name = "var3", action = "filter"),
   list(name = "var3", action = "filter"),
   list(name = c("var1, var2, var3"), action = "multivar"),
   list(name = "var2", type = "sample")
)

cogDFtable <- iris[1:10,]


names(cogDFtable)


btnLookup <- list(sort = "btn-primary", filter = "btn-success", multivar = "btn-warning", sample = "btn-danger")
iconLookup <- list(sort = list(numeric = list(asc = "icon-sort-numeric-asc", desc = "icon-sort-numeric-desc"), categ = list(asc = "icon-sort-alpha-asc", desc = "icon-sort-alpha-desc")), filter = "icon-filter", multivar = "icon-multivar", sample = "icon-sample")

state2template <- function(x) {
   if(x$action == "sort") {
      icon <- iconLookup[[x$action]][[x$info$type]][[x$info$dir]]
   } else {
      icon <- iconLookup[[x$action]]
   }
   list(
      name = paste(x$vars, collapse = ","),
      class = btnLookup[[x$action]],
      icon = icon,
      state = toJSON(x[c("vars", "action")])
   )
}

iris2 <- cbind(iris, iris)
# iris2 <- iris
iris2[,2] <- as.character(iris2[,2])
names(iris2)[c(1, 5)] <- paste(names(iris2)[c(1, 5)], ".long.long", sep = "")
# iris2 <- iris2[,1:2]

dummyPanel <- "<div style=\"width:404px; height:242px; background-color: #ddd\"></div>"

cogShow <- data.frame(cog_name = c("cog1", "cog2", "cog3"), cog_value = c("val1", "val2", "val3"))

panelTableContent <- lapply(list(1:3, 4:6), function(a) {
   lapply(a, function(i) {
      list(
         i = i,
         panel_content = dummyPanel,
         cogs = cogShow
      )
   })
})

a <- list(
   panels = list(
      "panelLayoutOutput" = panelLayoutOutputData(currentDisplay),
      "panelFunctionOutput" = panelFunctionOutputData(currentDisplay),
      "panelLabelListOutput" = panelLabelListOutputData(currentDisplay),
      "relatedDisplayListOutput" = relatedDisplayListOutputData(currentDisplay),
      "cogTableControlsOutput" = cogTableControlsOutputData(currentDisplay),
      "cogUniFilterControlsOutput" = cogUniFilterControlsOutputData(currentDisplay),
      "cogBiFilterControlsOutput" = cogBiFilterControlsOutputData(currentDisplay),
      "cogMultiFilterControlsOutput" = cogBiFilterControlsOutputData(currentDisplay),
      "cogSampleControlsOutput" = list()
   ),
   "panelPageNavOutput" = panelPageNavOutputData(currentDisplay),
   "cogBreadcrumbOutput" = list(buttons = lapply(cogState, state2template)),
   "displayListOutput" = displayListOutputData(displayList),
   "panelTableContentOutput" = panelTableContent
)

cat(jsonlite::toJSON(a, pretty = TRUE), file = "www/dummyData.json")














a <- list(
   "panel-layout" = list(
      icon = "icon-th",
      panel_header = "Panel Layout",
      panel_description = "Specify the arrangement of the panels on each page by entering the number of rows and columns of panels per page.  Future functionality for fixing / varying levels of cognostics per page to come.",
      body_id = "panelLayoutOutput",
      apply_button = "panelLayoutApplyButton"
   ),
   "panel-function" = list(
      icon = "icon-panel-edit",
      panel_header = "Panel Function Editor",
      panel_description = "Edit the code that generates each panel.",
      body_id = "panelFunctionOutput",
      apply_button = "panelFunctionApplyButton",
      footer = "Note: this feature is currently disabled - panels will not update when you click 'Apply'..."
   ),
   "panel-labels" = list(
      icon = "icon-cog-list",
      panel_header = "Panel Labels",
      panel_description = "Specify which cognostics to view underneath each panel. <br/>Select cognostics to view by clicking and/or dragging the 'Show' column.",
      body_id = "panelLabelListOutput",
      apply_button = "panelLabelListApplyButton"
   ),
   "add-related-display" = list(
      icon = "icon-plus-square-o",
      panel_header = "Related Displays",
      panel_description = "Select additional displays (if any) that have been created on the same partitioning to view with the currently selected display.  If additional displays are selected, only one subset per page will be shown.",
      body_id = "relatedDisplayListOutput",
      apply_button = "relatedDisplayListApplyButton"
   ),
   "active-cog" = list(
      icon = "icon-active-cog",
      panel_header = "Active Cognostics",
      panel_description = "Specify which cognostics are available for sorting, filtering, and sampling.",
      body_id = "activeCogListOutput",
      apply_button = "activeCogListApplyButton"
   ),
   "cog-table-sort-filter" = list(
      icon = "icon-sort-filter",
      panel_header = "Cognostics View / Sort / Filter",
      panel_description = "View cognostics in a table and specify sort order or filtering of panels.  <br/>Shift-click on the panel header sorting buttons for multi-column sorting.",
      body_id = "cogTableControlsOutput",
      apply_button = "cogTableControlsApplyButton"
   ),
   "univar-filter" = list(
      icon = "icon-bars",
      panel_header = "Visual Univariate Filter",
      panel_description = "Filter panels based on a single cognostic guided by a visual display of the distribution of the cognostic.  <br/>Select a variable to view and click and drag on the display to specify filtering.",
      body_id = "cogUniFilterControlsOutput",
      apply_button = "cogUniFilterControlsApplyButton"
   ),
   "bivar-filter" = list(
      icon = "icon-stats",
      panel_header = "Visual Bivariate Filter",
      panel_description = "Filter panels based on pairs of cognostics through an interactive scatterplot.  <br/>Currently only quantitative variables are supported.",
      body_id = "cogBiFilterControlsOutput",
      apply_button = "cogBiFilterControlsApplyButton"
   ),
   "multivar-filter" = list(
      icon = "icon-multivar",
      panel_header = "Visual Multivariate Filter",
      panel_description = "Filter panels based on a two-dimensional projection pursuit of multiple quantitative cognostics.  <br/>This can help uncover interesting regions of the cognostics space.",
      body_id = "cogMultiFilterControlsOutput",
      apply_button = "cogMultiFilterControlsApplyButton",
      footer = "Note: this interactive filter is not yet bound to the back-end - panels will not update when you click 'Apply'..."
   ),
   "sample-panels" = list(
      icon = "icon-sample",
      panel_header = "Sample Panels",
      panel_description = "Specify how to sample from panels in their current sorted and filtered state (coming soon).",
      body_id = "cogSampleControlsOutput",
      updateButton = "cogSampleControlsApplyButton"
   )
)
cat(jsonlite::toJSON(a, pretty = TRUE), file = "www/templateData.json")
