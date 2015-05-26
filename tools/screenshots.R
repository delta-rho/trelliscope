# devtools::install_github("wch/webshot")

library(webshot)

library(housingData)
library(datadr)
library(trelliscope)

byCounty <- divide(housing,
  by = c("county", "state"))

timePanel <- function(x)
   xyplot(medListPriceSqft + medSoldPriceSqft ~ time,
      data = x, auto.key = TRUE, ylab = "Price / Sq. Ft.")

priceCog <- function(x) {
   zillowString <- gsub(" ", "-", do.call(paste, getSplitVars(x)))
   list(
      slope = cog(coef(lm(medListPriceSqft ~ time, data = x))[2],
         desc = "list price slope"),
      meanList = cogMean(x$medListPriceSqft),
      meanSold = cogMean(x$medSoldPriceSqft),
      nObs = cog(length(which(!is.na(x$medListPriceSqft))),
         desc = "number of non-NA list prices"),
      zillowHref = cogHref(
         sprintf("http://www.zillow.com/homes/%s_rb/", zillowString),
         desc = "zillow link")
   )
}

conn <- vdbConn("vdb", name = "tesseraTutorial", autoYes = TRUE)
makeDisplay(byCounty,
   name = "list_sold_vs_time_quickstart",
   desc = "List and sold price over time",
   panelFn = timePanel,
   cogFn = priceCog,
   width = 400, height = 400,
   lims = list(x = "same"))

view(port = 8100)


##
##---------------------------------------------------------

library(webshot)

# to be called at beginning - set zoom to 2 for retina-like shots
setZoom <- "casper.zoom(2);
casper.then(function() {
  this.wait(200);
});"

# screenshot of initial view
webshot("http://127.0.0.1:8100", "tools/screenshots/initial-view.png",
  vwidth = 992 * 2, vheight = 744 * 2,
  cliprect = c(0, 0, 992 * 2 + 535, 744 * 2),
  eval = setZoom)

# screenshot of after list_sold_vs_time_quickstart display is selected
selectDisplay <- "casper.thenEvaluate(function() {
  $('*[data-name=\"list_sold_vs_time_quickstart\"]').click();
});
casper.then(function() {
  this.wait(2000);
});"

webshot("http://127.0.0.1:8100", "tools/screenshots/display-main.png",
  vwidth = 992 * 2, vheight = 744 * 2,
  cliprect = c(0, 0, 992 * 2 + 535, 744 * 2),
  eval = paste(setZoom, selectDisplay, sep = "\n"))

# each control panel
panels <- c("display-info-nav-link", "panel-layout-nav-link", "panel-labels-nav-link", "add-related-display-nav-link", "cog-table-sort-filter-nav-link", "univar-filter-nav-link", "bivar-filter-nav-link", "active-cog-nav-link")
files <- paste0("tools/screenshots/", gsub("\\-nav\\-link", ".png", panels))

for(ii in seq_along(panels)) {
  panelEval <- paste0("casper.thenEvaluate(function() {
    $('#", panels[ii], "').click();
  });
  casper.then(function() {
    this.wait(200);
  });")

  webshot("http://127.0.0.1:8100", files[ii],
    vwidth = 992 * 2, vheight = 744 * 2,
    cliprect = c(0, 0, 992 * 2 + 535, 744 * 2),
    eval = paste(setZoom, selectDisplay, panelEval, sep = "\n"))
}

