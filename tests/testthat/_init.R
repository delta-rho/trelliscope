library(datadr)
library(lattice)
library(base64enc)
library(knitr)
library(markdown)
library(shiny)
library(digest)
library(codetools)
library(hexbin)
library(ggplot2)
library(jsonlite)
library(png)

ff <- list.files("R", full.names = TRUE)

for(f in ff) {
   cat("loading", f, "\n")
   source(f)
}

Sys.setenv(TRELLISCOPE_DEV_APP_PREFIX = "~/Documents/Code/Tessera/hafen/trelliscope/inst/trelliscopeViewer/")
Sys.setenv(TRELLISCOPE_LOGGING = "true")

vdbConn("~/Documents/Code/housingData/housingjunk/vdb")

bySite <- datadr::divide(barley, by = "site")

pf <- function(x)
   dotplot(variety ~ yield, data = x)

cf <- function(x) {
   list(
      meanYield = cogMean(x$yield, desc = "mean yield"),
      range = cogRange(x$yield, desc = "yield range"),
      min = cog(min(x$yield), desc = "min yield"),
      max = cog(max(x$yield), desc = "max yield"),
      test = cog(x$yield[1], type = "factor", filterable = FALSE)
   )
}

makeDisplay(bySite,
   name = "variety_vs_yield",
   desc = "test display with barley data",
   panelFn = pf, cogFn = cf,
   width = 400, height = 400,
   lims = list(x = "same", y = "free")
)

webSync()

aa <- 100

pf2 <- function(x) {
   x$yield <- x$yield * aa
   dotplot(variety ~ yield, groups = year, data = x)
}

makeDisplay(bySite,
   name = "variety_vs_yield2",
   desc = "test display with barley data",
   panelFn = pf2, cogFn = cf,
   width = 400, height = 400,
   lims = list(x = "same", y = "free")
)

makeDisplay(bySite,
   name = "variety_vs_yield3",
   desc = "test display with barley data",
   panelFn = pf2, cogFn = cf,
   width = 600, height = 400,
   lims = list(x = "same", y = "free")
)



applyCogFn(cf, bySite[[1]], datadr::getAttribute(bySite, "conn"))

cogEx <- validateCogFn(bySite, cf)

getCogInfo(cogEx)


x <- getDisplay("variety_vs_yield")


# state = list(
#    layout = c(2, 4),
#    panelLabels = c("v1", "v2"),
#    startPage = ,
# )


