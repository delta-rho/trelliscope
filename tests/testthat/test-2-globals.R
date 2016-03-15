library(lattice)

context("Global variables")

vdbPath <- file.path(tempdir(), "vdb")
vdbConn(vdbPath, autoYes = TRUE)

test_that("find globals", {
  # divide the ddf by the variable "site"
  bySite <- datadr::divide(barley, by = "site")

  a <- 0

  # simple dotplot panel function
  pf <- function(x) {
    x$yield <- x$yield + a
    dotplot(variety ~ yield, data = x)
  }

  b <- 0

  # simple cognostics function
  cf <- function(x) {
    list(
      meanYield = cogMean(x$yield + b, desc = "mean yield"),
      range = cogRange(x$yield, desc = "yield range"),
      min = cog(min(x$yield), desc = "min yield"),
      max = cog(max(x$yield), desc = "min yield")
    )
  }

  makeDisplay(bySite,
    name = "variety_vs_yield_gv",
    desc = "test display with barley data",
    panelFn = pf, cogFn = cf,
    width = 400, height = 400,
    lims = list(x = "same", y = "free")
  )

  disp <- getDisplay("variety_vs_yield_gv")
  expect_identical(disp$relatedData[c("a", "b")], list(a = 0, b = 0))

  makeDisplay(bySite,
    name = "variety_vs_yield_gv2",
    desc = "test display with barley data",
    panelFn = pf, cogFn = cf,
    width = 400, height = 400,
    lims = list(x = "same", y = "free"),
    params = list(a = a, b = b, gv = 24)
  )

  disp <- getDisplay("variety_vs_yield_gv2")
  expect_identical(disp$relatedData[c("a", "b", "gv")], list(a = 0, b = 0, gv = 24))
})
