# not all test environments have Hadoop installed
TEST_HDFS <- Sys.getenv("DATADR_TEST_HDFS")
if(TEST_HDFS == "")
  TEST_HDFS <- FALSE

context("Simple display")

vdbPath <- file.path(tempdir(), "vdb")

# divide the ddf by the variable "site"
bySite <- datadr::divide(barley, by = "site")

# simple dotplot panel function
pf <- function(x)
  dotplot(variety ~ yield, data = x)

# simple cognostics function
cf <- function(x) {
  list(
    meanYield = cogMean(x$yield, desc = "mean yield"),
    range = cogRange(x$yield, desc = "yield range"),
    # min = min(x$yield),
    min = cog(min(x$yield), desc = "min yield"),
    max = cog(max(x$yield), desc = "min yield")
  )
}

test_that("vdbConn works", {
  vdbConn(vdbPath, autoYes = TRUE)
})

test_that("makeDisplay works", {
  makeDisplay(bySite,
    name = "variety_vs_yield",
    desc = "test display with barley data",
    panelFn = pf, cogFn = cf,
    width = 400, height = 400,
    lims = list(x = "same", y = "free"),
    state = list(layout = list(nrow = 2, ncol = 3))
  )

  expect_true(file.exists(file.path(vdbPath, "displays", "common", "variety_vs_yield", "displayObj.Rdata")))
})

if(TEST_HDFS) {

library(Rhipe)
rhinit()

test_that("makeDisplay works with RHIPE", {
  try(rhdel("/tmp/test_bySite"), silent = TRUE)

  bySiteRh <- datadr::convert(bySite, hdfsConn("/tmp/test_bySite", autoYes = TRUE))
  bySiteRh <- makeExtractable(bySiteRh)
  makeDisplay(bySiteRh,
    name = "variety_vs_yield_rh",
    desc = "test display with barley data",
    panelFn = pf, cogFn = cf,
    width = 400, height = 400,
    lims = list(x = "same", y = "free")
  )

  expect_true(file.exists(file.path(vdbPath, "displays", "common", "variety_vs_yield_rh", "displayObj.Rdata")))
})


}

