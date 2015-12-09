# not all test environments have Hadoop installed

context("Simple globals by hand works for displays")

vdbPath <- file.path(tempdir(), "vdb")

# divide the ddf by the variable "site"
bySite <- datadr::divide(barley, by = "site")

# simple dotplot panel function
pf <- function(x)
  dotplot(variety ~ yield, data = x, pch = globalPCH)

# nesting breaks global dection routine in Trelliscope
pf2 <- function(x)
  pf(x)

# simple cognostics function
cf <- function(x) {
  list(
    meanYield = cogMean(x$yield, desc = "mean yield"),
    range = cogRange(x$yield, desc = "yield range"),
    # min = min(x$yield),
    min = cog(min(x$yield), desc = "min yield"),
    max = cog(max(x$yield), desc = "min yield"),
    misc = sample(c("a", "b"), 1)
  )
}

globalPCH <- 3

test_that("global saving for displays works", {
  vdbConn(vdbPath, autoYes = TRUE)

  garbage <- "just noise for save file"

  ## globalPCH will enter the display through saving to vdbGlobalsFile
  ##---------------------------------------------------------

  save(globalPCH, garbage, file = vdbGlobalsFile())

  list.files(file.path(vdbPath, "data"))

  ## on the other hand pf will enter the display through saving to an R source file
  ##---------------------------------------------------------

  tmpdir <- tempfile()
  dir.create(tmpdir)
  str  <- as.character(bquote({pf = .(pf)}))[[2]]
  cat(str, "\n", file = file.path(tmpdir, "pf.R"))
  vdbCopyRSource(tmpdir)
  unlink(tmpdir)

  list.files(file.path(vdbPath, "data"))

  ## make display
  ##---------------------------------------------------------

  makeDisplay(bySite,
    name = "variety_vs_yield",
    desc = "test display with barley data",
    panelFn = pf2, cogFn = cf,
    width = 400, height = 400,
    lims = list(x = "same", y = "free"),
    detectGlobals = FALSE
  )

  rm(globalPCH)
  rm(pf)
  rm(pf2)

  # view()
  expect_true(file.exists(file.path(vdbPath, "displays", "common", "variety_vs_yield", "displayObj.Rdata")))
  expect_true(file.exists(vdbGlobalsFile()))
  expect_true(file.exists(file.path(vdbPath, "data", "R", "pf.R")))
  expect_true(file.exists(file.path(vdbPath, "global.R")))
})
