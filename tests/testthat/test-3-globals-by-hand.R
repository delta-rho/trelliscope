# not all test environments have Hadoop installed

context("Simple globals by hand works for displays")

vdbPath <- file.path(tempdir(), "vdb")

# divide the ddf by the variable "site"
bySite <- divide(barley, by = "site")

pf2 = function(x)  #<-- nesting breaks global dection routine in Trelliscope
	pf(x)
# simple dotplot panel function
pf <- function(x)
  dotplot(variety ~ yield, data = x, pch=globalPCH)   

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

test_that("global saving for displays works", {
  	vdbConn(vdbPath, autoYes = TRUE)
	garbage = "just noise for save file"
	#---------------------------------------------------------------------------------
	# globalPCH will enter the display through saving to globalsFile
	#---------------------------------------------------------------------------------
	globalPCH = 3  
	save(globalPCH,garbage, file=globalsFile())
	#---------------------------------------------------------------------------------
	# On the other hand pf will enter the display through saving to an R source file
	#---------------------------------------------------------------------------------
	tmpdir = tempfile()
	dir.create(tmpdir)
	str = as.character(bquote({pf = .(pf)}))[[2]] 
	cat(str, "\n", file=file.path(tmpdir,"pf.R"))
	copyRSource(tmpdir)
	unlink(tmpdir)
	#---------------------------------------------------------------------------------
	# Make display
	#---------------------------------------------------------------------------------
	

	 makeDisplay(bySite,
	    name = "variety_vs_yield",
	    desc = "test display with barley data",
	    panelFn = pf2, cogFn = cf,
	    width = 400, height = 400,
	    lims = list(x = "same", y = "free", detect.globals=FALSE)
  	)
	rm(globalPCH)
	rm(pf)
	rm(pf2)
	# view()
  expect_true(file.exists(file.path(vdbPath, "displays", "common", "variety_vs_yield", "displayObj.Rdata")))
  expect_true(file.exists(globalsFile()))
  expect_true(file.exists(file.path(vdbPath, "data", "R", "pf.R")))
  expect_true(file.exists(file.path(vdbPath, "global.R")))
 })

 