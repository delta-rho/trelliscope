d <- divide(iris, by = "Species")

# cognostics function that links to wikipedia
cogFn <- function(x) {
  link <- paste0("https://en.wikipedia.org/wiki/Iris_", getSplitVar(x, "Species"))
  list(wiki = cogHref(link, desc = "Look up species on wikipedia", defLabel = TRUE))
}

# test the cognostics function on a subset
applyCogFn(cogFn, d[[1]])

# make a display with this cognostics function
vdbConn(tempfile(), autoYes = TRUE)
makeDisplay(d, name = "iris_sl_sw",
  cogFn = cogFn, panelFn = function(x)
    lattice::xyplot(Sepal.Length ~ Sepal.Width, data = x))

\dontrun{
# clicking the link under each panel will open wikipedia
view(name = "iris_sl_sw")
}
