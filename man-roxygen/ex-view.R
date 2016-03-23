library(ggplot2)

vdbConn(tempfile(), autoYes = TRUE)

# make a simple display
d <- divide(iris, by = "Species")
makeDisplay(d, name = "sl_vs_sw",
  panelFn = function(x)
    qplot(Sepal.Width, Sepal.Length, data = x))

\dontrun{
# open viewer with default providing list of displays to veiw
view()

# open viewer on port 8100
view(port = 8100)

# make port 8100 the default port for all future calls to view()
options(trelliscopePort = 8100)

# open viewer directly to "sl_vs_sw"
view(name = "sl_vs_sw")

# open viewer directly to "sl_vs_sw" and set intial state
view(state = stateSpec(
  name = "sl_vs_sw",
  layout = list(nrow = 1, ncol = 3),
  labels = c("panelKey", "Species")))
}
