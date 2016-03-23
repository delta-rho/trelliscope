\dontrun{

library(ggplot2)

vdbConn(tempfile(), autoYes = TRUE)

# make a simple display
d <- divide(iris, by = "Species")
makeDisplay(d, name = "sl_vs_sw",
  panelFn = function(x)
    qplot(Sepal.Width, Sepal.Length, data = x))

# add additional displays...

# assuming an account has already been configured with shinyapps.io
# or RStudio Connect
deployVDB(appName = "deployVDB-example")
}
