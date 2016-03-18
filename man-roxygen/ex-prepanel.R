d <- datadr::divide(iris, "Species")

irisPreFn <- function(x) {
  list(
    xlim = range(x$Sepal.Length),
    ylim = range(x$Sepal.Width)
  )
}

irisPre <- prepanel(d, prepanelFn = irisPreFn)

plot(irisPre)

irisLims <- setLims(irisPre, x = "same", y = "sliced")
