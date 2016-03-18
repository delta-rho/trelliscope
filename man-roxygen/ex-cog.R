d <- stack(data.frame(EuStockMarkets))
d$time <- rep(as.numeric(time(EuStockMarkets)), 4)
d$year <- floor(d$time)

byIndexYear <- divide(d, by = c("ind", "year"))

cogFn <- function(x)
  list(lormse = cogLoessRMSE(values ~ time, data = x, span = 0.3, degree = 2),
       slope  = cogSlope(values ~ time, data = x),
       range  = cogRange(x$values),
       mean   = cogMean(x$values),
       max    = cog(max(x$values, na.rm = TRUE), desc = "max value"))

applyCogFn(cogFn, byIndexYear[[1]])

library(lattice)
panelFn <- function(x)
  xyplot(values ~ time, data = x,
    panel = function(x, y, ...) {
      panel.xyplot(x, y, ...)
      panel.loess(x, y, span = 0.3,
        degree = 2, evaluation = 200, col = "black")
    })

vdbConn(tempfile(), autoYes = TRUE)
makeDisplay(byIndexYear, name = "ts_index_year",
  cogFn = cogFn, panelFn = panelFn)

\dontrun{
# sort and fiter the index/year panels by slope and loess RMSE
view(name = "ts_index_year")
}
