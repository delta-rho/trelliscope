\dontrun{
library(ggplot2)

vdbConn(tempfile(), autoYes = TRUE)

# divide housing data by county
byCounty <- divide(housingData::housing, by = c("county", "state"))

xlim <- as.Date(c("2008-01-31", "2016-01-31"))

# plot list price vs. time for each county
makeDisplay(byCounty, name = "county_time",
  panelFn = function(x)
    ggplot(x, aes(time, medListPriceSqft)) +
      geom_point() + xlim(xlim))

# divide housing data by state
byState <- divide(housingData::housing, by = "state")

# create a "displayHref" cognostic that links to the by county display
# filtered down to all counties in the current state
cogFn <- function(x) {
  state <- stateSpec(
    name = "county_time",
    sort = list(county = "asc"),
    layout = list(nrow = 2, ncol = 4),
    filter = list(state = list(select = getSplitVar(x, "state"))))

  list(countyPlots = cogDisplayHref(state = state, defLabel = TRUE))
}

# plot distribution of list price vs. time for each state
makeDisplay(byState, name = "state_time_CI",
  panelFn = function(x)
    ggplot(x, aes(time, medListPriceSqft)) +
      stat_summary(fun.data = "mean_cl_boot") + xlim(xlim),
  cogFn = cogFn)

# open up the state display
# try clicking on the link for "countyPlots"
# the by county display will be loaded filtered to the state
view("state_time_CI")
}

