



# latitude and longitude "square" boundaries
latCuts <- seq(38.2, 39.4, by=0.1)
lonCuts <- seq(-121.8, -120.8, by=0.1)

# divide the data by lat / lon
byLatLon <- divide(airplane, by=c("latCut", "lonCut"),
   preTransFn = function(x) {
      x$latCut <- cut(x$latitude, latCuts)
      x$lonCut <- cut(x$longitude, lonCuts)
      x
   },
   update=TRUE)



# look at the resulting object
byLatLon



# see what a subset key-value pair looks like
str(byLatLon[[1]])



# a simple panel function for a trelliscope display
coPanelFn <- function(d) {
   xyplot(log2(co) ~ dat_ams, data=d, 
      type=c("p", "g"),
      panel=function(x, y, ...) {
         panel.xyplot(x, y, ...)
         try(panel.lmline(x, y, lty=2), silent=TRUE)
      })
}



# apply the panel function to a subset
kvApply(coPanelFn, byLatLon[[2]])



# create a simple display
makeDisplay(byLatLon,
   panelFn = coPanelFn,
   name    = "co_vs_time_bb",
   group   = "co",
   desc    = "Bare bones display of co vs. time for each geographic 'square' with fitted linear model"
)



# setting axis limits in the call to makeDisplay()
makeDisplay(byLatLon,
   name    = "co_vs_time_same",
   group   = "co",
   desc    = "Plot of co vs. time for each geographic 'square' with with fitted line, illustrating the use of 'same' axis limits for x and y",
   panelFn = coPanelFn,
   lims    = list(x="same", y="same")
)



# same display but using ggplot2
ggCoPanelFn <- function(x) {
   qplot(dat_ams, log2(co), data=x)
}

makeDisplay(byLatLon,
   name    = "co_vs_time_gg",
   group   = "co",
   desc    = "Plot of co vs. time for each geographic 'square' with with fitted line, illustrating the use of 'same' axis limits for x and y, and using ggplot2",   
   panelFn = ggCoPanelFn,
   lims    = list(x="same", y="same")
)



# using a prepanel function to compute axis limits
preFn <- function(x) {
   list(ylim=range(log2(x$co)), xlim=range(x$dat_ams))
}

makeDisplay(byLatLon,
   name    = "co_vs_time_pre",
   group   = "co",
   desc    = "Plot of co vs. time for each geographic 'square' with with fitted line, illustrating the use of 'same' axis limits for x and y using a prepanel function",
   panelFn = coPanelFn,
   lims    = list(x="same", y="same" , prepanelFn=preFn)
)



# compute axis limits prior to creating display using prepanel()
coTimePre <- prepanel(byLatLon, prepanelFn=coPanelFn)



# visualize the axis limit computations
plot(coTimePre)



# set limits from our prepanel calculations
coTimeLims <- setLims(coTimePre) # default is x="same", y="same"



# create a display using axis limits from prepanel() and setLims()
makeDisplay(byLatLon,
   name    = "co_vs_time_lims",
   group   = "co",
   desc    = "Plot of co vs. time for each geographic 'square' with with fitted line, illustrating the use of 'same' axis limits for x and y by pre-specifying the axis limits using prepanel() and setLims()",
   panelFn = coPanelFn,
   lims    = coTimeLims
)



# create a cognostics function to be applied to each subset
coCogFn <- function(x) {
   slp <- coef(lm(co ~ as.integer(dat_ams), data=x))[2]
   if(is.na(slp))
      slp <- 0
   list(
      nobs = cog(length(which(!is.na(x$co))), desc="number of observations"),
      slope = cog(slp, desc="slope of fitted line"),
      meanLat = cogMean(x$latitude, desc="mean latitude"),
      meanLon = cogMean(x$longitude, desc="mean longitude"),
      timeRange = cogRange(as.integer(x$dat_ams[!is.na(x$co)]) / 60^2, desc="Time range (hours)")
   )
}



# test the cognostics function on a subset
kvApply(coCogFn, byLatLon[[1]])



# add cognostics to the display
makeDisplay(byLatLon,
   name    = "co_vs_time",
   group   = "co",
   desc    = "Plot of co vs. time for each geographic 'square' with with fitted line, illustrating the use of 'same' axis limits and a cognostics function",
   panelFn = coPanelFn,
   cogFn   = coCogFn,
   lims    = coTimeLims
)



# make display with panels pre-rendered
makeDisplay(byLatLon,
   name      = "co_vs_time_rend",
   group     = "co",
   desc      = "Plot of co vs. time for each geographic 'square' with with fitted line, illustrating the use of 'same' axis limits and a cognostics function, and pre-rendered panels",
   panelFn   = coPanelFn,
   cogFn     = coCogFn,
   lims      = coTimeLims,
   preRender = TRUE
)



# initiate a MongoDB cognostics connection
mongoConn <- mongoCogConn()   



# create the display using MongoDB to store the cognostics
makeDisplay(byLatLon,
   name      = "co_vs_time_mon",
   group     = "co",
   desc      = "Plot of co vs. time for each geographic 'square' with with fitted line, illustrating the use of 'same' axis limits and a cognostics function, and storing cognostics in MongoDB",
   panelFn   = coPanelFn,
   cogFn     = coCogFn,
   cogConn   = mongoConn,
   lims      = coTimeLims
)



# panel function for a geographical display
latLonPanelFn <- function(a) {
   yy <- as.numeric(strsplit(gsub("\\(|\\]", "", attr(a, "split")$latCut), ",")[[1]])
   xx <- as.numeric(strsplit(gsub("\\(|\\]", "", attr(a, "split")$lonCut), ",")[[1]])
   
   xyplot(airplane$latitude ~ airplane$longitude,
      groups=airplane$dat_ams < as.POSIXct("2010-06-28 21:00:00 UTC"),
      panel=function(x, y, ...) {
         panel.rect(xx[1], yy[1], xx[2], yy[2], col="darkgray")
         panel.abline(v=lonCuts, col="gray")
         panel.abline(h=latCuts, col="gray")
         panel.xyplot(x, y, ...)
      }
   )
}

# test the panel function on a subset
kvApply(latLonPanelFn, byLatLon[[2]])

# create the display
makeDisplay(
   data = byLatLon,
   name = "lat_vs_lon",
   group = "co",
   desc = "airplane tracks",
   panelFn = latLonPanelFn
)



# create data for scatterplot display
airplaneSplodDat <- makeSplodData(airplane, 
   id.vars=c("dat_ams", "flight", "datCut"))



# look at a subset of airplaneSplotDat
str(kvExample(airplaneSplodDat))



splod(airplaneSplodDat)


