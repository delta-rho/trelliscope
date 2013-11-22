

# require(lattice)
# require(ggplot2)
# require(digest)
# require(knitr)
# require(markdown)
# require(base64enc)
# require(scagnostics)
# require(data.table)
# ff <- list.files("~/Documents/Code/datadr/R", full.names=TRUE)
# for(f in ff) {
#    cat(f, "\n"); source(f)
# }
# ff <- list.files("~/Documents/Code/trelliscope/R", full.names=TRUE)
# for(f in ff) {
#    cat(f, "\n"); source(f)
# }
# load("~/Documents/Code/trelliscope/inst/data/elnino.rda")
# load("~/Documents/Code/trelliscope/inst/data/airplane.rda")



library(trelliscope)





# initialize a connection to a new VDB to be located at /private/tmp/vdbtest
vdbDir <- "/private/tmp/vdbtest"
conn <- vdbConn(vdbDir, autoYes=TRUE, name="myProject")



data(airplane)
head(airplane)



# look at co vs. time
Sys.setenv(TZ="UTC")
xyplot(co ~ dat_ams, data=airplane, aspect=0.2)



# distinguish "am" and "pm" flights
airplane$flight <- ifelse(airplane$dat_ams < as.POSIXct("2010-06-28 21:00:00 UTC"), "am", "pm")



# look at altitude vs. time
xyplot(altitude ~ dat_ams, data=airplane, aspect=0.2)



# create a discrete version of altitude
airplane$altCut <- cut(airplane$altitude, seq(0, 3000, by=500))



# take a look at airplane tracks
xyplot(latitude ~ longitude, data=airplane,
   groups=flight,
   aspect="iso",
   alpha=0.5,
   auto.key=list(space="right")
)



# look at tracks with altitude and flight
library(RColorBrewer)
xyplot(latitude ~ longitude | flight, 
   data=airplane,
   panel=function(x, y, ...) {
      panel.fill("#E0E0E0")
      panel.grid(h=-1, v=-1, col="lightgray")
      panel.xyplot(x, y, ...)
   },
   groups=altCut,
   between=list(x=0.5),
   aspect="iso",
   par.settings=list(superpose.symbol=list(col=brewer.pal(6, "Accent"))),
   auto.key=list(space="right")
)



# break time into 15 minute intervals
airplane$datCut <- sprintf("%s:%02d",
   format(airplane$dat_ams, "%m-%d/%H"), 
   floor(as.integer(format(airplane$dat_ams, "%M")) / 15) * 15
)



# plot of airplane tracks by 15 minute intervals
arrowPanel <- function(x, y, ...) {
   n <- length(x)
   panel.grid(h=-1, v=-1)
   panel.arrows(x[1:(n-1)], y[1:(n-1)], x[2:n], y[2:n], length=0.05, alpha=0.3, col="blue")
}

xyplot(latitude ~ longitude | datCut,
   data=airplane,
   panel=arrowPanel,
   as.table=TRUE,
   between=list(x=0.25, y=0.25),
   layout=c(5, 2),
   aspect="iso",
   subset=flight=="pm"
)



# example of both x and y axes "free"
xyplot(latitude ~ longitude | datCut,
   data=airplane,
   panel=arrowPanel,
   as.table=TRUE,
   between=list(x=0.25, y=0.25),
   layout=c(5, 2),
   scales=list(x=list(relation="free"), y=list(relation="free")),
   subset=flight=="pm"
)



# example of both x and y axes "sliced"
xyplot(latitude ~ longitude | datCut,
   data=airplane,
   panel=arrowPanel,
   as.table=TRUE,
   between=list(x=0.25, y=0.25),
   layout=c(5, 2),
   scales=list(x=list(relation="sliced"), y=list(relation="sliced")),
   subset=flight=="pm"
)



# prepanel example
xyplot(latitude ~ longitude | datCut,
   data=airplane,
   panel=arrowPanel,
   prepanel=function(x, y) {
      list(xlim=range(x) + c(-1, 1), ylim=range(y) + c(-1, 1))
   },
   as.table=TRUE,
   between=list(x=0.25, y=0.25),
   layout=c(5, 2),
   scales=list(x=list(relation="free"), y=list(relation="free")),
   subset=flight=="pm"
)



# re-connect to our VDB
conn <- vdbConn("/private/tmp/vdbtest")



# add a lattice plot to our VDB
p <- xyplot(latitude ~ longitude | flight, 
   data=airplane,
   panel=function(x, y, ...) {
      panel.fill("#E0E0E0")
      panel.grid(h=-1, v=-1, col="lightgray")
      panel.xyplot(x, y, ...)
   },
   groups=altCut,
   between=list(x=0.5),
   aspect="iso",
   par.settings=list(superpose.symbol=list(col=brewer.pal(6, "Accent"))),
   auto.key=list(space="right"))

addDisplay(p, 
   name  = "tracks_byflight", 
   group = "exploratory",
   desc  = "The tracks of the airplane with a panel for each of the morning and evening flights.  Color by altitude of plane.",
   dim   = list(width=800, height=450, res=150))



# list the files in the "displays" directory of the VDB
list.files(file.path(vdbDir, "displays"))



# list the files in the "exploratory" group
list.files(file.path(vdbDir, "displays", "exploratory"))



# look at files in the "tracks_byflight" directory
list.files(file.path(vdbDir, "displays", "exploratory", "tracks_byflight"))



# list all displays in the VDB
listDisplays()



# view the tracks_by_flight display
view("tracks_byflight")


