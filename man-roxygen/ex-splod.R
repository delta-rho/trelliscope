\dontrun{
library(lattice)

vdbConn(tempfile(), autoYes = TRUE)

# make "splod" directly from a data frame
splod(batting, name = "batting",
  id.vars = c("playerID", "yearID", "stint", "teamID", "lgID"))

# first transform the data into a "splodDat" object
batSplodDat <- makeSplodData(batting,
  id.vars = c("playerID", "yearID", "stint", "teamID", "lgID"))
# now make "splod"
splod(batSplodDat, name = "batting2", data = batSplodDat)

# custom panel function (color by league)
mySplodFn <- function(d) {
  xyplot(jitter(y) ~ jitter(x), groups = lgID, data = d,
    xlab = getSplitVar(d, "xVar"),
    ylab = getSplitVar(d, "yVar"),
    auto.key = TRUE
  )
}

splod(batSplodDat, name = "batting3",
  data = batSplodDat, panelFn = mySplodFn)

view()
}
