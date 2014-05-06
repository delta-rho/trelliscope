# lim is an object of class "trsLims"
# gives a min/max for either "x" or "y" axis based on this object

## internal
trsCurLim <- function(lim, which, dat, curlim) {
   # dat <- plasma[[1]]
   # lim <- plasmaLims
   # which <- "y"
   # prop <- 0.07
   
   if(!is.null(curlim)) {
      curRange <- curlim
   } else {
      curRange <- lim$preFn(dat)[[paste(which, "lim", sep="")]]
   }
   
   lim <- lim[[which]]
   if(lim$type == "same") {
      lim$lim
   } else if(lim$type == "sliced") {
      delta <- (lim$range - diff(curRange)) / 2
      curRange + c(-1, 1) * delta
   } else {
      # need to add in axis padding if it was specified
      # this has already been done for same and sliced in setLims()
      curRange + diff(curRange) * lim$prop * c(-1, 1)
   }
}

## internal
trsCurXLim <- function(lim, dat, xlim) {
   trsCurLim(lim, "x", dat, xlim)
}

## internal
trsCurYLim <- function(lim, dat, ylim) {
   trsCurLim(lim, "y", dat, ylim)
}

