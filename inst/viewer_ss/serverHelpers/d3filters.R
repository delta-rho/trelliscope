
getD3HistData <- function(dat, xlab) {
   # TODO: add logic about number of breaks
   hst <- hist(dat, plot=FALSE)
   HTML(paste("{\"xlab\": \"", xlab, "\", \"data\": [", paste(apply(matrix(data=c(
      hst$breaks,
      c(hst$counts, 0)
   ), ncol=2), 1, function(x) paste("{\"xdat\":", x[1], ",\"ydat\":", x[2], "}", sep="")), collapse=","), "]}", sep=""))
}

renderTimeSeries <- function(expr, env=parent.frame(), quoted=FALSE) {
  # Convert the expression + environment into a function
  func <- exprToFunction(expr, env, quoted)
  
  function() {
    val <- func()
    list(start = tsp(val)[1],
         end = tsp(val)[2],
         freq = tsp(val)[3],
         data = as.vector(val))
  }
}

makeBivarJSON <- function(x, y, xlab="x", ylab="y", shape=460/660, xbin=50) {
   dat <- hexbin(x, y, shape=shape, xbin=xbin)
   
   # make a scatterplot of less than 2500 points
   type <- ifelse(length(x) < 2500, "scatter", "hex")

   if(type=="hex") {
      style <- "lattice"
      minarea <- 0.05
      maxarea <- 0.8
      mincnt <- 1
      maxcnt <- max(dat@count)
      style <- "lattice"
      trans <- NULL

      cnt <- dat@count
      xbins <- dat@xbins
      shape <- dat@shape
      tmp <- hcell2xy(dat)
      good <- mincnt <= cnt & cnt <= maxcnt

      xnew <- tmp$x[good]
      ynew <- tmp$y[good]
      cnt <- cnt[good]

      sx <- xbins/diff(dat@xbnds)
      sy <- (xbins * shape)/diff(dat@ybnds)

      if (is.null(trans)) {
         if (min(cnt, na.rm = TRUE) < 0) {
            pcnt <- cnt + min(cnt)
            rcnt <- {
               if (maxcnt == mincnt) rep.int(1, length(cnt)) else (pcnt - mincnt)/(maxcnt - mincnt)
            }
         } else rcnt <- {
            if (maxcnt == mincnt) rep.int(1, length(cnt)) else (cnt - mincnt)/(maxcnt - mincnt)
         }
      } else {
         rcnt <- (trans(cnt) - trans(mincnt))/(trans(maxcnt) - trans(mincnt))
         if (any(is.na(rcnt))) stop("bad count transformation")
      }
      area <- minarea + rcnt * (maxarea - minarea)
      area <- pmin(area, maxarea)
      radius <- sqrt(area)

      inner <- 0.5
      outer <- (2 * inner)/sqrt(3)
      dx <- inner/sx
      dy <- outer/(2 * sy)
      rad <- sqrt(dx^2 + dy^2)
      hexC <- hexcoords(dx, dy, sep = NULL)

      dd <- data.frame(xdat=xnew, ydat=ynew, r=radius)

      dataStr <- paste("\"data\": [", paste(apply(as.matrix(dd), 1, function(x) paste("{\"x\":", x[1], ",\"y\":", x[2], ",\"r\":", x[3], "}", sep="")), collapse=","), "]", sep="")

      paste("{\"type\":\"", type, "\", \"shape\":", shape, ", \"xlab\":\"", xlab, "\", \"ylab\":\"", ylab, "\", \"hexx\": [", 
         paste(hexC$x, collapse=","), 
         "], \"hexy\":[",
         paste(hexC$y, collapse=","), 
         "], ", dataStr, "}", sep=""
      )
   } else {
      dd <- data.frame(x=x, y=y)
      dataStr <- paste("\"data\": [", paste(apply(as.matrix(dd), 1, function(x) paste("{\"x\":", x[1], ",\"y\":", x[2], "}", sep="")), collapse=","), "]", sep="")
      
      paste("{\"type\":\"", type, "\", \"xlab\":\"", xlab, "\", \"ylab\":\"", ylab, "\", ", dataStr, "}", sep=""
      )
   }
}


