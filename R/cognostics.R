
#' @export
cogLoessRMSE <- function(x, y, ..., desc="RMSE of residuals from loess fit") {
   tmp <- try(loess(y ~ x, ...))
   if(inherits(tmp, "try-error"))
      return(NA)
   cog(tmp$s, desc=desc, type="num")
}

#' @export
cogRange <- function(x, desc="range (max - min)") {
   res <- suppressWarnings(diff(range(x, na.rm=TRUE)))
   if(is.infinite(res))
      res <- NA
   cog(res, desc=desc, type="num")
}

#' @export
cogMean <- function(x, desc="mean") {
   res <- suppressWarnings(mean(x, na.rm=TRUE))
   if(is.infinite(res))
      res <- NA
   cog(res, desc=desc, type="num")
}

#' @export
cogScagnostics <- function(x, y) {
   suppressMessages(require(scagnostics))
   tmp <- try(scagnostics(x, y))
   if(inherits(tmp, "try-error")) {
      # make a data.frame of NA
      res <- scagnostics(1:10, 1:10)
      res <- list(t(as.matrix(res)))
      res[1,] <- NA
      res$cor <- NA
   } else {
      res <- data.frame(t(as.matrix(tmp)))
   }
   names(res)[9] <- "Monoton" # so it's not too wide in cog table
   list(
      outly   = cog(res[1] , type="num",  
         desc = "Proportion of the total edge length due to extremely long edges connected to points of single degree"),
      skew    = cog(res[2] , type="num",  
         desc = "Ratio of quantiles of edge lengths"),
      clumpy  = cog(res[3] , type="num",  
         desc = "A runt-based measure that emphasizes clusters with small intra-cluster distances relative to the length of their connecting edge"),
      sparse  = cog(res[4] , type="num",  
         desc = "Measures whether points in a 2D scatterplot are confined to a lattice or a small number of locations on the plane"),
      striated= cog(res[5] , type="num",  
         desc = "Measure of coherence"),
      convex  = cog(res[6] , type="num",  
         desc = "Ratio of the area of the alpha hull and the area of the convex hull"),
      skinny  = cog(res[7] , type="num",  
         desc = "Ratio of perimeter to area of a polygon -- roughly, how skinny it is. A circle yields a value of 0, a square yields 0.12 and a skinny polygon yields a value near one."),
      stringy = cog(res[8] , type="num",  
         desc = "A stringy shape is a skinny shape with no branches"),
      monoton = cog(res[9] , type="num",  
         desc = "Squared Spearman correlation coefficient")
   )
}

#' @export
cog <- function(val=NULL, desc="", type=NULL) {
   cogTypes <- list(
      int  = as.integer  ,
      num  = as.numeric  ,
      fac  = as.character,
      date = as.Date     ,
      time = as.POSIXct  ,
      geo  = as.cogGeo   ,
      rel  = as.cogRel   ,
      hier = as.cogHier
   )
   
   types <- names(cogTypes)
   
   if(!is.null(type)) {
      if(!type %in% types)
         stop("Invalid cognostics type: ", type)
         
      val <- try(cogTypes[[type]](val))
      if(inherits(val, "try-error"))
         val <- NA
   } else { # try to infer type
      if(is.factor(val))
         val <- as.character(val)

      if(!(is.character(val) || is.numeric(val) || inherits(val, "Date") || inherits(val, "POSIXct")))
         val <- NA
   }
   
   attr(val, "desc") <- desc
   class(val) <- c("cog", class(val))
   val
}

#' @export
print.cog <- function(x, ...) { 
   attr(x, "desc") <- NULL 
   class(x) <- setdiff(class(x), "cog")
   print(x)
}




## internal
## some special cognostics, such as relations, need to be concatenated to a comma-separated string if we are storing them as a data.frame
cog2df <- function(x) {
   # TODO: when class(x[[i]])=="vdbCogRel", first concatenate
   as.data.frame(x)
}

as.cogGeo <- function(x) {
   x <- x[1:2]
   names(x) <- c("lat", "lon")
   class(x) <- c("vdbCogGeo", "list")
   x
}

as.cogRel <- function(x) {
   class(x) <- c("vdbCogRel")
   x
}

as.cogHier <- function(x) {
   stop("not implemented...")
}

cogFlatten <- function(x) {
   if(inherits(x, "vdbCogRel"))
      return(paste(x, collapse=","))
   x
}

getCogDesc <- function(x, df=TRUE) {
   do.call(c, lapply(x, function(a) {
      if(df) {
         fl <- cogFlatten(a)
         rep(attr(a, "desc"), length(fl))
      } else {
         attr(a, "desc")         
      }
   }))
}


# gets not only cognostics returned by cogFn, but also panelKey and levels of conditioning variables
getCognosticsSub <- function(data, cogFn, isCondDiv, splitKey) {
   condList <- NULL
   if(isCondDiv)
      condList <- lapply(attr(data, "split"), function(x) {
         cog(x, desc="conditioning variable", type="fac")
   })

   # if the user didn't wrap it in "cog()", do it for them...
   if(is.null(cogFn)) {
      cd <- NULL
   } else {
      cd <- cogFn(data)
      notCogClass <- which(sapply(cd, function(x) !inherits(x, "cog")))
      for(i in notCogClass) {
         cd[[i]] <- cog(cd[[i]])
      }
   }
   
   c(
      list(panelKey = cog(splitKey, desc="panel key", type="fac")),
      condList,
      cd
   )
}

getCognostics <- function(data, cogFn, splitKeys=NULL) {
   if(is.null(splitKeys))
      splitKeys <- names(data)

   isCondDiv <- data$divBy$type=="condDiv"
   lapply(seq_along(data), function(ii) {
      getCognosticsSub(data[[ii]], cogFn, isCondDiv, splitKeys[[ii]])
   })
}
