listrbind  <- function(datlist, stringsAsFactors=FALSE) {
   nms <- as.list(names(datlist[[1]]))

   a <- lapply(nms, function(x) data.frame(do.call(c, lapply(datlist, function(y) y[,x])), stringsAsFactors=stringsAsFactors))
   res <- do.call(cbind, a)
   names(res) <- nms
   res
}

keyHash <- function(key, nbins) {
   sum(strtoi(charToRaw(digest(key)), 16L)) %% nbins   
}

## internal
vdbFindGlobals <- function(f) {
   tildeHandler <- codetools:::collectUsageHandlers[["~"]]
   remove("~", envir=codetools:::collectUsageHandlers)
   res <- findGlobals(f, merge=FALSE)$variables
   assign("~", tildeHandler, envir=codetools:::collectUsageHandlers)
   res
}

## internal
vdbValidateCogFn <- function(dat, cogFn, verbose=FALSE) {
   if(verbose)
      message("* Testing cognostics function on a subset ... ", appendLF=FALSE)
   isCondDiv <- dat$divBy$type=="condDiv"

   ex <- getCognosticsSub(divExample(dat), cogFn, isCondDiv, names(dat)[1])
   if(!all(sapply(ex, function(x) inherits(x, "cog"))))
      stop("Each cognostic must have class 'cog' - please make sure you are specifying: var=cog(...)")
   
   exdf <- cog2df(ex)
   if(nrow(exdf) > 1)
      stop("'cogFn' must return something that can be coerced into a 1-row data.frame")
   if(verbose)
      message("ok")
   ex
}

## internal
vdbValidatePlotFn <- function(plotFn, dat, verbose=FALSE) {
   if(verbose)
      message("* Validating 'plotFn'...")
   
   plotFn(divExample(dat))
}

## internal
vdbValidatePlotDim <- function(plotDim, dat, plotFn, verbose=FALSE) {
   # browser()
   # TODO: 
   # TODO: handle aspect ratio
   width <- plotDim$width
   height <- plotDim$height
   res <- plotDim$res
   aspect <- plotDim$aspect
   
   if(verbose)
      message("* Validating plot dimensions...")
   
   limsUpdated <- FALSE
   # # TODO: this lattice stuff isn't working...
   # p <- plotFn(divExample(dat))
   # if(inherits(p, "trellis")) {
   #    if(is.null(p$layout)) 
   #       p$layout <- c(1, 1)
   #    
   #    if(!p$aspect.fill) {
   #       if(verbose)
   #          message("... attempting to honor plot aspect ratio from plotFn()")
   #       browser()
   #       aspect <- p$aspect.ratio
   # 
   #       tryRes <- try({
   #          if(is.null(width) && is.null(height)) {
   #             width <- 480
   #             height <- vdbGetHeight(p, width)
   #          } else if(!is.null(width) && is.null(height)) {
   #             height <- vdbGetHeight(p, width)
   #          } else if(is.null(width) && !is.null(height)) {
   #             width <- vdbGetWidth(p, height)
   #          }            
   #       })
   #       if(inherits(tryRes, "try-error") && verbose) {
   #          message("... attempt failed ... moving to plan B")            
   #       } else {
   #          limsUpdated <- TRUE
   #       }
   #    }
   # }
   if(!limsUpdated) {
      if(!is.null(aspect)) {
         if(is.null(width) && is.null(height)) {
            width <- 480
            height <- width * aspect
         } else if(!is.null(width) && is.null(height)) {
            height <- width * aspect
         } else if(is.null(width) && !is.null(height)) {
            width <- height / aspect
         }
      } else {
         if(is.null(height))
            height <- 480
         if(is.null(width))
            width <- 480
      }
   }
   
   if(is.null(res))
      res <- 150
      
   width <- width * res / 72
   height <- height * res / 72      

   if(is.null(aspect))
      aspect <- height / width

   list(height=height, width=width, res=res, aspect=aspect)
}

vdbValidateCogStorage <- function(cogStorage, conn) {
   if(is.null(cogStorage))
      cogStorage <- "local"

   if(!cogStorage %in% c("local", "mongo"))
      stop("cogStorage must be 'local' or 'mongo'")

   cogStorage
}

## internal
vdbValidateStorage <- function(storage, conn, datClass) {
   # if "storage" is not specified, use the conn default
   if(is.null(storage))
      if("rhData" %in% datClass) {
         storage <- "hdfs"         
      } else {
         storage <- conn$defaultStorage         
      }
   # if that doesn't do it, default to "local"
   if(is.null(storage)) {
      storage <- "local"
   }
   if("rhData" %in% datClass && storage == "local") {
      message("* ---WARNING: It is not advised to use storage='local' with input of type 'rhData'.  Unless you are using a local file system in place of HDFS on your hadoop cluster, this is not a good idea!---")
      # message("Changing storage to \"hdfs\" because this is the default option for dat of class \"rhData\"")
      # storage <- "hdfs"
   }
   if("rhData" %in% datClass && storage == "localData") {
      message("* Changing storage to \"hdfs\" because this is the default option for dat of class \"rhData\"")
      storage <- "hdfs"
   }
   
   if(storage=="localData" && (!"localDiv" %in% datClass))
      stop("When storage=='localData', dat must be of class 'localDiv'.")
   
   if(!storage %in% c("local", "mongo", "hdfs", "localData", "hdfsData"))
      stop(paste("storage=\"", storage, "\" is not recognized.  Must be either 'local', 'localData', 'hdfs', or 'mongo'.\n", sep=""))
      
   storage
}

# Check prefix and get directory ready
## internal
vdbGetDisplayPrefix <- function(conn, group, name) {
   vdbPrefix <- vdbValidatePrefix(conn)

   if(!file.exists(vdbPrefix)) {
      stop(paste("Directory ", vdbPrefix, " does not exist.  Please use vdbInit(", vdbPrefix, ") to initialize.", sep=""))
   }
   
   file.path(vdbPrefix, "displays", group, name)
}

## internal
vdbValidatePrefix <- function(conn) {
   prefix <- conn$vdbPrefix

   if(is.null(prefix))
      stop("Could not get vdb connection.  Must specify an option ...")
      
   if(!file.exists(prefix)) 
      stop(paste("vdbPrefix", prefix, "does not exist"))

   prefix
}

## internal
vdbValidateDisplayPrefix <- function(displayPrefix) {
   if(file.exists(displayPrefix)) {
      bakFile <- paste(displayPrefix, "_bak", sep="")
      message(paste("* Plot exists... backing up previous to", bakFile))
      if(file.exists(bakFile)) {
         message("* Removing previous backup plot directory")
         unlink(bakFile, recursive=TRUE)
      }
      file.rename(displayPrefix, bakFile)
   }
   dir.create(displayPrefix, recursive=TRUE)
}

## internal
vdbUpdateDisplayList <- function(vdbPrefix, name, group, desc, n, storage, cogStorage, hdfsPrefix, width, height, aspect, updated=Sys.time(), keySig, dataSig=dataSig, subDirN) {
   # TODO: make this more robust (take list as parameter, etc.)

   if(is.null(hdfsPrefix))
      hdfsPrefix <- NA
   
   displayListPath <- file.path(vdbPrefix, "displays", "_displayList.Rdata")
   
   displayListNames <- c("uid", "Group", "Name", "Description", "Pages", "Storage Mode", "Cognostics Storage Mode", "Storage Prefix", "Width (px)", "Height (px)", "Aspect Ratio", "Last Updated", "Key Signature", "Data Signature")
   
   curPlot <- data.frame(
      uid=1,
      group=group,
      name=name,
      desc=desc,
      n=n,
      storage=storage,
      cogStorage=cogStorage,
      hdfs=hdfsPrefix,
      width=width,
      height=height,
      aspect=aspect,
      updated=updated,
      keySig=keySig,
      dataSig=dataSig,
      subDirN=subDirN,
      stringsAsFactors=FALSE
   )
   
   if(!file.exists(displayListPath)) {
      displayList <- curPlot
   } else {
      load(displayListPath)
      idx <- which(displayList$name == name & displayList$group == group)
      if(length(idx) == 0) {
         curPlot$uid <- max(displayList$uid) + 1
         displayList <- rbind(displayList, curPlot)
      } else {
         curPlot$uid <- displayList[idx,]$uid
         displayList[idx,] <- curPlot
      }
   }
   # make sure all other plots still exist
   existsInd <- file.exists(
      file.path(vdbPrefix, "displays", displayList$group, displayList$name)
   )
   displayList <- displayList[existsInd,]
   
   # sort it by group and name
   displayList <- displayList[order(displayList$group, displayList$name),]
   
   save(displayList, displayListNames, file=displayListPath)
}

## internal
vdbUpdateDisplayListJson <- function(vdbPrefix) {
   load(file.path(vdbPrefix, "displays", "_displayList.Rdata"))
   
   string <- paste("{ \"displayList\": [\n",
      paste(
         paste("[\"", apply(displayList, 1, function(x) {
            paste(x, collapse="\",\"")
         }), "\"]", sep=""),
         collapse=",\n"
      ),
      "\n],",
      "\n\"displayListNames\": [\n\"",
      paste(displayListNames, collapse="\",\""),
      "\"\n]\n}", sep=""
   )
   cat(string, file=file.path(vdbPrefix, "displays", "_displayList.json"))
}

## internal
vdbValidateInputs <- function(input) {
   if(inherits(input, "inputVars")) {
      input <- list(input)
   } else if(!all(sapply(input, function(x) inherits(x, "inputVars"))) && !is.null(input)) {
      stop("cogInput must be either of class 'inputVars' or a list of objects of class 'inputVars'")
   }
   input
}


# # if there is an aspect ratio and layout specified, then 
# # set width or height accordingly to get rid of margins around plot
# # assumes this is the same for all objects in the list
# if(!p[[1]]$aspect.fill && !is.null(p[[1]]$layout) && p[[1]]$layout[1] !=0) {
#    if(is.null(width) && is.null(height)) {
#       width <- 7; height <- vdb_getHeight(p[[1]], width)
#    } else if(!is.null(width) && is.null(height)) {
#       height <- vdb_getHeight(p[[1]], width)
#    } else if(is.null(width) && !is.null(height)) {
#       width <- vdb_getWidth(p[[1]], height)
#    }
# }


## internal
vdbMakePNG <- function(dat, plotFn=NULL, file, width, height, res, xLimType=NULL, yLimType=NULL, lims=NULL) {

   a <- suppressWarnings(try({
      png(file=file, width=width, height=height, res=res)
   }, silent=TRUE))
   if(inherits(a, "try-error")) {
      # a <- suppressWarnings(try({
         suppressMessages(require(Cairo))
         
         # width <- 480; height <- 480; res <- 150
         # file <- "/pic/people/hafe647/asdf.png"
         CairoPNG(file=file, width=width, height=height, dpi=res, pointsize=12*res/72)
         # plot(rnorm(100))
         # dev.off()

      # }, silent=TRUE))
      
      # if(inherits(a, "try-error")) {
      #    stop("Couldn't make a png!  Check your png device.")
      # }
   }
   
   if(inherits(dat, "trellis") || inherits(dat, "ggplot")) { # single panel plot
      print(dat)
   } else if (inherits(dat, "expression")) { # expression (can't change limits)
      eval(dat)
   } else { # plot objects such as trellis or lattice
      tmp <- plotFn(dat)
      # browser()
      
      if(!is.null(lims)) {
         if(inherits(tmp, "trellis")) {
            # if there are multiple panels inside of one plot, we can't do this
            if(!(inherits(tmp$x.limits, "list") || inherits(tmp$y.limits, "list"))) {
               plotXLim <- tmp$x.limits
               plotYLim <- tmp$y.limits               
               curXLim <- vdbCurXLim(lims, dat, plotXLim)
               curYLim <- vdbCurYLim(lims, dat, plotYLim)         

               if(xLimType != "free")
                  tmp$x.limits <- curXLim
               if(yLimType != "free")
                  tmp$y.limits <- curYLim
            }
         } else if(inherits(tmp, "ggplot")) {
            gglims <- ggplot_build(tmp)$panel$ranges
            if(length(gglims) == 1) {
               plotXLim <- gglims[[1]]$x.range
               plotYLim <- gglims[[1]]$y.range
               curXLim <- vdbCurXLim(lims, dat, plotXLim)
               curYLim <- vdbCurYLim(lims, dat, plotYLim)         

               if(xLimType != "free")
                  suppressMessages(tmp <- tmp + xlim(curXLim))
               if(yLimType != "free")
                  suppressMessages(tmp <- tmp + ylim(curYLim))
            }
         }
      }
      
      if(inherits(tmp, c("trellis"))) {
         print(tmp)
      }
      if(inherits(tmp, c("ggplot"))) {
         print(tmp)
      }
      if(inherits(tmp, "expression")) {
         eval(tmp)
      }
   }
   dev.off()
}

# Translate Rhipe key/value pairs into a format for vdbPlot
# internal
vdbRhKeyValTrans <- function(curKey, curVal) {
   # TODO: if current map.key has length 1, and if its as.character is okay for a filename, then make that the splitKey.  If not, make a md5 hash of it
   if(length(curKey) > 1)
      curKey <- paste(curKey, collapse="|")

   # if the transformation created a new split
   # then prepend curKey to each splitKey
   if(inherits(curVal, "localDiv")) {
      dat <- curVal
      for(j in seq_along(dat))
         dat[[j]]$splitKey <- paste(curKey, dat[[j]]$splitKey, sep="|")
   } else {
      if(inherits(curVal, "list")) {
         # dat <- list(c(splitKey=curKey, curVal))         
         dat <- curVal
         names(dat) <- curKey
      } else {
         # dat <- list(list(splitKey=curKey, data=curVal))
         dat <- list(curVal)
         names(dat) <- curKey
      }
      class(dat) <- c("localDiv", "list")
   }
   dat
}
