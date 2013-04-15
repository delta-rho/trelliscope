
# TODO: remove when vdb is available - this exists in vdb
encodePNG <- function(plotLoc) {
   bytes <- file.info(plotLoc)$size
   # b64 <- base64encode(readBin(plotLoc, "raw", n = bytes))
   b64 <- base64enc:::base64encode(readBin(plotLoc, "raw", n = bytes))
   paste("data:image/png;base64,", b64, sep = "")   
}

getPNGs <- function(cogDF, cdo, localData, hdfsData, vdbPrefix, conn=NULL) {
   if(cdo$storage=="local") {
      hasSubDir <- cdo$subDirN > 0
      if(hasSubDir) {
         cogDF$subDir <- sapply(cogDF$panelKey, function(x) vdb:::keyHash(x, cdo$subDirN))
      } else {
         cogDF$subDir <- ""
      }
      
      ff <- file.path(vdbPrefix, "displays", cdo$group, cdo$name, "png", cogDF$subDir, paste(cogDF$panelKey, ".png", sep=""))
      pngs <- sapply(ff, encodePNG) # pngs <- ff
   } else if(cdo$storage=="hdfs") {
      pngs <- unlist(cdo$mapfile[cogDF$panelKey])
   } else if(cdo$storage=="mongo") {
      mongoConn <- vdbMongoInit(conn)
      mongoNS <- mongoCollName(conn$vdbName, cdo$group, cdo$name, "panel")
      pngs <- sapply(as.character(cogDF$panelKey), function(x) getMongoPlot(mongoConn, mongoNS, x))
   } else if(cdo$storage=="localData") {
      tmpfile <- tempfile()
      # browser()
      pngs <- sapply(localData[cogDF$panelKey], function(x) {
         # TODO: since we're making the plot on the fly, why not make it the right size instead of creating a large one...
         vdb:::vdbMakePNG(dat=x, plotFn=cdo$plotFn, file=tmpfile, width=cdo$plotDim$width, height=cdo$plotDim$height, res=cdo$plotDim$res, xLimType=cdo$xLimType, yLimType=cdo$yLimType, lims=cdo$lims)
         vdb:::encodePNG(tmpfile)
      })
   } else if(cdo$storage=="hdfsData") {
      dat <- hdfsData[as.character(cogDF$panelKey)]
      tmpfile <- tempfile()
      pngs <- sapply(dat, function(x) {
         vdb:::vdbMakePNG(dat=x, plotFn=cdo$plotFn, file=tmpfile, width=cdo$plotDim$width, height=cdo$plotDim$height, res=cdo$plotDim$res, xLimType=cdo$xLimType, yLimType=cdo$yLimType, lims=cdo$lims)
         vdb:::encodePNG(tmpfile)
      })
   }
   pngs
}

plotTabSkeleton <- function(nRow, nCol, relList, cdo) {
   
   # innerInnerTables
   
   if(!is.null(relList)) {
      # do some aspect ratio calculations...
      nDisp <- length(relList) + 1
      aspects <- c(cdo$plotDim$aspect, sapply(relList, function(x) x$plotDim$aspect))
      innerNcol <- ceiling(sqrt(nDisp))
      innerNrow <- ceiling(nDisp / innerNcol)
      
      groups <- sapply(relList, function(x) x$group)
      names <- sapply(relList, function(x) x$name)
      # browser()
      inner <- matrix(nrow=nRow*nCol, ncol=nDisp)
      inner[,1] <- paste("<div id='plotTable_panel_", seq_len(nRow*nCol), "' class='panelWrap'></div>", sep="")

      tmp <- expand.grid(panel=seq_len(nRow*nCol), rel=seq_len(nDisp - 1))
      tmp$group <- rep(groups, each=nRow*nCol)
      tmp$name <- rep(names, each=nRow*nCol)
      
      inner[,-1] <- paste("<div id='plotTable_panel_", tmp$panel, "_rel_", tmp$rel, "' class='panelWrap'><span class='relatedInfo'>", tmp$group, " / ", tmp$name, "</span></div>", sep="")
      
      # pngs[-1] <- paste("<div><img src='", pngs[-1], "' class='png_img' alt='not loaded'/><span class='relatedInfo'>", paste(groups, names, sep=" / "), "</span></div>", sep="")

      innerInner <- apply(inner, 1, function(x) {
         x <- matrix(data=x, ncol=innerNcol, byrow=TRUE)

         paste("
      <table class='table-borderless' cellpadding='0' cellspacing='0' align='center' valign='center'>
      <tbody>",
            paste("<tr>",
            apply(x, 1, function(x) paste("<td>", x, "</td>", collapse="", sep="")),
      "</tr>",
            sep="", collapse=""
            ),
            "</tbody></table>",
            sep="", collapse=""
         )
      })
   } else {
      innerInner <- paste("<div id='plotTable_panel_", seq_len(nRow*nCol), "'></div>", sep="")
   }
   
   innerTables <- matrix(
      paste("
         <table class='table-condensed table-bordered'>
         <tbody>
         <tr>
         <td align='center' class='img_td' name='", seq_len(nRow*nCol), "'>
         <div class='plotTableWrap' id='plotTableWrap_", seq_len(nRow*nCol), "'>", innerInner, "</div>
         </td>
         </tr>
         <tr>
         <td>
         <div id='plotTableCog_panel_", seq_len(nRow*nCol), "'></div>         
         </td>
         </tr>
         </tbody>
         </table>", 
         sep=""
      ),
      nrow=nRow, ncol=nCol, byrow=TRUE
   )
   
   outerTable <- paste("
<table border='0' cellpadding='0' cellspacing='0' align='center' id='plotTable' valign='center'>
<tbody>",
      paste("<tr>",
      apply(innerTables, 1, function(x) paste("<td>", x, "</td>", collapse="", sep="")),
"</tr>",
      sep="", collapse=""
      ),
      "</tbody></table>"
   )
   HTML(outerTable)
}


# getPlotString <- function(cogDF, plotInfo, nRow, nCol, plotWidth, plotHeight, storage) {
#    pngs <- getPNGs(cogDF, plotInfo, storage, vdbPrefix)
#    n <- nrow(cogDF)
# 
#    extra <- nRow*nCol - n
#    if(n > 0)
#       pngs <- c(pngs, rep("", extra))
# 
#    innerTables <- paste("
# <table cellpadding='0' cellspacing='0'>
# <tbody>
# <tr>
#    <td align='center' class='img_td' name='0'><a href='#' class='plotThumbnail'><img src='", pngs, "' class='png_img' alt=''/></a>
#    </td>
# </tr>
# </tbody>
# </table>", 
#       sep=""
#    )
#    innerTables <- matrix(innerTables, nrow=nRow, ncol=nCol, byrow=TRUE)
#    
#    outerTable <- paste("
# <table border='0' cellpadding='0' cellspacing='0' align='center' id='plotTable' valign='center'>
# <tbody>",
#       paste("<tr>",
#       apply(innerTables, 1, function(x) paste("<td>", x, "</td>", collapse="", sep="")),
# "</tr>",
#          sep="", collapse=""
#       ),
#       "</tbody></table>"
#    )
#    outerTable
# }