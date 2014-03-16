
# TODO: remove when trelliscope is available - this exists in trelliscope
encodePNG <- function(plotLoc) {
   bytes <- file.info(plotLoc)$size
   # b64 <- base64encode(readBin(plotLoc, "raw", n = bytes))
   b64 <- base64enc:::base64encode(readBin(plotLoc, "raw", n = bytes))
   paste("data:image/png;base64,", b64, sep = "")   
}

getPNGs <- function(cogDF, cdo, vdbPrefix, conn=NULL) {
   if(cdo$preRender) {
      pngs <- unlist(lapply(cdo$panelDataSource[cogDF$panelKey], "[[", 2))
   } else {
      # TODO: since we're making the plot on the fly, why not make it the right size instead of creating a large one...
      tmpfile <- tempfile()
      
      # load relatedData
      rel <- cdo$relatedData
      for(i in seq_along(rel)) {
         assign(names(rel)[i], rel[[i]], environment())
      }
      environment(cdo$panelFn) <- environment()
      
      curDat <- cdo$panelDataSource[cogDF$panelKey]
      if(is.null(curDat))
         warning("data for key ", cogDF$panelKey, " could not be found.")
      
      pngs <- sapply(curDat, function(x) {
         makePNG(dat=x, panelFn=cdo$panelFn, file=tmpfile, width=cdo$panelDim$width, height=cdo$panelDim$height, res=cdo$panelDim$res, lims=cdo$lims)
         encodePNG(tmpfile)
      })
   }
   pngs
}

plotTabSkeleton <- function(nRow, nCol, relList, cdo) {
   
   # innerInnerTables
   
   if(!is.null(relList)) {
      # do some aspect ratio calculations...
      nDisp <- length(relList) + 1
      aspects <- c(cdo$panelDim$aspect, sapply(relList, function(x) x$panelDim$aspect))
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
#    n <- cogNrow(cogDF))
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