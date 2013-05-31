

cogTableBodyHTML <- function(data, nr) {
   # browser()
   nc <- ncol(data)
   if(nc == 0) {
      return(NULL)      
   } else if(nrow(data) == 0) {
      tDataString <- matrix(nrow=nr, ncol=nc, data="<td>&nbsp;</td>")   
   } else {
      tDataString <- matrix(sapply(seq_len(nc), function(i) {
         if(inherits(data[[i]], c("integer", "numeric"))) {
            val <- format(data[[i]], big.mark=",") #, scientific=-3)
         } else {
            val <- data[[i]]
         }
         paste("<td>", val, "</td>", sep="")
      }), ncol=nc)
      # if fewer then the number of rows, fill with blank <td>s
      # (so that the height is always fixed)
      if(nrow(tDataString) < nr) {
         rnr <- nrow(tDataString)
         nn <- nr - rnr
         tDataString <- rbind(tDataString, matrix(nrow=nn, ncol=nc))
         tDataString[(rnr + 1):nr, 1:nc] <- "<td>&nbsp;</td>"
      }      
   }
   tDataString <- paste(paste(t(cbind("<tr>", tDataString, "</tr>"))), collapse="")
   HTML(tDataString)
}


cogTableHead <- function(data) {
   dnames <- names(data)
   dindex <- seq_along(dnames)
   
   tHeadString <- paste(
      "<tr role='row' id='cogColumnSortInput' class='columnSortInput'>",
      paste(
         "<th align='center' class='sorting' role='columnheader' 'tabindex='", 
         dindex - 1, 
         "' aria-controls='cogTable' rowspan='1' colspan='1' aria-label=', ", 
         dnames, 
         ": activate to sort'>", 
         dnames, 
         "<i class='icon-minus'></th>", 
         sep="", collapse=""),
      "</tr>", 
      sep=""
   )
   HTML(tHeadString)
}
# https://github.com/FontAwesome/Font-Awesome/issues/25

cogTableFootFilter <- function(data) {
   # browser()
   tFoot <- paste("<tr role='row' id=cogColumnFilterInput class='columnFilters'>",
      paste(sapply(seq_along(data), function(i) {
         if(inherits(data[,i], c("numeric", "integer"))) {
            curInput <- sprintf(
               "<td id='cogTable_uniplot_%d' class='columnUniPlot'><input style='text-align:right; width: 83px' id='lower_column_%d' name='%d' type='text' pattern=\"\\d+(\\.\\d*)?\" placeholder='From' class='noShinyDefaultBinding columnFilter columnFilterFrom'/><br/>
               <input style='text-align:right; width: 83px' id='upper_column_%d' name='%d' type='text' pattern=\"\\d+(\\.\\d*)?\" placeholder='To' class='noShinyDefaultBinding columnFilter columnFilterTo'/></td>", i, i, i, i, i)
         } else {
            curInput <- sprintf("<td id='cogTable_uniplot_%d' class='columnUniPlot'><input style='text-align:right; width: 83px' id='text_column_%d' type='text' name='%d' placeholder='regex' class='noShinyDefaultBinding columnFilter columnFilterRegex'/></td>", i, i, i)
         }
         curInput
      }), collapse="\n", sep=""),
      "</tr>", sep=""
   )
   tFoot
}

makeFootBar <- function(x, selected=NULL) {
   dat <- data.frame(table(x))
   dat$x <- as.character(dat$x)
   # if(is.null(selected)) {
   #    selected <- unique(dat$x)
   # }
   dat$order <- order(dat$Freq)
   dat$selected <- dat$x %in% selected
   delta <- 0.5
   xyplot(order ~ Freq,
      data=dat, 
      panel=function(x, y, ...) {
         for(i in seq_along(x)) {
            panel.rect(0, y[i] - delta, x[i], y[i] + delta, border="white", col=ifelse(dat$selected[i], "#7080D7", "darkgray"))
         }
      },
      ylim=range(dat$order) + c(-1, 1) * delta,
      xlim=c(0, max(dat$Freq)),
      scales=list(draw=FALSE),
      xlab="",
      ylab="",
      par.settings=footPlotParList
   )
}

# makeFootHist(iris[,1])
# makeFootHist(iris[,1], 5.2, 7.3)
# makeFootHist(iris[,1], 4.7, NULL)
# makeFootHist(iris[,1], NULL, 6.8)
# 
# set.seed(234)
# x <- iris[sample(1:150, 100, replace=TRUE),5]
# makeFootBar(x)
# makeFootBar(x, "virginica")
# makeFootBar(x, c("virginica", "setosa"))

# Univariate (marginal) distribution filter:
cogTableFootUnivar <- function(data) {
   tFoot <- paste("<tr><td colspan='", ncol(data), "' style='text-align:left; background-color: #777777; color:white'><strong>Uni Filter </strong><i class='icon-info-sign icon-white'></i></td></tr><tr role='row' id=cogTable_univar class='columnUnivar'>",
      paste(sapply(seq_along(data), function(i) {
         curClass <- ifelse(is.numeric(data[1,i]), "univarPlotHist", "")
         paste(
            "<td><div class='", curClass, "' title='", i, "'><div id='cogTable_univarPlot_", i, "' style='width: 95px; height: 53px;'></div><div id='cogTable_univarPlotDat_", i, "' style='display:none;'></div></div></td>", sep="")
      }), collapse="", sep=""),
      "</tr>", sep=""
   )
   tFoot
}

# (select 2+ boxes below and click <span class='bivarPlotScatter'>here</span>)
cogTableFootBivar <- function(data) {
   tFoot <- paste("<tr><td colspan='", ncol(data), "' style='text-align:left; background-color: #777777; color:white' class='bivarPlotScatter'><strong>Multi Filter </strong><i class='icon-info-sign icon-white'></i></td></tr><tr role='row' id=cogTable_bivar_select>",
      paste(sapply(seq_along(data), function(i) {
         curClass <- ifelse(is.numeric(data[1,i]), "", "not-numeric")
         paste(
            "<td class='", curClass, "' name='", i, "'>&nbsp;</td>", sep="")
      }), collapse="", sep=""),
      "</tr>",
      tdSelectString("#cogTable_bivar_select td"),
      sep=""
   )
   tFoot
}


makeFootHist <- function(x, rmin=NULL, rmax=NULL) {
   # browser()
   hst  <- hist(x, plot=FALSE)
   delta <- diff(hst$mids[1:2]) / 2

   rng <- diff(range(x, na.rm=TRUE))

   if(is.na(rmax) && is.na(rmin)) {
      rmin <- max(x, na.rm=TRUE) + rng
      rmax <- min(x, na.rm=TRUE) - rng
      sMin <- sMax <- numeric(0)
   } else {
      if(is.na(rmax))
         rmax <- max(x, na.rm=TRUE) + rng
      if(is.na(rmin))
         rmin <- min(x, na.rm=TRUE) - rng

      sMin <- which(hst$mids + delta > rmin & hst$mids - delta < rmin)
      sMax <- which(hst$mids + delta > rmax & hst$mids - delta < rmax)      
   }

   xyplot(hst$density ~ hst$mids,
      panel=function(x, y, ...) {
         for(i in setdiff(seq_along(hst$mids), c(sMin, sMax))) {
            panel.rect(x[i] - delta, 0, x[i] + delta, y[i], border="white", col=ifelse(x[i] + delta < rmin || x[i] - delta > rmax, "darkgray", "#7080D7"))
         }
         if(length(sMin) > 0) {
            panel.rect(x[sMin] - delta, 0, rmin, y[sMin], col="darkgray", border=NA)
            panel.rect(rmin, 0, x[sMin] + delta, y[sMin], col="#7080D7", border=NA)
            panel.rect(x[sMin] - delta, 0, x[sMin] + delta, y[sMin], col=NA, border="white")            
         }
         if(length(sMax) > 0) {
            panel.rect(x[sMax] - delta, 0, rmax, y[sMax], col="#7080D7", border=NA)
            panel.rect(rmax, 0, x[sMax] + delta, y[sMax], col="darkgray", border=NA)
            panel.rect(x[sMax] - delta, 0, x[sMax] + delta, y[sMax], col=NA, border="white")            
         }
      },
      ylim=c(0, max(hst$density)),
      xlim=range(hst$mids) + c(-1, 1) * delta,
      scales=list(draw=FALSE),
      xlab="",
      ylab="",
      par.settings=footPlotParList
   )
}



footPlotParList <- list(axis.line = list(col = 0), layout.heights = list(top.padding = 0, main.key.padding = 0, key.axis.padding = 0, axis.xlab.padding = 0, xlab.key.padding = 0, key.sub.padding = 0, bottom.padding = 0), layout.widths = list(left.padding = 0, key.ylab.padding = 0, ylab.axis.padding = 0, axis.key.padding = 0, right.padding = 0), axis.components = list(top = list(pad1 = 2, 0, pad2 = 0), right = list(pad1 = 0, pad2 = 0)))
