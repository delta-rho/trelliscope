# on last check, this stuff wasn't working - don't export it for now

vdbGetWidth <- function(x, height) {
   # gets the aspect ratio, etc. for a lattice object
   require(grid)

   lyt <- vdbTrellis.getlayout(x)
   page.layout <- lyt$lyt
   nrows <- lyt$nrows
   ncols <- lyt$ncols

   H <- height

   h1 <- sum(unlist(lapply(page.layout$heights, function(x) as.numeric(convertY(x, "inches")))))
   w1 <- sum(unlist(lapply(page.layout$widths, function(x) as.numeric(convertX(x, "inches")))))
   
   w2 <- (H - h1) / (x$aspect.ratio * nrows)
   W <- w1 + w2*ncols
   W
}

vdbGetHeight <- function(x, width) {
   require(grid)
   lyt <- vdbTrellis.getlayout(x)
   page.layout <- lyt$lyt
   nrows <- lyt$nrows
   ncols <- lyt$ncols

   W <- width

   h1 <- sum(unlist(lapply(page.layout$heights, function(x) as.numeric(convertY(x, "inches")))))
   w1 <- sum(unlist(lapply(page.layout$widths, function(x) as.numeric(convertX(x, "inches")))))
   
   h2 <- (W - w1) * x$aspect.ratio / ncols
   H <- h1 + h2*nrows
   H
}

vdbTrellis.getNumPages <- function(p) {
   original.condlevels <- 
      used.condlevels <-
         lapply(p$condlevels, function(x) seq_along(x))
   used.condlevels <- 
      mapply("[", used.condlevels, p$index.cond,
            MoreArgs = list(drop = FALSE),
            SIMPLIFY = FALSE)
   used.condlevels <- used.condlevels[p$perm.cond]
   inverse.permutation <- order(p$perm.cond) # used later

   cond.max.levels <- sapply(used.condlevels, length)
   panel.layout <- lattice:::compute.layout(p$layout, cond.max.levels, skip = p$skip) 
   panel.layout[3]  
}

vdbTrellis.getlayout <- function(x,
   panel.height = lattice.getOption("layout.heights")$panel,
   panel.width = lattice.getOption("layout.widths")$panel
) {
   
   if (!is.null(x$plot.args)) {
      supplied <- names(x$plot.args)
      if ("panel.height" %in% supplied && missing(panel.height)) panel.height <- x$plot.args$panel.height
      if ("panel.width"  %in% supplied && missing(panel.width))  panel.width  <- x$plot.args$panel.width
   }
   
   original.condlevels <- 
      used.condlevels <-
         lapply(x$condlevels, function(x) seq_along(x))
   used.condlevels <- 
      mapply("[", used.condlevels, x$index.cond,
            MoreArgs = list(drop = FALSE),
            SIMPLIFY = FALSE)
   used.condlevels <- used.condlevels[x$perm.cond]
   inverse.permutation <- order(x$perm.cond) # used later
   
   cond.max.levels <- sapply(used.condlevels, length)
   number.of.cond <- length(cond.max.levels)
   
   panel.layout <- lattice:::compute.layout(x$layout, cond.max.levels, skip = x$skip)
   
   if(panel.layout[1] == 0)
      stop("invalid layout")
   
   axis.text <- trellis.par.get("axis.text")
   
   if (!x$aspect.fill)
      panel.height[[1]] <- x$aspect.ratio * panel.width[[1]]
   
   legend <- lattice:::evaluate.legend(x$legend)

   xaxis.lineheight <-
       if (is.logical(x$x.scales$lineheight)) axis.text$lineheight
       else x$x.scales$lineheight   
   xaxis.cex <-
      if (is.logical(x$x.scales$cex)) rep(axis.text$cex, length = 2)
      else x$x.scales$cex
   xaxis.rot <-
      if (is.logical(x$x.scales$rot)) c(0, 0)
      else x$x.scales$rot

   yaxis.lineheight <-
       if (is.logical(x$y.scales$lineheight)) axis.text$lineheight
       else x$y.scales$lineheight   
   yaxis.cex <-
      if (is.logical(x$y.scales$cex)) rep(axis.text$cex, length = 2)
      else x$y.scales$cex
   yaxis.rot <-
      if (!is.logical(x$y.scales$rot)) x$y.scales$rot
      else if (x$y.scales$relation != "same" && is.logical(x$y.scales$labels)) c(90, 90)
      else c(0, 0)
   
   cols.per.page <- panel.layout[1]
   rows.per.page <- panel.layout[2]
   
   x.alternating <- rep(x$x.scales$alternating, length = cols.per.page)
   y.alternating <- rep(x$y.scales$alternating, length = rows.per.page)
   x.relation.same <- x$x.scales$relation == "same"
   y.relation.same <- x$y.scales$relation == "same"
   
   main <-
       lattice:::grobFromLabelList(lattice:::getLabelList(x$main,
                                      trellis.par.get("par.main.text")),
                         name = trellis.grobname("main", type=""))
   sub <-
       lattice:::grobFromLabelList(lattice:::getLabelList(x$sub,
                                      trellis.par.get("par.sub.text")),
                         name = trellis.grobname("sub", type=""))
   xlab <-
       lattice:::grobFromLabelList(lattice:::getLabelList(x$xlab,
                                      trellis.par.get("par.xlab.text"),
                                      x$xlab.default),
                         name = trellis.grobname("xlab", type=""))
   ylab <-
       lattice:::grobFromLabelList(lattice:::getLabelList(x$ylab,
                                      trellis.par.get("par.ylab.text"),
                                      x$ylab.default),
                         name = trellis.grobname("ylab", type=""),
                         orient = 90)
   xlab.top <-
       lattice:::grobFromLabelList(lattice:::getLabelList(x$xlab.top,
                                      trellis.par.get("par.xlab.text")),
                         name = trellis.grobname("xlab.top", type=""))
   ylab.right <-
       lattice:::grobFromLabelList(lattice:::getLabelList(x$ylab.right,
                                      trellis.par.get("par.ylab.text")),
                         name = trellis.grobname("ylab.right", type=""),
                         orient = 90)

   par.strip.text <- trellis.par.get("add.text")
   par.strip.text$lines <- 1
   if (!is.null(x$par.strip.text)) 
      par.strip.text[names(x$par.strip.text)] <- x$par.strip.text
   
   trellis.par.set(x$par.settings) # set padding, etc.

   layoutCalculations <-
       lattice:::calculateGridLayout(x,
                           rows.per.page, cols.per.page,
                           number.of.cond,
                           panel.height, panel.width,
                           main, sub,
                           xlab, ylab, 
                           xlab.top, ylab.right, 
                           x.alternating, y.alternating,
                           x.relation.same, y.relation.same,
                           xaxis.rot, yaxis.rot,
                           xaxis.cex, yaxis.cex,
                           xaxis.lineheight, yaxis.lineheight,
                           par.strip.text,
                           legend)
   
   page.layout <- layoutCalculations$page.layout

   list(lyt=page.layout, calcs=layoutCalculations, ncols=cols.per.page, nrows=rows.per.page, npanels=as.numeric(cond.max.levels))
}

vdbTrellis.getncond <- function(x) {
   original.condlevels <- 
      used.condlevels <-
         lapply(x$condlevels, function(x) seq_along(x))
   used.condlevels <- 
      mapply("[", used.condlevels, x$index.cond,
            MoreArgs = list(drop = FALSE),
            SIMPLIFY = FALSE)
   used.condlevels <- used.condlevels[x$perm.cond]
   inverse.permutation <- order(x$perm.cond) # used later

   cond.max.levels <- sapply(used.condlevels, length)
   number.of.cond <- length(cond.max.levels)   
   as.numeric(cond.max.levels)
}


