#' Create a New Notebook File
#' 
#' Create a new notebook file skeleton.
#' 
#' @param name name of the .Rmd file to be created
#' @param blank do you just want a blank page, or do you want a template with several markdown examples filled out for reference?
#' @param pageTitle, title, author, toc, css these are parameters to be placed in the \code{\link{bsSetup}} call at the top of the .Rmd file, and can be ignored if you plan to change them later in the file itself
#' @param conn VDB connection info, typically stored in options("vdbConn") at the beginning of a session, and not necessary to specify here if a valid "vdbConn" object exists
#' @param overwrite if the file exists, should it be overwritten?
#' 
#' @return creates a new .Rmd file that will go in the "notebook" directory of the vdb directory
#' 
#' @author Ryan Hafen
#' 
#' @seealso \code{\link{typeset}}, \code{\link{viewNotebook}}, \code{\link{bsSetup}}
#' 
#' @export
newNotebook <- function(name="index", blank=FALSE, title="My Test Page", pageTitle=title, author="Author Name", toc=TRUE, css=NULL, conn=getOption("vdbConn"), overwrite=FALSE) {
   if(is.null(name))
      stop("Must specify name for new notebook")
      
   prefix <- conn$path
   
   filePath <- file.path(prefix, "notebook", paste(name, ".Rmd", sep=""))
   
   if(file.exists(filePath)) {
      if(overwrite) {
         ans <- readline(paste("The file '", name, ".Rmd' exists.  Are you sure you want to overwrite it? (y = yes)", sep=""))
      	if(!tolower(substr(ans, 1, 1)) == "y")
      	   stop("Backing out...")
      } else {
         stop(paste("File ", filePath, " already exists"))         
      }
   }
      
   pkgPath <- system.file(package="trelliscope")
   template <- file.path(pkgPath, "rmd_template.Rmd")
   
   fileString <- ifelse(blank, "", paste(readLines(template), collapse="\n"))
   fileString <- paste(paste("```{r, echo=FALSE}
# do not remove this block - set parameters accordingly
bsSetup(
   pageTitle = \"", pageTitle, "\", 
   title     = \"", title, "\",
   author    = \"", author, "\",
   toc       = \"", toc, "\",
   css       = ", ifelse(is.null(css), "NULL", paste("\"", css, "\"", sep="")), "
)
```
", sep=""), fileString, collapse="\n", sep="")
   
   cat(fileString, file=filePath)
   message("New notebook available for editing at ", filePath)
}

#' View a Notebook File
#'
#' Use \code{\link{browseURL}} to open a notebook .html file that is in the "notebook" directory of the vdb directory and that has typically been created with \code{\link{newNotebook}} and typeset with \code{\link{typeset}}.
#'
#' @param name name of the notebook file (.html extension not required)
#' @param conn VDB connection info, typically stored in options("vdbConn") at the beginning of a session, and not necessary to specify here if a valid "vdbConn" object exists
#'
#' @author Ryan Hafen
#' @seealso \code{\link{newNotebook}}, \code{\link{typeset}}
#'
#' @export
viewNotebook <- function(name="index", conn=getOption("vdbConn"), local=TRUE) {
   # TODO: look at this: http://jeffreyhorner.tumblr.com/post/33814488298/deploy-rook-apps-part-ii
   # (when in local mode, only works on safari)
   
   if(is.null(name))
      stop("Must specify name for notebook")
      
   if(local) {
      prefix <- conn$path
      
      if(grepl("~", prefix))
         prefix <- path.expand(prefix)
         
      if(!grepl("\\.html$", name))
         name <- paste(name, ".html", sep="")
         
      filePath <- file.path(prefix, "notebook", name)
      
      browseURL(filePath)
      
      # launch shiny
      view(openBrowser=FALSE)
   } else {
      # build URL for web server and launch
   }
}

# this just makes an href to be shown inline that sends you to the plot
# vdbLink <- function(name, group)

# if desc is null, it pulls from the displayList

# vdb_string <- function(x, path, showthumbs, thumb_max_width, png_viewer) 

## internal
mediaListStr <- function(hrefs, src, name, group, desc, n, height, width) {
   if(desc=="")
      desc <- "-- no description --"
   paste("
<div class='media'>
  <!--<a class='pull-left' href='#'>-->
    <img class='media-object pull-left img-polaroid' src='", src, "' height='", height, "' width='", width, "' target='_blank'>
  <!--</a>-->
  <div class='media-body'>
    <h4 class='media-heading vdblisthead'>", group, " / ", name, "</h4>
    <p><span class='vdblisttext'>", desc, "</span><br />
    <span class='vdbhreftext'>", hrefs, "</span><br />
    <span class='vdblisttext2'>(", n, " panels)</span></p>
  </div>
</div>", sep="")
}

## internal
thumbnailStr <- function(href, src, name, group, desc, height, width) {
   paste("
<div class='span9' style='margin:0 auto; float:none;'>
<ul class='thumbnails'>
  <li>
    <div class='thumbnail'>
      <a href='", href, "' target='_blank' title='", name, ": ", desc, "'>
      <img src='", href, "' alt='' width='", width, "' height='", height, "' class='img-polaroid'>
      </a>
      <h4 class='media-heading vdblisthead'>", group, " / ", name, "</h4>
      <p>", desc, "</p>
    </div>
  </li>
</ul>
</div>\n\n", sep="")
}

## internal
checkPlotExists <- function(displayList, name, group=NULL) {
   errStr <- ""
   nms <- sapply(displayList, function(x) x$name)
   gps <- sapply(displayList, function(x) x$group)
   if(is.null(group)) {
      curPlot <- which(nms == name)
   } else {
      curPlot <- which(nms == name & gps == group)
      errStr <- paste(" from group \"", group, "\"", sep="")
   }
   
   if(length(curPlot) == 0) {
      errMsg(paste("The plot \"", name, "\"", errStr, " wasn't found.", sep=""))
      return(NULL)
   } else if (length(curPlot) > 1) {
      if(is.null(group)) {
         errMsg(paste("There is more than one plot of name \"", name, "\".  Try specifying a group as well.", sep=""))
         return(NULL)
      } else {
         errMsg(paste("There is more than one plot of name \"", name, "\" from group \"", group, "\".  This should not be possible"))
         return(NULL)
      }
   } else {
      return(curPlot)
   }
}

## internal
errMsg <- function(message) {
   cat(paste("
   <div class=\"alert alert-error\">
      <button type=\"button\" class=\"close\" data-dismiss=\"alert\">&times;</button>
      <strong>Oh snap!</strong> ", message, "
   </div>", sep=""
   ))
}

# name <- "irisTest1.1"
# name <- c("irisTest1.1", "irisTest1.2")
# group <- NULL
# conn <- getOption("vdbConn")

# nbDisplayList(c("irisTest1.1", "irisTest1.2"))

## internal
makeHref <- function(group, name, type, server=NULL) {
   href <- ""
   if(type=="simple") {
      href <- paste("<a href='../displays/", group, "/", name, "/thumb.png' target='_blank'>view</a>", sep="")
   } else if(type=="shiny") {
      href <- paste("<a class='displayHref' href=\"#group=", group, "&name=", name, "\" target='_blank'>view</a>", sep="")
   }
   href
}

#' Make a List of Displays in a Notebook
#' 
#' Make html code to display a list of displays in a web notebook.  To be used inside of a notebook .Rmd file.
#' 
#' @param name a vector of display names to be in the list
#' @param group (needs to be fixed...)
#' @param conn VDB connection info, typically stored in options("vdbConn") at the beginning of a session, and not necessary to specify here if a valid "vdbConn" object exists
#' 
#' @author Ryan Hafen
#' 
#' @seealso \code{\link{typeset}}, \code{\link{newNotebook}}
#' 
#' @export
nbDisplayList <- function(name, group=NULL, conn=getOption("vdbConn")) {
   prefix <- conn$path
   
   thumbHeight <- conn$thumbHeight
   if(is.null(thumbHeight))
      thumbHeight <- 120
   
   load(file.path(prefix, "displays", "_displayList.Rdata"))
   
   plotIdx <- sapply(seq_along(name), function(x) 
      checkPlotExists(displayList, name[x], group))
   plotIdx <- plotIdx[!sapply(plotIdx, is.null)]
   
   # TODO: if desc is given, use that
   if(length(plotIdx) > 0) {
      tabStr <- sapply(plotIdx, function(x) {
         
         p <- displayList[[x]]
         displayType <- ifelse(grepl("^kv", p$dataClass), "shiny", "simple")
         href <- makeHref(p$group, p$name, displayType)
         
         curWidth <- thumbHeight * p$width / p$height
         src <- paste("../displays/", p$group, "/", p$name, "/thumb.png", sep="")
         return(mediaListStr(href, src, p$name, p$group, p$desc, p$n, thumbHeight, curWidth))
      })
      cat(paste("<div class='thumbnail' markdown='1'>", paste(tabStr, collapse="<hr class='disphr' />"), "\n</div>\n\n<p></p>", sep=""))
   }
}

#' Make Single Display in a Notebook
#' 
#' Make html code to show a large plot of a single display in a web notebook.  To be used inside of a notebook .Rmd file.  Meant for single-panel displays.
#' 
#' @param name a vector of display names to be in the list
#' @param group (needs to be fixed...)
#' @param conn VDB connection info, typically stored in options("vdbConn") at the beginning of a session, and not necessary to specify here if a valid "vdbConn" object exists
#' 
#' @author Ryan Hafen
#' 
#' @seealso \code{\link{typeset}}, \code{\link{newNotebook}}
#' 
#' @export
nbDisplay <- function(name, group=NULL, conn=getOption("vdbConn")) {
   prefix <- conn$path
   
   maxHeight <- conn$maxHeight
   if(is.null(maxHeight))
      maxHeight <- 500
   
   load(file.path(prefix, "displays", "_displayList.Rdata"))
   
   plotIdx <- checkPlotExists(displayList, name, group)
   
   if(length(plotIdx) > 0) {
      p <- displayList[[plotIdx]]
      curWidth <- maxHeight * p$width / p$height
      fileToPlot <- file.path(prefix, "displays", p$group, p$name, "thumb.png")
      displayType <- ifelse(grepl("^kv", p$dataClass), "shiny", "simple")
      href <- paste("..", "displays", p$group, p$name, "thumb.png", sep="/")
      src <- fileToPlot
      cat(thumbnailStr(href, src, p$name, p$group, p$desc, maxHeight, curWidth))
   }
}


# <span class="label label-info">test</span></a>

#    p <- displayList[plotIdx[x],]
#    ahrefStr1 <- paste("<a href='../trelliscopeViewer_cs/viewer.html?group=", p$group, "&plot=", p$name, "' target='_blank'>", sep="")
#    ahrefStr2 <- "</a>"
#    paste(
#       "<tr><td>", p$desc, "</td><td>",
#       ahrefStr1,
#       "<img src='../displays/", p$group, "/", p$name, "/thumb.png' height='", thumbHeight, "' width='", thumbHeight * p$width / p$height, "'>",
#       ahrefStr2,
#       "</td></tr>",
#    sep="")
# })
# tabStr <- paste(
#    "<table cellpadding='0' cellspacing='0' border='0' class='table table-condensed table-striped table-bordered'><thead><tr><th>Description</th><th>Plot</th></tr></thead>",
#    paste(tabStr, collapse="\n", sep=""),
#    "</table>",
#    collapse="\n", sep=""
# )
# cat(tabStr)

   # grepl()

   # paste("<img src='../displays/common/irisTest1.1/'>")