if(getRversion() >= "2.15.1") {
  utils::globalVariables(c("displayList", "displayListDF", "displayListNames"))
}



#' S3method print displayObj
#' @param x TODO
#' @param ... TODO
print.displayObj <- function(x, ...) {
   cat("display object...")
}

#' Retrieve Display Object from VDB
#'
#' Retrieve a display object from a VDB.
#'
#' @param name the name of the display
#' @param group the group of the display
#' @param conn VDB connection info, typically stored in options("vdbConn") at the beginning of a session, and not necessary to specify here if a valid "vdbConn" object exists
#'
#' @details If a display is uniquely determined by its name, then group is not required.
#'
#' @author Ryan Hafen
#'
#' @seealso \code{\link{makeDisplay}}, \code{\link{removeDisplay}}
#' @export
getDisplay <- function(name, group = NULL, conn = getOption("vdbConn")) {

   load(file.path(conn$path, "displays", "_displayList.Rdata"))

   displayInfo <- findDisplay(name = name, group = group, conn = conn)
   vdbPrefix <- conn$path

   load(file.path(vdbPrefix, "displays", displayInfo$group, displayInfo$name, "displayObj.Rdata"))

   # if it is a local disk connection, the location can change
   # this happens when we move things to a web server
   if(inherits(displayObj$panelDataSource, "kvLocalDisk")) {
      cn <- getAttribute(displayObj$panelDataSource, "conn")
      if(!file.exists(cn$loc)) {
         tmp <- file.path(conn$path, "displays", displayObj$group, displayObj$name, "panels")
         if(!file.exists(tmp))
            tmp <- file.path(conn$path, "data", basename(cn$loc))
         if(file.exists(tmp)) {
            if(inherits(displayObj$panelDataSource, "ddf")) {
               displayObj$panelDataSource <- ddf(localDiskConn(tmp, reset = TRUE, verbose = FALSE), verbose = FALSE)
            } else {
               displayObj$panelDataSource <- ddo(localDiskConn(tmp, reset = TRUE, verbose = FALSE), verbose = FALSE)
            }
         }
      }
   }

   displayObj
}

#' Remove a Display from a VDB
#'
#' Remove a display from a VDB.
#'
#' @param name the name of the display
#' @param group the group of the display
#' @param conn VDB connection info, typically stored in options("vdbConn") at the beginning of a session, and not necessary to specify here if a valid "vdbConn" object exists
#' @param verbose logical - print messages about what is being done
#'
#' @details If a display is uniquely determined by its name, then group is not required.
#'
#' @author Ryan Hafen
#'
#' @seealso \code{\link{makeDisplay}}
#' @export
removeDisplay <- function(name = NULL, group = NULL, conn = getOption("vdbConn"), verbose = TRUE) {
   load(file.path(conn$path, "displays", "_displayList.Rdata"))

   displayInfo <- findDisplay(name, group, conn)
   vdbPrefix <- conn$path

   displayList[paste(displayInfo$group, displayInfo$name, sep = "_")] <- NULL

   ind <- which(
      displayListDF$name == displayInfo$name
         & displayListDF$group == displayInfo$group)

   displayListDF <- displayListDF[-ind,]

   unlink(file.path(vdbPrefix, "displays", displayInfo$group, displayInfo$name), recursive = TRUE)

   save(displayList, displayListDF, displayListNames, file = file.path(conn$path, "displays", "_displayList.Rdata"))

   if(verbose)
      message("* Display removed successfully")
}

# TODO: listDisplays function (print list of displays)
# TODO: cleanupDisplays function (remove _bak displays)

## internal
## ensures that a display exists and returns its name and group
findDisplay <- function(name, group = NULL, conn = getOption("vdbConn")) {
   load(file.path(conn$path, "displays", "_displayList.Rdata"))

   errStr <- ""
   if(is.null(group)) {
      curDisplay <- which(displayListDF$name == name)
   } else {
      curDisplay <- which(displayListDF$name == name & displayListDF$group == group)
      errStr <- paste(" from group \"", group, "\"", sep = "")
   }

   if(length(curDisplay) == 0) {
      stop(paste("The display \"", name, "\"", errStr, " wasn't found.", sep = ""))
      return(NA)
   } else if (length(curDisplay) > 1) {
      if(is.null(group)) {
         stop(paste("There is more than one display of name \"", name, "\".  Try specifying a group as well.", sep = ""))
         return(NA)
      } else {
         stop(paste("There is more than one display of name \"", name, "\" from group \"", group, "\".  This should not be possible"))
         return(NA)
      }
   } else {
      curDisplay <- displayListDF[curDisplay,]
      return(list(name = curDisplay$name, group = curDisplay$group))
   }
}

#' List Displays in a VDB
#'
#' List displays in a VDB.
#'
#' @param conn VDB connection info, typically stored in options("vdbConn") at the beginning of a session, and not necessary to specify here if a valid "vdbConn" object exists
#'
#' @author Ryan Hafen
#'
#' @seealso \code{\link{makeDisplay}}, \code{\link{addDisplay}}, \code{\link{removeDisplay}}, \code{\link{view}}
#' @export
listDisplays <- function(conn = getOption("vdbConn")) {
   load(file.path(conn$path, "displays", "_displayList.Rdata"))

   tmp <- as.matrix(displayListDF[,c("name", "group", "desc", "n", "dataClass")])
   rownames(tmp) <- NULL
   # tmp[,"updated"] <- substr(tmp[,"updated"], 1, 16)
   tmp[is.na(tmp[,"dataClass"]),"dataClass"] <- "none (R plot)"
   tmp <- tmp[order(tmp[,"group"], tmp[,"name"]),,drop = FALSE]

   nc <- ncol(tmp)
   sepWidth <- (nc - 1) * 3

   headers <- colnames(tmp)

   colWidths <- apply(tmp, 2, function(x) max(nchar(x)))
   colWidths <- pmax(colWidths, nchar(headers))

   totWidth <- getOption("width")

   excess <- (totWidth - sepWidth) - sum(colWidths)
   # (totWidth - sepWidth) - (sum(colWidths) - colWidths["desc"])
   if(excess < 0) {
      descCut <- excess + colWidths["desc"]
      if(descCut < 3) {
         tmp <- tmp[,which(colnames(tmp) != "desc")]
      } else {
         ell <- ifelse(tmp[,"desc"] == "", "", "...")
         tmp[,"desc"] <- paste(substr(tmp[,"desc"], 1, descCut - 3), ell, sep = "")
      }
   }
   headers <- colnames(tmp)
   colWidths <- apply(tmp, 2, function(x) max(nchar(x)))
   colWidths <- pmax(colWidths, nchar(headers))
   nc <- length(headers)

   fmtStr <- paste(paste("%", colWidths, "s", sep = ""), collapse = " | ")

   cat(paste(c(
      do.call(sprintf, c(list(fmt = fmtStr), as.list(headers))),
      paste(sapply(colWidths, function(x) paste(rep("-", x), collapse = "")), collapse = "-+-"),
      apply(tmp, 1, function(x) {
      do.call(sprintf, c(list(fmt = fmtStr), as.list(x)))
   })), collapse = "\n"))

}

