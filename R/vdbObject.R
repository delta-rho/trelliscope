

## internal
## ensures that a display exists and returns its name and group
findDisplay <- function(name, group = NULL, conn = getOption("vdbConn"), displayList = NULL) {
   if(is.null(displayList))
      load(file.path(conn$vdbPrefix, "displays", "_displayList.Rdata"))

   errStr <- ""
   if(is.null(group)) {
      curDisplay <- which(displayList$name==name)   
   } else {
      curDisplay <- which(displayList$name==name & displayList$group==group)
      errStr <- paste(" from group \"", group, "\"", sep="")
   }

   if(length(curDisplay) == 0) {
      stop(paste("The display \"", name, "\"", errStr, " wasn't found.", sep=""))
      return(NA)
   } else if (length(curDisplay) > 1) {
      if(is.null(group)) {
         stop(paste("There is more than one display of name \"", name, "\".  Try specifying a group as well.", sep=""))
         return(NA)
      } else {
         stop(paste("There is more than one display of name \"", name, "\" from group \"", group, "\".  This should not be possible"))
         return(NA)
      }
   } else {
      curDisplay <- displayList[curDisplay,]
      return(list(name=curDisplay$name, group=curDisplay$group))
   }
}

#' @export
getDisplay <- function(name, group=NULL, displayList=NULL, conn = getOption("vdbConn")) {
   displayInfo <- findDisplay(name=name, group=group, conn=conn, displayList=displayList)
   vdbPrefix <- vdbValidatePrefix(conn)
   cat(file.path(vdbPrefix, "displays", displayInfo$group, displayInfo$name, "object.Rdata"), "\n")
   load(file.path(vdbPrefix, "displays", displayInfo$group, displayInfo$name, "object.Rdata"))
   displayObj
}


