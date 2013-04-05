getCdName <- function(uid, appHash, displayList, verbose) {
   # appHash <- "#group=common&plot=irisRhTest1"
   if(!is.na(uid)) {
      if(uid!="") {
         # browser()
         tmp <- displayList[displayList$uid == uid,]
         logMsg("Display ", tmp$group, " / ", tmp$name, " chosen from display modal", verbose=verbose)
         return(c(tmp$group, tmp$name))
      } 
   } else if(appHash!="") {
      appHash <- strsplit(appHash, "&")[[1]]
      appHash <- gsub(".*=(.*)", "\\1", appHash)         
      logMsg("Display ", appHash[1], " / ", appHash[2], " specified by app hash", verbose=verbose)
      return(appHash)
   }
   return(NULL)         
}
