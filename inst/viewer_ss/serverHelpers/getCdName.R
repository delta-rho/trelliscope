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
      vals <- getFromAppHash(appHash, c("group", "name"))
      
      logMsg("Display ", vals$group, " / ", vals$name, " specified by app hash", verbose=verbose)
      if(sum(sapply(vals, length)) == 2)
         return(c(vals$group, vals$name))
   }
   return(NULL)         
}

getFromAppHash <- function(appHash, names) {
   appHash <- strsplit(appHash, "&")[[1]]
   appHash <- gsub("^#(.*)", "\\1", appHash)
   keys <- gsub("(.*)=.*", "\\1", appHash)
   vals <- gsub(".*=(.*)", "\\1", appHash)

   ind <- which(keys %in% names)

   res <- lapply(ind, function(x) {
      vals[x]
   })
   names(res) <- keys[ind]
   res
}

