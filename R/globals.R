# note: findGlobals and getGlobalVarList are used both in datadr and trelliscope
# I don't want to export them, so they are defined in both places
# example usage:
# globalVars <- findGlobals(panelFn)
# globalVarList <- getGlobalVarList(globalVars, parent.frame())

## internal
findGlobals <- function(f) {
   if(!is.function(f)) 
      return(character(0))

   tildeHandler <- codetools:::collectUsageHandlers[["~"]]
   remove("~", envir=codetools:::collectUsageHandlers)
   res <- codetools:::findGlobals(f, merge=FALSE)$variables
   assign("~", tildeHandler, envir=codetools:::collectUsageHandlers)
   res
}


getGlobalVarList <- function(globalVars, parentFrame) {
   globalVarList <- list()
   # look first on parent frame
   tmp <- intersect(globalVars, ls(envir=parentFrame))
   if(length(tmp) > 0) {
      for(i in seq_along(tmp)) {
         globalVarList[[tmp[i]]] <- get(tmp[i], parentFrame)
      }
   }
   # then look through all others
   for(env in search()) {
      tmp <- intersect(globalVars, ls(envir=as.environment(env)))
      if(length(tmp) > 0) {
         for(i in seq_along(tmp)) {
            if(is.null(globalVarList[[tmp[i]]]))
               globalVarList[[tmp[i]]] <- get(tmp[i], as.environment(env))
         }
      }
   }
   globalVarList
}
