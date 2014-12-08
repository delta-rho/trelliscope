library(shiny, warn.conflicts = FALSE)
library(jsonlite, warn.conflicts = FALSE)

connFile <- "conn.Rdata"
vdbDir <- getwd()

LOGGING <- FALSE
if(Sys.getenv("TRELLISCOPE_LOGGING") == "true")
   LOGGING <- TRUE

logMsg <- function(...) {
   text <- list(...)
   if(LOGGING)
      message(paste(c("* ", text), sep=""))
}

if(file.exists(connFile)) {
   library(trelliscope) # TODO: check viewer / package versions match
   vdbConn <- vdbConn(vdbDir)
   vdbPrefix <- vdbConn$path
} else {
   vdbConn <- getOption("vdbConn")
   vdbPrefix <- vdbConn$path
}

logMsg("vdbPrefix is ", vdbPrefix)

options(vdbShinyPrefix = vdbPrefix)

load(file.path(vdbPrefix, "displays/_displayList.Rdata"))

ind <- which(is.na(displayListDF$dataClass))
if(length(ind) > 0) {
   displayList <- displayList[-ind]
   displayListDF <- displayListDF[-ind,]
}

source("server/_fns.R")

shinyServer(function(input, output, session) {
   source("server/currentDisplay.R", local = TRUE)
   source("server/visualFilters.R", local = TRUE)
   source("server/cogState.R", local = TRUE)
   source("server/cogTable.R", local = TRUE)
   source("server/panelTable.R", local = TRUE)
})
