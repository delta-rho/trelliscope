library(shiny)
library(jsonlite) # TODO: make trelliscope depend on this

# connFile <- "../conn.Rdata"
# vdbDir <- normalizePath(file.path(getwd(), ".."))
# hostname <- system("hostname", intern = TRUE)
# things are a little different on glimmer because app can't be in subdirectory
# if(hostname == "glimmer.rstudio.com") {
connFile <- "conn.Rdata"
vdbDir <- getwd()
# }

if(file.exists(connFile)) {
   library(trelliscope)
   vdbConn <- vdbConn(vdbDir)
   vdbPrefix <- vdbConn$path
} else {
   vdbConn <- getOption("vdbConn")
   vdbPrefix <- vdbConn$path
}
message("vdbPrefix is ", vdbPrefix)

options(vdbShinyPrefix = vdbPrefix)

LOGGING <- FALSE
if(Sys.getenv("TRELLISCOPE_LOGGING") == "true")
   LOGGING <- TRUE

logMsg <- function(...) {
   text <- list(...)
   if(LOGGING)
      message(paste(c("* ", text), sep=""))
}

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
