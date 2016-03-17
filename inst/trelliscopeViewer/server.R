suppressPackageStartupMessages(library(shiny, warn.conflicts = FALSE))
suppressPackageStartupMessages(library(dplyr, warn.conflicts = FALSE))
# htmlwidgets not required unless panel function is one
suppressPackageStartupMessages(suppressWarnings(require(htmlwidgets, warn.conflicts = FALSE)))
suppressPackageStartupMessages(library(jsonlite, warn.conflicts = FALSE))

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
  vdbConn <- vdbConn(vdbDir, updateFiles = FALSE)
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

# untar data if there is any
dataTar <- file.path(vdbPrefix, "data.tar")
if(file.exists(dataTar)) {
  utils::untar(dataTar, exdir = vdbPrefix)
  unlink(dataTar)
}

source(file.path(vdbPrefix, "server/_fns.R"), local = TRUE)

shinyServer(function(input, output, session) {
  source(file.path(vdbPrefix, "server/currentDisplay.R"), local = TRUE)
  source(file.path(vdbPrefix, "server/visualFilters.R"), local = TRUE)
  source(file.path(vdbPrefix, "server/cogState.R"), local = TRUE)
  source(file.path(vdbPrefix, "server/cogTable.R"), local = TRUE)
  source(file.path(vdbPrefix, "server/panelTable.R"), local = TRUE)
  source(file.path(vdbPrefix, "server/misc.R"), local = TRUE)
})
