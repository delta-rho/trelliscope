## internal
updateProgress <- function(value1, value2, start.time) {
   collapse <- function (x, sep = ",") {
      paste(x, collapse = sep)
   }

   catf <- function (..., file = "", append = FALSE, newline = TRUE) {
      cat(sprintf(...), ifelse(newline, "\n", ""), sep = "", file = file, append = append)
   }

   splitTime <- function (seconds, unit = c("years", "days", "hours", "minutes", "seconds")) {
      unit <- match.arg(unit)
      res <- c(years = NA, days = NA, hours = NA, minutes = NA, seconds = NA)
      divider <- c(31536000, 86400, 3600, 60, 1)
      start <- which(names(res) == unit)
      for (i in start:length(divider)) {
         res[i] <- seconds%/%divider[i]
         seconds <- seconds - res[i] * divider[i]
      }
      res
   }
   
   label.width <- 5
   max <- 100
   min <- 0

   width <- as.integer(Sys.getenv("COLUMNS"))
   if(is.na(width))
      width <- getOption("width")
   delta <- max - min

   bar.width <- width - label.width - 21
   bar1 <- rep(" ", bar.width)
   bar2 <- rep(" ", bar.width)

   if (value1 == max) {
      rate1 <- 1   
   } else {
      rate1 <- (value1 - min) / delta
   }

   if (value2 == max) {
      rate2 <- 1   
   } else {
      rate2 <- (value2 - min) / delta
   }

   bin1 <- round(rate1 * bar.width)
   bar1[seq(bin1)] <- "-"
   bin2 <- round(rate2 * bar.width)
   bar2[seq(bin2)] <- "-"

   delta.time <- as.integer(Sys.time()) - start.time
   if (value1 + value2 == min) {
      rest.time <- 0
   } else {
      rest.time <- (2*max - value1 - value2) * (delta.time / (value1 + value2 - 2*min))
   }
   rest.time <- splitTime(rest.time, "hours")
   if (rest.time["hours"] > 99) 
       rest.time[] <- 99

   space <- collapse(rep(" ", 10), "")
   msg1 = sprintf(sprintf("%%%is", label.width), "map   ")
   msg2 = sprintf(sprintf("%%%is", label.width), "reduce")
   catf("%s |%s| %3i%% %s %s |%s| %3i%% (%02i:%02i:%02i)", 
      newline = FALSE, 
      msg1, collapse(bar1, sep = ""), round(rate1 * 100), space,
      msg2, collapse(bar2, sep = ""), round(rate2 * 100), 
      rest.time["hours"], rest.time["minutes"], rest.time["seconds"]
   )
}

## internal
vdbRhStatus <- function(job) {
   if (class(job) != "jobtoken" && class(job) != "character") 
       stop("Must give a jobtoken object(as obtained from rhex)")
   if (class(job) == "character") {
      id <- job
   } else {
       job <- job[[1]]
       id <- job[["job.id"]]
   }

   start.time <- as.integer(Sys.time())

   width <- as.integer(Sys.getenv("COLUMNS"))
   if(is.na(width))
      width <- getOption("width")
   bb <- paste(rep("\b", 2*width - 1), collapse="")

   printed <- FALSE
   while (TRUE) {
      Sys.sleep(5)
      y <- Rhipe:::.rhstatus(id, autokill = TRUE, TRUE)
      
      p1 <- as.integer(y$progress$pct[1]*100)
      p2 <- as.integer(y$progress$pct[2]*100)
      
      if(printed)
         cat(bb)
      updateProgress(p1, p2, start.time)
      flush.console()
      
      if (!y$state %in% c("PREP", "RUNNING")) {
         cat("\n")
         break
      }
      printed <- TRUE
   }
   y
}


# map <- expression({
#    rhcollect("1", rnorm(n))
# })
# 
# rhwrite(list(1, 1), "/tmp/asdf2")
# 
# z <- rhmr(map, ifolder="/tmp/asdf2", ofolder="/tmp/asdf3", mapred=list(mapred.map.tasks=4, mapred.reduce.tasks=2), parameters=list(n=10))
# 
# rhex(z, async=FALSE)
# 
# rhJob <- rhex(z)
# 
# rhstatus(rhJob)

# tmp <- withCallingHandlers(suppressMessages(capture.output(job <- rhex(z))))
# 
# tmp <- suppressMessages(capture.output({
#    job <- rhex(z)
#    Sys.sleep(0.5)
# }))
# 
# 
# vdbRhStatus(job)
# 
# 




# width <- as.integer(Sys.getenv("COLUMNS"))
# if(is.na(width))
#    width <- getOption("width")
# bb <- paste(rep("\b", 2*width - 1), collapse="")
# for(i in 1:10) {
#    Sys.sleep(0.1)
#    if(i > 1)
#       cat(bb)
#    updateProgress(i*10, i*10/2, start.time)
#    flush.console()
#    if(i == 10)
#       cat("\n")
# }
# 



# 
# 
# 
# testit <- function(x = sort(runif(20)), ...)
# {
#     pb <- txtProgressBar(...)
#     for(i in c(0, x, 1)) {Sys.sleep(0.5); setTxtProgressBar(pb, i)}
#     Sys.sleep(1)
#     close(pb)
# }
# 
# testit(style=3, char="-")
# 
# 
# library(BBmisc)
# mapbar <- myProgressBar(max=5, label="map   ", char="-")
# redbar <- myProgressBar(max=5, label="reduce", char="-")
# for (i in 0:5) {
#   mapbar$set(i)
#   redbar$set(i)
#   Sys.sleep(0.6)
# }
# 
