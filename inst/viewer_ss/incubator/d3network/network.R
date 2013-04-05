### This file has a function to turn data of class "network" (from statnet)
### into .json that can be visualized using d3.
### This will be useful if the user supplies cognostics that indicate
### relationships with other subsets - an optional cognostic viewer
### of an interactive network plot

library(statnet)

data("faux.magnolia.high")
fmh <- faux.magnolia.high
plot(fmh, displayisolates = FALSE, vertex.col = "Grade", vertex.cex = 0.7)

# get a feel for attributes
list.vertex.attributes(fmh)
get.vertex.attribute(fmh, "Grade")
list.edge.attributes(fmh)

data("emon")

a <- makeD3NetworkData(fmh)
cat(a, file="data/call_me_al.json")

a <- makeD3NetworkData(emon[[1]])

makeD3NetworkData <- function(dat) {
   makeJObj <- function(keys, vals, isChar) {
      vals[isChar] <- paste("\"", vals[isChar], "\"", sep="")

      paste(c("{", 
         paste(
            paste("   \"", keys, "\":", vals, sep=""), collapse=",\n"), "}"
         ), collapse="\n"
      )
   }

   nodes <- sapply(dat$val, function(x) {
      nms <- names(x)
      nmsInd <- which(nms != "na")
      isChar <- sapply(x[nmsInd], function(x) is.character(x) || is.na(x))
      vals <- do.call(c, x[nmsInd])
      nms <- nms[nmsInd]
      nms[nms=="vertex.names"] <- "id"
      makeJObj(nms, vals, isChar)
   })

   nodes <- paste(nodes, collapse=",\n")

   nodeNames <- sapply(dat$val, function(x) {
      x$vertex.names
   })

   melNULL <- sapply(dat$mel, is.null)
   mel <- dat$mel[!melNULL]

   links <- sapply(mel, function(x) {
      nms <- names(x$atl)
      nmsInd <- which(nms != "na")
      isChar <- do.call(c, lapply(x[nmsInd], function(x) is.character(x) || is.na(x)))
      vals <- do.call(c, x[nmsInd])
      nms <- nms[nmsInd]

      nms <- c("source", "target", nms)
      isChar <- c(TRUE, TRUE, isChar)
      vals <- c(nodeNames[x$inl], nodeNames[x$outl], vals)

      makeJObj(nms, vals, isChar)
   })

   links <- paste(links, collapse=",\n")

   paste("{\"nodes\": [\n",
      nodes,
      "\n],\"links\": [\n",
      links,
      "\n]}",
      sep="", collapse=""
   )   
}


paste("   \"source\":\"", , "\",\n", "   \"target\":\"", nodeNames[x$outl], "\"", sep="", collapse="")


charInd <- sapply(x[nmsInd], is.character)
vals[charInd] <- paste("\"", vals[charInd], "\"", sep="")



