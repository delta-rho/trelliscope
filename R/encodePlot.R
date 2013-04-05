
## internal
mongoEncodePlot <- function(plotLoc, key, remove=TRUE) {
   # could do readBin and mongo.bson.buffer.append.raw
   buf <- mongo.bson.buffer.create()
   mongo.bson.buffer.append(buf, "_id", key)
   mongo.bson.buffer.append(buf, "plot", encodePNG(plotLoc))
   # mongo.insert(mongoConn, mongoNS, list(plot=b64))
   file.remove(plotLoc)
   mongo.bson.from.buffer(buf)   
}

## internal
encodePNG <- function(plotLoc) {
   bytes <- file.info(plotLoc)$size
   b64 <- base64encode(readBin(plotLoc, "raw", n = bytes))
   paste("data:image/png;base64,", b64, sep = "")   
}
