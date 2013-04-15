
## internal
encodePNG <- function(plotLoc) {
   bytes <- file.info(plotLoc)$size
   b64 <- base64encode(readBin(plotLoc, "raw", n = bytes))
   paste("data:image/png;base64,", b64, sep = "")   
}
