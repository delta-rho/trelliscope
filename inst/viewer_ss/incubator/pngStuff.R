### is it faster to write a png to disk, read it in, and encode
### or to write it to memory and encode?

# http://stackoverflow.com/questions/7171523/in-r-how-to-plot-into-a-memory-buffer-instead-of-a-file

library(Cairo)
library(png)
library(ggplot2)


toMemory <- function(dim=500) {
   Cairo(file='/dev/null', width=dim, height=dim)

   qplot(rnorm(5000)) # your plot

   # hidden stuff in Cairo
   i = Cairo:::.image(dev.cur())
   r = Cairo:::.ptr.to.raw(i$ref, 0, i$width * i$height * 4)
   dim(r) = c(4, i$width, i$height) # RGBA planes
   # have to swap the red & blue components for some reason
   r[c(1,3),,] = r[c(3,1),,]
   # now use the png library
   p = writePNG(r, raw()) # raw PNG bytes

   res <- base64enc:::base64encode(p)   
}

toDisk <- function(dim=500) {
   ff <- tempfile()
   Cairo(file=ff, width=dim, height=dim)
   qplot(rnorm(5000)) # your plot
   dev.off()

   bytes <- file.info(ff)$size
   res <- base64enc:::base64encode(readBin(ff, "raw", n = bytes))
}


system.time(toMemory())
system.time(toDisk())



