bb <- tempfile()
vdbConn(bb, autoYes = TRUE)
a <- datadr::ddo(list(list(1, 1), list(2, 2)))
p <- function(x) xyplot(1~1)
makeDisplay(a, panelFn = p, name = "asdf")
pp <- getOption("vdbConn")$path
list.files(file.path(pp, "displays", "common"))
makeDisplay(a, panelFn = p, name = "asdf2")
listDisplays()
removeDisplay("asdf")
listDisplays()
removeDisplay("asdf2", autoYes = TRUE)
listDisplays()

list.files(file.path(pp, "displays", "common"))
list.files(file.path(pp, "displays"))

