

newNotebook(title="Airplane data exploratory analysis")



cat(paste(readLines(file.path(vdbDir, "notebook", "index.Rmd"))[1:10], collapse="\n"))



# set up a web connection
wc <- webConn(user="rhafen", ip="glimmer.rstudio.com", appDir="~/ShinyApps", name="vdbexample2")


