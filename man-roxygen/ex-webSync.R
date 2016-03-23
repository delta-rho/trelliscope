library(ggplot2)

vdbConn(tempfile(), autoYes = TRUE)

# make a simple display
d <- divide(iris, by = "Species")
makeDisplay(d, name = "sl_vs_sw",
  panelFn = function(x)
    qplot(Sepal.Width, Sepal.Length, data = x))

\dontrun{
# to sync to a server 'myshinyserver.org' with login 'user'
# need: passwordless ssh for user@myshinyserver.org)
# need: rsync installed on local machine
# (these should both be easy to do with local linux / OS X)

# set up a connection to a shiny server
webConn(user = "hafen", ip = "myshinyserver.org", name = "myapp")

# webSync() uses rsync to sync your local vdb
# to the one on your shiny server pointed to with webConn()
webSync()

# if shiny server is running on the remote on port 3838
# then the VDB will now be viewable at
browseURL("http://myshinyserver.org:3838/myapp")
}
