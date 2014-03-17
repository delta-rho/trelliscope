

# convert byLatLon to a localDiskConn object
byLatLonLD <- convert(byLatLon, 
   localDiskConn("/private/tmp/byLatLon", autoYes=TRUE))



# make display using local disk connection as input
makeDisplay(byLatLonLD,
   name      = "co_vs_time_ld",
   group     = "co",
   desc      = "Plot of co vs. time for each geographic 'square' with with fitted line, illustrating the use of 'same' axis limits and a cognostics function, with the data source being a local disk connection",
   panelFn   = coPanelFn,
   cogFn     = coCogFn,
   lims      = coTimeLims
)



# convert byLatLon to hdfsConn
byLatLonHDFS <- convert(byLatLon, 
   hdfsConn("/tmp/byLatLon", autoYes=TRUE))



# make byLatLonHDFS subsets extractable by key
byLatLonHDFS <- makeExtractable(byLatLonHDFS)



# make display using local disk connection as input
makeDisplay(byLatLonHDFS,
   name      = "co_vs_time_hdfs",
   group     = "co",
   desc      = "Plot of co vs. time for each geographic 'square' with with fitted line, illustrating the use of 'same' axis limits and a cognostics function, with the data source being a HDFS connection",
   panelFn   = coPanelFn,
   cogFn     = coCogFn,
   lims      = coTimeLims
)


