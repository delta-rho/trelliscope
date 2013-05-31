options(vdbConn = list(
   ## local machine
   vdbName = @@vdbName@@,
   vdbPrefix = @@vdbPrefix@@,
   defaultStorage = @@defaultStorage@@,
   
   ## hadoop backend
   # hadoopConn = list(
   #    # default HDFS location for vdb plots to go
   #    hdfsPrefix = @@hdfsPrefix@@,
   #    # location of a hadoop bundle (see genLocalBundleFiles in Rhipe) for communication with hadoop cluster
   #    hadoopBundleDir = @@hadoopBundleDir@@,
   #    # an alternative to hadoopBundleDir: specify locations of hadoop directories necessary for communication with cluster
   #    HADOOP_CONF_DIR = @@HADOOP_CONF_DIR@@,
   #    HADOOP_HOME = @@HADOOP_HOME@@,
   #    HADOOP_LIBS = @@HADOOP_LIBS@@
   #    # if environment variables are already set up, environment variables and hadoopBundleDir can be NULL
   # ),
   
   ## mongo
   # mongoConn = list(
   #    mongoHost = @@mongoHost@@,
   #    mongoName = @@@mongoName@@,
   #    mongoUser = @@mongoUser@@,
   #    mongoPass = @@mongoPass@@
   # ),
   
   ## web server
   # webConn = list(
   #    # IP address of web server (need passwordless ssh to this address)
   #    ip = @@ip@@,
   #    # user of 
   #    user = @@user@@,
   #    # URL at which shiny-server is broadcasting
   #    url = @@url@@, 
   #    port = @@port@@, # port the shiny-server is configured to broadcast on
   #    # shiny app directory on web server
   #    appDir = @@appDir@@, 
   # )
))



