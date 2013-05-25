mongoCollName <- function(coll, ...) {
   res <- paste(..., sep="_")
   res <- gsub("\\.", "_", res)
   paste(coll, res, sep=".")
}

# clear out collections
mongoClear <- function(conn, group, name) {
   mongoConn <- vdbMongoInit(conn)
   
   mongoPanel <- mongoCollName(conn$vdbName, group, name, "panel")
   mongoCog <- mongoCollName(conn$vdbName, group, name, "cog")

   mongo.drop(mongoConn, mongoPanel)
   mongo.drop(mongoConn, mongoCog)

   mongo.disconnect(mongoConn)
}

mongoSaveCognostics <- function(cogDat, group, name, conn) {
   mongoConn <- vdbMongoInit(conn)
   coll <- mongoCollName(conn$vdbName, group, name, "cog")
   mongo.insert.batch(mongoConn, coll, cogDat)
   res <- mongo.get.err(mongoConn)
   mongo.disconnect(mongoConn)
   res
}

## internal
vdbMongoInit <- function(conn) {
   if(any(is.na(c(
      conn$mongoConn$mongoHost, conn$mongoConn$mongoName, conn$mongoConn$mongoUser, conn$mongoConn$mongoPass
   ))))
      stop("mongodb paramaters not set in conn")
   
   suppressMessages(require(caTools)) # for base64encode
   # rmongodb uses Rprintf and suppressMessages doesn't work
   tmpcapt <- suppressMessages(capture.output(require(rmongodb)))
   # TODO: error handling, defaults, etc.
   mongoConn <- mongo.create(
      host = conn$mongoConn$mongoHost,
      name = conn$mongoConn$mongoName,
      username = conn$mongoConn$mongoUser,
      password = conn$mongoConn$mongoPass,
      db = conn$mongoConn$vdbName
   )
   mongoConn
}

## internal
getMongoPlot <- function(mongoConn, mongoNS, key) {
   buf <- mongo.bson.buffer.create()
   mongo.bson.buffer.append(buf, "_id", key)
   query <- mongo.bson.from.buffer(buf)
   # buf <- mongo.bson.buffer.create()
   # mongo.bson.buffer.append(buf, "plot", 1L)
   # fields <- mongo.bson.from.buffer(buf)
   b <- mongo.find.one(mongoConn, mongoNS, query)
   mongo.bson.to.list(b)[["plot"]]
}

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

