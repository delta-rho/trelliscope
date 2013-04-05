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

