############################################################################
### mongoCogConn constructor / methods
############################################################################

#' Initiate a MongoDB Cognostics Connection
#' 
#' Initiate a MongoDB cognostics connection, to be passed as the \code{cogConn} argument to \code{\link{makeDisplay}}.
#' 
#' @param host,user,pass,name,db parameters used to initiate MongoDB connection (see \code{\link{mongo.create}})
#' 
#' @return a "cogConn" object of class "cogMongoConn"
#' @author Ryan Hafen
#' @seealso \code{\link{makeDisplay}}
#' 
#' @export
mongoCogConn <- function(host="127.0.0.1", user="", pass="", name="", db="") {
   
   conn <- list(
      host = host,
      name = name,
      user = user,
      pass = pass,
      db   = db
   )
   
   # test the connection
   mongoConn <- mongoConnect(list(conn=conn))
   stopifnot(mongo.get.err(mongoConn) == 0)
   mongo.disconnect(mongoConn)
   
   structure(list(conn=conn), class=c("mongoCogConn", "cogConn"))
}

#' @S3method print mongoCogConn
print.mongoCogConn <- function(x, ...) {
   cat(paste("mongoCogConn connection: host=", x$conn$host, "; db=", x$conn$db, sep=""))
}

#' @S3method cogPre mongoCogConn
cogPre.mongoCogConn <- function(cogConn, vdbConn, group, name, ...) {
   # clear out previous cognostics collection for this display
   mongoConn <- mongoConnect(cogConn)
   coll <- mongoCollName(vdbConn$name, group, name, "cog")
   mongo.drop(mongoConn, coll)
   mongo.disconnect(mongoConn)
}

#' @S3method cogEmit mongoCogConn
cogEmit.mongoCogConn <- function(cogConn, data, vdbConn, group, name) {
   # add to mongo collection
   # 'flatten' the cog list
   data <- lapply(data, function(x) {
      c(x[1], x$splitVars, x$bsv, x$cog)      
   })
   
   cogDatBson <- lapply(data, mongo.bson.from.list)
   mongoConn <- mongoConnect(cogConn)
   coll <- mongoCollName(vdbConn$name, group, name, "cog")
   
   mongo.insert.batch(mongoConn, coll, cogDatBson)
   res <- mongo.get.err(mongoConn)
   mongo.disconnect(mongoConn)
   res
}

# do nothing
#' @S3method cogCollect mongoCogConn
cogCollect.mongoCogConn <- function(cogConn, ...) {
   NULL 
}

# add indexes and return mongoCogDatConn object
#' @S3method cogFinal mongoCogConn
cogFinal.mongoCogConn <- function(cogConn, jobRes, conn, group, name, cogEx, ...) {
   
   mongoConn <- mongoConnect(cogConn)
   coll <- mongoCollName(conn$name, group, name, "cog")
   
   cogNames <- names(cogEx)
   
   for(i in seq_along(cogEx)) {
      if(cogNames[i] == "panelKey") {
         mongo.index.create(mongoConn, coll, cogNames[i], c(mongo.index.unique, mongo.index.background))
      } else if(inherits(cogEx[[i]], "cogGeo")) {
         ll <- list("2d")
         names(ll) <- cogNames[i]
         mongo.index.create(mongoConn, coll, ll, mongo.index.background)
      } else if(length(cogEx[[i]]) == 1) {
         mongo.index.create(mongoConn, coll, cogNames[i], mongo.index.background)
      }
   }
   mongo.disconnect(mongoConn)
   
   mongoCogDatConn(cogConn, coll)
}

############################################################################
### mongoCogDatConn constructor / methods
############################################################################

# internal (called from cogFinal.mongoCogConn)
mongoCogDatConn <- function(cogConn, coll, qry=NULL, srt=NULL) {
   mongoConn <- mongoConnect(cogConn)
   ex <- mongoCog2DF(mongo.bson.to.list(mongo.find.one(mongoConn, coll))[-1])
   ex <- data.frame(ex)
   
   structure(list(
      conn=cogConn,
      coll=coll,
      qry=qry,
      srt=srt,
      ncol=ncol(ex),
      nrow=mongo.count(mongoConn, coll, query=mongo.bson.empty()),
      ex=ex
   ),
   class = "mongoCogDatConn")
}

#' @S3method cogNcol mongoCogDatConn
cogNcol.mongoCogDatConn <- function(x) {
   x$ncol
}

#' @S3method cogNrow mongoCogDatConn
cogNrow.mongoCogDatConn <- function(x) {
   x$nrow
}

#' @S3method cogNames mongoCogDatConn
cogNames.mongoCogDatConn <- function(x) {
   names(x$ex)
}

#' @S3method getCogData mongoCogDatConn
getCogData.mongoCogDatConn <- function(x, rowIdx, colIdx) {
   if(is.null(x$qry))
      x$qry <- mongo.bson.empty()
   
   if(is.null(x$srt))
      x$srt <- mongo.bson.empty()
   
   mongoConn <- mongoConnect(x$conn)
   
   crs <- mongo.find(mongoConn, x$coll, skip=as.integer(rowIdx[1] - 1), sort=x$srt, query=x$qry)
   res <- list()
   for(i in seq_along(rowIdx)) {
      mongo.cursor.next(crs)
      tmp <- mongoCog2DF(mongo.bson.to.list(mongo.cursor.value(crs))[-1])
      res[[i]] <-
as.data.frame(tmp, stringsAsFactors=FALSE)
   }

   mongo.disconnect(mongoConn)
   
   do.call(rbind, res)[, colIdx, drop=FALSE]
}

#' @S3method getCurCogDat mongoCogDatConn
getCurCogDat.mongoCogDatConn <- function(cogDF, flt, ordering, colIndex, verbose=FALSE) {
   ex <- cogDF$ex
   exNames <- names(ex)[colIndex]
   
   # build ordering part of query
   srt <- mongo.bson.empty()
   if(!is.null(ordering)) {
      # need to know which columns are visible so we are sorting the right column
      ordIdx <- which(ordering != 0)
      if(length(ordIdx) > 0) {
         ordIdx <- ordIdx[order(ordering[ordIdx])]
         srt <- list()
         for(i in ordIdx) {
            srt[[exNames[colIndex[i]]]] <- as.integer(sign(ordering[i]))
         }
      }
   }
   
   # now build filtering part
   qry <- mongo.bson.empty()

   if(!is.null(flt)) {
      logMsg("Updating cognostic filter index", verbose=verbose)
      flt <- processFilterInput(flt)

      if(length(flt) > 0) {
         qry <- list()

         for(i in seq_along(flt)) {
            cur <- flt[[i]]
            curName <- exNames[as.integer(cur[2])]
            if(cur[1] == "from") {
               if(is.null(qry[[curName]])) {
                  qry[[curName]] <- list("$gte"=as.numeric(cur[3]))
               } else {
                  qry[[curName]][["$gte"]] <- as.numeric(cur[3])
               }
            } else if(cur[1] == "to") {
               if(is.null(qry[[curName]])) {
                  qry[[curName]] <- list("$lte"=as.numeric(cur[3]))
               } else {
                  qry[[curName]][["$lte"]] <- as.numeric(cur[3])
               }
            } else if(cur[1] == "regex") {
               qry[[exNames[as.integer(cur[2])]]] <-  list("$regex"=cur[3])
            }
         }
      }
   }
   # crs <- mongo.find(mongoConn, NS, query=qry) #, sort=srt)
   # mongo.cursor.next(crs)
   # mongo.cursor.value(crs)
   cogDF$qry <- qry
   cogDF$srt <- srt
   
   mongoConn <- mongoConnect(cogDF$conn)
   nr <- mongo.count(mongoConn, cogDF$coll, query=cogDF$qry)
   mongo.disconnect(mongoConn)
   cogDF$nrow <- nr
   
   return(cogDF)
}

############################################################################
### mongo-specific helper functions
############################################################################

# connect to mongodb
mongoConnect <- function(conn) {
   tmpcapt <- suppressMessages(capture.output(require(rmongodb)))
   mongo.create(
      host = conn$conn$host,
      name = conn$conn$name,
      username = conn$conn$user,
      password = conn$conn$pass,
      db = conn$conn$db
   )
}

# write list of cognostics to mongodb
mongoSaveCognostics <- function(cogDat, group, name, conn) {
   mongoConn <- vdbMongoInit(conn)
   coll <- mongoCollName(conn$vdbName, group, name, "cog")
   mongo.insert.batch(mongoConn, coll, cogDat)
   res <- mongo.get.err(mongoConn)
   mongo.disconnect(mongoConn)
   res
}

# convert data read in from mongodb to data.frame
mongoCog2DF <- function(x) {
   lns <- sapply(x, length)
   lns <- which(lns > 1)
   for(j in lns) {
      x[[j]] <- as.list(x[[j]])
   }
   x
}

# construct mongodb collection name
mongoCollName <- function(coll, ...) {
   res <- paste(..., sep="_")
   res <- gsub("\\.", "_", res)
   paste(coll, res, sep=".")
}
