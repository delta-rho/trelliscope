## internal
vdbWriteCogJson <- function(prefix, group, name, cog, cogdesc, inputVars, height, width, pp) {
   
	if(!is.null(inputVars)) {
      # append inputVars to the cognostics table
      for(i in seq_along(inputVars)) {
         cog <- data.frame(cog, newvar=inputVars[[i]]$default, stringsAsFactors=FALSE)
         names(cog)[ncol(cog)] <- inputVars[[i]]$name
         class(cog[,ncol(cog)]) <- inputVars[[i]]$dataType
      }
      
      # append cognostic variable descriptions
      cogdesc <- c(cogdesc, sapply(inputVars, function(x) x$description))
	}
   
   cog2json <- function(x) {
      res <- paste(
         "{ \"aaData\": [\n",
         paste(
            apply(x, 1, function(y) paste("[\"", paste(y, collapse="\",\"", sep=""), "\"]", sep="")),
         collapse=",\n"
         ),
         "\n]", 
         sep=""
      )
      colClasses <- sapply(1:ncol(x), function(i) class(x[,i]))
      # TODO: what to do if it's not one of these classes?
      rTypes <- c(
         "integer",
         "numeric",
         "factor",
         "character",
         "Date",
         "POSIXct"
      )   
      tabTypes <- c(
         "numeric_ignore_nan",
         "numeric_ignore_nan",
         "string",
         "string",
         "date",
         "date"
      )
      # for column filters:
      cfTypes <- c(
         "\"type\": \"number-range\"",
         "\"type\": \"number-range\"",
         "\"type\": \"text\", \"bRegex\": true, \"bSmart\": true",
         "\"type\": \"text\", \"bRegex\": true, \"bSmart\": true",
         "\"type\": \"date-range\"",
         "\"type\": \"date-range\""
      )
      
      # add aoColumns
      tabClasses <- as.character(
         sapply(colClasses, function(x) tabTypes[which(rTypes == x)])
      )
      
      res <- paste(res, ", \"aoColumns\": [\n",
         paste("{\"sType\": \"", tabClasses, "\"}", sep="", collapse=",\n"),
         "\n]", sep=""
      )
      
      # add cfColumns
      cfClasses <- as.character(
         sapply(colClasses, function(x) cfTypes[which(rTypes == x)])
      )
      res <- paste(res, ", \"cfColumns\": [\n",
         paste("{", cfClasses, "}", sep="", collapse=",\n"),
         "\n]", sep=""
      )
      
      # add column names
      res <- paste(res, ", \"colNames\": [\"", 
         paste(names(cog), collapse="\",\"", sep=""), "\"\n]",
         sep=""
      )
      
      # add column descriptions, if given
      if(is.null(cogdesc)) {
         res <- paste(res, "}")
      } else {
         res <- paste(res, ", \"colDesc\": [\"", 
            paste(cogdesc, collapse="\",\"", sep=""), "\"\n]",
            sep=""
         )
      }
      
      # add input column information, if given
      if(is.null(inputVars)) {
         res <- paste(res, "}")
      } else {
         res <- paste(res, ", \"inputVars\": [\n", 
				paste(sapply(inputVars, function(x) {
					tmp <- paste("{ \"name\": \"", x$name, "\", \"type\": \"", x$type, "\"", sep="")
					if(!is.null(x$args)) {
						tmp <- paste(tmp, ", \"args\": [\"", paste(x$args, collapse="\", \"", sep=""), "\"]}", sep="")
					} else {
						tmp <- paste(tmp, "}")
					}
					tmp
				}), collapse=",\n", sep=""),
				"\n]}",
				sep=""
			)
      }
      res
   }
   
   baseDir <- prefix
   
   if(!is.null(cog)) {
      cogjson <- cog2json(cog)
      dir.create(file.path(baseDir, "json"))
      cat(cogjson, file=file.path(baseDir, "json", "cog.json"))
   }

   meta <- 
      paste("{\"aspect\": [\"", height / width, "\"], ",
         "\"baseDir\": [\"", file.path(group, name), "\"], ",
         "\"nPanels\": [\"", pp, "\"]",
      "}", sep=""
   )
   
   cat(meta, file=file.path(baseDir, "json", "meta.json"))
}

