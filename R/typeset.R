#' Typeset a notebook (.Rmd) File
#'
#' Processes a .Rmd file created with \code{\link{newNotebook}}.  Basically a wrapper around knitr that additionally takes care of several layout issues to get a Bootstrap notebook web page.
#'
#' @param file file name (extension of .Rmd is not required)
#' @param conn VDB connection info, typically stored in options("vdbConn") at the beginning of a session, and not necessary to specify here if a valid "vdbConn" object exists
#'
#' @return output .html file goes in the "notebook" directory of the vdb directory, to be viewed with \code{\link{viewNotebook}}
#'
#' @author Ryan Hafen
#'
#' @seealso \code{\link{viewNotebook}}, \code{\link{newNotebook}}
#'
#' @export
typeset <- function(name="index", conn=getOption("vdbConn")) {
   prefix <- trsValidatePrefix(conn)
   
   fileBase <- sub("(.+)[.][^.]+$", "\\1", name)
   if(fileBase == name)
      name <- paste(name, ".Rmd", sep="")

   filePath <- file.path(prefix, "notebook", name)
   
   if(!file.exists(filePath))
      stop(paste("File", filePath, "doesn't exist"))

   message(paste("Processing file:", filePath))

   outFilePath <- file.path(prefix, "notebook", paste(fileBase, ".html", sep=""))
	
	tmpDir <- tempdir()

   out <- knit(input=filePath, output=file.path(tmpDir, "tmp.md"), envir = parent.frame())
   markdownToHTML(out, file.path(tmpDir, "tmp.html"), options=c("fragment_only", "toc", "base64_images", "use_xhtml"))
      
	preTOC <- readLines(file.path(tmpDir, "preTOC.html"))
	postTOC <- readLines(file.path(tmpDir, "postTOC.html"))
	postContent <- readLines(file.path(tmpDir, "postContent.html"))
	
	bodyContent <- readLines(file.path(tmpDir, "tmp.html"))
	
	tocLines <- bodyContent[1:min(which(bodyContent == ""))]
	tocStart <- which(tocLines == "<ul>")
	tocEnd <- which(tocLines == "</ul>")
	if(!(length(tocStart) > 0 && length(tocEnd) > 0)) {
	   warning("There are no headers for the table of contents")	   
      tocLines <- integer(0)
	} else {
	   tocStart <- min(tocStart)
	   tocEnd <- max(tocEnd)
   	tocLines <- tocLines[(tocStart + 1):(tocEnd - 1)]
	}
	
	prepend <- 0
	for(i in seq_along(tocLines)) {
		if(tocLines[i] == "<ul>") {
			prepend <- prepend + 10
			tocLines[i] <- ""
		} else if(tocLines[i] == "</ul>") {
			prepend <- prepend - 10
			tocLines[i] <- ""
		} else if(tocLines[i] == "<li>") {
			tocLines[i] <- paste("<li style='margin-left:", prepend, "px'>", sep="")
		}
	}
   
	bodyLines <- bodyContent[min(which(bodyContent == "")):length(bodyContent)]
	
	# fix stupid markdown problem:
	ind <- which(grepl("<p></div></p>", bodyLines))
   for(i in ind) {
      bodyLines[i] <- gsub("<p></div></p>", "</div>", bodyLines[i])
   }
   
	cat(paste(c(preTOC, tocLines, postTOC, bodyLines, postContent), collapse="\n"), file=outFilePath)	
}
