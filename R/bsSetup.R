#' Set Up Web Notebook Using Bootstrap
#'
#' Set up a Twitter Boostrap template for a notebook to be typeset with knitr.  This function should be called at the beginning of a .Rmd file.  If you use \code{\link{newNotebook}} to initialize a notebook .Rmd file, this will already be there.
#'
#' @param pageTitle the text to display on the title bar of the web browser
#' @param title the text to display on the header of the web page
#' @param author author
#' @param toc should a table-of-contents be included?
#' @param css path to additional style sheets, if desired
#'
#' @references
#' http://twitter.github.com/bootstrap/
#'
#' @author Ryan Hafen
#'
#' @seealso \code{\link{newNotebook}}, \code{\link{typeset}}
#'
#' @export
bsSetup <- function(
	pageTitle = "test",
	title = "test",
	author = "author name",
	toc = TRUE,
	css = NULL
) {

if(!is.null(css)) {
   cssPath <- file.path("assets/css/", css)
   if(!file.exists(cssPath))
      stop("File", cssPath, "does not exist")
   cssString <- paste("<link rel='stylesheet' type='text/css' href='", css, "'>", sep="")
} else {
   cssString <- ""
}

# TODO: add javascript as well

pages <- list.files(pattern=".html$")

pagesStr <- paste("<li class=''><a href='", pages, "' rel='tooltip' title='placeholder1'>", gsub("(.*)\\.html", "\\1", pages), "</a></li>", sep="", collapse="\n")

preTOC <- paste("<!DOCTYPE html>
	<html>
	<head>
	  <meta http-equiv='content-type' content='text/html; charset=UTF-8'>
	  <title>", pageTitle, "</title>

		<script type='text/javascript' src='assets/js/jquery.js'></script>
		<script type='text/javascript' src='assets/js/notebook.js'></script>
		<script type='text/javascript' src='assets/js/bootstrap.min.js'></script>

	   <link rel='stylesheet' type='text/css' href='assets/css/bootstrap.css'>
	   <link rel='stylesheet' type='text/css' href='assets/css/sidenav.css'>", cssString, "

      <link rel='stylesheet' href='assets/css/highlight/tomorrow.css'>
      <script src='assets/js/highlight.pack.js'></script>
      <script>hljs.initHighlightingOnLoad();</script>

      <script type=\"text/javascript\" src=\"assets/js/MathJax/MathJax.js?config=TeX-AMS-MML_HTMLorMML\"></script>

	<script type='text/javascript'>

	$(document).ready(function() {
	   tocOff = function() {
	      $('#sidebar').hide()
	      $('#content').removeClass('span9').addClass('span12')
	      $('#content').css('margin-left', 0)
	   }
	   tocOn = function() {
	      $('#sidebar').show()
	      $('#content').removeClass('span12').addClass('span9')
	      $('#content').css('margin-left', '21px')
	   }
		", ifelse(toc, "", "tocOff()"), "
		
		if(document.URL.match('file:///') == null) {
         $('.sslShiny').hide();
      } else {
         $('.ssShiny').hide();
      }
      
	});
	</script>

	</head>
	<body data-spy='scroll' data-offset='50' data-target='.sidebar-nav'>
	<div class='navbar navbar-fixed-top'>
	   <div class='navbar-inner'>
	     <div class='container-fluid'>
	       <a class='btn btn-navbar' data-toggle='collapse' data-target='.nav-collapse'>
	         <span class='icon-bar'></span>
	         <span class='icon-bar'></span>
	         <span class='icon-bar'></span>
	       </a>
	       <a class='brand' href='#'>", title, "</a>
          <div class='nav-collapse'>
             <ul class='nav'>		  
             <li class='mynavheadertext'>
             <a href='#'><i class='icon-home'></i> Home</a>
             </li>
             <li class='dropdown mynavheadertext'>
             <a href='#' class='dropdown-toggle' data-toggle='dropdown'><i class='icon-list'></i> Pages <b class='caret'></b></a>
             <ul class='dropdown-menu'>					
             ", pagesStr, "
             </ul>
             </li>			
             </ul>		
          </div>
	     </div>
	   </div>
	 </div>

	 <div class='container-fluid'>
	   <div class='row-fluid'>
	     <div class='span3' id='sidebar'>
	       <div class='well sidebar-nav sidebar-nav-fixed'>
	         <ul class='nav nav-list'>
	           <li class='nav-header'>Table of Contents</li>

", sep=""
)

postTOC <- paste("
    </ul>
  </div><!--/.well -->
</div><!--/span-->
<div class='span9' id='content'>
  <div class='row-fluid'>
", sep=""
)

postContent <- paste("
    </div><!--/row-->
  </div><!--/span-->
</div><!--/row-->

  <hr>

  <footer class='footer'>
    <p>&copy; ", author, " ", format(Sys.time(), "%Y"), ".</p>
  </footer>

</div><!--/.fluid-container-->
</html>
", sep="")

# tmpDir <- get("tmpDir")

# hopefully this is consistent (supposed to be per-session)
tmpDir <- tempdir()

cat(preTOC, file=file.path(tmpDir, "preTOC.html"))
cat(postTOC, file=file.path(tmpDir, "postTOC.html"))
cat(postContent, file=file.path(tmpDir, "postContent.html"))
}


