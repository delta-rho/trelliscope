logMsg <- function(..., verbose=TRUE) {
   text <- list(...)
   if(verbose)
      message(paste(c("* ", text), sep=""))
}

viewerBootstrapPage <- function (...) {
   importBootstrap <- function(min = TRUE, responsive = TRUE) {
      ext <- function(ext) {
         ifelse(min, paste(".min", ext, sep = ""), ext)
      }
      cssExt <- ext(".css")
      jsExt = ext(".js")
      bs <- ""
      result <- tags$head(tags$link(rel = "stylesheet", type = "text/css", href = paste(bs, "css/bootstrap", cssExt, sep = "")), tags$script(src = paste(bs, "js/bootstrap", jsExt, sep = "")))
      if (responsive) {
         result <- tagAppendChild(result, tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"))
         result <- tagAppendChild(result, tags$link(rel = "stylesheet", type = "text/css", href = paste(bs, "css/bootstrap-responsive", cssExt, sep = "")))
      }
      result
   }
   tagList(importBootstrap(), list(...))
}

viewerPage <- function (preHeader, headerPanel, mainPanel) {
    viewerBootstrapPage(preHeader, div(class = "container-fluid", div(class = "row-fluid", 
        headerPanel), div(class = "row-fluid", 
        mainPanel)))
}

viewerHeaderPanel <- function() {
   div(class='navbar navbar-fixed-top',
   div(class='navbar-inner',
   div(class='container-fluid',
      div(id='displayNameHeader', class='span3 shiny-html-output'),
      HTML("<div class='span1'>
            <ul class='nav'>
               <li><a data-toggle='modal' href='#aboutModal' class='active'>About</a></li>
            </ul>
         </div>
         <div class='span2'>
            <form class='navbar-search' id='pppForm'>
               <input type='text' class='input-medium search-query' placeholder='panels per screen' id='pppInput'>
            </form>
         </div>"
      ),
      div(class='span3', id='navbuttons',
         div(class='btn-group no_selection',
            HTML("<a class='btn' id='btnBeg'>&laquo;</a>               
               <a class='btn' id='btnPrev'>&larr;</a>"
            ),
            tags$a(class='btn', id='btnCurrentPage', htmlOutput("currentPageText")),
            HTML("<a class='btn' id='btnNext'>&rarr;</a>
               <a class='btn' id='btnEnd'>&raquo;</i></a>"
            )
         )
      ),
      HTML("<div class='span3'>
               <a data-toggle='modal' href='#viewOptionsModal' class='btn btn-primary btn-small' id='variables_plot_button'>
               <!--<i class='icon-eye-open icon-white'></i> --> View</a>
               <a data-toggle='modal' href='#cogModal' class='btn btn-primary btn-small' id='cognostics_button'>
               <i class='icon-cog icon-white'></i> Cog</a>
               <div class='btn-group' id='cognostics_button'>
                 <a class='btn dropdown-toggle btn-primary btn-small' data-toggle='dropdown' href='#'>
                   <!--<i class='icon-picture icon-white'></i> -->Display
                   <span class='caret'></span>
                 </a>
                 <ul class='dropdown-menu'>
                   <li><a data-toggle='modal' href='#displayListModal'><i class='icon-list'></i> Change Display</a></li>
                   <li><a data-toggle='modal' href='#relatedDisplayModal'><i class='icon-th'></i> Add Related</a></li>
                 </ul>
               </div>
            </div>"
      )
   )))
}

tdSelectString <- function(selector) {
   paste("<script>
   $(function () {
      var isMouseDown = false,
      isHighlighted;
      $('", selector, "')
      .mousedown(function () {
         isMouseDown = true;
         if(!$(this).hasClass('not-numeric')) {
            $(this).toggleClass('highlighted');
            isHighlighted = $(this).hasClass('highlighted');         
         } else {
            // alert('hi');
         }
         return false; // prevent text selection
      })
      .mouseover(function () {
         if (isMouseDown && !$(this).hasClass('not-numeric')) {
            $(this).toggleClass('highlighted', isHighlighted);
         }
      })
      .bind('selectstart', function () {
         return false;
      })
      $(document)
      .mouseup(function () {
         isMouseDown = false;
      });
   });
   </script>", sep="")
}
# http://stackoverflow.com/questions/2013902/select-cells-on-a-table-by-dragging

hiddenDimInputs <- function() {
   div(style="display: none",
      numericInput("nRow", "", 1),
      numericInput("nCol", "", 1),
      numericInput("plotWidth", "", 0),
      numericInput("plotHeight", "", 0),
      numericInput("currentPage", "", 1),
      # numericInput("panelAspect", "", 1),
      htmlOutput("panelAspect"),
      htmlOutput("nPages")
   )
}

# a render function that simply passes the data to the browser
renderData <- function(expr, env=parent.frame(), quoted=FALSE) {
   # Convert the expression + environment into a function
   func <- exprToFunction(expr, env, quoted)
   
   function() {
      val <- func()
      val
   }
}

# gutted from renderPlot - used to build my own list of pngs to send in bulk
myRenderPlot <- function(expr, width, height, env = parent.frame(), quoted = FALSE, class="") {
   func <- exprToFunction(expr, env, quoted)
   
   png.file <- tempfile(fileext = ".png")

   if (width <= 0 || height <= 0) return(NULL)
   if (capabilities("aqua")) {
      pngfun <- png
   } else if (nchar(system.file(package = "Cairo"))) {
      require(Cairo)
      pngfun <- CairoPNG
   } else {
      pngfun <- png
   }
   pngfun(filename = png.file, width = width, height = height)
   on.exit(unlink(png.file))
   tryCatch(func(), finally = dev.off())
   bytes <- file.info(png.file)$size
   if (is.na(bytes)) return(NULL)
   pngData <- readBin(png.file, "raw", n = bytes)
   b64 <- base64encode(pngData)
   return(paste("<img class='", class, "' src=\"data:image/png;base64,", b64, "\" width=\"", width, "px\" height=\"", height, "px\">", sep = ""))
}


