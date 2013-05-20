#' Create Input Variables for a VDB Display
#'
#' Create a 'inputVars' object which can be passed to the 'inputVars' argument of \code{\link{makeDisplay}} to create per-panel input variables in the VDB viewer.
#'
#' @param name name of the input variable
#' @param type variable type - must be "text", "textarea", or "select" (dropdown) - (checkbox to come?)
#' @param dataType the desired type of the data for this variable - must be "character", or "numeric"
#' @param default the default value for the variable to take prior to user input
#' @param args a character vector of dropdown values if type="select"
#'
#' @author Ryan Hafen
#'
#' @seealso \code{\link{makeDisplay}}
#' 
#' @export
makeInputVars <- function(name, type, dataType="character", default="", description="Input variable", args=NULL) {
   if(!type %in% c("text", "textarea", "select"))
      stop("Input type must be 'text', 'textarea', or 'select'")
   
   res <- list(name=name, type=type, default=default, description=description, args=args)
   class(res) <- c("inputVars", "list")
   res
}
