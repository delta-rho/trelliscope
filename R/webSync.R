#' Sync VDB files to a web server
#'
#' @param vdbConn VDB connection settings
#' @param webConn web connection settings
#' @param fixPermissions should an attempt be made to fix permissions in the web directory?
#' @param verbose show rsync output
#' @param rsync location of rsync binary
#'
#' @details This requires rsync to be installed on your machine.  If you are syncing via ssh, this only works if public key authentication is enabled between your local machine and the remote server.
#'
#' @seealso \code{\link{webConn}}, \code{\link{syncLocalData}}
#'
#' @example man-roxygen/ex-webSync.R
#' @export
webSync <- function(
  vdbConn = getOption("vdbConn"),
  webConn = getOption("vdbWebConn"),
  fixPermissions = FALSE,
  verbose = FALSE,
  rsync = NULL
) {
  # sync local vdb directory to web server's vdb directory
  if(is.null(rsync))
    rsync <- findRsync()

	# TODO: check to make sure passwordless ssh is set up

  if(is.null(webConn)) {
    message("* webConn has not been set up... attempting default config...")
    webConn <- webConn()
  }

  message("*** Copying latest viewer to vdb directory...")
  copyViewerFiles(vdbConn)

  syncLocalData(vdbConn)

  if(is.null(webConn$ip)) {
    sshFlag <- ""
    ip <- ""
  } else {
    sshFlag <- "-e ssh "
    ip <- paste(webConn$ip, ":", sep = "")
  }
  user <- ""
  if(!is.null(webConn$user))
    user <- paste(webConn$user, "@", sep = "")

  message("* Syncing local vdb directory to web server...")

  lns <- capture.output(system(paste(rsync, " -a -v ", sshFlag,
    path.expand(vdbConn$path), "/* ",
    user, ip, webConn$serverDir, "/", webConn$name,
    sep = ""
  ), intern = TRUE, ignore.stderr = FALSE, ignore.stdout = FALSE))
  if(verbose)
    cat(paste(c("*** Output ***", lns), collapse = "\n"))

  ## make a copy from viewerDir to serverDir/vdbName
  pkgPath <- system.file(package = "trelliscope")

  message("* Syncing latest shiny viewer to web app directory...")

  lns <- capture.output(system(paste(rsync, " -a -v ", sshFlag,
    file.path(pkgPath, "trelliscopeViewer/*"), " ",
    user, ip, webConn$serverDir, "/", webConn$name,
    sep = ""
  ), intern = TRUE, ignore.stderr = FALSE, ignore.stdout = FALSE))
  if(verbose)
    cat(paste(c("*** Output ***", lns), collapse = "\n"))

  # attempt to fix permissions
  if(fixPermissions) {
    if(is.null(webConn$ip)) {
      sshString <- ""
      sshQuote <- ""
    } else {
      sshString <- paste("ssh ", user, webConn$ip, " ", sep = "")
      sshQuote <- "'"
    }
    message("* Attempting to fix permissions...")
    lns <- capture.output(system(paste(sshString, sshQuote, "sudo chown -R shiny ", webConn$serverDir, sshQuote, sep = ""), intern = TRUE, ignore.stderr = FALSE, ignore.stdout = FALSE))
    if(verbose)
      cat(paste(c("*** Output ***", lns), collapse = "\n"))
  }

  NULL
}

#' Sync localDisk objects to VDB
#'
#' Sync localDisk data that is used for VDB displays located throughout the system to a 'data' directory inside the VDB - useful for collecting data before syncing with a web server, and used inside of \code{\link{webSync}} and \code{\link{deployVDB}}.
#'
#' @param vdbConn VDB connection settings
#' @param rsync location of rsync binary
#'
#' @seealso \code{\link{webSync}}, \code{\link{webConn}}
#'
#' @examples
#' library(ggplot2)
#'
#' vdbConn(tempfile(), autoYes = TRUE)
#'
#' # divide iris data and store it as a local disk connection
#' d <- divide(iris, by = "Species",
#'   output = localDiskConn(tempfile(), autoYes = TRUE))
#'
#' # make a simple display using this data
#' makeDisplay(d, name = "sl_vs_sw",
#'   panelFn = function(x)
#'     qplot(Sepal.Width, Sepal.Length, data = x))
#'
#' # look at files in our VDB directory
#' list.files(getVdbPath())
#' # since the data for our display resides in a different path
#' # if we try to share this file with someone on another machine
#' # the data will not be found
#'
#' # sync any local data objects used in any display to be
#' # centralized with the VDB directory, making it portable
#' \dontrun{
#' syncLocalData() # requires rsync
#' }
#'
#' # now there is a "data" directory that holds all local disk data
#' list.files(getVdbPath())
#' @export
syncLocalData <- function(vdbConn = getOption("vdbConn"), rsync = NULL) {
  if(is.null(rsync))
    rsync <- findRsync()

  validateVdbConn(vdbConn, mustHaveDisplays = TRUE)

  # if there are localDisk objects, put them in VDB
  load(file.path(vdbConn$path, "displays", "_displayList.Rdata"))
  ldDisp <- subset(displayListDF, dataClass == "kvLocalDisk")
  if(nrow(ldDisp) > 0) {
    message("* Syncing localDisk objects to vdb 'data' directory...")

    newDatDir <- file.path(vdbConn$path, "data")
    if(!file.exists(newDatDir))
      dir.create(newDatDir, recursive = TRUE)

    locs <- sapply(seq_len(nrow(ldDisp)), function(i) {
      tmp <- getDisplay(ldDisp$name[i], ldDisp$group[i])
      datadr::getAttribute(tmp$panelDataSource, "conn")$loc
    })

    locs <- locs[!grepl(normalizePath(vdbConn$path), locs)]
    if(length(locs) > 0) {
      locs <- unique(locs)
      if(any(duplicated(basename(locs))))
        stop("Currently can't sync multiple localDisk data sets with same name but different paths.")

      for(ll in locs) {
        system(paste(rsync, "-avh", ll, newDatDir))
      }
    }
  }
}

findRsync <- function(rsync = NULL, verbose = "FALSE") {

  # http://www.rsync.net/resources/howto/windows_rsync.html

  errorMsg <- "rsync executable not found. Use 'rsync' argument to specify the correct path."

  if (is.null(rsync))
    rsync = "rsync"

  rsync = Sys.which(rsync)
  if (rsync == "" || rsync == "rsync")
    stop(errorMsg)

  if (.Platform$OS == "windows") {
    if (length(grep("rtools", tolower(rsync))) > 0) {
      rsync.ftype <- shell("ftype rsync", intern = TRUE)
      if (length(grep("^rsync=", rsync.ftype)) > 0) {
        rsync <- sub('^rsync="([^"]*)".*', "\\1", rsync.ftype)
      }
    }
  }

  if (verbose) cat("Using rsync at", rsync, "\n")

  rsync
}

#' Deploy VDB to shinyapps.io or RStudio Connect
#'
#' @param vdbConn A vdbConn object containing the VDB connection settings
#' @param appName name of application (app will be available at https://[account].shinyapps.io/[appName]/ or at https://beta.rstudioconnect.com/[account]/[appName] if using RStudio connect) - if not supplied, will use the name of VDB connection
#' @param account passed to \code{rsconnect::configureApp}
#' @param redeploy passed to \code{rsconnect::configureApp}
#' @param size passed to \code{rsconnect::configureApp}
#' @param instances passed to \code{rsconnect::configureApp}
#' @param quiet passed to \code{rsconnect::configureApp}
#'
#' @details If you do not have a shinyapps.io account and have not set your account info, first visit here prior to calling this function: \url{http://shiny.rstudio.com/articles/shinyapps.html}.
#'
#' \code{\link{syncLocalData}}
#'
#' @export
#' @importFrom rsconnect configureApp deployApp
deployVDB <- function(
  vdbConn = getOption("vdbConn"),
  appName = NULL, account = NULL, redeploy = TRUE,
  size = NULL, instances = NULL, quiet = FALSE) {

  # check arguments
  stopifnot(inherits(vdbConn, "vdbConn"),
    is.logical(redeploy),
    is.logical(quiet))

  # get the appName
  if(is.null(appName)) {
    if(!is.null(vdbConn$name)) {
      appName <- vdbConn$name
    } else {
      stop("Must either supply 'appName' or set the name of your VDB in vdbConn()")
    }
  }

  # checks for the appName
  stopifnot(is.character(appName))

  # substitute spaces with hyphens
  if(grepl("\\ ", appName)) {
    message("*** Spaces in 'appName' will be replaced with hyphens")
    appName <- gsub("\\ ", "-", appName)
  }

  # verify appName has at least 4 characters, per the error message returned by rsconnect::deployApp()
  if(nchar(appName) < 4) {
    stop("'appName' must be at least four characters")
  }

  # check for reserved characters, per the error message returned by rsconnect::deployApp()
  if(grepl("[^-a-zA-Z0-9_]", appName)) {
    stop("'appName' may only contain letters, numbers, hyphens, and underscores")
  }

  # make sure viewer files are copied
  message("*** Copying latest viewer to vdb directory...")
  copyViewerFiles(vdbConn)

  message("*** Syncing local data...")
  syncLocalData(vdbConn)

  # if there is a data directory, tar it up
  wd <- getwd()
  setwd(vdbConn$path)
  on.exit({
    if(file.exists("data.tar"))
      utils::untar("data.tar")
    setwd(wd)
  })

  if(file.exists("data")) {
    utils::tar("data.tar", files = "data")
    if(file.exists("data.tar"))
      unlink("data", recursive = TRUE)
  }

  vdbDir <- vdbConn$path
  load(file.path(vdbDir, "displays", "_displayList.Rdata"))

  message("*** Configuring app...")
  try(rsconnect::configureApp(appName = appName, account = account, redeploy = redeploy, size = size, instances = instances, quiet = quiet))

  message("\n*** Getting package dependencies...")
  pkgs <- as.vector(do.call(c, lapply(displayList, function(x) {
    a <- getDisplay(x$name, x$group)
    a$relatedPackages
  })))

  pkgs <- sort(unique(c(pkgs, "datadr", "trelliscope")))

  depFile <- file.path(vdbDir, "dependencies.R")
  if(file.exists(depFile))
    file.remove(depFile)

  cat(paste("library(", pkgs, ")\n", sep = "", collapse = ""), file = depFile)

  message("*** Deploying app...\n")
  rsconnect::deployApp(vdbDir, appName = appName, account = account)
}

