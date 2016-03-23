## functions that get template data in proper form
source("server/currentDisplayData.R", local = TRUE)

## list of displays to select from for current vdb
output$displayListOutput <- renderDataLite({
  displayListOutputData(displayList)
})
outputOptions(output, "displayListOutput", suspendWhenHidden = FALSE)

appHash <- reactive({
  str <- input$appHashInput
  if(!is.null(str)) {
    if(substr(str, 1, 1) == "#")
      str <- substr(str, 2, nchar(str))
    res <- parseQueryString(str)
    cls <- paste(names(res), "Hash", sep = "")
    for(i in seq_along(res))
      class(res[[i]]) <- c("character", cls[i])
    return(res)
  }
})

## holds name and group of currently selected display
selectedDisplay <- reactive({
  # priority: input, appHash, options
  sld <- input$displaySelectInput

  if(is.null(sld)) {
    sld <- appHash()
    if(!is.null(sld)) {
      sld$labels <- fromHash(sld$labels)
      sld$layout <- fromHash(sld$layout)
      sld$sort <- fromHash(sld$sort)
      sld$filter <- fromHash(sld$filter)
    }
  }

  # if appHash didn't have it:
  if(is.null(sld)) {
    sld <- getOption("trsCurrentViewState")
  }

  sld
})

# http://127.0.0.1:8100/#name=list_sold_vs_time&group=common&layout=ncol:2&sort=state:asc,slope:desc&filter=county:(regex:a),state:(select:AL;AR),slope:(from:0;to:1),meanList:(from:50)&labels=county,state,slope


# if(!is.null(currentViewState$name))
#   sld <- list(name = currentViewState$name,
#     group = currentViewState$group)
# options(trsCurrentViewState = list(group = "common", name = "list_sold_vs_time"))


## name of the display printed in the header
output$headerDisplayNameOutput <- renderText({
  sld <- selectedDisplay()
  if(!is.null(sld))
    paste(sld$group, "/", sld$name)
})

## holds the currently selected displayObject
currentDisplay <- reactive({
  sld <- selectedDisplay()

  if(!is.null(sld)) {
    logMsg("Loading display: ", paste(sld$group, "/", sld$name))
    cdo <- do.call(getDisplay, sld[c("name", "group")])
    logMsg("Display loaded")

    # handle RHIPE
    if(inherits(cdo$panelDataSource, "kvHDFS")) {
      if(!"Rhipe" %in% loadedNamespaces()) {
        if(!is.null(cdo$envs))
          try(do.call(Sys.setenv, cdo$envs))
        # hacky way to make sure rsconnect doesn't try to find Rhipe
        eval(parse(text = paste0("libra", "ry(", "Rhipe)")))
        rhinit()
      }
    }

    # load required packages
    if(!is.null(cdo$relatedPackages)) {
      logMsg("Loading packages: ", paste(cdo$relatedPackages, collapse = ", "))
      for(pkg in cdo$relatedPackages)
        suppressMessages(require(pkg, character.only = TRUE))
    }
    # could set up environment here instead of when panelFn is evaluated
    # in getPanels()

    if(is.null(cdo$cogDistns))
      cdo$cogDistns <- trelliscope:::getCogDistns(cdo$cogDatConn)

    logMsg("Getting default state...")

    if(!is.null(sld$labels))
      cdo$state$labels <- sld$labels
    if(is.null(cdo$state$labels)) {
      defaultLabels <- cdo$cogInfo$name[cdo$cogInfo$defLabel]
      class(defaultLabels) <- c(class(defaultLabels), "labelsState")
      if(length(defaultLabels) == 0)
        defaultLabels <- NULL
      cdo$state$labels <- defaultLabels
    }

    if(!is.null(sld$layout))
      cdo$state$layout <- sld$layout
    if(is.null(cdo$state$layout)) {
      lyt <- list(nrow = 1, ncol = 1, arrange = "row")
      class(lyt) <- c("list", "layoutState")
      cdo$state$layout <- lyt
    }

    cdo$state$sort <- sld$sort
    cdo$state$filter <- sld$filter

    if(is.null(cdo$state$activeCog)) {
      defaultActive <- cdo$cogInfo$name[cdo$cogInfo$defActive]
      if(length(defaultActive) == 0)
        defaultActive <- NULL
      cdo$state$activeCog <- defaultActive
    }

    # if(!is.null(currentViewState))
    #   options(trsCurrentViewState = NULL)
    list(cdo = cdo)
  }
})

## initial UI template outputs dependent on selected display

output$displayInformationOutput <- renderDataLite({
  displayInformationOutputData(currentDisplay())
})

output$panelLayoutOutput <- renderDataLite({
  panelLayoutOutputData(currentDisplay())
})

# output$panelFunctionOutput <- renderDataLite({
#   panelFunctionOutputData(currentDisplay())
# })

output$panelLabelListOutput <- renderDataLite({
  panelLabelListOutputData(currentDisplay())
})

output$relatedDisplayListOutput <- renderDataLite({
  relatedDisplayListOutputData(currentDisplay(), displayList)
})

output$activeCogListOutput <- renderDataLite({
  activeCogListOutputData(currentDisplay())
})

output$cogTableControlsOutput <- renderDataLite({
  # browser()
  cogTableControlsOutputData(currentDisplay())
})

output$cogUniFilterControlsOutput <- renderDataLite({
  cogUniFilterControlsOutputData(currentDisplay())
})

output$cogBiFilterControlsOutput <- renderDataLite({
  cogBiFilterControlsOutputData(currentDisplay())
})

output$cogMultiFilterControlsOutput <- renderDataLite({
  cogBiFilterControlsOutputData(currentDisplay())
})

output$cogSampleControlsOutput <- renderDataLite({
  list()
})

output$panelPageNavOutput <- renderDataLite({
  panelPageNavOutputData(currentDisplay())
})

output$cogMapOutput <- renderDataLite({
  cogBiFilterControlsOutputData(currentDisplay())
})

