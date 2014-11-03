Version 0.8.0.2
---------------------------------------------------------------------

FEATURES / CHANGES

- remove strict dependency on scagnostics package
- add validation for name and group arguments in `makeDisplay()` and `addDisplay()`

BUG FIXES

- fix `vdbConn()` to properly retrieve VDB name from previous connection
- fix `makePNG()` to deal with "Date" class axis limits for lattice
- fix `addDispay()` to properly deal with 

Version 0.8.0
---------------------------------------------------------------------

FEATURES / CHANGES

- add state specification - specify default state through 
  `makeDisplay()`, viewing state when calling `view()`, or through 
  specifying a URL hash in viewer
- add `cogDisplayHref()` function for creating cognostic links to other
  displays

BUG FIXES

- fix cog sort/filter table column visibility dependence on active cogs
- fix hiding related display layout when other control panels are clicked
- fix column highlighting in cog table sort/filter when loading from state
- fix footer histograms when bin start/end values are not integers
- fix view() not working with name specified only (no state)
- fix issue with zero values in filter being ignore when restoring state
- fix issue with cognostics having too many levels for select drop-downs
- fix a few bugs in `deployToShinyApps()`
- fix bug in label, sort, filter state not resetting on display change
- fix bug in marginal distribution checking for viewer cog table

Version 0.7.9.1
-------------------------------------------------------------------------------

FEATURES / CHANGES

- add experimental support for rCharts panels
- add experimental support for deploying to shinyapps.io

BUG FIXES

- load packages and related data objects for related displays
- fix opening new display when current state is showing related displays
- fix path in webSync for changing app permissions on server


Version 0.7.9
-------------------------------------------------------------------------------

FEATURES / CHANGES

- add experimental support for rendering vega specs as panels (currently 
  through ggvis, in the future through more general means)
- don't allow selectpickers in cog table when there are too many levels (slows 
  things way down)
- when in related display mode, update dimensions when adding panel labels
- when in related display mode, disable panel layout with a note
- when there is no selectpicker, only send regex back to R
- use datadr's new `getGlobals()` method for panel and cognostic functions
- remove custom `runApp()` function to deal with passing parameters to 
  trelliscope viewer - will handle this through R options in the future
- enable `webSync()` to sync to the same machine without need for ssh
  by setting `ip` to NULL (default) when calling `webConn()`
- remove MongoDB cognostics connection code for now - it was not up to date and
  it is uncertain whether it will be able to do everything required of it
- remove elnino data to reduce package size - will put that in separate package
- if using in-memory ddo, but it is very large, convert it to local disk 
  connection in makeDisplay to help memory management when viewing
- add conditional / marginal (renamed to "filtered" / "all") in visual filters
  so that current filter state can be reflected in the visual filter plot 
  (previously was just "all")

Version 0.7.8
-------------------------------------------------------------------------------

FEATURES / CHANGES

- add "group" argument to cognostics to help organization in the case of many 
  cognostics
- add "href" cognostic type (wraps string in href tag, useful for panel labels)
- add custom viewer support for dev mode, settable through 
  TRELLISCOPE_DEV_APP_PREFIX environment variable
- add logging to R console, settable with TRELLISCOPE_LOGGING environment 
  variable
- add accessible web browser javascript log to viewer, available in info modal
- add active cogs panel - if there are many cognostics, you can downselect to 
  the ones you want to be active
- add keyboard shortcuts for opening control panels
- for performance reasons, now categorical cognostics with too many unique 
  values will not have a selectpicker in the cognostics table
- experimental support for "related displays" - view multiple displays made 
  against the same division simultaneously and control the layout
- added travis.ci support
- added infrastructure to do javascript unit tests
- addition of some simple R and javascript unit tests

BUG FIXES

- fix bug with quantile computation with NAs
- fix width of open display modal
- fix arrow key shortcuts to only operate when appropriate
- fix thumbnail generation to yield small files to speed up viewer startup
- fix robustness issues with page input in viewer


