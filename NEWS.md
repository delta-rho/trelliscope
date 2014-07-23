Version 0.7.8.1
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


