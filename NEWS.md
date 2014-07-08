trelliscope 0.7.8
-------------------------------------------------------------------------------

Features / changes:
- add "group" argument to cognostics to help organization in the case of many cognostics
- add "href" cognostic type (wraps string in href tag, useful for panel labels)
- add custom viewer support for dev mode, settable through TRELLISCOPE_DEV_APP_PREFIX environment variable
- add logging to R console, settable with TRELLISCOPE_LOGGING environment variable
- add accessible web browser javascript log to viewer, available in info modal
- add active cogs panel - if there are many cognostics, you can downselect to the ones you want to be active
- add keyboard shortcuts for opening control panels
- for performance reasons, now categorical cognostics with too many unique values will not have a selectpicker in the cognostics table
- experimental support for "related displays" - view multiple displays made against the same division simultaneously and control the layout

Big fixes:
- fix bug with quantile computation with NAs
- fix width of open display modal
- fix arrow key shortcuts to only operate when appropriate
- fix thumbnail generation to yield small files to speed up viewer startup
- fix robustness issues with page input in viewer




