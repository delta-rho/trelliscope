

function panelLayoutOutputApplyButton() {
   panelDims = $("#panel-layout-data").data("panelDims");
   
   if(panelDims != undefined) {
      var panelLayout = {
         "nrow" : parseInt($("#panel-rows").val()),
         "ncol" : parseInt($("#panel-cols").val()),
         "w"    : panelDims.w,
         "h"    : panelDims.h
      };
      
      $("#panelLayoutStateInput").data("myShinyData", panelLayout);
      $("#panelLayoutStateInput").trigger("change");
   }
}

function panelLayoutOutputCancelButton() {
   panelLayoutSetFromExposedState();
}

function panelLayoutSetFromExposedState() {
   
   var state = $("#exposedStateDataOutput").data("myShinyData");
   
   if(state) {
      if(state.panelLayout) {
         $("#panel-rows").val(state.panelLayout.nrow);
         $("#panel-cols").val(state.panelLayout.ncol);
         $("#panel-rows").trigger("change");
      }
   }
}

function panelLayoutOutputPostRender(data) {
   // actions for changing nrow and ncol
   $("#panel-rows").change(function() {
      panelLayoutPreview(parseInt($("#panel-rows").val()), parseInt($("#panel-cols").val()));
   });
   
   $("#panel-cols").change(function() {
      panelLayoutPreview(parseInt($("#panel-rows").val()), parseInt($("#panel-cols").val()));
   });

   $("#panel-rows").trigger("change");
   // call panel layout apply button to take number of labels, etc.
   // for some reason we have to do this to get width and height propogated...
   panelLayoutOutputApplyButton();
   
   // $("#panel-rows").val(data.nrow[0]);
   // $("#panel-cols").val(data.ncol[0]);
   // 
   // panelLayoutPreview(parseInt($("#panel-rows").val()), parseInt($("#panel-cols").val()));
}


function getPanelDims(nRow, nCol) {
   // var nRow = 2;
   // var nCol = 3;
   
   var panelAspect = $("#panel-layout-data").data("panelAspect");
   if(panelAspect) {
      var tPad = 3; // padding on either side of the panel
      var cogHeight = 30; // height of a row of cog output
      var nCog = $(".panel-labels-select.active").length; // number of cogs to show
      // extra padding beyond what is plotted
      // these remain fixed while width and height can change
      var wExtra = 2 + 2 * tPad; // 2 for border + tPad on either side
      var hExtra = 2 + 2 * tPad + nCog * cogHeight; // 2 for border + tPad on top / bottom + cogHeight for every row of visible cognostics
      
      var pageWidth = $("#panelTableContentOutput").width();
      var pageHeight = $(window).height() -
         (51 + 7) - // header height + padding
         (32 + 7); // footer height + padding
      var pageAspect = pageHeight / pageWidth;
            
      // first try stretching panels across full width:
      var newW = Math.round((pageWidth - (wExtra * nCol)) / nCol, 0);
      // given this, compute panel height
      var newH = Math.round(newW * panelAspect, 0);
      
      // did we preserve aspect ratio?
      // newH / newW;
      
      // check to see if this will make it too tall:
      // if so, do row-first full-height stretching
      var fullWidth = true; // does the current layout fill the available width?
      if((newH + hExtra) * nRow > pageHeight) {
         newH = Math.round((pageHeight - (hExtra * nRow)) / nRow, 0);
         newW = Math.round(newH / panelAspect, 0);
         fullWidth = false;
      }
      
      // check height
      // (newH + hExtra) * nRow
      
      // check width
      // (newW + wExtra) * nCol
      
      var allPanelsAspect = ((newH + hExtra) * nRow) / ((newW + wExtra) * nCol);
      
      var result = {};
      result.h = newH;
      result.w = newW;
      result.fullWidth = fullWidth;
      result.pageAspect = pageAspect;
      result.allPanelsAspect = allPanelsAspect;
      return(result);
   }
}

function panelLayoutPreview(nRow, nCol) {
   // var nRow = 2;
   // var nCol = 3;
   
   if(nRow < 1) {
      nRow = 1;
      $("#panel-rows").val(1);
   }

   if(nCol < 1) {
      nCol = 1;
      $("#panel-cols").val(1);      
   }
   
   var previewWidth = $("#panel-layout-preview-pane").width();
   var previewHeight = 491; // this is the hard-coded height for control panels
   var previewAspect = previewHeight / previewWidth;
   
   var pd = getPanelDims(nRow, nCol);
   
   if(pd.pageAspect < previewAspect) {
      previewHeight = previewWidth * pd.pageAspect;
      $("#panel-layout-preview-pane").height(previewHeight);
   } else {
      previewWidth = previewHeight / pd.pageAspect;
      $("#panel-layout-preview-pane").width(previewWidth);
      $("#panel-layout-preview-pane").height(previewHeight);      
   }
      
   var data = {};
   if(pd.fullWidth) {
      data.w = previewWidth - 2;
      data.h = Math.round(data.w * pd.allPanelsAspect, 0);
   } else {
      data.h = previewHeight - 2;
      data.w = Math.round(data.h / pd.allPanelsAspect, 0);
   }
   data.rows = new Array();
   for(var i = 0; i < nRow; i++) {
      data.rows[i] = new Array();
      for(var j = 0; j < nCol; j++) {
         data.rows[i].push(1);
      }
   }
   
   // change the value of pppInput accordingly
   $("#pppInput").val(nRow * nCol);
   
   // set panel dimensions
   $("#panel-layout-data").data("panelDims", pd);
   
   renderTemplate("panel-layout-preview-pane", data);
}




