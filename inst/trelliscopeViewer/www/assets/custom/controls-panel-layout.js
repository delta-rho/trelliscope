

function panelLayoutOutputApplyButton() {
  var panelDims = $("#panel-layout-data").data("panelDims");

  var arrangement = $("#panelArrangement").find("button.active").data("val");

  if(panelDims !== undefined) {
    var prevNrow, prevNcol;
    // determine what page to switch to depending on what we changed from
    var prevLayout = $("#panelLayoutStateInput").data("myShinyData");
    if(prevLayout) {
      prevNrow = prevLayout.nrow;
      prevNcol = prevLayout.ncol;
    } else {
      prevNrow = 1;
      prevNcol = 1;
    }
    var prevPage = $("#curPanelPageInput").val();

    var curNrow = parseInt($("#panel-rows").val());
    var curNcol = parseInt($("#panel-cols").val());
    var curPage = Math.floor(((prevPage - 1) * prevNrow * prevNcol) / (curNrow * curNcol)) + 1;
    $("#curPanelPageInput").val(curPage + "");
    $("#curPanelPageInput").trigger("change");

    var panelLayout = {
      "nrow"   : curNrow,
      "ncol"   : curNcol,
      "w"     : panelDims.w,
      "h"     : panelDims.h,
      "arrange" : arrangement
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
    if(state.layout) {
      $("#panel-rows").val(state.layout.nrow);
      $("#panel-cols").val(state.layout.ncol);
      $("#panel-rows").trigger("change");
      $("#pl-" + state.layout.arrange).click();
    }
  }
}

function panelLayoutOutputPostRender(data) {
  $("#panel-layout-data").data("nCog", data.n_panel_labels);

  // handle by-row / by-column toggle
  $(".pl-toggle").click(function() {
    // if this button is not the one currently selected
    if($(this).hasClass("btn-default")) {
      // make it active
      $(this).removeClass("btn-default");
      $(this).addClass("btn-info");
      $(this).addClass("active");
      // make all others in the group inactive
      var sib = $(this).siblings();
      sib.removeClass("btn-info");
      sib.removeClass("active");
      sib.addClass("btn-default");
    }
  });

  // actions for changing nrow and ncol
  $("#panel-rows").change(function() {
    panelLayoutPreview(parseInt($("#panel-rows").val()), parseInt($("#panel-cols").val()), $("#panel-layout-data").data("nCog"));
  });

  $("#panel-cols").change(function() {
    panelLayoutPreview(parseInt($("#panel-rows").val()), parseInt($("#panel-cols").val()), $("#panel-layout-data").data("nCog"));
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


function getPanelDims(nRow, nCol, nCog) {
  // var nRow = 2;
  // var nCol = 3;

  var panelAspect = $("#panel-layout-data").data("panelAspect");
  if(panelAspect) {
    var tPad = 3; // padding on either side of the panel
    // height of row of cog label depends on number of rows
    // based on font size decreasing wrt rows as 1->14, 2->12, 3->10, 4+->8
    // and on line-height of 1.2
    var cogHeightArr = [26, 24, 22, 19];
    var cogHeight = cogHeightArr[Math.min(nRow - 1, 3)];
    // var nCog = $(".panel-labels-select.active").length; // number of cogs to show
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

function panelLayoutPreview(nRow, nCol, nCog) {
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

  var pd = getPanelDims(nRow, nCol, nCog);

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
  data.rows = [];
  for(var i = 0; i < nRow; i++) {
    data.rows[i] = [];
    for(var j = 0; j < nCol; j++) {
      data.rows[i].push(1);
    }
  }

  // change the value of pppInput accordingly
  // $("#pppInput").val(nRow * nCol);
  // set panel dimensions
  $("#panel-layout-data").data("panelDims", pd);

  renderTemplate("panel-layout-preview-pane", data);
}




