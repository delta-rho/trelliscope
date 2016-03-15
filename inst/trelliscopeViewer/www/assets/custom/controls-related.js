
function relatedDisplayListOutputPostRender() {
  $(".related-display-select").click(function() {
    $(this).toggleClass("active");

    if($(".related-display-select.active").length > 0) {
      relatedLayout();
      $("#related-layout-container").show();
      $(".related-layout-box.active").show();
    } else {
      $(".related-layout-box").removeClass("active");
      $(".rl-layout").hide();
    }
  });
}


// compute sizes of things and rearrange them accordingly
// this should be called each time a related display is (de)selected
// or the the window size changes
// it re-computes the initial suggestion of where the panels should go
// it does not make them visible though - needs to be done separately
function relatedLayout() {

  var previewWidth = 780; // hard-coded width for control panels
  var previewHeight = 290; // what is left below display list
  var previewAspect = previewHeight / previewWidth;

  var tPad = 3; // padding on either side of the panel
  // height of row of cog label depends on number of rows
  // based on font size decreasing wrt rows as 1->14, 2->12, 3->10, 4+->8
  // and on line-height of 1.2
  var cogHeightArr = [26, 24, 22, 19];
  var cogHeight = cogHeightArr[0]; // can only have single row
  var nCog = $(".panel-labels-select.active").length; // number of cogs to show
  // extra padding beyond what is plotted
  // these remain fixed while width and height can change
  var hExtra = 2 + 2 * tPad + nCog * cogHeight; // 2 for border + tPad on top / bottom + cogHeight for every row of visible cognostics

  var pageWidth = $("#panelTableContentOutput").width();
  var pageHeight = $(window).height() -
    (51 + 7) - // header height + padding
    (32 + 7) - // footer height + padding
    hExtra; // only want to work with non-labels part of panel area
  var pageAspect = pageHeight / pageWidth;

  if(pageAspect < previewAspect) {
    previewHeight = previewWidth * pageAspect;
    $("#panel-layout-preview-pane").height(previewHeight);
  } else {
    previewWidth = previewHeight / pageAspect;
    $("#panel-layout-preview-pane").width(previewWidth);
    $("#panel-layout-preview-pane").height(previewHeight);
  }

  var off = $("#related-table-container").offset();

  var newLeft = off.left + (780 - previewWidth) / 2;
  var newTop = off.top + 210; // display list is 200 + 10 padding

  // pre-position things
  $(".rl-layout").css("left", newLeft);
  $(".rl-layout").css("top", newTop);
  $("#related-layout-container").width(previewWidth);
  $("#related-layout-container").height(previewHeight);

  // simple algorithm for now:
  // see how many related displays there are (n)
  // find all possible multiples (r x c = n)
  // find r and c such that if we split available space of
  // the screen into r rows and c columns, we choose a splitting
  // where the resulting panels fill the most area

  var disps = [];
  var curAspect = $("#panel-layout-data").data("panelAspect");
  var state = $("#exposedStateDataOutput").data("myShinyData");

  var curDisp = {"name": state.name, "group": state.group, "aspect": curAspect};

  disps.push(curDisp);
  $(".related-display-select.active").each(function() {
    disps.push($(this).data());
  });

  var n = disps.length;

  // find all possible ways to grid
  var grids = [];
  for(var i = 1; i <= n; i++) {
    if(n / i == Math.floor(n / i))
      grids.push([i, n / i]);
  }

  // find difference between total area and that of laying out
  // panels according to the different grid choices
  var areaDiff = [];
  var curGrid, nRow, nCol, gridAspect, gridWidth, gridHeight;
  var totArea = previewWidth * previewHeight;
  for(i = 0; i < grids.length; i++) {
    curGrid = grids[i];
    nRow = curGrid[0];
    nCol = curGrid[1];
    gridAspect = curGrid[1] / curGrid[0] * pageAspect;
    gridWidth = previewWidth / nCol;
    gridHeight = previewHeight / nRow;
    var runningTotal = 0;
    for(var j = 0; j < nRow * nCol; j++) {
      if(disps[j] !== undefined) {
        curDispAspect = disps[i].aspect;
        if(gridAspect < curDispAspect) {
          curHeight = gridHeight;
          curWidth = curHeight / curDispAspect;
        } else {
          curWidth = gridWidth;
          curHeight = curWidth * curDispAspect;
        }
        runningTotal += curWidth * curHeight;
      }
    }
    areaDiff[i] = totArea - runningTotal;
  }
  curGrid = grids[areaDiff.indexOf(Math.min.apply(null, areaDiff))];

  // make all boxes inactive and hide them all
  $(".related-layout-box").removeClass("active");
  $(".related-layout-box").hide();

  // store position information to help translate
  // from preview to actual page
  var layoutData = {
    actualToPreviewRatio: pageWidth / previewWidth,
    newTop: newTop,
    newLeft: newLeft,
    pageWidth: pageWidth,
    pageHeight: pageHeight
  };
  $("#related-layout-container").data("layout", layoutData);

  // now move the boxes to appropriate position
  // make them appropriate size
  // and give them appropriate labels (group / name)
  // ...
  nRow = curGrid[0];
  nCol = curGrid[1];
  gridAspect = curGrid[1] / curGrid[0] * pageAspect;
  gridWidth = previewWidth / nCol;
  gridHeight = previewHeight / nRow;
  var index = 0;
  var curLeft, curTop, curWidth, curHeight;
  for(var row = 0; row < nRow; row++) {
    for(var col = 0; col < nCol; col++) {
      if(disps[index] !== undefined) {
        curLeft = newLeft + 1 + col * gridWidth;
        curTop = newTop + 1 + row * gridHeight;
        $("#box" + index).css("left", curLeft);
        $("#box" + index).css("top", curTop);

        curDispAspect = disps[index].aspect;
        if(gridAspect < curDispAspect) {
          curHeight = gridHeight;
          curWidth = curHeight / curDispAspect;
        } else {
          curWidth = gridWidth;
          curHeight = curWidth * curDispAspect;
        }
        $("#box" + index).css("width", curWidth);
        $("#box" + index).css("height", curHeight);

        // make this one active and add label
        $("#box" + index).addClass("active");
        $("#box" + index).find(".box-group-label").html(disps[index].group);
        $("#box" + index).find(".box-name-label").html(disps[index].name);
      }
      index++;
    }
  }
}

// called when "Add Related Display" is clicked
// to decide whether to show the layout controls
function showRelatedLayoutControls() {
  if($(".related-display-select.active").length > 0) {
    $("#related-layout-container").show();
    $(".related-layout-box.active").show();
  }
}

function relatedDisplayListOutputCancelButton() {
  $(".rl-layout").hide();
}

function relatedDisplayListOutputApplyButton() {
  $(".rl-layout").hide();

  var activeDisps = $(".related-layout-box.active");
  // var activeDisps = $(".related-display-select.active");
  var relatedDisplays = {};

  if(activeDisps.length > 0) {
    var ld = $("#related-layout-container").data("layout");

    // build up panelLabel of active labels
    activeDisps.each(function(index) {
      var $thisbox = $("#box" + index);
      var curLeft = parseInt($thisbox.css("left"));
      var curTop = parseInt($thisbox.css("top"));
      var curWidth = $thisbox.width();
      var curHeight = $thisbox.height();

      var objData = {
        group: $thisbox.find(".box-group-label").html(),
        name: $thisbox.find(".box-name-label").html(),
        left: (curLeft - ld.newLeft) * ld.actualToPreviewRatio,
        top: (curTop - ld.newTop) * ld.actualToPreviewRatio,
        width: curWidth * ld.actualToPreviewRatio,
        height: curHeight * ld.actualToPreviewRatio,
        pageWidth: ld.pageWidth,
        pageHeight: ld.pageHeight
      };

      relatedDisplays[index] = objData;
    });

    // there can only be one panel per page
    $("#panel-rows").val(1);
    $("#panel-cols").val(1);
    $("#panel-rows").trigger("change"); // recalculate dims
    panelLayoutOutputApplyButton();

    // now disable these
    $("#panel-rows").prop("disabled", true);
    $("#panel-cols").prop("disabled", true);
    $("#panel-layout").find(".control-footer").html("Note: in related display mode, only one panel can be shown per page.  Disable related displays to control panel layout.");
  } else {
    $("#panel-rows").prop("disabled", false);
    $("#panel-cols").prop("disabled", false);
    $("#panel-layout").find(".control-footer").html("");
  }

  // console.log(relatedDisplays);
  $("#relatedDisplayStateInput").data("myShinyData", relatedDisplays);
  $("#relatedDisplayStateInput").trigger("change");
}

