function updateBivarPlot() {
  // set data to be currently-selected x/y variables, scatter/hexbin, and marg/cond
  // then trigger the shiny input$bivarFilterSelect to get this data
  // the input will be used to get plot data and trigger plot output
  var dat = {
    "distType" : $("#bivarDistType .active").data("dist-type"),
    "plotType" : $("#bivarPlotType .active").html().toLowerCase(),
    "xName"  : $("#bivar-x-filter-select li.active .bi-filter-x-name").html(),
    "yName"  : $("#bivar-y-filter-select li.active .bi-filter-y-name").html()
  };
  // console.log(dat);
  $("#bivarFilterSelect").data("myShinyData", dat);
  $("#bivarFilterSelect").trigger("change");
}

function removeBivarPlot() {
  // make sure the plot is a blank slate
  $("#bivarFilterPlot").removeClass("not");
  $("#bivarFilterPlot").html("");

  $("#bivarFilterSelect").data("myShinyData", null);
  $("#bivarFilterSelect").trigger("change");

  var target = document.getElementById("bivarFilterPlot");
  bivarSpinner.stop(target);
  // clear the brush
  bivarFilterPlotBrush.clear();
}

bivarFilterLocalSave = function() {
  // save current filter extent to filterData
  var xVar = $("#bivar-x-filter-select li.active").data("name");
  var yVar = $("#bivar-y-filter-select li.active").data("name");
  var xLog = $("#bivar-x-filter-select li.active").data("log");
  var yLog = $("#bivar-y-filter-select li.active").data("log");

  var filterData = $("#bivarFilterState").data("filterData");

  if(!bivarFilterPlotBrush.empty()) {
    if(!filterData)
      filterData = {};

      // if(!curBrush.empty()) {
      //   var filter = curBrush.extent();
      //   if(log !== "NA") {
      //     filter[0] = Math.pow(log, filter[0]);
      //     filter[1] = Math.pow(log, filter[1]);
      //   }
      //   if(!filterData[varName])


    var brush = bivarFilterPlotBrush.extent();
    if(xVar) {
      if(!filterData[xVar])
        filterData[xVar]= {};
      // **log**
      if(xLog !== "NA") {
        filterData[xVar].from = Math.pow(xLog, brush[0][0]);
        filterData[xVar].to = Math.pow(xLog, brush[1][0]);
      } else {
        filterData[xVar].from = brush[0][0];
        filterData[xVar].to = brush[1][0];
      }
    }
    if(yVar) {
      if(!filterData[yVar])
        filterData[yVar]= {};
      // **log**
      if(yLog !== "NA") {
        filterData[yVar].from = Math.pow(yLog, brush[0][1]);
        filterData[yVar].to = Math.pow(yLog, brush[1][1]);
      } else {
        filterData[yVar].from = brush[0][1];
        filterData[yVar].to = brush[1][1];
      }
    }
  } else {
    // console.log("no brush")
    if(xVar && yVar && filterData) {
      if(filterData[xVar])
        delete filterData[xVar];
      if(filterData[yVar])
        delete filterData[yVar];
    }
  }
  $("#bivarFilterState").data("filterData", filterData);
};

bivarFilterLocalLoad = function() {
  // load current filter extent for selected bivariate x and y vars
  var xVar = $("#bivar-x-filter-select li.active").data("name");
  var yVar = $("#bivar-y-filter-select li.active").data("name");
  var xLog = $("#bivar-x-filter-select li.active").data("log");
  var yLog = $("#bivar-y-filter-select li.active").data("log");

  var xDomain = bivarFilterPlotX.domain();
  var yDomain = bivarFilterPlotY.domain();
  if(xVar || yVar) {
    var filterData = $("#bivarFilterState").data("filterData");
    if(!filterData)
      filterData = {};

    var xFilter;
    var xf;
    if(xVar) {
      var xFrom = xDomain[0];
      var xTo = xDomain[1];
      xf = filterData[xVar];
      if(xf) {
        if(xf.from !== undefined) {
          xFrom = xf.from;
          if(xLog !== "NA") {
           xFrom = Math.log(xFrom) / Math.log(xLog);
          }
        }
        if(xf.to !== undefined) {
          xTo = xf.to;
          if(xLog !== "NA") {
           xTo = Math.log(xTo) / Math.log(xLog);
          }
        }
      }
      xFilter = [xFrom, xTo];
    }
    var yFilter;
    var yf;
    if(yVar) {
      var yFrom = yDomain[0];
      var yTo = yDomain[1];
      yf = filterData[yVar];
      if(yf) {
        if(yf.from !== undefined) {
          yFrom = yf.from;
          if(yLog !== "NA") {
           yFrom = Math.log(yFrom) / Math.log(yLog);
          }
        }
        if(yf.to !== undefined) {
          yTo = yf.to;
          if(yLog !== "NA") {
           yTo = Math.log(yTo) / Math.log(yLog);
          }
        }
      }
      yFilter = [yFrom, yTo];
    }
    var res;
    if(yFilter || xFilter) {
      if(!yFilter)
        yFilter = yDomain;
      if(!xFilter)
        xFilter = xDomain;

      if(xf || yf) {
        // console.log("brushing!");
        // set the filter on the screen
        res = [[xFilter[0], yFilter[0]], [xFilter[1], yFilter[1]]];
      } else {
        bivarFilterPlotBrush.clear();
        res = bivarFilterPlotBrush.extent();
      }
      d3.select("#bivarFilterPlot")
        .select(".brush")
        .call(bivarFilterPlotBrush.extent(res));
      bivarFilterPlotBrushFn();
    }
  }
};

function bivarFilterSetFromExposedState() {
  // trigger save in case currently-active filter hasn't been saved
  bivarFilterLocalSave();

  // for testing:
  // make a copy of filter data
  // var filterData = jQuery.extend(true, {}, $("#bivarFilterState").data("filterData"));
  // var state = {};
  // state["filter"] = filterData;
  // $("#exposedStateDataOutput").data("myShinyData", state);

  // get state data
  // make it a copy so it doesn't edit the exposed state data
  var state = jQuery.extend(true, {}, $("#exposedStateDataOutput").data("myShinyData"));

  if(!state.filter) {
    state.filter = null;
  }

  // deactivate all
  $("#bivar-x-select li").removeClass("active");
  $("#bivar-x-select li").removeClass("other-active");
  $("#bivar-y-select li").removeClass("active");
  $("#bivar-y-select li").removeClass("other-active");

  // remove all filter icons
  $("#bivar-x-select li i").addClass("hidden");
  $("#bivar-y-select li i").addClass("hidden");

  if(state.filter) {
    // set filter icons for those in state
    $.each(state.filter, function(key, value) {
      $("#bivar-x-" + key + " i").removeClass("hidden");
      $("#bivar-y-" + key + " i").removeClass("hidden");
    });
  }

  // remove plot
  removeBivarPlot();
  // set state data
  $("#bivarFilterState").data("filterData", state.filter);
}

function cogBiFilterControlsOutputApplyButton() {
  // reset to page one
  $("#curPanelPageInput").val("1");
  $("#curPanelPageInput").trigger("change");

  // trigger save in case currently-active filter hasn't been saved
  bivarFilterLocalSave();
  // trigger change
  var filterData = $("#bivarFilterState").data("filterData");
  $("#filterStateInput").data("myShinyData", filterData);
  $("#filterStateInput").trigger("change");
}

function cogBiFilterControlsOutputCancelButton() {
  bivarFilterSetFromExposedState();
}

// things to set after bivariate filter gets reset
function cogBiFilterControlsOutputPostRender() {
  // if an x or y variable in the list is clicked, "unclick" others in the list
  $(".list-group").on("click", "a", function() {
    $(this).toggleClass("selected").siblings().removeClass("selected");
  });

  // add tooltips
  $("#bivar-x-filter-select li").each(function() {
    $(this).tooltip({'placement': 'right', 'delay': { show: 500, hide: 100 }});
  });
  $("#bivar-y-filter-select li").each(function() {
    $(this).tooltip({'placement': 'right', 'delay': { show: 500, hide: 100 }});
  });


  // scatter / hexbin, etc. button behavior
  buttonToggleHandler();

  // handle clicking on x or y variables
  $("#bivar-x-filter-select li, #bivar-y-filter-select li").click(function(e) {
    // get base ID text for corresponding x or y list
    // (depending on whether current is x or y)
    var otherId = $(this).parent(".filter-select").attr("id") == "bivar-x-filter-select" ? "#bivar-y" : "#bivar-x";
    var otherLi = $(otherId + "-" + $(this).data("name"));

    // save state of currently active before changing state
    bivarFilterLocalSave();

    if($(this).hasClass("active")) {
      $(this).removeClass("active");
      otherLi.removeClass("other-active");
    } else {
      // this one can't be other-active
      $(this).removeClass("other-active");
      // make clicked item active and disable others
      $(this).addClass("active");
      $(this).siblings().removeClass("active");
      // corresponding one can't be active
      if(otherLi.hasClass("active"))
        otherLi.removeClass("active");
      // make corresponding item in other list "other-active"
      otherLi.addClass("other-active");
      otherLi.siblings().removeClass("other-active");
    }

    // if x and y lists have something selected, make the plot
    if($("#bivar-x-filter-select li.active, #bivar-y-filter-select li.active").length == 2) {

      // if there are too many panels, force hexbin
      if($("#cogNrow").data("myShinyData")[0] > 2000) {
        $("#bivar-scatter-btn").addClass("disabled");
        $("#bivar-hexbin-btn").click();
      } else {
        $("#bivar-scatter-btn").removeClass("disabled");
      }

      // remove gray background and restart spinner
      $("#bivarFilterPlot").addClass("not");
      var target = document.getElementById("bivarFilterPlot");
      bivarSpinner.stop(target);
      bivarSpinner.spin(target);
    } else {
      removeBivarPlot();
    }
    updateBivarPlot();
  });
}

// function updateCogNrow() {
//   alert($("#cogNrow").data("myShinyData")[0]);
//   asdfasdf;
//   if($("#cogNrow").data("myShinyData")[0] > 10) {
//     $("#bivar-scatter-btn").addClass("disabled");
//     $("#bivar-hexbin-btn").click();
//   } else {
//     $("#bivar-scatter-btn").removeClass("disabled");
//     $("#bivar-scatter-btn").click();
//   }
// }

