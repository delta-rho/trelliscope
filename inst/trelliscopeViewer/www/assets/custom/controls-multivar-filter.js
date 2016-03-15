
// get appropriate data and update multivariate filter
function updateMultivarPlot() {
  // remove gray background and restart spinner
  $("#multivarFilterPlot").addClass("not");
  var target = document.getElementById("multivarFilterPlot");
  multivarSpinner.stop(target);
  multivarSpinner.spin(target);

  // set data to be currently-selected x/y variables, hist/quant, and marg/cond
  // then trigger the shiny input$bivarFilterSelect to get this data
  // the input will be used to get plot data and trigger plot output
  var varNames = [];
  $("#multivarFilterSelect li.active").each(function() {
    varNames.push($(this).html());
  });
  var dat = {
    "distType" : $("#multivarDistType .active").data("dist-type"),
    "plotType" : $("#multivarPlotType .active").html().toLowerCase(),
    "varNames" : varNames
  };
  $("#multivarFilterSelect").data("myShinyData", dat);
  $("#multivarFilterSelect").trigger("change");

  // clear the brush
  d3.select("#multivarFilterPlot")
    .select(".brush")
    .call(multivarFilterPlotBrush.clear());
  multivarFilterPlotBrushFn();

  // disable plot button until other variables are selected
  $("#btn-multivar").prop("disabled", true);
}

function cogMultiFilterControlsOutputPostRender() {
  // for click and drag selection of variables
  $.getScript("assets/custom/selectables-multivar.js");

  $("#btn-multivar").click(function(e) {
    updateMultivarPlot();
  });
}
