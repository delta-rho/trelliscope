// http://stackoverflow.com/questions/19225086/select-cells-on-table-by-dragging-on-tablets

// this code allows for multiple selection of li and td elements
// by holding down the mouse and dragging
$(function() {
  var isMouseDown = false, isActive;

  $("#multivarFilterSelect li").unbind('mousedown');
  $("#multivarFilterSelect li").unbind('mouseover');
  $("#multivarFilterSelect li").unbind('selectstart');

  $("#multivarFilterSelect li")
  .mousedown(function () {
    isMouseDown = true;
    $(this).toggleClass("active");
    // enable plot button if enough are active
    if($("#multivarFilterSelect li.active").length > 1) {
      $("#btn-multivar").prop("disabled", false);
    } else {
      $("#btn-multivar").prop("disabled", true);
    }
    // something has changed, so remove the plot
    $("#multivarFilterPlot").removeClass("not");
    $("#multivarFilterPlot").html("");
    var target = document.getElementById("multivarFilterPlot");
    multivarSpinner.stop(target);
    multivarFilterPlotBrush.clear();
    // set data to be empty (so same selection will trigger change)
    $("#multivarFilterSelect").data("myShinyData", null);
    $("#multivarFilterSelect").trigger("change");

    isActive = $(this).hasClass("active");
    return false; // prevent text selection
  })
  .mouseover(function () {
    if (isMouseDown) {
      $(this).toggleClass("active", isActive);
      // enable plot button if enough are active
      if($("#multivarFilterSelect li.active").length > 1) {
        $("#btn-multivar").prop("disabled", false);
      } else {
        $("#btn-multivar").prop("disabled", true);
      }
    }
  })
  .bind("selectstart", function () {
    return false;
  });

  $(document)
  .mouseup(function () {
    isMouseDown = false;
  });
});


