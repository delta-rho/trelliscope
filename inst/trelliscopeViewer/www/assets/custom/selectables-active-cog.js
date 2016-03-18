// http://stackoverflow.com/questions/19225086/select-cells-on-table-by-dragging-on-tablets

// this code allows for multiple selection of li and td elements
// by holding down the mouse and dragging
$(function() {
  var isMouseDown = false, isActive;

  $("#active-cog-selection td.selectable").unbind('mousedown');
  $("#active-cog-selection td.selectable").unbind('mouseover');
  $("#active-cog-selection td.selectable").unbind('selectstart');

  $("#active-cog-selection td.selectable")
  .mousedown(function () {
    isMouseDown = true;
    $(this).toggleClass("active");
    isActive = $(this).hasClass("active");
    return false; // prevent text selection
  })
  .mouseover(function () {
    if (isMouseDown) {
      $(this).toggleClass("active", isActive);
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


