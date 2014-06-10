$(function() {
   var isMouseDown = false, isActive;
   
   $("#add-related-display-selection td.selectable")
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
   })
   
   $(document)
   .mouseup(function () {
      isMouseDown = false;
   });
});
