var displayListBinding = new Shiny.InputBinding();
$.extend(displayListBinding, {
   find: function(scope) {
      return $(scope).find("#displayListTable");
   },
   getValue: function(el) {
      var clicked = $(el).find(".clicked");
      var uid = clicked.find('td:eq(0)').text();
      clicked.removeClass("clicked");
      res = uid;
      if(uid != "") {
         console.log("** Received input to set current display to uid = " + res);
         $("#displayListModal").modal('hide');         
      }
      // set currentPage back to 1
      $("#currentPage").val(1);
      $("#currentPage").trigger("change");
      
      // in case there is stuff here
      $("#d3bivar").text("");
      
      // also reset column indexes
      $("#selectedCogTableVar").val("");
      $("#selectedCogTableVar").trigger("change");
      
      // set all related displays to unhighlighted and trigger change
      $(".selectableDisplayVar").each(function() {
         $(this).removeClass("highlighted")
      })
      $("#relatedDisplayUID").val("");
      $("#relatedDisplayUID").trigger("change");
      
      // reset panelRows, panelCols
      $("#panelRows").val(1);
      $("#panelCols").val(1);
      
      $("#panelRows").trigger("change");
      $("#panelCols").trigger("change");
      
      return(res);
   },
   subscribe: function(el, callback) {
      $(el).on("change.displayList", function(e) {
         callback();
      });
   },
   unsubscribe: function(el) {
      $(el).off(".displayList");
   }
});
Shiny.inputBindings.register(displayListBinding, "hafen.displayListBinding");

// this takes the row the user clicks on in the table
// and triggers the 'displayListTable' input to change
$(document).on("click", "#displayListTable tbody tr", function(evt) {
   var el = $(evt.target);
   $(el).closest("tr").addClass("clicked");
   $('#displayListTable').trigger("change");
});
