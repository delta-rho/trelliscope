
function visibleCogListOutputApplyButton() {
   var visibleCog = [];
   $(".visible-cog-select").each(function() {
      if($(this).hasClass("active"))
         visibleCog.push($(this).data("name"));
   });
   
   $("#visibleCogStateInput").data("myShinyData", visibleCog);
   $("#visibleCogStateInput").trigger("change");
   
   $("#panel-rows").trigger("change");
   panelLayoutOutputApplyButton();
}

function visibleCogListOutputCancelButton() {
   visibleCogListSetFromExposedState();
}

function visibleCogListSetFromExposedState() {
   
   var state = $("#exposedStateDataOutput").data("myShinyData");
   
   $(".visible-cog-select").each(function() {
      $(this).removeClass("active");
   });
   
   if(state) {
      if(state.visibleCog) {
         if(!(state.visibleCog instanceof Array)) {
            state.visibleCog = [state.visibleCog];
         }
         $.each(state.visibleCog, function(key, value) {
            $("#visible-cog-select-" + value).addClass("active");
         });
      }      
   }
}

function visibleCogListOutputPostRender() {
   $.getScript("assets/custom/selectables-visible.js");
   
   $("#panel-rows").trigger("change");
   
   // call panel layout apply button to take number of cogs, etc.
   panelLayoutOutputApplyButton();
}
