

function panelLabelListOutputApplyButton() {
   var panelLabel = [];
   var needActiveCogUpdate = false;
   
   // build up panelLabel of active labels
   $(".panel-labels-select").each(function() {
      if($(this).hasClass("active")) {
         var curName = $(this).data("name");
         
         panelLabel.push(curName);
         
         // also make it active in active-cog
         if(!$("#active-cog-select-" + curName).hasClass("active")) {
            needActiveCogUpdate = true;
            $("#active-cog-select-" + curName).addClass("active");
         }
      }
   });
   
   if(panelLabel.length == 0)
      panelLabel.push("__none__");
   
   // we need the new labels to be available as active cogs
   if(needActiveCogUpdate)
      activeCogListOutputApplyButton();
   
   $("#panelLabelStateInput").data("myShinyData", panelLabel);
   $("#panelLabelStateInput").trigger("change");
   
   // if related displays are selected, recompute there
   // instead of panel layout
   if($(".related-display-select.active").length > 0) {
      relatedLayout();
      relatedDisplayListOutputApplyButton();
   } else {
      panelLayoutPreview(parseInt($("#panel-rows").val()), parseInt($("#panel-cols").val()));
      $("#panel-rows").trigger("change");
      panelLayoutOutputApplyButton();      
   }
}

function panelLabelListOutputCancelButton() {
   panelLabelListSetFromExposedState();
}

function panelLabelListSetFromExposedState() {
   var state = $("#exposedStateDataOutput").data("myShinyData");
   
   $(".panel-labels-select").each(function() {
      $(this).removeClass("active");
   });
   
   if(state) {
      if(state.panelLabel) {
         // console.log(state);
         if(!(state.panelLabel instanceof Array)) {
            state.panelLabel = [state.panelLabel];
         }
         $.each(state.panelLabel, function(key, value) {
            $("#panel-labels-select-" + value).addClass("active");
         });
      }      
   }
}

function panelLabelListOutputPostRender() {
   $.getScript("assets/custom/selectables-panel-labels.js");
   
   // $("#panel-rows").trigger("change");
   // 
   // // call panel layout apply button to take number of labels, etc.
   // panelLayoutOutputApplyButton();
}
