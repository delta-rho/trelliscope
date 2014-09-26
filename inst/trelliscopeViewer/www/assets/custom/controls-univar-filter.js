function updateUnivarPlot() {
   // remove gray background and restart spinner
   $("#univarFilterPlot").addClass("not");
   var target = document.getElementById("univarFilterPlot");
   univarSpinner.stop(target);
   univarSpinner.spin(target);

   // set data to be currently-selected variable, hist/quant, and marg/cond
   // then trigger the shiny input$univarFilterSelect to get this data
   // the input will be used to get plot data and trigger plot output
   var dat = {
      "distType" : $("#univarDistType .active").data("dist-type"),
      "plotType" : $("#univarPlotType .active").html().toLowerCase(),
      "varName"  : $("#univarFilterSelect li.active .uni-filter-var-name").html()
   };
   $("#univarFilterSelect").data("myShinyData", dat);
   $("#univarFilterSelect").trigger("change");
}

function removeUnivarPlot() {
   // first remove the plot
   $("#univarFilterPlot").html("");
   
   // set data to emtpy and trigger change
   $("#univarFilterSelect").data("myShinyData", null);
   $("#univarFilterSelect").trigger("change");
   
   // add gray background, stop spinner (if running)
   $("#univarFilterPlot").removeClass("not");
   var target = document.getElementById("univarFilterPlot");
   univarSpinner.stop(target);
}

univarFilterLocalSave = function() {
   activeVar = $("#univarFilterSelect li.active");
   if(activeVar.length > 0) {
      var filterData = $("#univarFilterState").data("filterData");
      if(!filterData)
         filterData = {};
      
      var varName = activeVar.data("name");
      
      if(activeVar.data("type") == "numeric") {
         if($("#univarPlotType button.histogram-button").hasClass("active")) {
            var curBrush = d3univarXbrush;
         } else {
            var curBrush = d3univarYbrush;
         }
         
         if(!curBrush.empty()) {
            var filter = curBrush.extent();
            if(!filterData[varName])
               filterData[varName] = {};
            filterData[varName] = { from: filter[0], to: filter[1]};
            // console.log(filterData);
         } else {
            // remove the element
            delete filterData[varName];
         }
      } else {
         if(!filterData[varName])
            filterData[varName] = {};
         
         var res = [];
         d3.selectAll("#univarFilterPlot svg rect.selected").each(function(d) {
            res.push(d.label);
         });
         if(res.length > 0) {
            filterData[varName]["select"] = res;            
         } else {
            delete filterData[varName];
         }
      }
      $("#univarFilterState").data("filterData", filterData);
   }
}

univarFilterLocalLoad = function() {
   activeVar = $("#univarFilterSelect li.active");
   if(activeVar) {
      var filterData = $("#univarFilterState").data("filterData");
      if(!filterData)
         filterData = {};
      var varName = activeVar.data("name");
      var filter = filterData[varName];
      // console.log(filter);
      
      if(activeVar.data("type") == "numeric") {
         // filter is stored as {from: , to:} - make it array
         if($("#univarPlotType button.histogram-button").hasClass("active")) {
            if(filter) {
               var dm = d3univarX.domain();
               if(filter.from == undefined)
                  filter.from = dm[0];
               if(filter.to == undefined)
                  filter.to = dm[1];
               filter = [filter.from, filter.to];
               d3.select("#univarFilterPlot")
                  .select(".brush")
                  .call(d3univarXbrush.extent(filter));
               d3univarXbrushFn();
            } else {
               d3.select("#univarFilterPlot")
                  .select(".brush")
                  .call(d3univarXbrush.clear());
               d3univarXbrushFn();
            }
         } else { // quantile
            if(filter) {
               filter = [filter.from, filter.to];
               d3.select("#univarFilterPlot")
                  .select(".brush")
                  .call(d3univarYbrush.extent(filter));
               d3univarYbrushFn();
            } else {
               d3.select("#univarFilterPlot")
                  .select(".brush")
                  .call(d3univarYbrush.clear());
               d3univarYbrushFn();
            }
         }
      } else {
         // highlighted selected bars in barchart
         if(filter) {
            if(filter["select"]) {
               d3.selectAll("#univarFilterPlot svg rect").attr("class", function(d) {
                  if($.inArray(d.label, filter["select"]) >= 0 && !filter["empty"]) {
                     return("unifilter-bar selected");
                  } else {
                     return("unifilter-bar");
                  }
               });               
            }
         }
      }
   }
}

function cogUniFilterControlsOutputApplyButton() {
   // reset to page one
   $("#curPanelPageInput").val("1");
   $("#curPanelPageInput").trigger("change");
   
   // trigger save in case currently-active filter hasn't been saved
   univarFilterLocalSave();
   // trigger change
   var filterData = $("#univarFilterState").data("filterData");
   $("#filterStateInput").data("myShinyData", filterData);
   $("#filterStateInput").trigger("change");
   
   $("#univarFilterSelect li").removeClass("active");
   removeUnivarPlot();
}

function cogUniFilterControlsOutputCancelButton() {
   univarFilterSetFromExposedState();
}

function univarFilterSetFromExposedState() {
   // trigger save in case currently-active filter hasn't been saved
   univarFilterLocalSave();
   
   // for testing:
   // make a copy of filter data
   // var filterData = jQuery.extend(true, {}, $("#univarFilterState").data("filterData"));
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
   $("#univarFilterSelect li").removeClass("active");
   // remove all filter icons
   $("#univarFilterSelect li i").addClass("hidden");      
   
   if(state.filter) {
      // set filter icons for those in state
      $.each(state.filter, function(key, value) {
         $("#univar-var-" + key + " i").removeClass("hidden");
      });
   }
   
   // remove plot
   removeUnivarPlot();
   // set state data
   $("#univarFilterState").data("filterData", state.filter);
}

function cogUniFilterControlsOutputPostRender() {
   $(".list-group").on("click", "a", function() {
      $(this).toggleClass("selected").siblings().removeClass("selected");
   });
   
   // add tooltips
   $("#univarFilterSelect li").each(function() {
      $(this).tooltip({'placement': 'right', 'delay': { show: 500, hide: 100 }});
   });
   
   buttonToggleHandler();
   
   $("#univarFilterSelect li").click(function(e) {

      // first save the filter state of the current one
      univarFilterLocalSave();
      
      if(!$(this).hasClass("active")) {
         // make selected item active and all others not
         $(this).toggleClass("active");
         $(this).siblings().removeClass("active");
         
         // make sure the appropriate distribution type buttons are enabled
         var buttons = $("#univarPlotType");
         if($(this).data("type") == "numeric") {
            // only change them if we need to
            if(buttons.find("button.histogram-button.active,button.quantile-button.active").length == 0) {
               buttons.find("button.quantile-button")
                  .prop("disabled", false)
                  .addClass("active")
                  .removeClass("btn-default")
                  .addClass("btn-info");
               buttons.find("button.histogram-button")
                  .prop("disabled", false)
                  .removeClass("active")
                  .removeClass("btn-info").addClass("btn-default");
               buttons.find("button.bar-button")
                  .prop("disabled", true)
                  .removeClass("active")
                  .removeClass("btn-info").addClass("btn-default");               
            }
         } else {
            buttons.find("button.histogram-button")
               .prop("disabled", true)
               .removeClass("active")
               .removeClass("btn-info")
               .addClass("btn-default");
            buttons.find("button.quantile-button")
               .prop("disabled", true)
               .removeClass("active")
               .removeClass("btn-info")
               .addClass("btn-default");
            buttons.find("button.bar-button")
               .prop("disabled", false)
               .addClass("active")
               .addClass("btn-info")
               .removeClass("btn-default");
         }
         
         updateUnivarPlot();
      } else {
         // at this point there should be nothing selected and no plot
         $(this).removeClass("active");
         removeUnivarPlot();
      }
   });
}