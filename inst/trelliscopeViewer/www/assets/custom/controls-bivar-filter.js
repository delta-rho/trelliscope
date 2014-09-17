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
   
   var filterData = $("#bivarFilterState").data("filterData");
   
   if(!bivarFilterPlotBrush.empty()) {
      if(!filterData)
         filterData = {};
      
      var brush = bivarFilterPlotBrush.extent();
      if(xVar) {
         if(!filterData[xVar])
            filterData[xVar]= {};
         filterData[xVar]["from"] = brush[0][0];
         filterData[xVar]["to"] = brush[1][0];
      }
      if(yVar) {
         if(!filterData[yVar])
            filterData[yVar]= {};
         filterData[yVar]["from"] = brush[0][1];
         filterData[yVar]["to"] = brush[1][1];
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
}

bivarFilterLocalLoad = function() {
   // load current filter extent for selected bivariate x and y vars
   xVar = $("#bivar-x-filter-select li.active").data("name");
   yVar = $("#bivar-y-filter-select li.active").data("name");
   
   var xDomain = bivarFilterPlotX.domain();
   var yDomain = bivarFilterPlotY.domain();
   
   if(xVar || yVar) {
      var filterData = $("#bivarFilterState").data("filterData");
      if(!filterData)
         filterData = {};
      
      if(xVar) {
         var xf = filterData[xVar];
         if(xf) {
            if(xf.from == undefined)
               xf.from = xDomain[0];
            if(xf.to == undefined)
               xf.to = xDomain[1];
            xf = [xf.from, xf.to];
         }
      }
      if(yVar) {
         var yf = filterData[yVar];
         if(yf) {
            if(yf.from == undefined)
               yf.from = yDomain[0];
            if(yf.to == undefined)
               yf.to = yDomain[1];
            yf = [yf.from, yf.to];
         }
      }
      if(yf || xf) {
         if(!yf)
            yf = yDomain;
         if(!xf)
            xf = xDomain;
         
         // if both x and y fill the domain, don't brush
         yFill = (yf[1] - yf[0]) > (yDomain[1] - yDomain[0]) / 1.01;
         xFill = (xf[1] - xf[0]) > (xDomain[1] - xDomain[0]) / 1.01;
         xFill = false;
         yFill = false;
         
         if(!(xFill && yFill)) {
            // console.log("brushing!");
            // set the filter on the screen
            var res = [[xf[0], yf[0]], [xf[1], yf[1]]];
         } else {
            bivarFilterPlotBrush.clear();
            var res = bivarFilterPlotBrush.extent()
         }
         d3.select("#bivarFilterPlot")
            .select(".brush")
            .call(bivarFilterPlotBrush.extent(res));
         bivarFilterPlotBrushFn();
      }
   }
}

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
