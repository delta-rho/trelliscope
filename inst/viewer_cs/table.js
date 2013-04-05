
// TODO: look into this:
// http://datatables.net/forums/discussion/8447/bootstrap-2s-glyphicons/p1

$(document).ready(function() {
      
   $('#btnPrev').click(function() {
      pageBack();
   });

   $('#btnNext').click(function() {
      pageForward();
   });

   $('#btnBeg').click(function() {
      $("#currentPage").val(1);
      $("#currentPage").trigger("change");
   });
   
   $('#btnEnd').click(function() {
      var nPages = parseInt($("#nPages").text());
      $("#currentPage").val(nPages);
      $("#currentPage").trigger("change");
   });
   
   $('#cogTipPopover').popover({placement: 'bottom', trigger: 'click', offset: 1, html: true});
            
   function hideColumn(columnIndex) {
      $('#displayListTable td:nth-child('+(columnIndex+1)+'), #displayListTable th:nth-child('+(columnIndex+1)+')').hide();
   }
   
   hideColumn(0);
   hideColumn(6);
   hideColumn(7);
   hideColumn(8);
   hideColumn(9);
   hideColumn(11);
   hideColumn(12);
   
   var appHash = window.location.hash;
   if(appHash == "") {
      $("#displayListModal").modal('show');      
   } else {
      $("#appHash").val(appHash);
   }
   
   $("#pppInput").bind("keypress", function(e) {
      if(e.which == 13 || e.which== 9) { // if enter or tab
         // alert("ya done pressed tab or enter");
         $("#pppInput").trigger("change");
         return false;
      } else if(e.which == 37 || e.which == 39) {
         e.preventDefault();
      }
   });
   
   $("body").on('keyup.dismiss.modal', function(event) {
      // if enter, then do the action button
      if(event.keyCode == 13) {
         if($("#varModal").is(":visible"))
         $("#btnUpdateVariables").trigger("click");
         if($("#cogModal").is(":visible"))
         $("#btnUpdatePlots").trigger("click");
      } else if(event.keyCode == 27) {
         // // in Firefox, clicking "x" and hitting escape on the modal are not the same thing...
         // if($("#varModal").is(":visible"))
         // varModalRestoreSettings();
         // if($("#cogModal").is(":visible"))
         // cogModalRestoreSettings();
         if($("#varModal").is(":visible"))
            $("#varModal").modal('hide');
         if($("#cogModal").is(":visible"))
            $("#cogModal").modal('hide');
      }
   });
   
   $("#cogFilterMenu").css("min-width", "106px");
   
   $('#varCogModal').on('hidden', function () {
      $("#cogModal").modal("show");
   })
   
   $('#cogModal').on('show', function () {
      updateCogTableDims();
   })
   
   // remove fade-in behavior of modals
   var modals = $(".modal");
   var filtermodals = $(".modalFilter");
   
   modals.removeClass("fade")
   modals.removeClass("in")
   filtermodals.removeClass("fade")
   filtermodals.removeClass("in")
   
});


$(window).resize(function() {
   if(this.resizeTO) clearTimeout(this.resizeTO);
   this.resizeTO = setTimeout(function() {
      $(this).trigger('resizeEnd');
   }, 250);
});

$(window).bind('resizeEnd', function() {
   adjustPlotDims();
   updateCogTableDims();
});

$(document).keydown(function(e) {
   if(e.keyCode == 37) { // left
      pageBack();
   }
   if(e.keyCode == 39) { // right
      pageForward();
   }
});

function getHighlighted(obj) {
   var columns = new Array();
   for (var i=0; i < obj.length; i++) {
      if($(obj[i]).hasClass("highlighted"))
         columns.push($(obj[i]).attr("name"));
         // columns.push(i + 1);
   };
   return(columns);
}

function adjustPlotDims() {
   updateTableDims(); // this might be overkill?
   
   var plotWidth = parseInt($("#plotWidth").val());
   var plotHeight = parseInt($("#plotHeight").val());
   
   console.log(plotWidth);
   
   $(".img_td").css("width", plotWidth);
   $(".img_td").css("height", plotHeight);

   // $(".png_img").css("width", plotWidth - 4);
   // $(".png_img").css("height", plotHeight - 4);
}

function pageForward() {
   // get current page
   var curPage = parseInt($("#currentPage").val());
   var nPages = parseInt($("#nPages").text());
   if(curPage + 1 <= nPages) {
      $("#currentPage").val(curPage + 1);
      $("#currentPage").trigger("change");
   }
}

function pageBack() {
   // get current page
   var curPage = parseInt($("#currentPage").val());
   if(curPage - 1 >= 1) {
      $("#currentPage").val(curPage - 1);
      $("#currentPage").trigger("change");
   }
}

$(document).on("click", "th[role=columnheader]", function(evt) {

   var el = $(evt.target);
   if(el.prop("tagName") == "I") // icon clicked
      el = el.parent();

   var classVec = new Array("sorting", "sorting_asc", "sorting_desc");
   var iClassVec = new Array("icon-minus", "icon-chevron-up", "icon-chevron-down");
   
   // get current 'sorting' class
   var curClass = el.attr("class");
   
   var newClassIndex = ($.inArray(curClass, classVec) + 1) % 3;
   var newClass = classVec[newClassIndex];
   var newIconClass = iClassVec[newClassIndex];
   
   // if shift was not pressed, reset all columns
   if (!evt.shiftKey) {
      el.parent().children().each(function(index) {
         $(this).attr("class", "sorting");
         $(this).find("i").attr("class", "icon-minus");
         // also reset sort order
         $(this).attr("data-sort-order", 0);
      });
   }
   
   el.attr("class", newClass);
   el.find("i").attr("class", newIconClass);
   
   // now set sort order for the current class
   var curSortOrder = Math.max.apply(null, el.parent().children().map(function() {
      var tmp = parseInt($(this).attr("data-sort-order"));
      if(isNaN(tmp))
         tmp = 0;
      return Math.abs(tmp);
   }).get()) + 1;
   var multiplier = 1;
   
   if(newClass == "sorting")
      curSortOrder = 0;
   if(newClass == "sorting_desc")
      multiplier = -1;
   
   el.attr("data-sort-order", multiplier * curSortOrder);
   
   // Raise an event to signal that the value changed
   el.parent().trigger("change");      
});

var tableSortBinding = new Shiny.InputBinding();
$.extend(tableSortBinding, {
   find: function(scope) {
      return $(scope).find(".columnSortInput");
   },
   getValue: function(el) {
      // return the sort order
      var returnVal = $(el).children().map(function() {
         var tmp = parseInt($(this).attr("data-sort-order"));
         if(isNaN(tmp))
            tmp = 0;
         return tmp;
      }).get();
      // console.log($(el).attr("class"));
      console.log(returnVal);
      return returnVal;
   },
   subscribe: function(el, callback) {
      $(el).on("change.tableSortBinding", function(e) {
         callback();
      });
   },
   unsubscribe: function(el) {
      $(el).off(".tableSortBinding");
   }
});
Shiny.inputBindings.register(tableSortBinding);


//
$(document).on("click", "ul.tableNav", function(evt) {
   // evt.target is the button that was clicked
   var el = $(evt.target);
   if(el.prop("tagName") == "I")
      el = el.parent();
      
   el = el.parent();
   // alert(el.attr("class"));
   
   var elClass = el.attr("class");
   var curPage = parseInt(el.parent().find(".paginationValue").text());
   var newPage;
   var nRow = parseInt(el.parent().find(".paginationNrow").text());
   var nRowLength = parseInt(el.parent().find(".paginationNrowLength").text());
   var maxPages = Math.ceil(nRow / nRowLength);

   switch (elClass) {
      case "tableNavFirst":
         newPage = 1;
         break;
      case "tableNavPrev":
         newPage = Math.max(curPage - 1, 1);
         break;
      case "tableNavNext":
         newPage = Math.min(curPage + 1, maxPages);
         break;
      case "tableNavLast":
         newPage = maxPages;
         break;
   }

   // update new page
   el.parent().find(".paginationValue").text(newPage);
   
   // // update text in paginate
   // el.parent().find(".tableNavText").text(newPage + " of " + maxPages);
   
   // Raise an event to signal that the value changed
   el.parent().trigger("change");
});

var paginateBinding = new Shiny.InputBinding();
$.extend(paginateBinding, {
   find: function(scope) {
      return $(scope).find(".cogTable_paginate");
   },
   getValue: function(el) {
      return parseInt($(el).find(".paginationValue").text());
   },
   subscribe: function(el, callback) {
      $(el).on("change.tableNav", function(e) {
         callback();
      });
   },
   unsubscribe: function(el) {
      $(el).off(".tableNav");
   }
});
Shiny.inputBindings.register(paginateBinding);

// column filters
// instead of an individual input for each filter input
// make just one input that sends values back to R
// which will handle the multiple inputs
$(document).on("change", ".columnFilter", function(evt) {
   var el = $(evt.target);
   el.parent().parent().trigger("change");
   el.parent().trigger("change");
});

// a = $(".columnFilters").find("input").map(function() {
// }).get();

getInputVals = function() {
   var cur = $(this);
   var filterType="";
   if(cur.hasClass("columnFilterFrom")) {
      filterType = "from"
   } else if(cur.hasClass("columnFilterTo")) {
      filterType = "to"
   } else if(cur.hasClass("columnFilterRegex")) {
      filterType = "regex"
   }
   var filterVal = cur.attr("value");
   if(filterType=="from" || filterType=="to")
      filterVal = parseFloat(filterVal);
   
   // return { type:filterType, col:parseInt(cur.attr("name")), val:filterVal };
   return [[ filterType, parseInt(cur.attr("name")), filterVal ]];
   // return [ filterType, parseInt(cur.attr("name")), filterVal ];
}

// I had to change inputTextBinding shiny.js:
// find: function(scope) {
//   return $(scope).find('input[type="text"]:not(.noShinyDefaultBinding)');
// }
var tableFilterBinding = new Shiny.InputBinding();
$.extend(tableFilterBinding, {
   find: function(scope) {
      return $(scope).find(".columnFilters");
   },
   getValue: function(el) {
      // alert($(el).attr("class"));
      // return 1;
      var ret = $(el).find("input").map(getInputVals).get();
      // console.log(ret);
      return ret;
   },
   subscribe: function(el, callback) {
      $(el).on("change.tableFilter", function(e) {
         callback();
      });
   },
   unsubscribe: function(el) {
      $(el).off(".tableFilter");
   }
});
Shiny.inputBindings.register(tableFilterBinding, "hafen.tableFilterBinding");
Shiny.inputBindings.setPriority("hafen.tableFilterBinding", 10);

// for the univariate plot, we need a special input that
// triggers when that particular column was changed
// (the other one returns the filter inputs for all columns)
var tableUniPlotBinding = new Shiny.InputBinding();
$.extend(tableUniPlotBinding, {
   find: function(scope) {
      return $(scope).find(".columnUniPlot");
   },
   getValue: function(el) {
      return $(el).find("input").map(getInputVals).get();
   },
   subscribe: function(el, callback) {
      $(el).on("change.tableUniPlot", function(e) {
         callback();
      });
   },
   unsubscribe: function(el) {
      $(el).off(".tableUniPlot");
   }
});
Shiny.inputBindings.register(tableUniPlotBinding, "hafen.tableUniPlotBinding");

$(document).on("click", "#displayListTable tbody tr", function(evt) {
   // in case there is stuff here
   $("#d3bivar").text("");

   // also reset column indexes
   $("#selectedCogVar").val("");
   $("#selectedCogVar").trigger("change");
   $("#selectedPlotVar").val("");
   $("#selectedPlotVar").trigger("change");   

   var el = $(evt.target);
   $(el).closest("tr").addClass("clicked");
   $('#displayListTable').trigger("change");
   
   // update table dims
   updateCogTableDims();
});

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
      // console.log(res);
      if(uid != "")
         $("#displayListModal").modal('hide');
      // set currentPage back to 1
      $("#currentPage").val(1);
      $("#currentPage").trigger("change");
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

function updateCogTableDims() {
   var windowHeight = $(window).height();
   var windowWidth = $(window).width();

   var cogCols = $("#cogTable th").length;

   var cogTabWidth = Math.min(Math.max(550, 108*cogCols), windowWidth);

   var cogModal = $("#cogModal");

   cogModal.css("margin", "0px");
   cogModal.css("width", 
      (Math.min((cogTabWidth + 20), windowWidth - 20)) + "px"
   );
   cogModal.css("left", 
      (Math.max(windowWidth - (cogTabWidth + 10), 0) / 2) + "px"
   );

   // cogModal.css("height", "738px");
   cogModal.css("top", (Math.max((windowHeight - 738) / 2, 0)) + "px");
   $("#cogTable_wrapper").css("width", cogTabWidth + "px");
}

function updateTableDims() {
   // get window dimensions for plotting
   var windowHeight = $(window).height() - $('#plot-navbar').height() - 40; // 18 is the bottom margin - should change this
   var windowWidth = $(window).width();
   
   var ppp = $("#pppInput").val();
   if(ppp=="")
      ppp = 1;
   ppp = parseInt(ppp);

   // var panelAspect = parseFloat($("#plotAspect").val());
   var panelAspect = parseFloat($("#plotAspect").text());
   // var panelAspect = parseFloat($("#plotHeight").val()) / parseFloat($("#plotWidth").val)

   // see if there are related displays
   var nDisp = getHighlighted($(".selectableDisplayVar")).length + 1;

   var aspects = "";
   // TODO: add hidden output that has aspects of additional displays and take this into account with the calculation...

   if(nDisp > 1 && aspects != "") {
      var innerNcol = Math.ceil(Math.sqrt(nDisp));
      var innerNrow = Math.ceil(nDisp / innerNcol);

      console.log(innerNcol);
      console.log(innerNrow);
      
   }
   
   if(!isNaN(panelAspect)) {
      console.log("aspect " + panelAspect);

      var deviceAspect = windowHeight/windowWidth; 
      var m = Math.max(1, Math.round(Math.sqrt(ppp * deviceAspect/panelAspect))); 
      var n = Math.ceil(ppp/m);
      m = Math.ceil(ppp/n); 
      var nrow = m;
      var ncol = n;

      // TODO: if m*n is less than total number of panels, reduce

      // adjust window width to take padding into account
      windowWidth -= (ncol * 3 + 20);
      windowHeight -= (nrow * 3 + 20);
      plotWidth = Math.round(Math.min(windowWidth / ncol, (windowHeight / nrow) / panelAspect));
      plotHeight = Math.round(plotWidth * panelAspect);

      // now need to take cog values displayed under panels into account
      // each row takes 29 pixels of height, plus 1
      var nCog = getHighlighted($(".selectablePlotVar")).length;
      if(nCog > 0) {
         var totalPlotHeight = (plotHeight + 3) * nrow;
         var cogVarHeight = nCog * 29 + 1;
         
         if(totalPlotHeight + cogVarHeight * nrow > windowHeight) {
            plotHeight = plotHeight - cogVarHeight;
            plotWidth = Math.round(plotHeight / panelAspect);                     
         }
      }

      $("#nRow").val(nrow);
      $("#nRow").trigger("change");

      $("#nCol").val(ncol);
      $("#nCol").trigger("change");

      $("#plotWidth").val(plotWidth);
      $("#plotWidth").trigger("change");

      $("#plotHeight").val(plotHeight);
      $("#plotHeight").trigger("change");
   }
};

$(document).on("change", "#pppInput", function(evt) {
   $("#currentPage").val(1);
   $("#currentPage").trigger("change");
   updateTableDims();
});

var plotMatOutputBinding = new Shiny.OutputBinding();
$.extend(plotMatOutputBinding, {
   find: function(scope) {
     return $(scope).find('.shiny-plotMat-output');
   },
   renderValue: function(el, data) {
      Shiny.unbindAll(el);
      $(el).html(data);
      Shiny.bindAll(el);
      adjustPlotDims();
   }
});
Shiny.outputBindings.register(plotMatOutputBinding, 'shiny.plotMatOutput');

var plotFnOutputBinding = new Shiny.OutputBinding();
$.extend(plotFnOutputBinding, {
   find: function(scope) {
     return $(scope).find('.shiny-plotFn-output');
   },
   renderValue: function(el, data) {
      Shiny.unbindAll(el);
      editor.setValue(data);
      editor.moveCursorTo(0, 0);
      Shiny.bindAll(el);
   }
});
Shiny.outputBindings.register(plotFnOutputBinding, 'shiny.plotFnOutput');

$(document).on("click", "#variableCogSelectBtn", function(evt) {
   var columns = getHighlighted($(".selectableCogVar"));
   
   updateCogTableDims();
   
   $("#selectedCogVar").val(columns);
   $("#selectedCogVar").trigger("change");
   
   // var columns = getHighlighted($(".selectablePlotVar"))
   // $("#selectedPlotVar").val(columns);
   // $("#selectedPlotVar").trigger("change");   
});

$(document).on("click", "#viewOptionsBtn", function(evt) {
   var columns = getHighlighted($(".selectablePlotVar"))
   $("#selectedPlotVar").val(columns);
   $("#selectedPlotVar").trigger("change");

   var plotFn = editor.getValue();
   if(plotFn != "") {
      $("#plotFnInput").text(plotFn);
      $("#plotFnInput").trigger("change");
   }
   adjustPlotDims();
});

$(document).on("click", "#cogTable_varSelect", function(evt) {
   $("#cogModal").modal('hide');
   $("#varCogModal").modal('show');
});

$(document).on("click", "#relatedDisplaySelectBtn", function(evt) {
   var columns = getHighlighted($(".selectableDisplayVar"))

   $("#relatedDisplayVar").val(columns);
   $("#relatedDisplayVar").trigger("change");
   adjustPlotDims();
});

