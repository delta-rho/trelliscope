
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
   
   $('#varCogTipPopover').popover({placement: 'bottom', trigger: 'hover', offset: 1, html: true});
   
   $('#varPanelTipPopover').popover({placement: 'bottom', trigger: 'hover', offset: 1, html: true});
   
   $('#varRelatedTipPopover').popover({placement: 'bottom', trigger: 'hover', offset: 1, html: true});
   
   $('#plotFnUpdateTipPopover').popover({placement: 'bottom', trigger: 'hover', offset: 1, html: true});
   
   $('#panelLayoutTipPopover').popover({placement: 'bottom', trigger: 'hover', offset: 1, html: true});
            
   function hideColumn(columnIndex) {
      $('#displayListTable td:nth-child('+(columnIndex+1)+'), #displayListTable th:nth-child('+(columnIndex+1)+')').hide();
   }
   
   hideColumn(0);
   hideColumn(6);
   hideColumn(7);
   hideColumn(8);
   hideColumn(9);
   hideColumn(10);
   hideColumn(12);
   hideColumn(13);
   hideColumn(14);
   
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
         if($("#viewOptionsModal").is(":visible"))
            $("#viewOptionsModal").modal('hide');
         if($("#cogModal").is(":visible"))
            $("#cogModal").modal('hide');
         if($("#varCogModal").is(":visible"))
            $("#varCogModal").modal('hide');
         if($("#d3histModal").is(":visible"))
            $("#d3histModal").modal('hide');
         if($("#d3bivarModal").is(":visible"))
            $("#d3bivarModal").modal('hide');
         if($("#relatedDisplayModal").is(":visible"))
            $("#relatedDisplayModal").modal('hide');
         if($("#displayListModal").is(":visible"))
            $("#displayListModal").modal('hide');
      } else if(event.keyCode==67) { // show cog if press 'c'
         if(!$(".modal").is(":visible"))
            $("#cogModal").modal('show');
      } else if(event.keyCode==86) { // show cog if press 'v'
         if(!$(".modal").is(":visible"))
            $("#viewOptionsModal").modal('show');
      } else if(event.keyCode==82) { // show related if press 'r'
         if(!$(".modal").is(":visible"))
            $("#relatedDisplayModal").modal('show');
      } else if(event.keyCode==68) { // show displayList if press 'd'
         if(!$(".modal").is(":visible"))
            $("#displayListModal").modal('show');
      }
      
   });
   
   $("#cogFilterMenu").css("min-width", "106px");
   
   $('#varCogModal').on('hidden', function () {
      $("#cogModal").modal("show");
   })
   
   $('#cogModal').on('shown', function () {
      updateCogTableDims();
   })
   
   var windowWidth = $(window).width();

   var displayListModalWidth = Math.min(850, windowWidth - 100);
   $("#displayListModal").css("margin-left", "0px");
   $("#displayListModal").css("width", displayListModalWidth + "px")
   $("#displayListModal").css("left", 
      (Math.max(windowWidth - (displayListModalWidth + 10), 0) / 2) + "px"
   );
   
   
   // remove fade-in behavior of modals
   var modals = $(".modal");
   var filtermodals = $(".modalFilter");
   
   modals.removeClass("fade")
   modals.removeClass("in")
   filtermodals.removeClass("fade")
   filtermodals.removeClass("in")
   
   var hp = getHashParams();
   
   if(hp["ncol"] != undefined) {
      // $("#panelCols").val(hp["ncol"]);
      alert($("#panelCols").val());
      // $("#panelCols").trigger("change");
   }
   
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
   
   // console.log(plotWidth);
   
   $(".plotTableWrap").css("width", plotWidth);
   $(".plotTableWrap").css("height", plotHeight);

   // resize each image so firefox is happy...

   $(".panelWrap").height(plotHeight - 5);
   
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

function pageBeg() {
   $("#currentPage").val(1);
   $("#currentPage").trigger("change");
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
         if(isNaN(tmp)) {
            tmp = 0;            
         } else {
            // console.log("** Setting cognostics sort order:"+returnVal);            
         }
         return tmp;
      }).get();
      // console.log($(el).attr("class"));
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
      return $(scope).find(".cogTablePaginate");
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


getColumFilterInputs = function() {
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
   // if(filterType=="from" || filterType=="to")
   //    filterVal = parseFloat(filterVal);
   
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
      var ret = $(el).find("input").map(getColumFilterInputs).get();
      // console.log(ret);

      // need to redraw histogram for any that have filter values
      hasFilter = [];
      for (var i=0; i < ret.length; i++) {
         if(ret[i][0] != "regex" && ret[i][2] != "")
            hasFilter.push(ret[i]);
      };
      if(hasFilter.length > 0) {
         for (var i=0; i < hasFilter.length; i++) {
            // this could be doubly redundant but good enough for now
            updated3footHist(hasFilter[i][1]);
         };
      }

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
      return $(el).find("input").map(getColumFilterInputs).get();
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

function updateCogTableDims() {
   var windowHeight = $(window).height();
   var windowWidth = $(window).width();
   
   // var totColWidth = 0;
   // $.each($("#cogTable th"), function() {
   //     totColWidth += $(this).width();
   // });
   
   var totColWidth = $("#cogColumnSortInput").width();
   
   var cogTabWidth = Math.min(Math.max(550, totColWidth), windowWidth - 20);
   
   var cogModal = $("#cogModal");
   
   cogModal.css("margin", "0px");
   cogModal.css("width", 
      (Math.min((cogTabWidth + 20), windowWidth - 20)) + "px"
   );
   cogModal.css("left", 
      (Math.max(windowWidth - (cogTabWidth + 10), 0) / 2) + "px"
   );

   // cogModal.css("height", "738px");
   cogModal.css("top", (Math.max((windowHeight - 651) / 2, 0)) + "px");
   $("#cogTable_wrapper").css("width", cogTabWidth - 29 + "px");
}

function updateTableDims() {
   // rule for multiple displays:
   // display them in a table with at most 2 rows
   // for 1-3 total displays, stay with 1 row
   // for 4+ total displays, use 2 rows
   // the only variable will be height
   // based on height, calculate the total aspect ratio for each row
   // the maximum aspect ratio will be
   
   
   // get window dimensions for plotting
   var windowHeight = $(window).height() - $('#plot-navbar').height() - 50; // 18 is the bottom margin - should change this
   var windowWidth = $(window).width() - 20;
   
   var nrow = $("#panelRows").val();
   if(nrow=="")
      nrow = 1;
   nrow = parseInt(nrow);
   
   var ncol = $("#panelCols").val();
   if(ncol=="")
      ncol = 1;
   ncol = parseInt(ncol);
   
   // var panelAspect = parseFloat($("#panelAspect").val());
   var panelAspect = parseFloat($("#panelAspect").text());
   // var panelAspect = parseFloat($("#plotHeight").val()) / parseFloat($("#plotWidth").val)
   
   // see if there are related displays
   var nDisp = getHighlighted($(".selectableDisplayVar")).length + 1;
   
   var aspects = "";
   // TODO: add hidden output that has aspects of additional displays and take this into account with the calculation...
   
   if(nDisp > 1 && aspects != "") {
      var innerNcol = Math.ceil(Math.sqrt(nDisp));
      var innerNrow = Math.ceil(nDisp / innerNcol);

      // console.log(innerNcol);
      // console.log(innerNrow);
      
   }
   
   if(!isNaN(panelAspect)) {
      console.log("aspect " + panelAspect);
      
      // TODO: if m*n is less than total number of panels, reduce
      
      // adjust window width to take padding into account
      windowWidth -= (ncol * 3 + 20);
      windowHeight -= (nrow * 3 + 20);
      plotWidth = Math.round(Math.min(windowWidth / ncol, (windowHeight / nrow) / panelAspect));
      plotHeight = Math.round(plotWidth * panelAspect);
      
      // now need to take cog values displayed under panels into account
      // each row takes 29 pixels of height, plus 1
      // the td has padding above and below of 4px, so plus 8
      var nCog = getHighlighted($(".selectablePlotVar")).length;
      if(nCog > 0) {
         var totalPlotHeight = (plotHeight + 3) * nrow;
         var cogVarHeight = nCog * 29 + 9;
         
         if(totalPlotHeight + cogVarHeight * nrow > windowHeight) {
            plotHeight = plotHeight - cogVarHeight;
            plotWidth = Math.round(plotHeight / panelAspect);                     
         }
      }
      
      $("#plotWidth").val(plotWidth);
      $("#plotWidth").trigger("change");
      
      $("#plotHeight").val(plotHeight);
      $("#plotHeight").trigger("change");
   }
};


$(document).on("change", "#pppInput", function(evt) {
   
   // get window dimensions for plotting
   var windowHeight = $(window).height() - $('#plot-navbar').height() - 50; // 18 is the bottom margin - should change this
   var windowWidth = $(window).width() - 20;
   var panelAspect = parseFloat($("#panelAspect").text());
   
   var ppp = $("#pppInput").val();
   if(ppp=="")
      ppp = 1;
   ppp = parseInt(ppp);

   var deviceAspect = windowHeight/windowWidth; 
   var m = Math.max(1, Math.round(Math.sqrt(ppp * deviceAspect/panelAspect))); 
   var n = Math.ceil(ppp/m);
   m = Math.ceil(ppp/n); 
   var nrow = m;
   var ncol = n;

   console.log(ppp);
   
   $("#panelRows").val(nrow);
   $("#panelCols").val(ncol);
   
   $("#panelRows").trigger("change");
   $("#panelCols").trigger("change");
   
   $("#currentPage").val(1);
   $("#currentPage").trigger("change");
   
   updateTableDims();
});

$(document).on("change", "#panelCols", function(evt) {
   $("#currentPage").val(1);
   $("#currentPage").trigger("change");

   updateTableDims();   
});

$(document).on("change", "#panelRows", function(evt) {
   $("#currentPage").val(1);
   $("#currentPage").trigger("change");

   updateTableDims();
});

var plotMatOutputBinding = new Shiny.OutputBinding();
$.extend(plotMatOutputBinding, {
   find: function(scope) {
      return $(scope).find('.shiny-panelLayout-output');
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
   
   $("#selectedCogTableVar").val(columns);
   $("#selectedCogTableVar").trigger("change");
   
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
   
   $("#relatedDisplayUID").val(columns);
   $("#relatedDisplayUID").trigger("change");
   adjustPlotDims();
});


$(document).on("click", "#btnUpdatePlots", function(evt) {
   pageBeg();
});



// receives a list of base64 pngs, with key being the id it goes in
// used for panel output
var plotGroupOutputBinding = new Shiny.OutputBinding();
$.extend(plotGroupOutputBinding, {
   find: function(scope) {
      return $(scope).find('.shiny-plotgroup-output');
   },
   renderValue: function(el, data) {
      // set each element's data to the value
      if(data != null) {
         $.each(data, function(key, value) {
            var img = null;
            if (value) {
               img = document.createElement('img');
               img.src = value;
               img.setAttribute("class", "panelImg");
               img.setAttribute("height", "100%");
               img.setAttribute("width", "100%");
            }
            $("#"+key).empty();
            if (img)
               $("#"+key).append(img);
         });         
      }
   }
});
Shiny.outputBindings.register(plotGroupOutputBinding, 'shiny.plotGroupOutput');
Shiny.outputBindings.setPriority("shiny.plotGroupOutput", 10);

// receives a list of tables to show cognostic values under panels
// but could be used generally for other things - just needs a list of html text where each element is named according to the CSS id it wants its stuff put in
var htmlGroupOutputBinding = new Shiny.OutputBinding();
$.extend(htmlGroupOutputBinding, {
   find: function(scope) {
      return $(scope).find('.shiny-htmlgroup-output');
   },
   renderValue: function(el, data) {
      // set each element's data to the value
      if(data != null) {
         $.each(data, function(key, value) {
            $("#"+key).html(value);
         });         
      }
   }
});
Shiny.outputBindings.register(htmlGroupOutputBinding, 'shiny.htmlGroupOutput');
Shiny.outputBindings.setPriority("shiny.htmlGroupOutput", -20);

function getHashParams() {
   var hashParams = {};
   var e,
      a = /\+/g,  // Regex for replacing addition symbol with a space
      r = /([^&;=]+)=?([^&;]*)/g,
      d = function (s) { return decodeURIComponent(s.replace(a, " ")); },
      q = window.location.hash.substring(1);

   while (e = r.exec(q))
      hashParams[d(e[1])] = d(e[2]);

   return hashParams;
}




