function updateBigModalDims() {
   var ww = $(window).width();

   $("#cogModal").css("width");
   
   var cogw = $("#cogModal").width();
   // if few variables (it naturally is smaller than max-width), don't stretch it out
   if(cogw < 1100) {
      // if the table is too narrow, stretch it out to the min-width (550px)
      $("#example").css("width", 520)
      
      $(".modalBig").css("margin-left", "-" + (cogw/2) + "px");
   }
   
   // $(".modalBig").css("margin-left", "-550px")

   //    $(".modalBig").css("max-width", ww - 100);
   //    $(".modalBig").css("margin-left", -($(".modalBig").width() + 50)/2);
   //    $(".modalBig").css("margin-right", 50);
}

function updateMiscWindowDims() {
   // set bigModal's width:
	var ww = $(window).width();

   // updateBigModalDims();

   $(".span1").css("width", 60);
   $(".span2").css("width", 140);
   $(".span3").css("width", 220);

   // change spans to width of window if window width is bigger than 940
   if(ww > 940) {
      $(".span1").css("width", (ww - 20*11) / 12);
      $(".span2").css("width", 2 * (ww - 20*11) / 12);
      $(".span3").css("width", 3 * (ww - 20*11) / 12);
   } 
};

function exportToCsv() {
   var curIdx = oTable.fnSettings().aiDisplay;

   var outputText = colNames.join("\",\"") + "\"\n"
   for(i = 0; i < curIdx.length; i++) {
   	outputText += "\"" + oTable.fnGetData(curIdx[i]).join("\",\"") + "\"\n";
   }
   $("#csvText").text(outputText);
};

// first, figure out number of rows and number of columns
function getTableDims() {
   // get window dimensions for plotting
   var windowHeight = $(window).height() - $('#plot-navbar').height() - 40; // 18 is the bottom margin - should change this
   var windowWidth = $(window).width();

   var deviceAspect = windowHeight/windowWidth; 
   var m = Math.max(1, Math.round(Math.sqrt(ppp * deviceAspect/pageAspect))); 
   var n = Math.ceil(ppp/m);
   m = Math.ceil(ppp/n); 
   var nrow = m;
   var ncol = n;

   // adjust window width to take padding into account
   windowWidth -= (ncol * 3 + 20);
   windowHeight -= (nrow * 3 + 20);
   plotWidth = Math.round(Math.min(windowWidth / ncol, (windowHeight / nrow) / pageAspect));
   plotHeight = Math.round(plotWidth * pageAspect);

   tableDims = [nrow, ncol, Math.round(plotWidth)];

   dimString =  "width=\"" + plotWidth + "px\" height=\"" + plotHeight + "px\"";
};

function PadDigits(n, totalDigits) {  
   n = n.toString();  
   var pd = '';  
   if (totalDigits > n.length) {  
      for (i=0; i < (totalDigits-n.length); i++) {  
         pd += '0';  
      } 
   }  
   return pd + n.toString();  
};
 
function setUpVariables() {
   
   tabhtml = "<thead><tr><th>Variable</th><th>Description</th><th style='text-align:center;'>Show in Cognostics</th><th style='text-align:center;'>Show under Plot</th></tr></thead>";
   tabhtml += "<tbody>";
   
   var i = 0;
   var cogChecked;
   var cogAttrChecked;
   getInputCogShowChecked();
   getInputCogAttrShowChecked();
   
   for(i = 0; i < colNames.length; i++) {
       tabhtml += "<tr><td>" + colNames[i] + "</td><td>";
       if(colDescs != null) {
           tabhtml += colDescs[i]
       }
       cogChecked = "checked='checked'";
       cogAttrChecked = "checked='checked'";
       if(!inputCogShowChecked[colNames[i]])
           cogChecked = "";
     if(!inputCogAttrShowChecked[colNames[i]])
         cogAttrChecked = "";
       tabhtml += "</td><td style='text-align:center;'><input type='checkbox' class='inputCogShow' name='" + i + "' " + cogChecked + "></td><td style='text-align:center;'><input type='checkbox' class='inputCogAttrShow' name='" + i + "'" + cogAttrChecked + "></td></tr>";
   };
   tabhtml += "</tbody>";
   $('#varTable').append(tabhtml);   
};

// add click functionality to zoom in on plots
function updatePlotTableLinks() {
   var i = 0;
   var j = 0;
   var url = "";
   var curData;
   var oSettings = oTable.fnSettings();
   for(i = 0; i < tableDims[0] * tableDims[1]; i++) {
      $('#td_' + i).click(function(event) {
         var oSettings = oTable.fnSettings();
         var curDataString = "";
         // build string of cognostic values to send to be displayed
         // TODO: check to see if there are cognostics in the first place
         curTd = Number($(this).attr("name"));
         curData = oTable.fnGetData(currentStartPanel + curTd);
         curDataString = "";
         for(j = 0; j < curData.length; j++) {
            // alert(curDataString);
            curDataString += "<tr><td><strong>" + oSettings.aoColumns[j].sTitle + "</strong></td><td>" + curData[j] + "</td></tr>";
         };
         // alert(curDataString);
         // get plot URL and send to zoom viewer
         if(plotType=='png') {
            url = "viewer_zoom.html?plotURL=" + $(this).find('img').attr('src') + "&cog=" + curDataString;
         } else if(plotType=='swf') {
            url = "viewer_zoom.html?plotURL=" + $(this).find(".swfdiv").find("object").attr("data") + "&cog=" + curDataString;
         }
         window.open(url, url, "scrollbars=no");
         event.preventDefault();
      });
   }
   // update what is shown below each panel
   updateCogAttrShowHide();
};

function buildPngString(i, j, pd, pd_id) {
   if(i*tableDims[1] + j + currentStartPanel < numPanels) {
      curPlot = oTable.fnGetData(pd - 1)[$.inArray("panelKey", colNames)];
      imgsrcString = "<img src='" + plotBaseDir + "/" + curPlot + ".png' width='100%' height='100%' class='png_img' id='panel_" + pd_id + "' alt=''>";
   } else {
      imgsrcString = "<img src='viewerassets/img/blank.gif' width='100%' height='100%' class='png_img' id='panel_" + pd_id + "' alt=''>";
   }
	return "<a href='#' class='plotThumbnail'>" + imgsrcString + "</a>";
};

// see what cognostics table variables should be checked in the variables table
function getInputCogShowChecked() {
   var i = 0;
   // see if the state has been saved
   inputCogShowChecked = JSON.parse(localStorage.getItem(plotName + "_" + "inputCogShowChecked"));
   // if not, by default make all but input variables visible in cognostics table
   if(inputCogShowChecked == null) {
      inputCogShowChecked = {};
      for(i = 0; i < colNames.length; i++) {
         if(jQuery.inArray(colNames[i], cogInputNames) > -1) {
            inputCogShowChecked[colNames[i]] = false;
         } else {
            inputCogShowChecked[colNames[i]] = true;
         }
      }
   }
}

// see what cognstics attributes variables should be checked in the variables table
function getInputCogAttrShowChecked() {
   var i = 0;
   // see if the state has been saved
   inputCogAttrShowChecked = JSON.parse(localStorage.getItem(plotName + "_" + "inputCogAttrShowChecked"));
   // if not, by default make all not checked
   if(inputCogAttrShowChecked == null) {
      inputCogAttrShowChecked = {};
      for(i = 0; i < colNames.length; i++)
         inputCogAttrShowChecked[colNames[i]] = false;
   }
}

function setInputCogShowChecked() {
   $(".inputCogShow").each(function(index, value) {
      inputCogShowChecked[colNames[value.name]] = value.checked;
   });
   localStorage.setItem(plotName + "_" + "inputCogShowChecked", JSON.stringify(inputCogShowChecked));
}

function setInputCogAttrShowChecked() {
   $(".inputCogAttrShow").each(function(index, value) {
      inputCogAttrShowChecked[colNames[value.name]] = value.checked;
   });
   localStorage.setItem(plotName + "_" + "inputCogAttrShowChecked", JSON.stringify(inputCogAttrShowChecked));
}

function buildCogTdString(curPlotIndex) {
   var i = 0;
   var j = 0;
   var cogtdhtml = "<table cellspacing='0' cellpadding='0' border='0' class='table table-condensed table-striped table-bordered cogAttrTable hidden'>";
   var oSettings = oTable.fnSettings();
   var inputVarIdx;
   var selectedVar = "";
   if(curPlotIndex <= nPanels) {
      for(i = 0; i < oSettings.aoColumns.length; i++) {
         inputVarIdx = $.inArray(oSettings.aoColumns[i].sTitle, cogInputNames);
         if(inputVarIdx == -1) {
            cogtdhtml += "<tr class='hidden cogattr_" + i + "'><td><strong>" + oSettings.aoColumns[i].sTitle + "</strong></td><td>" + $.trim(oTable.fnGetData(curPlotIndex - 1)[i]) + "</td></tr>";         
         } else {
            cogtdhtml += "<tr class='hidden cogattr_" + i + "'><td>" + oSettings.aoColumns[i].sTitle + "</td><td>";

            switch(cogInputVars[inputVarIdx]['type']) {
               
               case "checkbox":
                  cogtdhtml += "<input type='checkbox' " + oTable.fnGetData(curPlotIndex - 1)[i] + " class='input cogInput' title='" + i + "' name='" + curPlotIndex + "' autocomplete='off'>";                  
                  break;
               case "text":
                  cogtdhtml += "<input type='text' value='" + oTable.fnGetData(curPlotIndex - 1)[i] + "' class='cogInput' title='" + i + "' name='" + curPlotIndex + "' autocomplete='off' style='margin-bottom:0px;'>";
                  break;
               case "textarea":
                  cogtdhtml += "<textarea rows='1'  style='margin-bottom:0px;' class='input cogInput' title='" + i + "' name='" + curPlotIndex + "' autocomplete='off'>" + oTable.fnGetData(curPlotIndex - 1)[i] + "</textarea>";
                  break;
               case "select":
                  // get the max number of characters
                  var maxnchar = Math.max.apply(Math, jQuery.map(cogInputVars[inputVarIdx]['args'], function(val, i) { return val.length; }));
                  cogtdhtml += "<select size='1' class='cogInput' title='" + i + "' name='" + curPlotIndex + "' style='width:" + (maxnchar * 18) + "px; margin-bottom:1px'>";
                  for(j = 0; j < cogInputVars[inputVarIdx]['args'].length; j++) {
                     // fill it with its current value by default
                     selectedVar = "";
                     if(oTable.fnGetData(curPlotIndex - 1)[i] == cogInputVars[inputVarIdx]['args'][j])
                        selectedVar = "selected";

                     cogtdhtml += "<option value='" + cogInputVars[inputVarIdx]['args'][j] + "' " + selectedVar + ">" + cogInputVars[inputVarIdx]['args'][j] + "</option>";
                  }
                  cogtdhtml += "</select>";
                  break;
            }
            cogtdhtml += "</td>";
         }
      }
   }

   cogtdhtml += "</table>";
   return cogtdhtml;
}

function updateCogAttrShowHide() {
   // get number of checked values to see if we need to unhide the table
   var len = $("#varTable").find(".inputCogAttrShow:checked").length
   if(len == 0) {
      $(".cogAttrTable").addClass("hidden");
   } else {
      $(".cogAttrTable").removeClass("hidden");      
   }

   $("#varTable").find(".inputCogAttrShow").each(function(index, value) {
       // console.log($(this).attr('name') + " " + index + " " + $(this).is(":checked"));
       if($(this).is(":checked")) {
           $(".cogattr_" + value.name).removeClass("hidden");                    
       } else {
           $(".cogattr_" + value.name).addClass("hidden");
       }
   });
   updateCogTdHeights();
   // updatePageAspectWithCog();
}

// should rename to reflect that it updates plot dimensions
function updateCogTdHeights() {
   // check heights of cog tds
   // this was here in case some were higher than others - perhaps should be used in future?
   // $(".td_cog").height("");
   // var tdCogHeights = new Array();
   // $(".td_cog").each(function() { tdCogHeights.push($(this).height());});
   // tdCogHeight = Math.max.apply(Math, tdCogHeights);
   // $(".td_cog").height(tdCogHeight);   
   
   // assume all have same height
   tdCogHeight = $("#td_cog_0").height();
   var extraHeight = tableDims[0] * tdCogHeight;
   var windowHeight = $(window).height() - $('#plot-navbar').height() - 40 - 15 * tableDims[0];
   var scaleFactor = (windowHeight - extraHeight) / (tableDims[0] * plotHeight);
   plotWidth = scaleFactor * plotWidth;
   plotHeight = scaleFactor * plotHeight;
   if((plotWidth * tableDims[1]) > $(window).width()) {
      scaleFactor = $(window).width() / (tableDims[1] * plotWidth);
      plotWidth = scaleFactor * plotWidth;
      plotHeight = scaleFactor * plotHeight;      
   }

   $(".img_td").prop("width", plotWidth);
   $(".img_td").prop("height", plotHeight);
   $(".td_cog").prop("width", plotWidth);
   // if(plotType=="png") {
   //    $(".png_img").width(plotWidth);
   //    $(".png_img").height(plotHeight);
   // }
   if(plotType=="swf") {
      $(".swfdiv").find("object").prop("width", plotWidth - 4);
      $(".swfdiv").find("object").prop("height", plotHeight - 4);
   }
};

function varModalRestoreSettings() {
   // http://stackoverflow.com/questions/426258/how-do-i-check-a-checkbox-with-jquery-or-javascript
   $("#varTable").find(".inputCogAttrShow").each(function(index, value) {
       this.checked = inputCogAttrShowChecked[colNames[index]];
   });
   $("#varTable").find(".inputCogShow").each(function(index, value) {
       this.checked = inputCogShowChecked[colNames[index]];
   });
};

function cogModalRestoreSettings() {
   if(cogTabSortedState != undefined)
      oTable.fnSort(JSON.parse(cogTabSortedState));
   if(cogTabFilterState != null) {
      $("#example").find("tfoot input").each(function(index, val) {
          // alert($(this).prop("value"));
          $(this).prop("value", cogTabFilterState[index]);
      });
      oTable.fnDraw();
   }
};

// TODO: get a little more sophisicated with finding and setting filter state
// example: show a column, set a filter for it, then hide it, then refresh - the filter is gone
function getAndSaveCogTabFilterState() {
   $("#example").find("tfoot input").each(function(index, val) {
       cogTabFilterState[index] = this.value;
   });
   localStorage.setItem(plotName + "_cogTabFilterState", JSON.stringify(cogTabFilterState));   
};



// function updatePageAspectWithCog() {
//    pageAspect = ($("#td_0").height() + tdCogHeight) / $("#td_0").width();
//    getTableDims();
//    updatePlotTable();
// }

function createPlotTable() {
   $("#plotTable").text("");
   // now loop through and create this many list items
   // LOOK AT THIS: fnGetFilteredData()
   var nrow = tableDims[0];
   var ncol = tableDims[1];
   // alert(nrow + " " + ncol);
   var i = 0;
   var j = 0;
   var curPlotIndex = 1;
   var imgsrcString = "";
   plottabhtml = "";
   for (i = 0; i < nrow; i++) { 
      plottabhtml += "<tr>"; 
      for(j = 0; j < ncol; j++) {
			curPlotIndex = $.inArray(oTable.fnSettings().aiDisplay[ncol*i + j + currentStartPanel], oTable.fnSettings().aiDisplayMaster);
			if(curPlotIndex > -1) {
   			curPlotIndex = oTable.fnSettings().aiDisplayMaster[curPlotIndex] + 1;
            // Number(oTable.$("tr").eq(oTable.fnSettings().aiDisplay[ncol*i + j + currentStartPanel]).find("td:first").html());
   			pd = curPlotIndex;
   			pd_id = i*ncol + j;
            plottabhtml += "<td><table cellpadding='0' cellspacing='0'><tr>";
   			if(plotType=='png') {
   				plottabhtml += "<td align='center' class='img_td' id='td_" + (i * ncol + j) + "' " + dimString + " name='" + (i * ncol + j) + "'>" + buildPngString(i, j, pd, pd_id) + "</td>";
               // <img src=\"http://placehold.it/260x180\" alt=\"\" width=\"" + tableDims[2] + "px\">           
   	      } else if(plotType=='swf') {
   				plottabhtml += "<td align='center' class='img_td' id='td_" + (i * ncol + j) + "' " + dimString + " name='" + (i * ncol + j) + "'><div class='swfdiv'><div id='swfdiv_" + pd_id + "'></div></div></td>";
   		   } else {
   			   // svg stuff here...
   			}
   			plottabhtml += "</tr><tr><td valign='top' " + "width=\"" + plotWidth + "px\"" + " class='td_cog' id='td_cog_" + (i * ncol + j) + "'>" + buildCogTdString(curPlotIndex) + "</td></tr></table></td>";			   
			}
      };
      plottabhtml += "</tr>"; 
   };
   $("#plotTable").append(plottabhtml);
   updateCogTdHeights();

   // for swf, we just put empty divs - now we need to populate them
   if(plotType=="swf") {
      var nLoaded = 0;
      for (i = 0; i < nrow; i++) { 
         for(j = 0; j < ncol; j++) {
            curPlotIndex = $.inArray(oTable.fnSettings().aiDisplay[ncol*i + j + currentStartPanel], oTable.fnSettings().aiDisplayMaster);
   			curPlotIndex = oTable.fnSettings().aiDisplayMaster[curPlotIndex] + 1;
   			pd = curPlotIndex;
   			pd_id = Pi*ncol + j;
            // swfobject.embedSWF(plotBaseDir + "/" + plotName + "_" + pd + ".swf", "swfdiv_" + pd_id, tableDims[2] - 0, Math.round(tableDims[2] * pageAspect) - 10, "9");
            // swfobject.embedSWF(plotBaseDir + "/" + plotName + "_" + pd + ".swf", "swfdiv_" + pd_id, "100%", "100%", "9");
            swfobject.embedSWF(plotBaseDir + "/" + plotName + "_" + pd + ".swf", "swfdiv_" + pd_id, tableDims[2] - 10, Math.round(tableDims[2] * pageAspect) - 10, "9");
         }
      }
      $(document).delay(200).queue(function() {
         updateCogTdHeights();
         $(this).dequeue();
      });
   }
   
   updatePageCounter();
   updatePlotTableLinks();
   updateCogChangeListener();
};

function updatePlotTable() {
	// step through each id and update what plot it points to
   numPanels = oTable.fnSettings().fnRecordsDisplay();
	var nrow = tableDims[0];
   var ncol = tableDims[1];
   var pd;
   var i = 0;
   var curPlotIndex = 1;
	for(i = 0; i < nrow*ncol; i++) {
	   if(currentStartPanel + i >= numPanels) {
         pd = -1;
	   } else {
			curPlotIndex = $.inArray(oTable.fnSettings().aiDisplay[i + currentStartPanel], oTable.fnSettings().aiDisplayMaster);
			curPlotIndex = oTable.fnSettings().aiDisplayMaster[curPlotIndex] + 1;
         // curPlotIndex = Number(oTable.$("tr").eq(oTable.fnSettings().aiDisplay[i + currentStartPanel]).find("td:first").html());
	      pd = curPlotIndex;
	   }
	   // update annotations below each panel
      
      if(plotType=="png") {
         if(pd == -1) {
            $("#td_cog_" + i).html("");
            newImgSrc = "viewerassets/img/blank.gif";            
         } else {
            $("#td_cog_" + i).html(buildCogTdString(curPlotIndex));
            curPlot = oTable.fnGetData(pd - 1)[$.inArray("panelKey", colNames)];
          	newImgSrc = plotBaseDir + "/" + curPlot + ".png";
         }
   		$("#panel_" + PadDigits(i, nZeropad)).attr("src", newImgSrc);
      } else if(plotType=="swf") {
         // http://learnswfobject.com/advanced-topics/load-a-swf-using-javascript-onclick-event/
         pd_id = PadDigits(i, nZeropad);
         // alert(pd_id);
         swfobject.removeSWF("swfdiv_" + pd_id);
         
         if(pd == -1) {
            $("#td_cog_" + i).html("");
         } else {
            $("#td_cog_" + i).html(buildCogTdString(curPlotIndex));            
            $("#td_" + i).find(".swfdiv").html("<div id='swfdiv_" + pd_id + "'></div");
            // swfobject.embedSWF(plotBaseDir + "/" + plotName + "_" + pd + ".swf", "swfdiv_" + pd_id, "100%", "100%", "9");                        
            swfobject.embedSWF(plotBaseDir + "/" + plotName + "_" + pd + ".swf", "swfdiv_" + pd_id, tableDims[2] - 10, Math.round(tableDims[2] * pageAspect) - 10, "9");
         }
      }
   }
   updatePageCounter();
   updatePlotTableLinks();
   updateCogChangeListener();
};

var delay = (function(){
  var timer = 0;
  return function(callback, ms){
    clearTimeout (timer);
    timer = setTimeout(callback, ms);
  };
})();

// update json if user adds input
function updateCogChangeListener() {
   $(".cogInput").change(function() {
      // alert($(this).attr('type'));
      var row = Number($(this).attr('name')) - 1;
      var col = Number($(this).attr('title'));
      
      if($(this).attr('type') == "checkbox") {
         var val = "";
         if($(this).is(":checked"))
            val = "checked";
         oTable.fnUpdate("checked", row, col);
         localStorage.setItem(plotName + "_inputData_" + row + "_" + colNames[col], JSON.stringify([row, colNames[col],val]));
      } else {
         oTable.fnUpdate($(this).val(), row, col);
         // also add to localStorage
         localStorage.setItem(plotName + "_inputData_" + row + "_" + colNames[col], JSON.stringify([row, colNames[col],$(this).val()]));
      }
   });
   // $(".cogInput").keyup(function() {
   //    alert("hi");
   // });
   // 
   // $(".cogInput").keyup(function() {
   //     if(this.keyTO) clearTimeout(this.keyTO);
   //     this.keyTO = setTimeout(function() {
   //         $(this).trigger('keyEnd');
   //     }, 250);
   //     
   //     $(this).bind('keyEnd', function() {
   //        alert("hi");
   //     });
   // });
   
   // $(".cogInput").bind('keyEnd', function() {
   //    alert("hi");
   // });
}

// $('.cogInput').keyup(function() {
//     delay(function() {
//        alert($(this).attr('name'));
//        var row = Number($(this).attr('name')) - 1;
//        var col = Number($(this).attr('title'));
// 
//        if($(this).attr('type') == "checkbox") {
//           var val = "";
//           if($(this).is(":checked"))
//              val = "checked";
//           oTable.fnUpdate("checked", row, col);
//           localStorage.setItem(plotName + "_inputData_" + row + "_" + colNames[col], JSON.stringify([row, colNames[col],val]));
//        } else {
//           oTable.fnUpdate($(this).val(), row, col);
//           // also add to localStorage
//           localStorage.setItem(plotName + "_inputData_" + row + "_" + colNames[col], JSON.stringify([row, colNames[col],$(this).val()]));
//        }
//     }, 300 );
// });


function filteredKeys(obj, filter) {
   var keys = [];
   for (var key in obj) {
      if (obj.hasOwnProperty(key) && key.match(filter)) {
         keys.push(key);
      }
   }
   return keys;
}

// if there is user input in localstorage, update cog table with it
// TODO: it might be good to track the number of rows in localstorage - if this changes from one session to another
// then user input might be invalid because of new records added
function addLocalStorageUserInput() {
   var inputKeys = filteredKeys(localStorage, plotName + "_inputData_");
   var i;
   var current;
   var row;
   for(i = 0; i < inputKeys.length; i++) {
      current = JSON.parse(localStorage.getItem(inputKeys[i]));
      row = colNames.indexOf(current[1])
      if(row > -1)
         oTable.fnUpdate(current[2], current[0], row);
   }
};

function updateCogVariableVisibility() {
   $("#varTable").find(".inputCogShow").each(function(index, value) {
       // console.log($(this).attr('name') + " " + index + " " + $(this).is(":checked"));
       oTable.fnSetColumnVis(index, $(this).is(":checked"));
   });
};

function updatePageCounter() {
   numScreens = Math.ceil(numPanels / (tableDims[0] * tableDims[1]));
   $("#btnCurrentPage").text((currentStartPanel / (tableDims[0]*tableDims[1]) + 1) + " / " + numScreens);
};

function pageBack() {
   if(currentStartPanel != 0) {
      currentStartPanel -= tableDims[0]*tableDims[1];
      if(currentStartPanel < 0)
         currentStartPanel = 0;
      updatePlotTable();
	   localStorage.removeItem(plotName + "_" + "currentStartPanel");
      localStorage.setItem(plotName + "_" + "currentStartPanel", String(currentStartPanel));                                
      updatePageCounter();
   }
};

function pageForward() {
   if(currentStartPanel + tableDims[0]*tableDims[1] < numPanels) {
      currentStartPanel += tableDims[0]*tableDims[1];
      updatePlotTable();
		localStorage.removeItem(plotName + "_" + "currentStartPanel");
      localStorage.setItem(plotName + "_" + "currentStartPanel", String(currentStartPanel));
      updatePageCounter();
   }
};

// for reset button:
// need something like this...
// $(".number_filter").each(function(intIndex) { $(this).value=""; })
function resetTable() {
   // go through all column filters and get rid of values
   $("tfoot").find("input").each(function() {$(this).val("");})
   $("tfoot").find("input").each(function() {$(this).focus();})
   // then go through and apply the blur function so the
   // column names are restored
   $("tfoot").find("input").each(function() {$(this).blur();})
   // clear the overall filter
   oTable.fnFilterClear();
   // restore sort order
   oTable.fnSortNeutral();      
};

// make NAs sort right
jQuery.fn.dataTableExt.oSort['numeric_ignore_nan-asc']  = function(x,y) {
    if (isNaN(x) && isNaN(y)) return ((x < y) ? 1 : ((x > y) ?  -1 : 0));

    if (isNaN(x)) return 1;
    if (isNaN(y)) return -1;

    x = parseFloat( x );
    y = parseFloat( y );
    return ((x < y) ? -1 : ((x > y) ?  1 : 0));
};
jQuery.fn.dataTableExt.oSort['numeric_ignore_nan-desc'] = function(x,y) {
    if (isNaN(x) && isNaN(y)) return ((x < y) ? 1 : ((x > y) ?  -1 : 0));

    if (isNaN(x)) return 1;
    if (isNaN(y)) return -1;

    x = parseFloat( x );
    y = parseFloat( y );
    return ((x < y) ?  1 : ((x > y) ? -1 : 0));
};

