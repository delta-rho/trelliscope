
function bivarFilterPlotBrushFn() {
  var curBrush = bivarFilterPlotBrush.empty() ? bivarFilterPlotX.domain() : bivarFilterPlotBrush.extent();

  var prec = d3.format(".5r");

  activeXvar = $("#bivar-x-filter-select li.active");
  correspYvar = $("#bivar-y-" + activeXvar.data("name"));
  activeYvar = $("#bivar-y-filter-select li.active");
  correspXvar = $("#bivar-x-" + activeYvar.data("name"));

  if(curBrush[0].length === undefined) {
    curBrush = "";
    $("#bivarFilterPlotRange").html("");
    // make sure filter icon is hidden
    activeXvar.find("i").addClass("hidden");
    correspYvar.find("i").addClass("hidden");
    activeYvar.find("i").addClass("hidden");
    correspXvar.find("i").addClass("hidden");
    $("#bivar-y-filter-select li.active i").addClass("hidden");
  } else {
    // console.log(curBrush[0][0]);
    $("#bivarFilterPlotRange").html("x: " + prec(curBrush[0][0]) + " to " + prec(curBrush[1][0]) + " </br> y: " + prec(curBrush[0][1]) + " to " + prec(curBrush[1][1]) + "");

    // // show filter icon if brush is legitimate
    // var xDomain = bivarFilterPlotX.domain();
    // var yDomain = bivarFilterPlotY.domain();
    // var xf = [curBrush[0][0], curBrush[1][0]];
    // var yf = [curBrush[0][1], curBrush[1][1]];
    //
    // if both x and y fill the domain, don't brush
    // yFill = (yf[1] - yf[0]) > (yDomain[1] - yDomain[0]) / 1.01;
    // xFill = (xf[1] - xf[0]) > (xDomain[1] - xDomain[0]) / 1.01;
    //
    // if(xFill) {
    //   activeXvar.find("i").addClass("hidden");
    //   correspYvar.find("i").addClass("hidden");
    // } else if(xf[0] != xf[1]) {
    //   activeXvar.find("i").removeClass("hidden");
    //   correspYvar.find("i").removeClass("hidden");
    // }
    //
    // if(yFill) {
    //   activeYvar.find("i").addClass("hidden");
    //   correspXvar.find("i").addClass("hidden");
    // } else if(yf[0] != yf[1]) {
    //   activeYvar.find("i").removeClass("hidden");
    //   correspXvar.find("i").removeClass("hidden");
    // }
  }
}

function multivarFilterPlotBrushFn() {
  var curBrush = multivarFilterPlotBrush.empty() ? multivarFilterPlotX.domain() : multivarFilterPlotBrush.extent();

  var prec = d3.format(".5r");

  if(curBrush[0].length === undefined) {
    curBrush = "";
    $("#multivarFilterPlotRange").html("");
  } else {
      $("#multivarFilterPlotRange").html("x: " + prec(curBrush[0][0]) + " to " + prec(curBrush[1][0]) + " </br> y: " + prec(curBrush[0][1]) + " to " + prec(curBrush[1][1]) + "");
  }
}

var bivarFilterPlotMargin = {top: 10, right: 10, bottom: 60, left: 60},
   width = 585 - bivarFilterPlotMargin.left - bivarFilterPlotMargin.right,
   height = 440 - bivarFilterPlotMargin.top - bivarFilterPlotMargin.bottom;
   // width: 515
   // height: 370

var bivarFilterPlotX = d3.scale.linear()
   .range([0, width]);

var bivarFilterPlotY = d3.scale.linear()
   .range([height, 0]);

var bivarFilterPlotXaxis = d3.svg.axis()
   .scale(bivarFilterPlotX)
   .orient("bottom");

var bivarFilterPlotYaxis = d3.svg.axis()
   .scale(bivarFilterPlotY)
   .orient("left");

var bivarFilterPlotBrush = d3.svg.brush()
   .x(bivarFilterPlotX)
   .y(bivarFilterPlotY)
   .on("brush", bivarFilterPlotBrushFn);

var multivarFilterPlotX = d3.scale.linear()
   .range([0, width]);

var multivarFilterPlotY = d3.scale.linear()
   .range([height, 0]);

var multivarFilterPlotXaxis = d3.svg.axis()
   .scale(multivarFilterPlotX)
   .orient("bottom");

var multivarFilterPlotYaxis = d3.svg.axis()
   .scale(multivarFilterPlotY)
   .orient("left");

var multivarFilterPlotBrush = d3.svg.brush()
   .x(multivarFilterPlotX)
   .y(multivarFilterPlotY)
   .on("brush", multivarFilterPlotBrushFn);

var multivarFilterPlotBrush = d3.svg.brush()
   .x(multivarFilterPlotX)
   .y(multivarFilterPlotY)
   .on("brush", multivarFilterPlotBrushFn);

function d3bivar(data, id) {

  // var data ={ "data" : [ { "x" : 29.51, "y" : 12.43 }, { "x" : 41.51, "y" : 12.8 }, { "x" : 29.05, "y" : 15 }, { "x" : 25.7, "y" : 9.13 }, { "x" : 35.83, "y" : 18.6 }, { "x" : 20.81, "y" : 17.8 }, { "x" : 41.87, "y" : 24.7 }, { "x" : 43.66, "y" : 11.73 }, { "x" : 54.35, "y" : 19 }, { "x" : 30.29, "y" : 8.23 }, { "x" : 31.18, "y" : 21.2 }, { "x" : 29.29, "y" : 21.17 } ], "plotType" : [ "scatter" ], "xlab" : [ "meanYield" ], "ylab" : [ "range" ] };

  // var data = { "data" : [ { "x" : 30.2, "y" : 8.23, "r" : 0.89 }, { "x" : 25.51, "y" : 9.03, "r" : 0.89 }, { "x" : 43.95, "y" : 11.81, "r" : 0.89 }, { "x" : 29.53, "y" : 12.2, "r" : 0.89 }, { "x" : 41.6, "y" : 13, "r" : 0.89 }, { "x" : 29.19, "y" : 14.98, "r" : 0.89 }, { "x" : 20.81, "y" : 17.76, "r" : 0.89 }, { "x" : 35.57, "y" : 18.55, "r" : 0.89 }, { "x" : 54.68, "y" : 18.95, "r" : 0.89 }, { "x" : 29.19, "y" : 21.33, "r" : 0.89 }, { "x" : 31.21, "y" : 21.33, "r" : 0.89 }, { "x" : 41.94, "y" : 24.51, "r" : 0.89 } ], "plotType" : [ "hexbin" ], "xlab" : [ "meanYield" ], "ylab" : [ "range" ], "shape" : [ 0.72 ], "hexx" : [ 0.34, 0.34, 0, -0.34, -0.34, 0 ], "hexy" : [ 0.13, -0.13, -0.26, -0.13, 0.13, 0.26 ] };

  var xLog = data.xLog[0];
  var yLog = data.yLog[0];
  // **log**
  if(xLog !== "NA") {
    data.xlab = "log base " + xLog + " " + data.xlab;
  }
  if(yLog !== "NA") {
    data.ylab = "log base " + yLog + " " + data.ylab;
  }

  $("#" + id).html("");
  $("#" + id).append("<div id=\"" + id + "Range\" class=\"filterRange\"></div>");

  var svg = d3.select("#" + id).append("svg:svg")
     .attr("width", width + bivarFilterPlotMargin.left + bivarFilterPlotMargin.right)
     .attr("height", height + bivarFilterPlotMargin.top + bivarFilterPlotMargin.bottom)
    .append("g")
     .attr("transform", "translate(" + bivarFilterPlotMargin.left + "," + bivarFilterPlotMargin.top + ")");

  var xFrom = null;
  var xTo = null;
  var filterData, varName;
  activeXvar = $("#bivar-x-filter-select li.active");
  if(activeXvar) {
    filterData = $("#univarFilterState").data("filterData");
    if(filterData) {
      varName = activeXvar.data("name");
      if(filterData[varName]) {
        xFrom = filterData[varName].from;
        xTo = filterData[varName].to;
        // **log**
        if(xLog !== "NA") {
          xFrom = Math.log(xFrom) / Math.log(xLog);
          xTo = Math.log(xTo) / Math.log(xLog);
        }
      }
    }
  }

  var yFrom = null;
  var yTo = null;
  activeYvar = $("#bivar-y-filter-select li.active");
  if(activeYvar) {
    filterData = $("#univarFilterState").data("filterData");
    if(filterData) {
      varName = activeYvar.data("name");
      if(filterData[varName]) {
        yFrom = filterData[varName].from;
        yTo = filterData[varName].to;
        // **log**
        if(yLog !== "NA") {
          yFrom = Math.log(yFrom) / Math.log(yLog);
          yTo = Math.log(yTo) / Math.log(yLog);
        }
      }
    }
  }

  var xrange = d3.extent(data.data.map(function(d) { return d.x; }));
  xrange[0] = xrange[0] - (xrange[1] - xrange[0]) * 0.07;
  xrange[1] = xrange[1] + (xrange[1] - xrange[0]) * 0.07;

  if(xFrom) {
    xrange[0] = Math.min(xrange[0], xFrom);
    xrange[0] = xrange[0] - 0.1 * (xrange[1] - xrange[0]);
  }
  if(xTo) {
    xrange[1] = Math.max(xrange[1], xTo);
    xrange[1] = xrange[1] + 0.1 * (xrange[1] - xrange[0]);
  }

  var yrange = d3.extent(data.data.map(function(d) { return d.y; }));
  yrange[0] = yrange[0] - (yrange[1] - yrange[0]) * 0.07;
  yrange[1] = yrange[1] + (yrange[1] - yrange[0]) * 0.07;

  if(yFrom) {
    yrange[0] = Math.min(yrange[0], yFrom);
    yrange[0] = yrange[0] - 0.1 * (yrange[1] - yrange[0]);
  }
  if(yTo) {
    yrange[1] = Math.max(yrange[1], yTo);
    yrange[1] = yrange[1] + 0.1 * (yrange[1] - yrange[0]);
  }

  window[id + "X"].domain(xrange);
  window[id + "Y"].domain(yrange);

  if(data.plotType == "scatter") {
    svg.selectAll(".points")
      .data(data.data)
      .enter().append("svg:circle")
      .attr("class", "bivar-points")
      .attr("r", function(d) { return 4; })
      .attr("cx", function(d) { return window[id + "X"](d.x); })
      .attr("cy", function(d) { return window[id + "Y"](d.y); });
  } else {
    svg.selectAll(".polygon")
      .data(data.data)
      .enter()
      .append("svg:polygon")
      .attr("points",function(d) {
        var polystr = "";
        for (var i = 0; i < data.hexx.length; i++) {
          polystr += window[id + "X"](d.x + data.hexx[i] * d.r) + "," + window[id + "Y"](d.y + data.hexy[i] * d.r) + " ";
        }
        return(polystr);
      })
      .attr("fill", "steelblue")
      .attr("stroke-width", 0);
  }

  // TODO?: tooltips: http://bl.ocks.org/1373263

  svg.append("g")
    .attr("class", "x brush")
    .call(window[id + "Brush"])
    .selectAll("rect")
    .attr("y", -6);

  // y-axis label
  svg.append("text")
    .attr("text-anchor", "middle")
    .attr("transform", "translate(" + (-50) + "," + (height/2) + ")rotate(-90)")
    .text(data.ylab);

  // x-axis label
  svg.append("text")
    .attr("text-anchor", "middle")
    .attr("transform", "translate("+ (width/2) + "," + (height + 50) +")")
    .text(data.xlab);

  svg.append("g")
    .attr("class", "x axis")
    .attr("transform", "translate(0," + height + ")")
    .call(window[id + "Xaxis"]);

  svg.append("g")
    .attr("class", "y axis")
    .call(window[id + "Yaxis"]);
}


// $(document).on("click", "#d3bivarSubmit", function(evt) {
//   var curRange = bivarFilterPlotBrush.empty() ? "" : bivarFilterPlotBrush.extent();
//
//   if(curRange != "") {
//     // clear out all columns
//     // (this currently doesn't operate as a marginal filter)
//     $(".columnFilterFrom,.columnFilterTo").each(function() {
//       $(this).val(""); $(this).trigger("change");
//     })
//
//     // get the current columns
//     var columns = getHighlighted($("#cogTable_bivar_select").children());
//
//     if(columns.length==2) {
//       // trigger a change on the appropriate range
//       $("#lower_column_" + columns[0]).val(curRange[0][0]);
//       $("#upper_column_" + columns[0]).val(curRange[1][0]);
//       $("#lower_column_" + columns[1]).val(curRange[0][1]);
//       $("#upper_column_" + columns[1]).val(curRange[1][1]);
//
//       $("#lower_column_" + columns[0]).trigger("change");
//       $("#upper_column_" + columns[0]).trigger("change");
//       $("#lower_column_" + columns[1]).trigger("change");
//       $("#upper_column_" + columns[1]).trigger("change");
//       updated3footHist(columns[0]);
//       updated3footHist(columns[1]);
//
//       // TODO: when other columns have had univariate filters, the d3foothist might not get cleared out
//     }
//   }
//
//   $("#d3bivarModal").modal("hide");
//   bivarFilterPlotBrush.clear()
//
//   // clear the inputs too
//   $("#cogTable_bivar_select").children().each(function(i, el) { $(el).removeClass("highlighted") });
//   $("#bivarColumns").val("");
//   $("#bivarColumns").trigger("change");
// });

