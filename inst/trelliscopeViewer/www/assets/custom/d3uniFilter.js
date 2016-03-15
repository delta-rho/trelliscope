
function d3univarXbrushFn() {
  var curBrush = d3univarXbrush.empty() ? "" : d3univarXbrush.extent();
  var prec = d3.format(".5r");
  if(curBrush === "") {
    $("#univarFilterPlotRange").text("");
    // make sure filter icon is hidden
    $("#univarFilterSelect li.active i").addClass("hidden");
  } else {
    $("#univarFilterPlotRange").text(prec(curBrush[0]) + " to " + prec(curBrush[1]));
    // show filter icon
    if(curBrush[0] != curBrush[1])
      $("#univarFilterSelect li.active i").removeClass("hidden");
  }
}

function d3univarYbrushFn() {
  var curBrush = d3univarYbrush.empty() ? "" : d3univarYbrush.extent();
  var prec = d3.format(".5r");
  if(curBrush === "") {
    $("#univarFilterPlotRange").text("");
    // make sure filter icon is hidden
    $("#univarFilterSelect li.active i").addClass("hidden");
  } else {
    $("#univarFilterPlotRange").text(prec(curBrush[0]) + " to " + prec(curBrush[1]));
    // show filter icon
    if(curBrush[0] != curBrush[1])
      $("#univarFilterSelect li.active i").removeClass("hidden");
  }
}

var d3univarMargin = {top: 10, right: 10, bottom: 60, left: 60},
   d3univarWidth = 585 - d3univarMargin.left - d3univarMargin.right,
   d3univarHeight = 440 - d3univarMargin.top - d3univarMargin.bottom;

var d3univarX = d3.scale.linear()
   .range([0, d3univarWidth]);

var d3univarY = d3.scale.linear()
   .range([d3univarHeight, 0]);

var d3univarXaxis = d3.svg.axis()
   .scale(d3univarX)
   .orient("bottom");

var d3univarYaxis = d3.svg.axis()
   .scale(d3univarY)
   .orient("left");

var d3univarXbrush = d3.svg.brush()
   .x(d3univarX)
   .on("brush", d3univarXbrushFn);

var d3univarYbrush = d3.svg.brush()
   .y(d3univarY)
   .on("brush", d3univarYbrushFn);

function d3univar(data, id) {

  var log = data.log[0];
  var plotType = data.plotType[0];
  var xlab = data.name[0];
  if(log !== "NA") {
    xlab = "log base " + log + " " + xlab;
  }
  var ylab = "Frequency";
  var yAxisOffset = -45;
  if(plotType == "quant") {
    ylab = data.name[0];
    // **log**
    if(log !== "NA") {
      ylab = "log base " + log + " " + ylab;
    }
    xlab = "f-value";
  } else if(plotType == "bar") {
    ylab = data.name[0];
    xlab = "Frequency";
    yAxisOffset = -20;
  }

  data = data.data;

  $("#" + id).html("");
  $("#" + id).append("<div id=\"" + id + "Range\" class=\"filterRange\"></div>");

  var svg = d3.select("#" + id).append("svg:svg")
     .attr("width", d3univarWidth + d3univarMargin.left + d3univarMargin.right)
     .attr("height", d3univarHeight + d3univarMargin.top + d3univarMargin.bottom)
    .append("g")
     .attr("transform", "translate(" + d3univarMargin.left + "," + d3univarMargin.top + ")");

  // if there is a filter for this variable
  // we will use that to make sure the extent of the axes
  // reaches far enough
  activeVar = $("#univarFilterSelect li.active");
  var filter;
  if(activeVar) {
    var filterData = $("#univarFilterState").data("filterData");
    if(!filterData)
      filterData = {};
    var varName = activeVar.data("name");
    if(filterData[varName]) {
      var filterFrom = NaN;
      var filterTo = NaN;
      var theFilter = filterData[varName];
      if(theFilter.from)
        filterFrom = theFilter.from;
      if(theFilter.to)
        filterTo = theFilter.to;
      filter = {from: filterFrom, to: filterTo};
      // **log**
      if(filter && log !== "NA") {
        filter.to = Math.log(filter.to) / Math.log(log);
        filter.from = Math.log(filter.from) / Math.log(log);
      }
    }
  }

  var xrange, yrange, rdelta, delta;

  if(plotType == "hist") {
    delta = data[1].xdat - data[0].xdat;

    xrange = d3.extent(data.map(function(d) { return d.xdat; }));
    xrange[0] = xrange[0] - (xrange[1] - xrange[0]) * 0.07;
    xrange[1] = xrange[1] + (xrange[1] - xrange[0]) * 0.07;

    if(filter !== undefined) {
      if(isNaN(filter.from)) {
        filter.from = xrange[0];
      }
      if(isNaN(filter.to)) {
        filter.to = xrange[1];
      }

      xrange[0] = Math.min(xrange[0], filter.from);
      xrange[1] = Math.max(xrange[1], filter.to);
      rdelta = xrange[1] - xrange[0];
      xrange[0] = xrange[0] - 0.3 * rdelta;
      xrange[1] = xrange[1] + 0.3 * rdelta;
    }

    d3univarX.domain(xrange);
    d3univarY.domain([0, d3.max(data.map(function(d) { return d.ydat; }))]);

    svg.selectAll(".bar")
      .data(data)
      .enter().append("rect")
      .attr("class", "bar")
      .attr("x", function(d) { return d3univarX(d.xdat); })
      .attr("width", d3univarX(delta) - d3univarX(0) - 0.75)
      .attr("y", function(d) { return d3univarY(d.ydat); })
      .attr("height", function(d) { return d3univarHeight - d3univarY(d.ydat); });

    svg.append("g")
      .attr("class", "x brush")
      .call(d3univarXbrush)
     .selectAll("rect")
      .attr("y", -6)
      .attr("height", d3univarHeight + 7);

  } else if(plotType == "quant") {
    xrange = d3.extent(data.map(function(d) { return d.x; }));
    xrange[0] = xrange[0] - (xrange[1] - xrange[0]) * 0.07;
    xrange[1] = xrange[1] + (xrange[1] - xrange[0]) * 0.07;

    yrange = d3.extent(data.map(function(d) { return d.y; }));
    yrange[0] = yrange[0] - (yrange[1] - yrange[0]) * 0.07;
    yrange[1] = yrange[1] + (yrange[1] - yrange[0]) * 0.07;

    if(filter !== undefined) {
      if(isNaN(filter.from)) {
        filter.from = yrange[0];
      }
      if(isNaN(filter.to)) {
        filter.to = yrange[1];
      }

      yrange[0] = Math.min(yrange[0], filter.from);
      yrange[1] = Math.max(yrange[1], filter.to);
      rdelta = yrange[1] - yrange[0];
      yrange[0] = yrange[0] - 0.3 * rdelta;
      yrange[1] = yrange[1] + 0.3 * rdelta;
    }

    d3univarX.domain(xrange);
    d3univarY.domain(yrange);

    svg.selectAll(".points")
      .data(data)
      .enter().append("svg:circle")
      .attr("class", "univar-points")
      // .style("fill", function(d){ return d.color; })
      .attr("r", function(d) { return 4; })
      .attr("cx", function(d) { return d3univarX(d.x); })
      .attr("cy", function(d) { return d3univarY(d.y); });

    svg.append("g")
      .attr("class", "y brush")
      .call(d3univarYbrush)
     .selectAll("rect")
      .attr("x", 0)
      .attr("width", d3univarWidth);
  } else if(plotType == "bar") {
    // remove last dummy record
    data.pop();

    delta = data[1].ind - data[0].ind;

    d3univarX.domain([0, d3.max(data.map(function(d) { return d.Freq; }))]);
    var yMax = d3.max(data.map(function(d) { return d.ind; }));
    d3univarY.domain([0, yMax]);

    // console.log(data);
    // console.log(d3univarY.domain);
    // console.log(d3univarX.domain);

    hasClass = function(el, cls) {
      return($(el).attr("class").split(/\s/).indexOf(cls) >= 0);
    };

    var isMouseDown = false, isSelected;

    // svg.selectAll("*").remove();
    svg.selectAll(".bar")
      .data(data)
      .enter().append("rect")
      .attr("class", "unifilter-bar")
      .attr("x", function(d) { return 0; })
      .attr("width", function(d) { return d3univarX(d.Freq); })
      .attr("y", function(d) { return d3univarY(yMax + 1 - d.ind); })
      .attr("height", d3univarY(0) - d3univarY(delta) - 2)
      .on("mouseover", function() {
        if(isMouseDown) {
          d3.select(this).classed("selected", isSelected);
          // change filter icon if more than one is selected
          if(d3.selectAll("#univarFilterPlot svg rect.selected")[0].length > 0) {
            $("#univarFilterSelect li.active i").removeClass("hidden");
          } else {
            $("#univarFilterSelect li.active i").addClass("hidden");
          }
        } else {
          d3.select(this).classed("hover", true);
        }
      })
      .on("mouseout", function() {
        d3.select(this).classed("hover", false);
      })
      .on("mousedown", function() {
        isMouseDown = true;
        if(hasClass(this, "selected")) {
          d3.select(this).classed("selected", false);
        } else {
          d3.select(this).classed("selected", true);
        }
        // change filter icon if more than one is selected
        if(d3.selectAll("#univarFilterPlot svg rect.selected")[0].length > 0) {
          $("#univarFilterSelect li.active i").removeClass("hidden");
        } else {
          $("#univarFilterSelect li.active i").addClass("hidden");
        }
        isSelected = d3.select(this).classed("selected");
      })
      .on("mouseup", function() {
        isMouseDown = false;
      });

    // svg.selectAll("text").remove();
    svg.append("g")
      .attr("class", "bar-labels")
      .selectAll(".text")
      .data(data)
      .enter().append("text")
      .attr("class", "unifilter-bar-text")
      .attr("x", function(d) { return 10; })
      .attr("y", function(d) { return d3univarY(yMax + 1 - d.ind - delta / 2) + 4; })
      .text(function(d) { return d.label; });
  }

  svg.append("g")
    .attr("class", "x axis")
    .attr("transform", "translate(0," + d3univarHeight + ")")
    .call(d3univarXaxis);

  svg.append("text")
    .attr("class", "axis-label")
    .attr("text-anchor", "middle")
    .attr("transform", "translate(" + (d3univarWidth / 2) + "," + (d3univarHeight + 50) + ")")
    .text(xlab);

  if(plotType != "bar") {
    svg.append("g")
      .attr("class", "y axis")
      .call(d3univarYaxis);
  }
  svg.append("text")
    .attr("class", "axis-label")
    .attr("text-anchor", "middle")
    .attr("transform", "translate(" + yAxisOffset + "," + (d3univarHeight / 2) + ")rotate(-90)")
    .text(ylab);

}



// // if the update button is clicked, change the values of the lower and upper inputs and trigger a change
// $(document).on("click", "#univarFilterPlotSubmit", function(evt) {
//   var curRange = d3univarXbrush.empty() ? "" : d3univarXbrush.extent();
//
//   if(curRange != "") {
//     // clear out all columns
//     // (this currently doesn't operate as a marginal filter)
//     $(".columnFilterFrom,.columnFilterTo").each(function() {
//       $(this).val(""); $(this).trigger("change");
//     })
//
//     // get the current column
//     var el = $(evt.target);
//     var column = el.attr("name");
//
//     console.log("column" + column);
//
//     // trigger a change on the appropriate range
//     $("#lower_column_" + column).val(curRange[0]);
//     $("#upper_column_" + column).val(curRange[1]);
//     $("#lower_column_" + column).trigger("change");
//     $("#upper_column_" + column).trigger("change");
//     updated3footHist(column);
//   }
//
//   d3univarXbrush.clear()
// });
//
//
