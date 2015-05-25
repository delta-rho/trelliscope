
var d3footHistMargin = {top: 1, right: 1, bottom: 1, left: 1},
  d3footHistWidth = 120 - d3footHistMargin.left - d3footHistMargin.right,
  d3footHistHeight = 62 - d3footHistMargin.top - d3footHistMargin.bottom;

var d3footHistX = d3.scale.linear()
  .range([0, d3footHistWidth]);

var d3footHistY = d3.scale.linear()
  .range([d3footHistHeight, 0]);

function d3footPlot(data) {

  var name = data.name;
  var type = data.type;
  // if(typeof data.log === 'undefined') {
  //   var log = "NA"
  // } else {
  //   var log = data.log[0];
  // }

  var plotType = data.plotType;

  var barClass = type == "numeric" ? "ct-bar-num" : "ct-bar-char";

  data = data.data;

  var svg = d3.select("#cog-table-univar-" + name).append("svg:svg")
    .attr("width", d3footHistWidth + d3footHistMargin.left + d3footHistMargin.right)
    .attr("height", d3footHistHeight + d3footHistMargin.top + d3footHistMargin.bottom)
    .append("g")
    .attr("transform", "translate(" + d3footHistMargin.left + "," + d3footHistMargin.top + ")");

  if(plotType == "bar") {

  }

  var delta = data[1].xdat - data[0].xdat;
  var xrange = d3.extent(data.map(function(d) { return d.xdat; }));
  xrange[0] = xrange[0] - (xrange[1] - xrange[0]) * 0.01;
  xrange[1] = xrange[1] + (xrange[1] - xrange[0]) * 0.01;

  var yrange = d3.extent(data.map(function(d) { return d.ydat; }));

  d3footHistX.domain(xrange);
  d3footHistY.domain([0, d3.max(data.map(function(d) { return d.ydat; }))]);

  svg.selectAll("." + barClass)
    .data(data)
    .enter().append("rect")
    .attr("class", barClass)
    .attr("x", function(d) { return d3footHistX(d.xdat); })
    .attr("width", d3footHistX(delta) - d3footHistX(0) - 0.75)
    .attr("y", function(d) { return d3footHistY(d.ydat); })
    .attr("height", function(d) {
      return d3footHistHeight - d3footHistY(d.ydat);
    })
    .on("mouseover", function(d) {
      // Update the tooltip position and value
      d3.select("#d3tooltip")
        .style("left", (d3.event.pageX - 5) + "px")
        .style("top", (d3.event.pageY - 29) + "px")
        .text(d.label);

      // Show the tooltip
      d3.select("#d3tooltip").classed("hidden", false);
    })
    .on("mousemove", function() {
      d3.select("#d3tooltip")
        .style("left", (d3.event.pageX - 5) + "px")
        .style("top", (d3.event.pageY - 29) + "px");
    })
    .on("mouseout", function() {
      // Hide the tooltip
      d3.select("#d3tooltip").classed("hidden", true);
    });
}
