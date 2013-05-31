//// begin d3 histogram stuff

var d3footHistMargin = {top: 1, right: 1, bottom: 1, left: 1},
    d3footHistWidth = 93 - d3footHistMargin.left - d3footHistMargin.right,
    d3footHistHeight = 53 - d3footHistMargin.top - d3footHistMargin.bottom;

var d3footHistX = d3.scale.linear()
    .range([0, d3footHistWidth]);

var d3footHistY = d3.scale.linear()
    .range([d3footHistHeight, 0]);

// var d3footHistXaxis = d3.svg.axis()
//     .scale(d3footHistX)
//     .orient("bottom");
// 
// var d3footHistYaxis = d3.svg.axis()
//     .scale(d3footHistY)
//     .orient("left");

// var d3footHistBrush = d3.svg.brush()
//     .x(d3footHistX)
//     // .y(d3footHistY) // for both axes
//     .on("brush", d3footHistBrushFn);

function updated3footHist(column) {
   var jsonData = $("#cogTable_univarPlotDat_" + column).text();
   
   if(jsonData != "" && jsonData != null) {
      
      var data = JSON.parse(jsonData);
      var xlab = data.xlab;
      data = data.data;
      
      $("#cogTable_univarPlot_" + column).text("")
      
      var svg = d3.select("#cogTable_univarPlot_" + column).append("svg:svg")
          .attr("width", d3footHistWidth + d3footHistMargin.left + d3footHistMargin.right)
          .attr("height", d3footHistHeight + d3footHistMargin.top + d3footHistMargin.bottom)
        .append("g")
          .attr("transform", "translate(" + d3footHistMargin.left + "," + d3footHistMargin.top + ")");
      
      data.forEach(function(d) {
         d.xdat = +d.xdat;
         d.ydat = +d.ydat;
      });
      
      var delta = data[1].xdat - data[0].xdat
      // console.log("delta: " + delta);

      var xrange = d3.extent(data.map(function(d) { return d.xdat; }));
      xrange[0] = xrange[0] - (xrange[1] - xrange[0]) * 0.01;
      xrange[1] = xrange[1] + (xrange[1] - xrange[0]) * 0.01;

      var yrange = d3.extent(data.map(function(d) { return d.ydat; }));
      
      d3footHistX.domain(xrange);
      d3footHistY.domain([0, d3.max(data.map(function(d) { return d.ydat; }))]);
      
      svg.selectAll(".bar2")
         .data(data)
        .enter().append("rect")
         .attr("class", "bar2")
         .attr("x", function(d) { return d3footHistX(d.xdat); })
         .attr("width", d3footHistX(delta) - d3footHistX(0) - 0.75)
         .attr("y", function(d) { return d3footHistY(d.ydat); })
         .attr("height", function(d) { return d3footHistHeight - d3footHistY(d.ydat); });
      
      
      // see if there is filtering (and show that on the plot)
      var lval = $("#lower_column_" + column).val();
      var uval = $("#upper_column_" + column).val();

      if(lval != "" && uval=="")
         uval = data[data.length - 1].xdat;
      if(lval != "" && uval=="")
         uval = data[data.length - 1].xdat;

      if(lval!="" && uval != "")
         svg.append("rect")
            .attr("fill", "steelblue")
            .attr("stroke", "steelblue")
            .attr("stroke-opacity", 0.7)
            .attr("fill-opacity", 0.2)
            // .attr("class", "barFilter")
            .attr("x", d3footHistX(lval))
            .attr("width", d3footHistX(uval) - d3footHistX(lval))
            .attr("y", d3footHistY(yrange[1]))
            .attr("height", d3footHistY(yrange[0]) - d3footHistY(yrange[1]));
      
      // svg.append("g")
      //    .attr("class", "x brush")
      //    .call(d3footHistBrush)
      //  .selectAll("rect")
      //    .attr("y", -6)
      //    .attr("height", d3footHistHeight + 7);
         
      // svg.append("g")
      //    .attr("class", "x axis")
      //    .attr("transform", "translate(0," + d3footHistHeight + ")")
      //    .call(d3footHistXaxis);
      //    
      // svg.append("g")
      //    .attr("class", "y axis")
      //    .call(d3footHistYaxis);
      
      // svg.append("text")
      //    .attr("text-anchor", "middle")
      //    .attr("transform", "translate(" + (-50) +","+(height/2)+")rotate(-90)")
      //    .text("Frequency");
      //    
      // svg.append("text")
      //    .attr("text-anchor", "middle")
      //    .attr("transform", "translate("+ (width/2) +","+(height + 50) +")")
      //    .text(xlab);
   }
}
