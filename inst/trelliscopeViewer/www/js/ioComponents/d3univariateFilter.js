//// begin d3 histogram stuff

var d3histMargin = {top: 10, right: 10, bottom: 60, left: 60},
    d3histWidth = 730 - d3histMargin.left - d3histMargin.right,
    d3histHeight = 510 - d3histMargin.top - d3histMargin.bottom;

var d3histX = d3.scale.linear()
    .range([0, d3histWidth]);

var d3histY = d3.scale.linear()
    .range([d3histHeight, 0]);

var d3histXaxis = d3.svg.axis()
    .scale(d3histX)
    .orient("bottom");

var d3histYaxis = d3.svg.axis()
    .scale(d3histY)
    .orient("left");

var d3histBrush = d3.svg.brush()
    .x(d3histX)
    // .y(d3histY) // for both axes
    .on("brush", d3histBrushFn);

function updateD3hist(column) {
   
   // first, add an attribute to submit button so it knows which
   // input to update
   $("#d3histSubmit").attr("name", column);
   
   var jsonData = $("#cogTable_univarPlotDat_" + column).text();
   
   if(jsonData != "" && jsonData != null) {
      
      var data = JSON.parse(jsonData);
      var xlab = data.xlab;
      data = data.data;
      
      $("#d3hist").text("")
      
      var svg = d3.select("#d3hist").append("svg:svg")
          .attr("width", d3histWidth + d3histMargin.left + d3histMargin.right)
          .attr("height", d3histHeight + d3histMargin.top + d3histMargin.bottom)
        .append("g")
          .attr("transform", "translate(" + d3histMargin.left + "," + d3histMargin.top + ")");
      
      data.forEach(function(d) {
         d.xdat = +d.xdat;
         d.ydat = +d.ydat;
      });
      
      var delta = data[1].xdat - data[0].xdat
      // console.log("delta: " + delta);
      
      var xrange = d3.extent(data.map(function(d) { return d.xdat; }));
      xrange[0] = xrange[0] - (xrange[1] - xrange[0]) * 0.07;
      xrange[1] = xrange[1] + (xrange[1] - xrange[0]) * 0.07;
      
      d3histX.domain(xrange);
      d3histY.domain([0, d3.max(data.map(function(d) { return d.ydat; }))]);
      
      svg.selectAll(".bar")
         .data(data)
        .enter().append("rect")
         .attr("class", "bar")
         .attr("x", function(d) { return d3histX(d.xdat); })
         .attr("width", d3histX(delta) - d3histX(0) - 0.75)
         .attr("y", function(d) { return d3histY(d.ydat); })
         .attr("height", function(d) { return d3histHeight - d3histY(d.ydat); });
         
      svg.append("g")
         .attr("class", "x brush")
         .call(d3histBrush)
       .selectAll("rect")
         .attr("y", -6)
         .attr("height", d3histHeight + 7);
         
      svg.append("g")
         .attr("class", "x axis")
         .attr("transform", "translate(0," + d3histHeight + ")")
         .call(d3histXaxis);
         
      svg.append("g")
         .attr("class", "y axis")
         .call(d3histYaxis);
         
      svg.append("text")
         .attr("text-anchor", "middle")
         .attr("transform", "translate(" + (-50) +","+(height/2)+")rotate(-90)")
         .text("Frequency");
         
      svg.append("text")
         .attr("text-anchor", "middle")
         .attr("transform", "translate("+ (width/2) +","+(height + 50) +")")
         .text(xlab);
   }
}

function d3histBrushFn() {
   var curBrush = d3histBrush.empty() ? "" : d3histBrush.extent();
   // d3histX.domain()
      
   var prec = d3.format(".5r");
   
   if(curBrush == "") {
      $("#d3histRange").text("");
   } else {
         $("#d3histRange").text(prec(curBrush[0]) + " to " + prec(curBrush[1]));
   }
}

//// end d3 histogram stuff

// this receives a json with the data to go in each location
var histDatOutputBinding = new Shiny.OutputBinding();
$.extend(histDatOutputBinding, {
   find: function(scope) {
      return $(scope).find('.shiny-d3histdat-output');
   },
   renderValue: function(el, data) {
      if(data != null) {
         Shiny.unbindAll(el);
         // set each element's data to the value
         $.each(data, function(key, value) {
            $("#cogTable_univarPlotDat_"+key).text(value);
            updated3footHist(key);
         });
         Shiny.bindAll(el);         
      }
   }
});
Shiny.outputBindings.register(histDatOutputBinding, 'shiny.histDatOutput');

// if the update button is clicked, change the values of the lower and upper inputs and trigger a change
$(document).on("click", "#d3histSubmit", function(evt) {
   var curRange = d3histBrush.empty() ? "" : d3histBrush.extent();

   if(curRange != "") {
      // clear out all columns
      // (this currently doesn't operate as a marginal filter)
      $(".columnFilterFrom,.columnFilterTo").each(function() { 
         $(this).val(""); $(this).trigger("change"); 
      })
      
      // get the current column
      var el = $(evt.target);
      var column = el.attr("name");

      console.log("column" + column);

      // trigger a change on the appropriate range
      $("#lower_column_" + column).val(curRange[0]);
      $("#upper_column_" + column).val(curRange[1]);
      $("#lower_column_" + column).trigger("change");
      $("#upper_column_" + column).trigger("change");
      updated3footHist(column);
   }

   $("#d3histModal").modal("hide");
   d3histBrush.clear()
});

// for d3 histogram
$(document).on("click", ".univarPlotHist", function(evt) {
   var column = parseInt($(this).attr("title"));
   updateD3hist(column);
   $("#cogModal").modal("hide");
   $("#d3histModal").modal("show");
   // TODO: see here about stacking modals on top of each other:
   // http://stackoverflow.com/questions/13649459/twitter-bootstrap-multiple-modal-error
});

$(document).ready(function() {
   $('#d3histModal').on('hidden', function () {
      $("#cogModal").modal("show");
   })
});