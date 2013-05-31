// td highlithing for multiple variable selection



//// plot stuff

var d3bvMargin = {top: 10, right: 10, bottom: 60, left: 60},
    width = 730 - d3bvMargin.left - d3bvMargin.right,
    height = 510 - d3bvMargin.top - d3bvMargin.bottom;
    // width: 660
    // height: 440

var d3bvX = d3.scale.linear()
    .range([0, width]);

var d3bvY = d3.scale.linear()
    .range([height, 0]);

var d3bvXaxis = d3.svg.axis()
    .scale(d3bvX)
    .orient("bottom");

var d3bvYaxis = d3.svg.axis()
    .scale(d3bvY)
    .orient("left");

var d3bvBrush = d3.svg.brush()
    .x(d3bvX)
    .y(d3bvY)
    .on("brush", d3bvBrushFn);

function updateD3bivar() {
   
   var jsonData = $("#d3bivarPlotDat").text();
   
   if(jsonData != "" && jsonData != null) {
      
      var bivardata = JSON.parse(jsonData);
      
      $("#d3bivar").text("");
      
      var svg = d3.select("#d3bivar").append("svg:svg")
          .attr("width", width + d3bvMargin.left + d3bvMargin.right)
          .attr("height", height + d3bvMargin.top + d3bvMargin.bottom)
        .append("g")
          .attr("transform", "translate(" + d3bvMargin.left + "," + d3bvMargin.top + ")");

      var xrange = d3.extent(bivardata.data.map(function(d) { return d.x; }));
      xrange[0] = xrange[0] - (xrange[1] - xrange[0]) * 0.07;
      xrange[1] = xrange[1] + (xrange[1] - xrange[0]) * 0.07;

      var yrange = d3.extent(bivardata.data.map(function(d) { return d.y; }));
      yrange[0] = yrange[0] - (yrange[1] - yrange[0]) * 0.07;
      yrange[1] = yrange[1] + (yrange[1] - yrange[0]) * 0.07;
      
      d3bvX.domain(xrange);
      d3bvY.domain(yrange);
      
      if(bivardata.type=="scatter") {
         svg.selectAll(".points")
            .data(bivardata.data)
           .enter().append("svg:circle")
            .attr("class", "points")
            // .style("fill", function(d){ return d.color; })
            .attr("fill", "steelblue")
            .attr("r", function(d) { return 2; })
            .attr("cx", function(d) { return d3bvX(d.x); })
            .attr("cy", function(d) { return d3bvY(d.y); });
      } else {
         svg.selectAll(".polygon")
            .data(bivardata.data)
           .enter()
            .append("svg:polygon")
            .attr("points",function(d) {
               var polystr = "";
               for (var i=0; i < bivardata.hexx.length; i++) {
                  polystr += d3bvX(d.x + bivardata.hexx[i] * d.r) + "," + d3bvY(d.y + bivardata.hexy[i] * d.r) + " ";
               };
               return(polystr);
            })
            .attr("fill", "steelblue")
            .attr("stroke-width", 0);   
      }
      
      // TODO?: tooltips: http://bl.ocks.org/1373263
      
      svg.append("g")
         .attr("class", "x brush")
         .call(d3bvBrush)
        .selectAll("rect")
         .attr("y", -6)
         
      // y-axis label
      svg.append("text")
         .attr("text-anchor", "middle")
         .attr("transform", "translate(" + (-50) + "," + (height/2) + ")rotate(-90)")
         .text(bivardata.ylab);
         
      // x-axis label
      svg.append("text")
         .attr("text-anchor", "middle")
         .attr("transform", "translate("+ (width/2) + "," + (height + 50) +")")
         .text(bivardata.xlab);
         
      svg.append("g")
         .attr("class", "x axis")
         .attr("transform", "translate(0," + height + ")")
         .call(d3bvXaxis);
         
      svg.append("g")
         .attr("class", "y axis")
         .call(d3bvYaxis);
   }
}

function d3bvBrushFn() {
   var curBrush = d3bvBrush.empty() ? d3bvX.domain() : d3bvBrush.extent();

   var prec = d3.format(".5r");

   if(curBrush[0].length == undefined) {
      // curBrush = [[curBrush[0], curBrush[1]], [curBrush[0], curBrush[1]]]
      curBrush = "";
      $("#d3bivarRange").text("");
   } else {
         $("#d3bivarRange").text("(" + prec(curBrush[0][0]) + "," + prec(curBrush[0][1]) + ") to (" +  + prec(curBrush[1][0]) + "," + prec(curBrush[1][1]) + ")");
   }
}

// output where the json is stored as input for the d3 bivariate plot
var bivarDatOutputBinding = new Shiny.OutputBinding();
$.extend(bivarDatOutputBinding, {
   find: function(scope) {
      return $(scope).find('.shiny-d3bivarDat-output');
   },
   renderValue: function(el, data) {
      Shiny.unbindAll(el);
      $(el).html(data);
      Shiny.bindAll(el);
      // TODO: we don't even need to store the data in the div
      // we can just pass the data to the function...
      if(data != "" && data != null) {
         // alert(data);
         updateD3bivar();
         $("#cogModal").modal("hide");
         $("#d3bivarModal").modal("show");
      }
   }
});
Shiny.outputBindings.register(bivarDatOutputBinding, 'shiny.bivarDatOutput');

$(document).on("click", "#d3bivarSubmit", function(evt) {
   var curRange = d3bvBrush.empty() ? "" : d3bvBrush.extent();

   if(curRange != "") {
      // clear out all columns
      // (this currently doesn't operate as a marginal filter)
      $(".columnFilterFrom,.columnFilterTo").each(function() {    
         $(this).val(""); $(this).trigger("change"); 
      })

      // get the current columns
      var columns = getHighlighted($("#cogTable_bivar_select").children());
      
      if(columns.length==2) {
         // trigger a change on the appropriate range
         $("#lower_column_" + columns[0]).val(curRange[0][0]);
         $("#upper_column_" + columns[0]).val(curRange[1][0]);
         $("#lower_column_" + columns[1]).val(curRange[0][1]);
         $("#upper_column_" + columns[1]).val(curRange[1][1]);

         $("#lower_column_" + columns[0]).trigger("change");
         $("#upper_column_" + columns[0]).trigger("change");      
         $("#lower_column_" + columns[1]).trigger("change");
         $("#upper_column_" + columns[1]).trigger("change"); 
         updated3footHist(columns[0]);
         updated3footHist(columns[1]);
         
         // TODO: when other columns have had univariate filters, the d3foothist might not get cleared out
      }
   }
   
   $("#d3bivarModal").modal("hide");
   d3bvBrush.clear()

   // clear the inputs too
   $("#cogTable_bivar_select").children().each(function(i, el) { $(el).removeClass("highlighted") }); 
   $("#bivarColumns").val("");
   $("#bivarColumns").trigger("change");
});

$(document).on("click", ".bivarPlotScatter", function(evt) {
   // get the selected columns
   var columns = getHighlighted($("#cogTable_bivar_select").children());
   // console.log(columns)
   // need at least 2 variables
   if(columns.length > 1) {
      $("#bivarColumns").val(columns.join(","));
      $("#bivarColumns").trigger("change");   
   }

   // see here about stacking modals on top of each other:
   // http://stackoverflow.com/questions/13649459/twitter-bootstrap-multiple-modal-error
});

$(document).ready(function() {
   $('#d3bivarModal').on('hidden', function () {
      $("#cogModal").modal("show");
   })
});

