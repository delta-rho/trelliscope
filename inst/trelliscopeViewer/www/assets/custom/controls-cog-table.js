// updateCogTableSort
// updateCogTableFilter

// cogTableControlsOutputPostRender
// cogTableContentOutputPostRender - rows of data in cog table

// cogTableCancel
// cogTableSetFromState


function hideColumn(columnIndex, name) {
  // table head and foot: hide by name
  // table body: hide by columnIndex

  $("#cogTableContentOutput td:nth-child(" + (columnIndex + 1) + ")").addClass("hidden");
  $("#cog-table-th-" + name).addClass("hidden");
  $("#cog-filter-td-" + name).addClass("hidden");
  $("#cog-table-univar-td-" + name).addClass("hidden");
}

function showColumn(columnIndex, name) {
  // $("#cogTable td:nth-child(" + (columnIndex + 1) + "), #cogTable th:nth-child(" + (columnIndex + 1) + ")").removeClass("hidden");
  $("#cogTableContentOutput td:nth-child(" + (columnIndex + 1) + ")").removeClass("hidden");
  $("#cog-table-th-" + name).removeClass("hidden");
  $("#cog-filter-td-" + name).removeClass("hidden");
  $("#cog-table-univar-td-" + name).removeClass("hidden");
}

function updateCogTableColumnVisibility() {
  // the index of the column to hide will match the order
  // of the non-hidden li elements
  $("#cog-table-vars li.nothidden").each(function(index) {
    if($(this).hasClass("active")) {
      // showColumn(parseInt($(this).data("col-index")));
      showColumn(index, $(this).data("name"));
    } else {
      // hideColumn(parseInt($(this).data("col-index")));
      hideColumn(index, $(this).data("name"));
    }
  });
}

// take what user has specified for sorting through the UI
// and send it to shiny as a data input cogColumnSortInput
updateCogTableSort = function() {
  var iconLookup = {
    "icon-sort-up" : {
      "numeric" : "icon-sort-numeric-asc",
      "factor" : "icon-sort-alpha-asc"
    },
    "icon-sort-down" : {
      "numeric" : "icon-sort-numeric-desc",
      "factor" : "icon-sort-alpha-desc"
    }
  };

  var dirLookup = {
    "icon-sort-up" : "asc",
    "icon-sort-down" : "desc"
  };

  // put sort order in array
  var sortData = {};
  $("#cogColumnSortInput th").each(function() {
    curEl = $(this).find("i");
    curVar = $(this).data("variable");
    curType = $(this).data("type");

    // hide all sort icons in variable list
    $("#cog-table-col-select-li-" + curVar + " i[class^='icon-sort']").addClass("hidden");

    if(curEl.attr("class") != "icon-unsorted") {
      // add sort icon for this in variable list
      var curIcon = iconLookup[curEl.attr("class")][curType];
      var curDir = dirLookup[curEl.attr("class")];
      $("#cog-table-col-select-li-" + curVar + " i." + curIcon).removeClass("hidden");

      sortData[$(this).data("variable")] = {
        "dir"   : curDir,
        "order"  : curEl.data("sort-order"),
      };
    }
  });
  // console.log(sortData);

  // save sort order to shiny data output element
  $("#cogColumnSortInput").data("myShinyData", sortData);
  $("#cogColumnSortInput").trigger("change");
};

function cogTableSetFromExposedState() {

  // for testing:
  // make a copy of filter data
  // var filterData = jQuery.extend(true, {}, $("#cogColumnFilterInput").data("myShinyData"));
  // var sortData = jQuery.extend(true, {}, $("#cogColumnSortInput").data("myShinyData"));
  // var state = {};
  // state["filter"] = filterData;
  // state["sort"] = sortData;
  // $("#exposedStateDataOutput").data("myShinyData", state);

  // get state data
  // make it a copy so it doesn't edit the exposed state data
  var state = jQuery.extend(true, {}, $("#exposedStateDataOutput").data("myShinyData"));

  // deselect everything and clear out all inputs
  $(".column-filter").val("");
  $(".column-filter-select").selectpicker("deselectAll");
  $(".column-filter-select").selectpicker("refresh");

  // hide all filter icons in variable list (will reset ones that are exposed)
  $("#cog-table-vars li i").addClass("hidden");

  // apply filter state
  if(state.filter) {
    $.each(state.filter, function(key, value) {
      // show filter icon in variable list
      $("#cog-table-col-select-li-" + key + " i.icon-filter").removeClass("hidden");

      // fill in input elements
      curTd = $("#cog-filter-td-" + key);
      if(curTd.data("type") == "numeric") {
        if(value.from !== undefined)
          curTd.find(".column-filter-from").val(value.from);
        if(value.to !== undefined)
          curTd.find(".column-filter-to").val(value.to);
      } else {
        // select categorical values
        if(value.select !== undefined) {
          curTd.find(".column-filter-select").selectpicker("val", value.select);
          curTd.find(".column-filter-select").selectpicker("refresh");
        } else if(value.regex !== undefined) {
          curTd.find(".column-filter-regex").val(value.regex);
        }
      }
    });
    $("#filterStateInput").data("myShinyData", state.filter);
  }
  $("#cogColumnFilterInput").data("myShinyData", state.filter);
  $("#cogColumnFilterInput").trigger("change");

  // apply sort state
  if(state.sort) {
    // set all sort icons in table headings to unsorted
    $(".cog-table-sort-span i").attr("class", "icon-unsorted");
    // set icons for ones that should be
    $.each(state.sort, function(key, value) {
      var iconLookup = {
        "asc" : "icon-sort-up",
        "desc" : "icon-sort-down"
      };

      $("#cog-table-sort-icon-" + key).attr("class", iconLookup[value.dir]);
      $("#cog-table-sort-icon-" + key).data("sort-order", value.order);
    });
    $("#sortStateInput").data("myShinyData", state.sort);

    updateCogTableSort();
  }
}

updateCogTableFilter = function(el) {
  // reset to page 1
  $("#cogTablePaginationInput").data("myShinyData", 1);
  $("#cogTablePaginationInput").trigger("change");

  var name;

  // before we step through each input to get its value
  // take care of currently-changed element
  if(el.hasClass("selectpicker")) {
    // if user is manually picking things, get rid of the regex
    // as it will no longer be valid
    name = el.closest("td").data("variable");
    $("#text-column-" + name).val("");
  } else if(el.hasClass("column-filter-regex")) {
    // if user is typing regex and there is a select picker
    // then clear the select picker
    name = el.closest("td").data("variable");
    if(!$("#text-select-" + name).hasClass("disabled")) {
      $("#text-select-" + name).selectpicker("deselectAll");
      $("#text-select-" + name).selectpicker("refresh");
    }
  }

  // put filter inputs in array
  var filterData = {};

  $("#cogColumnFilterInput td").each(function() {
    var curVar = $(this).data("variable");

    if(!$(this).hasClass("hidden")) {
      if($(this).hasClass("filter-numeric")) {
        var from = $(this).find(".column-filter-from").val();
        var to = $(this).find(".column-filter-to").val();
        if(from !== "") {
          if(!filterData[curVar])
            filterData[curVar] = {};
          filterData[curVar].from = parseFloat(from);
        }
        if(to !== "") {
          if(!filterData[curVar])
            filterData[curVar] = {};
          filterData[curVar].to = parseFloat(to);
        }
      } else {
        var regex = $(this).find(".column-filter-regex").val();
        var picker = $(this).find(".column-filter-select").selectpicker("val");
        var vals = [];
        if(regex !== "") {
          // only store regex if there is no select picker
          // otherwise, apply it to everything in the picker list
          // and select it if it matches
          // note that if this has happened, select is already blank
          var re = new RegExp(regex);

          if(!$("#text-select-" + curVar).hasClass("disabled")) {
            $("#text-select-" + curVar + " option").each(function() {
              var cur = $(this).html();
              if(re.test(cur))
                vals.push(cur);
            });

            // when nothing is selected, it means show everything
            // except in the case of when a regex leads to nothing being selected
            if(vals.length === 0) {
              if(!filterData[curVar])
                filterData[curVar] = {};
              filterData[curVar].empty = true;
            }
            // set a class on the element so it knows it's being populated
            // by the regex (so we will not fire off our on change trigger)
            // now "pick" them
            $(this).find(".column-filter-select").addClass("regex-lock");
            $(this).find(".column-filter-select").selectpicker("val", vals);
            $(this).find(".column-filter-select").selectpicker("refresh");
          } else {
            if(!filterData[curVar])
              filterData[curVar] = {};
            filterData[curVar].regex = regex;
          }
        } else if(picker) {
          vals = picker;
        }
        if(vals.length > 0) {
          // console.log(vals);
          if(!filterData[curVar])
            filterData[curVar] = {};
          filterData[curVar].select = vals;
        }
      }
    }

    // add / remove filter icon
    if(filterData[curVar]) {
      $("#cog-table-col-select-li-" + curVar + " i.icon-filter").removeClass("hidden");
    } else {
      $("#cog-table-col-select-li-" + curVar + " i.icon-filter").addClass("hidden");
    }
  });
  // console.log(filterData);

  // save filter data to shiny data output element
  $("#cogColumnFilterInput").data("myShinyData", filterData);
  $("#cogColumnFilterInput").trigger("change");
};

function cogTableControlsOutputApplyButton() {
  // reset to page one
  $("#curPanelPageInput").val("1");
  $("#curPanelPageInput").trigger("change");

  // trigger change
  var filterData = $("#cogColumnFilterInput").data("myShinyData");
  $("#filterStateInput").data("myShinyData", filterData);
  $("#filterStateInput").trigger("change");

  var sortData = $("#cogColumnSortInput").data("myShinyData");
  $("#sortStateInput").data("myShinyData", sortData);
  $("#sortStateInput").trigger("change");
}

function cogTableControlsOutputCancelButton() {
  cogTableSetFromExposedState();
}

function cogTableControlsOutputPostRender(data) {
  $.getScript("assets/custom/selectables-cogtable.js");

  // initialize multiselect dropups in cognostics table
  $(".selectpicker").selectpicker();
  $(".bootstrap-select.disabled").css("visibility", "hidden");

  // show / hide columns
  updateCogTableColumnVisibility();

  $("#cog-table-vars li").mouseup(function(e) {
    updateCogTableColumnVisibility();
  });

  // make histograms / bar charts in footer
  $.each(data.plotDat, function(index, value) {
    if(value.data !== undefined) {
      try {
        d3footPlot(value);
      } catch (e) {
        console.log(e);
        return;
      }
    }
  });

  // handle clicks on column sort
  $(".cog-table-sort-span").click(function(e) {

    var el = $(e.target);
    if(el.prop("tagName") == "SPAN") // icon clicked but want span
      el = el.find("i");
    // console.log(el);

    var classVec = new Array("icon-unsorted", "icon-sort-up", "icon-sort-down");

    // get current 'sort' class
    var curClass = el.attr("class");

    var newClassIndex = ($.inArray(curClass, classVec) + 1) % 3;
    var newClass = classVec[newClassIndex];
    // console.log(curClass);
    // console.log(newClass);

    // if shift was not pressed, reset all columns
    var sortOrder = 1;
    if (!e.shiftKey) {
      // console.log(el);
      $("#cogColumnSortInput i").each(function() {
        $(this).attr("class", "icon-unsorted");
      });
    } else {
      sortOrder = $("#cogColumnSortInput i").not(".icon-unsorted").length + 1;
    }

    // set class of clicked column
    el.attr("class", newClass);
    el.data("sort-order", sortOrder);

    updateCogTableSort();
  });

  // handle filter changes
  $("input.column-filter.numbers-only").keyup(debounce(function() {
    // make numeric inputs only work with numbers
    if (this.value != this.value.replace(/[^0-9\-\.]/g, '')) {
      this.value = this.value.replace(/[^0-9\-\.]/g, '');
    }
    updateCogTableFilter($(this));
  }, 200));

  $("input.column-filter-regex").keyup(debounce(function() {
    updateCogTableFilter($(this));
  }, 200));

  $(".selectpicker").change(function() {
    if($(this).hasClass("regex-lock")) {
      $(this).removeClass("regex-lock");
    } else {
      updateCogTableFilter($(this));
    }
  });

  // handle clicking on pagination
  $("#cogTablePageStart").click(function() {
    $("#cogTablePaginationInput").data("myShinyData", 1);
    $("#cogTablePaginationInput").trigger("change");
  });

  $("#cogTablePagePrev").click(function() {
    var curPage = parseInt($("#cogTableCurPageOutput").html());
    if(curPage - 1 > 0) {
      $("#cogTablePaginationInput").data("myShinyData", curPage - 1);
      $("#cogTablePaginationInput").trigger("change");
    }
  });

  $("#cogTablePageNext").click(function() {
    var curPage = parseInt($("#cogTableCurPageOutput").html());
    var lastPage = parseInt($("#cogTableNpagesOutput").html());
    if(curPage + 1 <= lastPage) {
      $("#cogTablePaginationInput").data("myShinyData", curPage + 1);
      $("#cogTablePaginationInput").trigger("change");
    }
  });

  $("#cogTablePageEnd").click(function() {
    var lastPage = parseInt($("#cogTableNpagesOutput").html());
    $("#cogTablePaginationInput").data("myShinyData", lastPage);
    $("#cogTablePaginationInput").trigger("change");
  });
}

function cogTableContentOutputPostRender() {
  updateCogTableColumnVisibility();
}
