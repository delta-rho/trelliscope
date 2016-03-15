function activeCogListOutputPostRender() {
  $.getScript("assets/custom/selectables-active-cog.js");
}

function activeCogListOutputApplyButton() {
  var activeCog = [];
  var needPanelLabelUpdate = false;
  $(".active-cog-select").each(function() {
    var curName = $(this).data("name");
    if($(this).hasClass("active")) {
      activeCog.push(curName);

      // make sure it is an option in uni, bi, multi, table
      $("#univar-var-" + curName).removeClass("hidden");
      $("#bivar-x-" + curName).removeClass("hidden");
      $("#bivar-y-" + curName).removeClass("hidden");
      $("#multivar-var-" + curName).removeClass("hidden");

      if($("cog-table-col-select-li-" + curName).hasClass("active")) {
        $("#cog-table-th-" + curName).removeClass("hidden");
        $("#cog-filter-td-" + curName).removeClass("hidden");
        $("#cog-table-univar-td-" + curName).removeClass("hidden");
      }
      $("#cog-table-col-select-li-" + curName).removeClass("hidden").addClass("nothidden");
    } else {
      // remove any filters for this variable
      $("#cog-state-remove-sort-" + curName).click();
      $("#cog-state-remove-filter-" + curName).click();

      // make sure it is deselected in "panel labels"
      if($("#panel-labels-select-" + curName).hasClass("active")) {
        needPanelLabelUpdate = true;
        $("#panel-labels-select-" + curName).removeClass("active");
      }

      // make sure it is not an option in uni, bi, multi, table
      $("#univar-var-" + curName).addClass("hidden").removeClass("active");
      $("#bivar-x-" + curName).addClass("hidden").removeClass("active");
      $("#bivar-y-" + curName).addClass("hidden").removeClass("active");
      $("#multivar-var-" + curName).addClass("hidden").removeClass("active");

      $("#cog-table-th-" + curName).addClass("hidden");
      $("#cog-filter-td-" + curName).addClass("hidden");
      $("#cog-table-univar-td-" + curName).addClass("hidden");
      $("#cog-table-col-select-li-" + curName).addClass("hidden").removeClass("nothidden");
    }
  });

  // console.log($(".panel-labels-select.active"));
  if(needPanelLabelUpdate)
    panelLabelListOutputApplyButton();

  $("#activeCogStateInput").data("myShinyData", activeCog);
  $("#activeCogStateInput").trigger("change");
}
