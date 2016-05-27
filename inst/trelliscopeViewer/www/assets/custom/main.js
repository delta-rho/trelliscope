var spinnerOpts = {
  lines: 13, // The number of lines to draw
  length: 16, // The length of each line
  width: 7, // The line thickness
  radius: 21, // The radius of the inner circle
  corners: 1, // Corner roundness (0..1)
  rotate: 0, // The rotation offset
  direction: 1, // 1: clockwise, -1: counterclockwise
  color: '#000', // #rgb or #rrggbb or array of colors
  speed: 1, // Rounds per second
  trail: 60, // Afterglow percentage
  shadow: false, // Whether to render a shadow
  hwaccel: false, // Whether to use hardware acceleration
  className: 'spinner', // The CSS class to assign to the spinner
  zIndex: 2e9, // The z-index (defaults to 2000000000)
  top: '50%', // Top position relative to parent
  left: '50%' // Left position relative to parent
};

var spinnerOptsCorner = {
  lines: 11, // The number of lines to draw
  length: 5, // The length of each line
  width: 2, // The line thickness
  radius: 5, // The radius of the inner circle
  corners: 1, // Corner roundness (0..1)
  rotate: 0, // The rotation offset
  direction: 1, // 1: clockwise, -1: counterclockwise
  color: '#000', // #rgb or #rrggbb or array of colors
  speed: 1, // Rounds per second
  trail: 60, // Afterglow percentage
  shadow: false, // Whether to render a shadow
  hwaccel: false, // Whether to use hardware acceleration
  className: 'spinner', // The CSS class to assign to the spinner
  zIndex: 2e9, // The z-index (defaults to 2000000000)
  top: '50%', // Top position relative to parent
  left: '50%' // Left position relative to parent
};

var univarSpinner = new Spinner(spinnerOpts);
var bivarSpinner = new Spinner(spinnerOpts);
var multivarSpinner = new Spinner(spinnerOpts);
var panelSpinner = new Spinner(spinnerOptsCorner);
var displayLoadSpinner = new Spinner(spinnerOpts);

// store the URL hash
var appHash = window.location.hash;

function debounce(fn, delay) {
  var timer = null;
  return function () {
    var context = this, args = arguments;
    clearTimeout(timer);
    timer = setTimeout(function () {
      fn.apply(context, args);
    }, delay);
  };
}

// if (typeof console  != "undefined")
//    if (typeof console.log != 'undefined')
//      console.olog = console.log;
//    else
//      console.olog = function() {};
//
// console.log = function(message) {
//    console.olog(message);
//    $('#error-log').append('<p>' + message + '</p>');
// };
// console.error = console.debug = console.info =  console.log;

// let a user resize for 250ms before triggering actions
$(window).resize(function() {
  if(this.resizeTO) clearTimeout(this.resizeTO);
  this.resizeTO = setTimeout(function() {
    $(this).trigger('resizeEnd');
  }, 250);
});

$(window).bind('resizeEnd', function() {
  // recompute the panel preview layout after window resize
  // TODO: if that control panel is open, only change it there

  // if related displays are selected, recompute there
  // instead of panel layout
  if($(".related-display-select.active").length > 0) {
    relatedLayout();
    relatedDisplayListOutputApplyButton();
  } else {
    panelLayoutPreview(parseInt($("#panel-rows").val()), parseInt($("#panel-cols").val()), $(".panel-labels-select.active").length);
    $("#panel-rows").trigger("change");
    panelLayoutOutputApplyButton();
  }
});

// bind keys for navigating controls / paging through panels
// var keymap = {};
$(document).keydown(function(e) {
  // some keys should only work when no panels are open
  var slidePanel = $(".slide-panel.slide-left");
  var modals = $(".modal:visible");
  // keymap["k" + e.keyCode] = true;

  if($(document.activeElement).attr("id") != "curPanelPageInput" &&
    slidePanel.length === 0 && modals.length === 0) {
    switch(e.keyCode) {
      case 37: // left
        pageBack();
        return false;
      case 39: // right
        pageForward();
        return false;
      case 68: // d
        $("#display-info-nav-link").click();
        return false;
      case 80: // p
        $("#panel-layout-nav-link").click();
        return false;
      // case 70: // f
      //   $("#panel-function-nav-link").click();
      //   return false;
      case 76: // l
        $("#panel-labels-nav-link").click();
        return false;
      case 82: // r
        $("#add-related-display-nav-link").click();
        return true; // allow refresh with cmd-r
      case 65: // a
        $("#active-cog-nav-link").click();
        return false;
      case 84: // t
        $("#cog-table-sort-filter-nav-link").click();
        return false;
      case 85: // u
        $("#univar-filter-nav-link").click();
        return false;
      case 66: // b
        $("#bivar-filter-nav-link").click();
        return false;
      case 77: // m
        $("#multivar-filter-nav-link").click();
        return false;
      case 83: // s
        $("#sample-panels-nav-link").click();
        return false;
      case 79: // o
        $("#openModal").modal("show");
        return false;
      case 73: // i
        $("#aboutModal").modal("show");
        return false;
    }
  } else if(slidePanel.length === 1) {
    var returnVal = true;
    if(e.keyCode === 27) { // escape
      slidePanel.find("button.btn-panel-close").click();
      returnVal = false;
    }
    if(e.keyCode === 13) { //enter
      slidePanel.find("button.btn-panel-apply").click();
      returnVal = false;
    }
    return returnVal;
  }
});

// $(document).keyup(function(e) {
//   if (e.keyCode + "" in keymap) {
//     delete keymap["k" + e.keyCode];
//   }
// });

function pageForward() {
  var curPage = parseInt($("#curPanelPageInput").val());
  var nPages = parseInt($("#panelPageTotOutput").text());
  var by = parseInt($("#skip-button-value").html().replace("x", ""));

  if(curPage + by <= nPages) {
    $("#curPanelPageInput").val(curPage + by);
    $("#curPanelPageInput").trigger("change");
  }
}

function pageBack() {
  var curPage = parseInt($("#curPanelPageInput").val());
  var by = parseInt($("#skip-button-value").html().replace("x", ""));

  if(curPage - by >= 1) {
    $("#curPanelPageInput").val(curPage - by);
    $("#curPanelPageInput").trigger("change");
  }
}

function pageBeg() {
  $("#curPanelPageInput").val(1);
  $("#curPanelPageInput").trigger("change");
}


function masterControlPostRender() {
  // if any .slide-left divs are open, hide the backdrop, else show
  function toggleBackdrop() {
    if($(".slide-left").length === 0) {
      $("#control-panel-backdrop").removeClass("bd-visible");
      $("#control-panel-backdrop").addClass("bd-hidden");
    } else {
      $("#control-panel-backdrop").removeClass("bd-hidden");
      $("#control-panel-backdrop").addClass("bd-visible");
    }
  }

  // panel sliding by navigation
  $("div.list-group-sidebar a.list-group-item").on("click", function() {
    // when a nav element is clicked
    $(".slide-panel").not($("#" + $(this).data("divlink"))).removeClass("slide-left");
    $(this).toggleClass("selected");
    // make sure related layout div is hidden (this isn't contained in the control panel)
    $(".rl-layout").hide();
    // if the user opens a control panel, call action function
    // which typically will be making sure the currently exposed state is set
    if($(this).hasClass("selected")) {
      var actionFn = $(this).data("action");
      if(window[actionFn])
        window[actionFn]();
    }
    // open the corresponding panel with a matching class to the button's id
    $("#" + $(this).data("divlink")).toggleClass("slide-left");
    // dispatch callback...
    toggleBackdrop();
  });

  $("#control-panel-backdrop").click(function() {
    // close all .slide-left
    $(".slide-left").each(function() {
      $(this).toggleClass("slide-left");
    });
    $("div.list-group a.list-group-item").removeClass("selected");
    $("#control-panel-backdrop").removeClass("bd-visible");
    $("#control-panel-backdrop").addClass("bd-hidden");
  });

  // handle "cancel" button of each control panel
  $(".btn-panel-close").click(function() {
    // update action dispatch
    var actionFn = $(this).data("action");
    if(window[actionFn])
      window[actionFn]();

    $(".slide-panel").removeClass("slide-left");
    $("div.list-group a.list-group-item").removeClass("selected");
    toggleBackdrop();
  });

  // handle "apply" button of each control panel
  $(".btn-panel-apply").click(function() {
    // update action dispatch
    var actionFn = $(this).data("action");
    if(window[actionFn])
      window[actionFn]();

    // every time apply is called, set it back to page 1
    // $("#curPanelPageInput").val("1");
    // $("#curPanelPageInput").trigger("change");

    // get rid of panel and backdrop
    $(".slide-panel").removeClass("slide-left");
    $("div.list-group a.list-group-item").removeClass("selected");
    toggleBackdrop();
  });
}

// // initialize code editor
// function panelFunctionOutputPostRender() {
//   var editor = ace.edit("editor");
//   editor.setTheme("ace/theme/tomorrow");
//   editor.getSession().setTabSize(3);
//   editor.getSession().setUseSoftTabs(true);
//   editor.getSession().setMode("ace/mode/r");
// }

function updateControlsExposedState() {
  univarFilterSetFromExposedState();
  bivarFilterSetFromExposedState();
  cogTableSetFromExposedState();
  panelLabelListSetFromExposedState();
  panelLayoutSetFromExposedState();
}

function cogBreadcrumbOutputPostRender() {
  // remove sorting if "x" clicked on filter breadcrumb
  $(".filter-breadcrumb.cog-state-remove").click(function() {
    var filterData = $("#filterStateInput").data("myShinyData");
    delete filterData[$(this).data("name")];
    $("#filterStateInput").trigger("change");
  });

  // if main part of a filter breadcrumb button is clicked
  // open up the univariate filter control panel and select that variable
  $(".filter-breadcrumb.cog-state-edit").click(function() {
    if(!$("#univar-filter").hasClass("slide-left"))
      $("#univar-filter-nav-link").trigger("click");

    $("#univar-var-" + $(this).data("name")).trigger("click");
  });

  // remove sorting if "x" clicked on sort breadcrumb
  $(".sort-breadcrumb.cog-state-remove").click(function() {
    var sortData = $("#sortStateInput").data("myShinyData");
    delete sortData[$(this).data("name")];
    $("#sortStateInput").trigger("change");
  });

  // open Table Sort / Filter control if main part of sort button clicked
  $(".sort-breadcrumb.cog-state-edit").click(function() {
    if(!$("#cog-table-sort-filter").hasClass("slide-left"))
      $("#cog-table-sort-filter-nav-link").trigger("click");

    // make sure appropriate column is active
    // console.log("#cog-table-col-select-li-" + $(this).data("name"));
    var el = $("#cog-table-col-select-li-" + $(this).data("name"));

    if(!el.hasClass("active")) {
      el.trigger("mousedown");
      el.trigger("mouseup");
    }
  });
}

// handle "marginal / conditional" style toggle buttons
function buttonToggleHandler() {
  $(".mc-toggle").click(function() {
    // if this button is not the one currently selected
    if($(this).hasClass("btn-default")) {
      // save filter state
      univarFilterLocalSave();
      // make it active
      $(this).removeClass("btn-default");
      $(this).addClass("btn-info");
      $(this).addClass("active");
      // make all others in the group inactive
      var sib = $(this).siblings();
      sib.removeClass("btn-info");
      sib.removeClass("active");
      sib.addClass("btn-default");
      // update the plot
      var actionFn = $(this).parent().data("action");
      if(actionFn)
        window[actionFn]();
    }
  });
}


function panelPageNavOutputPostRender() {
  // set text in panel pagination skip dropdown button to the default value
  $("#skip-button-menu li a").click(function(e) {
    $("#skip-button-value").html($(this).html());
  });

  $("#pageLeftButton").click(function() {
    pageBack();
  });

  $("#pageRightButton").click(function() {
    pageForward();
  });

  // when page input is changed, fire up spinner
  $("#curPanelPageInput").change(function() {
    var target = document.getElementById("panelTableSpinner");
    panelSpinner.stop(target);
    // setTimeout(function(){ panelSpinner.spin(target); }, 500);
    panelSpinner.spin(target);
  });

  // $("#curPanelPageInput").bind("keydown", function(e) {
  //   console.log(e.keyCode);
  //   if(e.keyCode == 37 || e.keyCode == 39) {
  //     e.preventDefault();
  //   }
  // });
}

function cogMapOutputPostRender() {
  $(".cogmap-dropdown").click(function(e) {
    alert($(this).html());
    $(this).parent(".cogmap-button").html($(this).html());
  });
}

function displayInformationOutputPostRender(data) {
  $("#md-desc").html(marked($("#md-desc").html()));
  try {
    renderMathInElement(document.getElementById("display-information"));
  } catch(err) {
    console.log(err.message);
  }
  $("#display-information table").each(function() {
    $(this).addClass("table table-condensed table-bordered");
  });
}

function panelTableContentOutputPostRender(data) {
  // stop spinner
  var target;
  target = document.getElementById("panelTableSpinner");
  panelSpinner.stop(target);

  // stop display load spinner too (in case it's spinning)
  target = document.getElementById("displayLoadSpinner");
  if(displayLoadSpinner.el) {
    displayLoadSpinner.stop(target);
    // if it is spinning, open display modal is open
    $("#openModal").modal("hide");
  }

  // if it is not a raster image expect an htmlwidget
  for (var i = 0; i < data[0][0].panel_content.length; i++) {
    var pc = data[0][0].panel_content[i];

    if(pc.class && pc.class[0] === "htmlwidget") {
      Shiny.renderDependencies(pc.deps);

      try {
        HTMLWidgets.staticRender();
      } catch(err) {
        console.log(err.message);
      }
      if(pc.scale[0] !== "")
        $(".html-widget-static-bound").zoomscale(pc.scale[0]);
    }
  }

  // make width of cog name column uniform across
  // TODO: compute this as part of panel labels up front and save it with exposed state
  var maxCogNameWidth = 0;
  var tmp;
  $(".panel-cog-table").first().find(".cog-name-td").each(function() {
    tmp = $(this).find("strong").width();
    if(maxCogNameWidth < tmp)
      maxCogNameWidth = tmp;
  });
  $(".cog-name-td").width(maxCogNameWidth - 1);
  // var totWidth = $("#exposedStateDataOutput").data("myShinyData").layout.w;

  var totWidth = 100;
  if($("#panel-layout-data").data("panelDims"))
    totWidth = $("#panel-layout-data").data("panelDims").w;

  $(".cog-value-td").width(totWidth - maxCogNameWidth - 21);
  // $(".panel-cog-table").width(totWidth);

  // change font size of cog labels depending on how many rows
  // 1 row -> 14
  // 2 rows -> 12
  // 3 rows -> 10
  // 4+ rows -> 8
  var font_size = [14, 12, 10, 8];
  var n_rows = Math.min(data.length - 1, 3);
  $(".panel-label-row").css("line-height", "1.2");
  $(".panel-label-row").css("font-size", font_size[n_rows] + "px");
}

// $.getScriptCached = function(url, callback, cache) {
//   $.ajax({
//     type: "GET",
//     url: url,
//     success: callback,
//     dataType: "script",
//     cache: true
//   });
// };
// filename.split('.').pop();
// http://stackoverflow.com/questions/22736281/dynamic-ajax-promise-chain-with-jquery


$(document).ready(function() {
  $("#infoTab a").click(function (e) {
    e.preventDefault();
    $(this).tab("show");
  });

  // render outer templates
  var outerRender = $.getJSON("templateData.json", function(json) {
    var masterTemplate = document.getElementById("controls-master-template").innerHTML;

    $.each(json, function(key, value) {
      var output = Mustache.render(masterTemplate, value);
      document.getElementById(key).innerHTML = output;
    });
  })

  .complete(function() {
    // register bindings for newly created elements
    masterControlPostRender();

    // if we are viewing index.html outside of being invoked from shiny
    // then grab some dummy data to fill in the mustache templates
    // this is a very useful way to design outside of shiny
    if(!window.Shiny) {
      console.log("Running outside of shiny... filling templates with dummy data...");

      $.getJSON("dummyData.json", function(json) {

        $.each(json.panels, function(key, value) {
          renderTemplate(key, value);
        });

        renderTemplate("panelPageNavOutput", json.panelPageNavOutput);
        renderTemplate("cogBreadcrumbOutput", json.cogBreadcrumbOutput);
        renderTemplate("displayListOutput", json.displayListOutput);
        renderTemplate("panelTableContentOutput", json.panelTableContentOutput);
      })
      .complete(function() {
        $("#headerDisplayNameOutput").html("group / display_name");
      });
    } else {
      console.log("Running in shiny mode...");
      try {
        Shiny.bindAll();
      } catch (e) {
       // do nothing
      }

      if(appHash === "") {
        $("#openModal").modal("show");
      } else {
        $("#appHashInput").data("myShinyData", appHash);
        $("#appHashInput").trigger("change");
      }
    }
  });

  $(".right-sticky").click(function() {
    $(".right-panel").toggleClass("right-slide");
    $("#sticky-icon").toggleClass("icon-chevron-left icon-chevron-right");
  });

  $('#aboutModal').on('show.bs.modal', function (e) {
    $("#aboutModalButton").addClass("hovered");
  });

  $('#openModal').on('show.bs.modal', function (e) {
    $("#openModalButton").addClass("hovered");
  });

  $('#aboutModal').on('hide.bs.modal', function (e) {
    $("#aboutModalButton").removeClass("hovered");
  });

  $('#openModal').on('hide.bs.modal', function (e) {
    $("#openModalButton").removeClass("hovered");
  });

  $('.info-tab-label').click(function (e) {
    $('.tab-pane').removeClass('active');
    $('#' + $(this).data('tabname')).addClass('active');
  });

});

// for scaling down htmlwidget panels when there are many on a page
$.fn.zoomscale = function(x) {
  if(!$(this).filter(':visible').length && x != 1) return $(this);
  if(!$(this).parent().hasClass('scaleContainer')){
    $(this).wrap($('<div class="scaleContainer">').css('position','relative'));
    $(this).data({
        'originalWidth': $(this).width(),
        'originalHeight': $(this).height()});
  }
  $(this).css({
    'transform': 'scale('+x+')',
    '-ms-transform': 'scale('+x+')',
    '-moz-transform': 'scale('+x+')',
    '-webkit-transform': 'scale('+x+')',
    'transform-origin': 'right bottom',
    '-ms-transform-origin': 'right bottom',
    '-moz-transform-origin': 'right bottom',
    '-webkit-transform-origin': 'right bottom',
    'position': 'absolute',
    'bottom': '0',
    'right': '0',
  });
  if(x == 1)
    $(this).unwrap().css('position','static'); else
      $(this).parent()
        .width($(this).data('originalWidth')*x)
        .height($(this).data('originalHeight')*x);
  return $(this);
};
