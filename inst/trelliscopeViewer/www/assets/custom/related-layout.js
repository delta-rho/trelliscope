
// http://threedubmedia.com/code/event/
jQuery(function($) {
  var cntnr = $("#related-layout-container");
  var snap = 1; // increase for a more discrete grid
  $(".related-layout-box")
  .mousedown(function() {
    if($(this).hasClass("related-layout-box")) {
       $(".related-layout-box").css("z-index", 5009);
       $(this).css("z-index", 5010);
    }
  })
  .drag("init", function() {
    if ($(this).is(".selected"))
    return $(".selected");
  })
  .drag("start", function(ev, dd) {
    dd.attr = $(ev.target).prop("className");
    dd.width = $(this).width();
    dd.height = $(this).height();
    dd.limit = cntnr.offset();
    dd.limit.bottom = dd.limit.top + cntnr.outerHeight() - $(this).outerHeight();
    dd.limit.right = dd.limit.left + cntnr.outerWidth() - $(this).outerWidth();
    dd.limit.left = dd.limit.left + 1;
    dd.limit.top = dd.limit.top + 1;
    dd.limit.right = dd.limit.right - 1;
    dd.limit.bottom = dd.limit.bottom - 1;
  })
  .drag(function(ev, dd) {
    var props = {};
    var deltax1, deltax2, deltax;
    if (dd.attr.indexOf("SE") > -1) {
      deltax1 = Math.min(dd.deltaX, dd.limit.right - dd.originalX);
      deltax2 = Math.min(dd.deltaX * dd.height / dd.width,
        dd.limit.bottom - dd.originalY) * dd.width / dd.height;
      deltax = Math.round(Math.min(deltax1, deltax2) / snap) * snap;

      props.width = Math.max(50, dd.width + deltax);
      props.height = Math.max(50 * dd.height / dd.width,
        dd.height + deltax * dd.height / dd.width);
    }

    if (dd.attr.indexOf("NE") > -1) {
      deltax1 = Math.min(dd.deltaX, dd.limit.right - dd.originalX);
      deltax2 = Math.min(dd.deltaX * dd.height / dd.width,
        dd.originalY - dd.limit.top) * dd.width / dd.height;
      deltax = Math.round(Math.min(deltax1, deltax2) / snap) * snap;

      props.width = Math.max(50, dd.width + deltax);
      props.height = Math.max(50 * dd.height / dd.width,
        dd.height + deltax * dd.height / dd.width);
      props.top = dd.originalY + (dd.width - props.width) * dd.height / dd.width;
    }

    if (dd.attr.indexOf("SW") > -1) {
      deltax1 = Math.max(dd.deltaX, dd.limit.left - dd.originalX);
      deltax2 = - Math.max(dd.deltaX * dd.height / dd.width,
        dd.limit.bottom - dd.originalY) * dd.width / dd.height;
      deltax = Math.round(Math.max(deltax1, deltax2) / snap) * snap;

      props.width = Math.max(50, dd.width - deltax);
      props.height = Math.max(50 * dd.height / dd.width,
        dd.height - deltax * dd.height / dd.width);
      props.left = dd.originalX + dd.width - props.width;
    }

    if (dd.attr.indexOf("NW") > -1) {
      deltax1 = Math.max(dd.deltaX, dd.limit.left - dd.originalX);
      deltax2 = Math.max(dd.deltaX * dd.height / dd.width, dd.limit.top - dd.originalY) * dd.width / dd.height;
      deltax = Math.round(Math.max(deltax1, deltax2) / snap) * snap;

      props.width = Math.max(50, dd.width - deltax);
      props.height = Math.max(50 * dd.height / dd.width, dd.height - deltax * dd.height / dd.width);
      props.left = dd.originalX + dd.width - props.width;
      props.top = dd.originalY + (dd.width - props.width) * dd.height / dd.width;
    }

    if (dd.attr.indexOf("related-layout-box") > -1) {
      props.top = Math.min(dd.limit.bottom, Math.max(dd.limit.top, dd.offsetY));
      props.left = Math.min(dd.limit.right, Math.max(dd.limit.left, dd.offsetX));
      props.top = Math.round(props.top / snap) * snap;
      props.left = Math.round(props.left / snap) * snap;
    }
    $(this).css(props);
  });
});
