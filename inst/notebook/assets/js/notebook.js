
$(document).ready(function() {
   // any lnk to a display that requires the trelliscope viewer
   // needs to be resolved whether on local or web server
   $(".displayHref").each(function() {
      if(window.location.protocol == "file:") {
         prefix = "http://localhost:8100/";
      } else {
         p = window.location.pathname;
         p = p.substring(0, p.lastIndexOf('/'));
         p = p.substring(0, p.lastIndexOf('/') + 1);
         prefix = window.location.origin + p + "trelliscopeViewer/";
      }
      $(this).attr("href", prefix + $(this).attr("href"));
   });
});

