
$(document).ready(function() {
   $(".r").each(function() {
     $(this).addClass("language-r");
     $(this).removeClass("r");
   });
   Prism.highlightAll();
   
   $('code.language-r').parent().next().each(function(index) {
      if($(this).is("pre") & $(this).attr("class") == undefined)
         $(this).addClass("r-output");
   });
});


