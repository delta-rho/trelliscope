function manageNextPrev() {
   $('a#next').parent().toggleClass('disabled', $('#toc.nav li.active').nextAll('li:not(.nav-header)').size() == 0);
   $('a#previous').parent().toggleClass('disabled', $('#toc.nav li.active').prevAll('li:not(.nav-header)').size() == 0);
   $("html, body").animate({ scrollTop: 0 }, "fast");
}

$('a#next').click(function(e) {
   e.preventDefault();
   location.href = $('#toc.nav li.active').nextAll('li:not(.nav-header)').first().find('a').attr('href');
   manageNextPrev();
});

$('a#previous').click(function(e) {
   e.preventDefault();
   location.href = $('#toc.nav li.active').prevAll('li:not(.nav-header)').first().find('a').attr('href');
   manageNextPrev();
});

$(window).hashchange(function() {
  $('.tab-pane').hide();
  var tab = location.hash || $(".nav-not-header").first().attr("href");
  $(tab + '.tab-pane').show();

  $('#toc.nav li.active').removeClass('active');
  $('#toc.nav li a[href="' + tab + '"]').parent().addClass('active');
  manageNextPrev();
});

$('li.nav-header').dblclick(function(e) {
   var url = $(this).data('edit-href');
   if(url!="")
      window.open(url, '', '');
   return false;
});

manageNextPrev();
$(window).hashchange();
