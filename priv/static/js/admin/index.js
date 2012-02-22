
$(function() {
  return $('a.pop').bind('click', function() {
    var id;
    id = "ajax_" + ($(this).attr("id"));
    $("#" + id).remove();
    $("<div id='" + id + "'></div>").appendTo($("#nil")).load($(this).attr("href"));
    return false;
  });
});
