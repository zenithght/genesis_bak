$ ->
  $('a.pop').bind 'click', ->
    id = "ajax_#{$(@).attr("id")}"
    $("##{id}").remove()

    $("<div id='#{id}'></div>").
      appendTo($("#nil")).
      load($(@).attr("href"))

    return false
  

