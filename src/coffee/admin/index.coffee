$ ->
  Errors = {
    "repeat_identity": "此代理已經存在，請更換用户名。"
    "repeat_player": "此玩家已經存在，請更換用户名。"
    "invalidate_identity": "輸入的用户名格式錯誤，請使用4~10位的小寫英文與下劃線[a-z_]作爲用户名。"
    "invalidate_password": "密碼輸入格式有誤，請使用6位以上的英文字母數字與符號的組合作爲密碼。"
    "invalidate_repassword": "密碼輸入有誤，請再次輸入同樣的密碼。"
  }

  $('a.pop').bind 'click', ->
    $.blockUI({ message: $("##{$(@).attr("id")}_wapper"), css: { width: 'auto', left: '40%' } })
    $("#create_agent_form, #create_player_form").clearForm()
    return false

  $("a.cancel").click ->
    $(@).parent().parent().clearForm()
    $.unblockUI()

  $("#create_agent_form, #create_player_form").ajaxForm {
    dataType: 'json'

    success: (data, st, xhr, form) ->
      show_error data.errors[0] if !data.successful
      if data.successful
        $(form).clearForm()
        show_successful()

    beforeSubmit: (formData) ->
      if get(formData, "password") != get(formData, "re-password")
        show_error "invalidate_password"
        return false
  }


  show_successful = (key) ->
    alert "操作成功" if key == undefined

  show_error = (key) ->
    if Errors[key] != undefined
      alert Errors[key]
    else
      alert "ERROR: #{key}"

  get = (array, name) ->
    val = null
    $.each array, (i, o) ->
      val = o if o.name == name

    return val.value

  return
