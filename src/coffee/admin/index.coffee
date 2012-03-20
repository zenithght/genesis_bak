$ ->
  Errors = {
    "repeat_identity": "此代理已經存在，請更換用户名。"
    "repeat_player": "此玩家已經存在，請更換用户名。"
    "invalidate_identity": "輸入的用户名格式錯誤，請使用4~10位的小寫英文與下劃線[a-z_]作爲用户名。"
    "invalidate_password": "密碼輸入格式有誤，請使用6位以上的英文字母數字與符號的組合作爲密碼。"
    "invalidate_repassword": "密碼輸入有誤，請再次輸入同樣的密碼。"
    "less_balance": "對不起，您可用餘額不足，請確保账户内有可用餘額。"
    "invalidate_amount": "账户信息輸入有誤，請輸入正確的账户額度信息。"
  }

  show_successful = (key) ->
    alert "操作成功" if key == undefined

  show_error = (key) ->
    if Errors[key] != undefined
      alert Errors[key]
    else
      alert "ERROR: #{key}"

  $.init_form = (form_id, before, successful) ->
    $("a.cancel").click ->
      tb_remove()

    $("##{form_id}").ajaxForm {
      dataType: 'json'
      beforeSubmit: before
      success: (data, st, xhr, form) ->
        if data.successful
          $("[type=number]").val(0)
          $(form).clearForm()
          show_successful()
          successful()
        else
          show_error data.errors[0]
    }

  $.check_password = (formData) ->
    if $.get(formData, "password") != $.get(formData, "re-password")
      show_error "invalidate_password"
      return false

    return true

  $.get = (array, name) ->
    val = null
    $.each array, (i, o) ->
      val = o if o.name == name

    return val.value

  return
