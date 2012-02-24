
$(function() {
  var Errors, get, show_error, show_successful;
  Errors = {
    "repeat_identity": "此代理已經存在，請更換用户名。",
    "repeat_player": "此玩家已經存在，請更換用户名。",
    "invalidate_identity": "輸入的用户名格式錯誤，請使用4~10位的小寫英文與下劃線[a-z_]作爲用户名。",
    "invalidate_password": "密碼輸入格式有誤，請使用6位以上的英文字母數字與符號的組合作爲密碼。",
    "invalidate_repassword": "密碼輸入有誤，請再次輸入同樣的密碼。",
    "less_balance": "對不起，您可用餘額不足，請確保账户内有可用餘額。",
    "invalidate_amount": "账户信息輸入有誤，請輸入正確的账户額度信息。"
  };
  $('a.pop').bind('click', function() {
    $.blockUI({
      message: $("#" + ($(this).attr("id")) + "_wapper"),
      css: {
        width: 'auto',
        left: '40%'
      }
    });
    $("#create_agent_form, #create_player_form").clearForm();
    $("[type=number]").val(0);
    return false;
  });
  $("a.cancel").click(function() {
    $(this).parent().parent().clearForm();
    return $.unblockUI();
  });
  $("#create_agent_form, #create_player_form").ajaxForm({
    dataType: 'json',
    success: function(data, st, xhr, form) {
      if (!data.successful) show_error(data.errors[0]);
      if (data.successful) {
        $(form).clearForm();
        $("[type=number]").val(0);
        return show_successful();
      }
    },
    beforeSubmit: function(formData, form) {
      var identity;
      if ($(form).attr('id') === "create_player_form") {
        identity = get(formData, "identity");
        formData.push({
          name: "nick",
          type: "text",
          value: $.base64Encode(identity)
        });
      }
      if (get(formData, "password") !== get(formData, "re-password")) {
        show_error("invalidate_password");
        return false;
      }
    }
  });
  show_successful = function(key) {
    if (key === void 0) return alert("操作成功");
  };
  show_error = function(key) {
    if (Errors[key] !== void 0) {
      return alert(Errors[key]);
    } else {
      return alert("ERROR: " + key);
    }
  };
  get = function(array, name) {
    var val;
    val = null;
    $.each(array, function(i, o) {
      if (o.name === name) return val = o;
    });
    return val.value;
  };
});
