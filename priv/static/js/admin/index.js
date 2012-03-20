
$(function() {
  var Errors, show_error, show_successful;
  Errors = {
    "repeat_identity": "此代理已經存在，請更換用户名。",
    "repeat_player": "此玩家已經存在，請更換用户名。",
    "invalidate_identity": "輸入的用户名格式錯誤，請使用4~10位的小寫英文與下劃線[a-z_]作爲用户名。",
    "invalidate_password": "密碼輸入格式有誤，請使用6位以上的英文字母數字與符號的組合作爲密碼。",
    "invalidate_repassword": "密碼輸入有誤，請再次輸入同樣的密碼。",
    "less_balance": "對不起，您可用餘額不足，請確保账户内有可用餘額。",
    "invalidate_amount": "账户信息輸入有誤，請輸入正確的账户額度信息。"
  };
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
  $.init_form = function(form_id, before, successful) {
    $("a.cancel").click(function() {
      return tb_remove();
    });
    return $("#" + form_id).ajaxForm({
      dataType: 'json',
      beforeSubmit: before,
      success: function(data, st, xhr, form) {
        if (data.successful) {
          $("[type=number]").val(0);
          $(form).clearForm();
          show_successful();
          return successful();
        } else {
          return show_error(data.errors[0]);
        }
      }
    });
  };
  $.check_password = function(formData) {
    if ($.get(formData, "password") !== $.get(formData, "re-password")) {
      show_error("invalidate_password");
      return false;
    }
    return true;
  };
  $.get = function(array, name) {
    var val;
    val = null;
    $.each(array, function(i, o) {
      if (o.name === name) return val = o;
    });
    return val.value;
  };
});
