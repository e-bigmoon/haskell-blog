---
title: Contact
---

お問い合わせの際は、以下のフォームに内容をご記入ください。

<form name="contact" method="POST" netlify>
  <p class="input-field">
    <i class="material-icons prefix">account_circle</i>
    <input id="name" type="text" class="validate">
    <label for="name">なまえ</label>
  </p>
  <p class="input-field">
    <i class="material-icons prefix">mail</i>
    <input id="mail" type="email" class="validate">
    <label for="mail">Email</label>
  </p>
  <p class="input-field">
    <i class="material-icons prefix">star</i>
    <input id="twitter" type="text">
    <label for="twitter">twitter id (option)</label>
  </p>
  <p class="input-field">
    <i class="material-icons prefix">mode_edit</i>
    <textarea id="message" class="materialize-textarea"></textarea>
    <label for="message">自由記入</label>
  </p>
  <p>
    <button type="submit" class="waves-effect waves-light btn">送信</button>
  </p>
</form>
