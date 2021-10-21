function genHTML(html) {
  let template = document.createElement('template');
  html = html.trim();
  template.innerHTML = html;
  return template.content.firstChild;
}

function genMessageFeed(id) {
  return genHTML(
    `<div class="d-none d-flex flex-column-reverse feed"
          id="${id}"
          style="height: 100%; overflow-y: auto; overflow-x: clip; padding: 15px;">
    </div>
  `);
}

function genChatButton(name, prefix, chatID) {
  return genHTML(`<div class="border-bottom fs-5 text-center text-white btn btn-secondary w-100" id="${prefix}${chatID}" onmouseup="switchChat(${chatID})">${name}</div>`);
}

function genAdminMessage(content) {
  return genHTML(`
    <div class="chat-message chat-from-admin chat-middle">
      <p class="msg-content msg-from-admin">${content}</p>
    </div>
  `);
}

function genChatMessage(sender, senderType, content, middleMessage = false) {
  let dispSender = middleMessage ? 'd-none' : '';
  let marginStyle = middleMessage ? 'chat-middle' : 'chat-end';
  return genHTML(`
    <div class="d-flex flex-column chat-message chat-from-${senderType} ${marginStyle}">
      <p class="msg-caption ${dispSender}">${sender}</p>
      <p class="msg-content msg-from-${senderType}">${content}</p>
    </div>
  `);
}
