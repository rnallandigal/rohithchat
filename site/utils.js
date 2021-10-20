function genHTML(html) {
  let template = document.createElement('template');
  html = html.trim();
  template.innerHTML = html;
  return template.content.firstChild;
}

function genMessageFeed(id) {
//  return genHTML(`<div class="flex-grow-1 d-flex flex-column justify-content-end text-wrap text-break d-none" id="${id}" style="height: 0; overflow-y: auto;"></div>`);
  return genHTML(`<div class="d-none d-flex flex-column-reverse text-wrap text-break" id="${id}" style="height: 100%; overflow: auto;" ></div>`);
}

function genChatButton(name, prefix, chatID) {
  return genHTML(`<div class="border-bottom fs-5 text-center text-white btn btn-secondary w-100" id="${prefix}${chatID}" onmouseup="switchChat(${chatID})">${name}</div>`);
}

function genMessage(content) {
  return genHTML(`<h3>${content}</h3>`);
}
