var bsInsts = {};
var conn;
var currentUser;
var currentChatID = 1;
var users = new Map();
var chats = new Map();

window.onload = (event) => {
  bsInsts = {
    'welcomeModal': bootstrap.Modal.getOrCreateInstance(
      welcomeModal, { keyboard: false, focus: true }
    ),
  };

  signinForm.addEventListener('submit', signinHandler);
  sendMessageButton.addEventListener('mouseup', sendMessageHandler);
  chatBox.addEventListener('keyup', (event) => {
    if(event.keyCode === 13) {
      event.preventDefault();
      sendMessageHandler();
    }
  });
  newChatButton.addEventListener('mouseup', newChatHandler);
  newChatBox.addEventListener('keyup', (event) => {
    if(event.keyCode === 13) {
      event.preventDefault();
      newChatHandler();
    }
  });
  inviteButton.addEventListener('mouseup', inviteHandler);
  inviteBox.addEventListener('keyup', (event) => {
    if(event.keyCode === 13) {
      event.preventDefault();
      inviteHandler();
    }
  });

  bsInsts.welcomeModal.toggle();
};

async function switchChat(newChatID) {
  document.getElementById(`messageFeed${currentChatID}`).classList.add('d-md-none');
  document.getElementById(`messageFeed${newChatID}`).classList.remove('d-md-none');
  currentChatID = newChatID;
}

async function signinHandler(event) {
  event.preventDefault();
  event.stopPropagation();

  let username = usernameInput.value;
  let user = await getUser({ _user_query_username: username });
  currentUser = user ? user : await postUser({ _user_req_username: username });

  let memberships = await getMemberships({
    _membership_query_user: currentUser._user_id,
    _membership_query_pagination: { _pagination_size: -1 }
  });

  for(let membership of memberships) {
    await joinChatroom(membership._membership_chat_id);
  }

  currentChatID = 1;
  messageFeed0.remove();
  messageFeed1.classList.remove('d-md-none');
  chatBox.focus();

  initWebSocket();
  return false;
}

function sendMessageHandler() {
  if(!chatBox.value) return;
  conn.send(JSON.stringify({
    tag: "SubmitMessage",
    contents: [ currentChatID, chatBox.value ]
  }));
  chatBox.value = '';
  return true;
}

async function newChatHandler() {
  if(!newChatBox.value) return;

  try {
    let chat = await newChat({ _chat_req_chatname: newChatBox.value });
    chats.set(chat._chat_id, chat);
    newChatGroup.before(genChatButton(chat._chat_name, 'chatButton', chat._chat_id));
    sendGroup.before(genMessageFeed(`messageFeed${chat._chat_id}`));
    switchChat(chat._chat_id);

    await newMembership({
      _membership_req_user_id: currentUser._user_id,
      _membership_req_chat_id: chat._chat_id
    });
    newChatBox.value = '';
  } catch(e) {
    alert(`chat ${newChatBox.value} already exists`);
  }
  return true;
}

async function inviteHandler() {
  if(!inviteBox.value) return;
  let chat = await getChatByID(currentChatID);
  try {
    let user = await getUser({ _user_query_username: inviteBox.value });
    await newMembership({
      _membership_req_user_id: user._user_id,
      _membership_req_chat_id: chat._chat_id
    });
    inviteBox.value = '';
  } catch(e) {
    alert(`user ${inviteBox.value} not found or is already a member of ${chat._chat_name}`);
  }
}

function initWebSocket() {
  conn = new WebSocket(`ws://${location.host}/ws/v1`);
  conn.onopen = openConnection;
  conn.onerror = (event) => {
    console.log(`conn errored`);
    console.log(event);
  }
  conn.onclosed = (event) => {
    console.log(`conn closed`);
    console.log(event);
  }
  conn.onmessage = handleMessage;
}

function openConnection(event) {
  console.log(`conn opened`);
  conn.send(JSON.stringify({
    tag: "Authenticate",
    contents: currentUser._user_id
  }));

  bsInsts.welcomeModal.toggle();
}

async function handleMessage(event) {
  let data = JSON.parse(event.data);
  let user, chatID;
  switch(data.tag) {
    case "PushMessage":
      user = await getUserByID(data.contents._message_user_id);
      chat = chats.get(data.contents._message_chat_id);
      await pushMessage(
        user._user_id,
        chat._chat_id,
        `${user._user_username}: ${data.contents._message_content}`
      );
      break;
    case "MembershipUpdate":
      user = await getUserByID(data.contents[0]);
      if(chats.has(data.contents[1])) {
        chat = chats.get(data.contents[1]);
        await pushMessage(
          user._user_id,
          chat._chat_id,
          `${user._user_username} joined the chat`
        );
      } else {
        await joinChatroom(data.contents[1]);
        switchChat(data.contents[1]);
      }
      break;
    default:
      console.log(`unknown message type: ${data}`);
      break;
  }
}

async function pushMessage(userID, chatID, content) {
  let feed = document.getElementById(`messageFeed${chatID}`);
  if(currentUser._user_id == userID) {
    feed.append(genMessage(content));
  } else {
    feed.append(genMessage(content));
  }
}

async function joinChatroom(chatID) {
  let chat = await getChatByID(chatID);
  newChatGroup.before(genChatButton(chat._chat_name, 'chatButton', chatID));
  sendGroup.before(genMessageFeed(`messageFeed${chatID}`));

  for(let msg of await chatHistory(chatID)) {
    await pushMessage(0, chatID, msg.content);
  }
  return chat;
}

async function chatHistory(chatID) {
  let feed = [];
  for(let msg of await getChatMessages(chatID)) {
    let user = await getUserByID(msg._message_user_id);
    feed.push({
      timestamp: new Date(msg._message_sent),
      content: `${user._user_username}: ${msg._message_content}`
    });
  }

  let memberships = await getMemberships({
    _membership_query_chat: chatID,
    _membership_query_pagination: { _pagination_size: -1 }
  });

  for(let membership of memberships) {
    let user = await getUserByID(membership._membership_user_id);
    feed.push({
      timestamp: new Date(membership._membership_joined),
      content: `${user._user_username} joined the chat`
    });
  }

  feed.sort((a, b) => a.timestamp < b.timestamp ? -1 : 1);
  return feed;
}

async function getUser(body) {
  let id = body._user_query_id; 
  if(id && users.has(id)) return users.get(id);

  let resp = await fetch('api/v1/user/search', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(body)
  });

  let data = await resp.json();
  if(data.length) {
    users.set(data[0]._user_id, data[0]);
    return data[0];
  } else return null;
}

async function getUserByID(id) {
  return await getUser({ _user_query_id: id });
}

async function postUser(body) {
  resp = await fetch('api/v1/user', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(body)
  });
  return await resp.json();
}

async function getChat(body) {
  let id = body._chat_query_id; 
  if(id && chats.has(id)) return chats.get(id);

  let resp = await fetch('api/v1/chat/search', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(body)
  });

  let data = await resp.json();
  if(data.length) {
    chats.set(data[0]._chat_id, data[0]);
    return data[0];
  } else return null;
}

async function getChatByID(id) {
  return await getChat({ _chat_query_id: id });
}

async function newChat(body) {
  let resp = await fetch('api/v1/chat', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(body)
  });
  return await resp.json();
}

async function getMemberships(body) {
  let resp = await fetch('api/v1/membership/search', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(body)
  });
  return await resp.json();
}

async function newMembership(body) {
  let resp = await fetch('api/v1/membership', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(body)
  });
  return await resp.json();
}

async function getMessages(body) {
  let resp = await fetch('api/v1/message/search', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(body)
  });
  return await resp.json();
}

async function getChatMessages(chat_id) {
  return await getMessages({
    _message_query_chat: chat_id,
    _message_query_pagination: { _pagination_size: 25 }
  });
}
