var wechaty = require('wechaty');

var FriendRequest = wechaty.FriendRequest;

exports.createSend = function (contact) {
  return function(hello) {
    return FriendRequest.createSend(contact, hello)
  }
}

exports.createConfirm = function(contact) {
  return FriendRequest.createConfirm(contact);
}

exports.createReceive = function(contact) {
  return function(hello) {
    return function(ticket) {
      return FriendRequest.createReceive(contact, hello, ticket);
    }
  }
}

exports._send = function(req) {
  return function() {
    return req.send();
  }
}

exports._accept = function(req) {
  return function() {
    return req.accept();
  }
}

exports.hello = function(req) {
  return req.hello();
}

exports.contact = function(req) {
  return req.contact();
}

exports._reject = function(req) {
  return function() {
    return req.reject();
  }
}

exports._type = function(unknow) {
  return function(send) {
    return function(receive) {
      return function(confirm) {
        return function(req) {
          switch (req.type()) {
            case FriendRequest.Type.Unknow:
              return unknow;
            case FriendRequest.Type.Send:
              return send;
            case FriendRequest.Type.Receive:
              return receive;
            case FriendRequest.Type.Confirm:
              return confirm;
            default:
              return unknow;
          }
        }
      }
    }
  }
}
