var wechaty = require('wechaty');
var Room = wechaty.Room;

exports._say = function(room, obj) {
  return function() {
    return room.say(obj);
  }
}

exports._sayTo = function(room, replyTo, obj) {
  return function() {
    return room.say(obj, replyTo);
  }
}

exports.getRoomTopic = function(room) {
  return room.topic();
}

exports._delete = function(room) {
  return function(contact) {
    return function() {
      return room.del(contact);
    }
  }
}

exports._memberAll = function(room) {
  return function(name) {
    return function() {
      return room.memberAll(name);
    }
  }
}

exports._add = function(room) {
  return function(contact) {
    return function() {
      return room.add(contact);
    }
  }
}
