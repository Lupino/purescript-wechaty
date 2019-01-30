exports._say = function(msg, obj) {
  return function() {
    return msg.say(obj);
  }
}

exports._sayTo = function(msg, replyTo, obj) {
  return function() {
    return msg.say(obj, replyTo);
  }
}

var re_emoji = /<img[^\[]+\[([^\]]+)][^>]+>/gi;
exports._getText = function(msg) {
  return function() {
    var text = msg.text();
    text = text.replace(/<br[^>]*>/ig, "\n");
    text = text.replace(/<\/?a[^>]*>/ig, "");
    text = text.replace(re_emoji, function(s) {
      var m = re_emoji.exec(s);
      if (m && m.length > 1) {
        return '[' + m[1] + ']';
      }
      return '';
    });
    return text;
  }
}

exports._getFrom = function(msg) {
  return function() {
    return msg.from();
  };
}

exports._getSelf = function(msg) {
  return function() {
    return msg.self();
  }
}

exports._room = function(just, nothing, msg) {
  return function() {
    if (msg.room()) {
      return just(msg.room());
    } else {
      return nothing;
    }
  }
}
