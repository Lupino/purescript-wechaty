var wechaty = require('wechaty');
var Wechaty = wechaty.Wechaty;
var config = wechaty.config;
var QrcodeTerminal = require('qrcode-terminal');

exports.initWechaty = function() {
  return Wechaty.instance({profile: config.default.DEFAULT_PROFILE});
}

var wrapOn = function(wrap) {
  return function(bot) {
    return function(event) {
      return function(callback) {
        return function() {
          bot.on(event, wrap(callback));
        }
      }
    }
  }
};

exports._on = wrapOn(function(cb) {
  return function() {
    cb();
  }
});

exports._on1 = wrapOn(function(cb) {
  return function(arg1) {
    cb(arg1)();
  }
});

exports._on2 = wrapOn(function(cb) {
  return function(arg1, arg2) {
    cb(arg1)(arg2)();
  }
});

exports._on3 = wrapOn(function(cb) {
  return function(arg1, arg2, arg3) {
    cb(arg1)(arg2)(arg3)();
  }
});

exports.showQrcode = function(url) {
  return function() {
    var loginUrl = url.replace(/\/qrcode\//, '/l/')
    QrcodeTerminal.generate(loginUrl, {small: true}, function(qrcode) {
      console.error(qrcode);
    });
  }
}

exports._call = function(bot) {
  return function(func) {
    return function() {
      return bot[func]();
    }
  }
}

exports._find = function(bot) {
  return function(obj) {
    return function(name) {
      return function(just) {
        return function(nothing) {
          return function() {
            return bot[obj].find({name: name})
              .then(function(c) {
              if (c) {
                return just(c);
              } else {
                return nothing;
              }
            });
          }
        }
      }
    }
  }
}

exports._findAll = function(bot) {
  return function(obj) {
    return function(name) {
      return function() {
        return bot[obj].findAll({name: new RegExp(name)});
      }
    }
  }
}
