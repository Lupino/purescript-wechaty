exports._call = function(obj) {
  return function(func) {
    return function() {
      return obj[func]();
    }
  }
}

exports._call1 = function(obj) {
  return function(func) {
    return function(arg) {
      return function() {
        return obj[func](arg)();
      }
    }
  }
}

exports.property = function(prop) {
  return function(obj) {
    var r = obj[prop];
    if (typeof r === 'function') {
      return r();
    } else {
      return r;
    }
  }
}
