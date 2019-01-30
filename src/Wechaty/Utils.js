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
