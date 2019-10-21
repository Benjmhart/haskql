exports.setItemString = function(key) {
  return function(value) {
    return function() {
      localStorage.setItem(key, value);
    };
  };
};

exports.getItemString = function(key) {
  return function() {
    return localStorage.getItem(key) || '';
  };
};

exports.removeItem = function(key) {
  return function() {
    localStorage.removeItem(key);
  };
};
