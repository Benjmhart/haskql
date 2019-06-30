exports.setItemString = function(key) {
  return function(value) {
    return function() {
      Window.localStorage && Window.localStorage.setItem(key, value);
    };
  };
};

exports.getItemString = function(key) {
  return function() {
    (Window.localStorage && Window.localStorage.getItem(key)) || '';
  };
};
