'use strict';

exports.getCurrentRoute = function() {
  return decodeURI(location.pathname + location.search);
};

exports.onNavigate = function(f) {
  return function() {
    var pushState = history.pushState;
    history.pushState = function(state, title, url) {
      pushState.apply(history, [state, title, url]);
      f(decodeURI(url))();
    };
    window.addEventListener('popstate', function(event) {
      f(document.location.pathname)();
    });
  };
};

exports.push = function(root) {
  return function(path) {
    return function() {
      history.pushState(null, null, root + path);
    };
  };
};
