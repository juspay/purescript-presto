exports["showUI'"] = function(sc,screen) {
  return function() {
   var screenJSON = JSON.parse(screen);
    var screenName = screenJSON.tag;
    screenJSON.screen = screenName;
    window.__duiShowScreen(sc, screenJSON); 
  }
};

exports["getPermissionStatus'"] = function(err, success, permission) {
  return function() {
    console.log("Yet to implement");
  }
};

exports["requestPermission'"] = function(err, success, permissions) {
  return function() {
    console.log("Yet to implement");
  }
};

exports["callAPI'"] = function() {
  console.log("Yet to implement");
}

