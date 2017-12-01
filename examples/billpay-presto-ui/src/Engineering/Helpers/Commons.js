exports["showUI'"] = function(sc,screen) {
	return function() {
		var screenJSON = JSON.parse(screen);
		var screenName = screenJSON.tag;
		screenJSON.screen = screenName;
    if(screenName=="InitScreen") {
      screenJSON.screen = "INIT_UI";
    }
		window.__duiShowScreen(sc, screenJSON);
	};
};

exports["callAPI'"] = function() {
	console.log("Yet to be implemented");
}
