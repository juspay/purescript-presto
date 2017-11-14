const View = require("@juspay/mystique").baseView;

class SplashScreen extends View {

	constructor(props, children, state) {
		super(props, children, state);
		this.shouldCacheScreen = false;
	}

}

module.exports = SplashScreen;
