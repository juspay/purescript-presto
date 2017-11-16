const View = require("presto-ui").baseView;

class StatusScreen extends View {

	constructor(props, children, state) {
		super(props, children, state);
		this.shouldCacheScreen = false;
	}

	onBackPress = () => {
		window.__runDuiCallback(JSON.stringify({tag:"StatusScreenAbort"}));
	}

}

module.exports = StatusScreen;
