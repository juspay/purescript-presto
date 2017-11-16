const View = require("presto-ui").baseView;

class ChooseOperatorScreen extends View {

	constructor(props, children, state) {
		super(props, children, state);
		this.shouldCacheScreen = false;

		this.props_Operator = {
			onClick : this.onOperatorSelected.bind(this)
		}

	}

	onOperatorSelected = (operator) => {
		window.__runDuiCallback(JSON.stringify({tag:"OperatorSelected",contents:operator}));
	}

	onBackPressed = () => {
		window.__runDuiCallback(JSON.stringify({tag:"ChooseOperatorScreenAbort"}));
	}

}

module.exports = ChooseOperatorScreen;
