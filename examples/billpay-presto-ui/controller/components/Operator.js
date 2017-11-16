const View = require("presto-ui").baseView;

class Operator extends View {

	constructor(props, children, state) {
		super(props, children, state);
		this.shouldCacheScreen = false;
		this.style_Operator = {
			onClick : this.onOperatorSelected.bind(this)
		}
	}

	onOperatorSelected = () => {
		this.props.parentProps.onClick("Operator") // operator has to be replaced with data which is passed from Screen
	}

}

module.exports = Operator;
