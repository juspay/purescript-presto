const View = require("presto-ui").baseView;

class AskAmountScreen extends View {

	constructor(props, children, state) {
		super(props, children, state);
		this.shouldCacheScreen = false;
	
		this.amount = "";

		this.style_Placeholder2 = {
			inputType : "numeric",
			onChange : this.onEnteringAmount.bind(this)
		}

		this.style_ButtonText = {
			width : "match_parent"
		}

		this.style_Button = {
			onClick : this.onRechargeClick.bind(this)
		}

		this.style_Icon = {
			onClick : this.onBackPressed.bind(this)
		}

	}

	onEnteringAmount = (data) => {
		this.amount = data;
	}

	onRechargeClick = () => {
		if(Number(this.amount) > 0){
			window.__runDuiCallback(JSON.stringify({tag:"SubmitAmount",contents:Number(this.amount)}));
		}
	}

	onBackPressed = () => {
		window.__runDuiCallback(JSON.stringify({tag:"AskAmountScreenAbort"}));
	}
}

module.exports = AskAmountScreen;
