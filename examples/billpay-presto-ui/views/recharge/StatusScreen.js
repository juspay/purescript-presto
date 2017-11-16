const View = require("presto-ui").baseView;
const dom = require("presto-ui").doms;

const LinearLayout = require("presto-ui").views.LinearLayout;
const ImageView = require("presto-ui").views.ImageView;
const TextView = require("presto-ui").views.TextView;

const Config = require('./../../globalConfig');
const Controller = require('./../../controller/pages/Recharge/StatusScreen');
const Strings = require('./../../res/strings');
const Accessibility = require('./../../res/accessibility');

let STR = {};
let HINT = {};

class StatusScreen extends Controller {

	constructor(props, children, state) {
		super(props, children, state);
		STR = Strings();
		this.state = state;
		HINT = Accessibility();
	}

	onBackPressed = () => {
		this.onBackPress();
	}

	onPop = () => {}

	render = () => {
		this.layout = (
			<LinearLayout
				height="match_parent"
				width="match_parent"
				orientation="vertical"
				padding="20,273,20,137"
				background="#ffffffff"
				cornerRadius="0"
				root={true}
				accessibilityHint={HINT.STATUSSCREEN_GROUP}
				style={this.style_Group}>
				<ImageView
					id={this.idSet.Illustration}
					height="95"
					width="112"
					orientation="vertical"
					imageUrl="illustration2"
					accessibilityHint={HINT.STATUSSCREEN_ILLUSTRATION}
					style={this.style_Illustration} />
				<LinearLayout
					id={this.idSet.Group_2}
					height="85"
					width="match_parent"
					orientation="vertical"
					gravity="center_horizontal"
					margin="0,20,0,0"
					accessibilityHint={HINT.STATUSSCREEN_GROUP_2}
					style={this.style_Group_2}>
					<TextView
						id={this.idSet.SuccessLabel}
						height="45"
						width="match_parent"
						textSize="36"
						color="#ff484848"
						fontStyle="Source Sans Pro-Bold"
						gravity="left"
						accessibilityHint={HINT.STATUSSCREEN_SUCCESSLABEL}
						text={STR.STATUSSCREEN_SUCCESSLABEL}
						style={this.style_SuccessLabel} />
					<TextView
						id={this.idSet.SuccessText}
						height="40"
						width="match_parent"
						textSize="16"
						color="#ff484848"
						fontStyle="Source Sans Pro-Regular"
						gravity="left"
						accessibilityHint={HINT.STATUSSCREEN_SUCCESSTEXT}
						text={STR.STATUSSCREEN_SUCCESSTEXT+this.state.contents[0]+STR.STATUSSCREEN_SUCCESSTEXT2+this.state.contents[1]}
						style={this.style_SuccessText} />
				</LinearLayout>
			</LinearLayout>
		);
		return this.layout.render();
	}

};

module.exports = StatusScreen;
