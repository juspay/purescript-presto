const View = require("presto-ui").baseView;
const dom = require("presto-ui").doms;

const LinearLayout = require("presto-ui").views.LinearLayout;
const TextView = require("presto-ui").views.TextView;

const Config = require('./../../globalConfig');
const Controller = require('./../../controller/pages/Recharge/SplashScreen');
const Strings = require('./../../res/strings');
const Accessibility = require('./../../res/accessibility');

let STR = {};
let HINT = {};

class SplashScreen extends Controller {

	constructor(props, children, state) {
		super(props, children, state);
		STR = Strings();
		HINT = Accessibility();
	}

	afterRender = () => {
		setTimeout(()=> {
			window.__runDuiCallback(JSON.stringify({tag:"SplashScreenRendered"}));
		},1500)
	}

	onPop = () => {}

	render = () => {
		this.layout = (
			<LinearLayout
				height="match_parent"
				width="match_parent"
				orientation="vertical"
				gravity="center_horizontal"
				padding="20,273,20,297"
				background="#ffffffff"
				cornerRadius="0"
				root={true}
				accessibilityHint={HINT.SPLASHSCREEN_CONTENT}
				style={this.style_Content}>
				<TextView
					id={this.idSet.Main_Title}
					height="50"
					width="match_parent"
					textSize="40"
					color="#ff0a338d"
					fontStyle="Source Sans Pro-Semibold"
					gravity="center"
					accessibilityHint={HINT.SPLASHSCREEN_MAIN_TITLE}
					text={STR.SPLASHSCREEN_MAIN_TITLE}
					style={this.style_Main_Title} />
				<TextView
					id={this.idSet.Sub_Title}
					height="20"
					width="match_parent"
					textSize="16"
					color="#ff8ea8e1"
					fontStyle="Source Sans Pro-Semibold"
					gravity="center"
					accessibilityHint={HINT.SPLASHSCREEN_SUB_TITLE}
					text={STR.SPLASHSCREEN_SUB_TITLE}
					style={this.style_Sub_Title} />
			</LinearLayout>
		);
		return this.layout.render();
	}

};

module.exports = SplashScreen;
