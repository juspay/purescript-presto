const View = require("presto-ui").baseView;
const dom = require("presto-ui").doms;

const LinearLayout = require("presto-ui").views.LinearLayout;
const TextView = require("presto-ui").views.TextView;

const Config = require('./../globalConfig');
const Controller = require('./../controller/components/Button');
const Strings = require('./../res/strings');
const Accessibility = require('./../res/accessibility');

let STR = {};
let HINT = {};

class Button extends Controller {

	constructor(props, children, state) {
		super(props, children, state);
		STR = Strings();
		HINT = Accessibility();
		this.ButtonText = STR.DEFAULT_BUTTON_BUTTONTEXT;
	}

	onPop = () => {}

	render = () => {
		let ButtonText = (this.props.ButtonText) ? this.props.ButtonText : this.ButtonText;
		this.layout = (
			<LinearLayout
				id={this.idSet.Button}
				height={this.props.height ? this.props.height:"48"}
				width={this.props.width ? this.props.width:"320"}
				orientation="vertical"
				padding="0,15,0,15"
				background="#ff202296"
				cornerRadius="4"
				root={true}
				margin={this.props.margin ? this.props.margin:"0,0,0,0"}
				weight={this.props.weight ? this.props.weight:"0"}
				accessibilityHint={HINT.BUTTON_BUTTON}
				style={this.style_Button}>
				<TextView
					id={this.idSet.ButtonText}
					height="18"
					width="320"
					text={ButtonText}
					textSize="14"
					color="#ffffffff"
					fontStyle="SourceSansPro-Semibold"
					gravity="center"
					accessibilityHint={HINT.BUTTON_BUTTONTEXT}
					style={this.style_ButtonText} />
			</LinearLayout>
		);
		return this.layout.render();
	}

};

module.exports = Button;
