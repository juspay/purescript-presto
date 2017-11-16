const View = require("presto-ui").baseView;
const dom = require("presto-ui").doms;

const LinearLayout = require("presto-ui").views.LinearLayout;
const ImageView = require("presto-ui").views.ImageView;
const TextView = require("presto-ui").views.TextView;

const Config = require('./../globalConfig');
const Controller = require('./../controller/components/HeaderWithArrow');
const Strings = require('./../res/strings');
const Accessibility = require('./../res/accessibility');

let STR = {};
let HINT = {};

class HeaderWithArrow extends Controller {

	constructor(props, children, state) {
		super(props, children, state);
		STR = Strings();
		HINT = Accessibility();
		this.HeaderText = STR.DEFAULT_HEADERWITHARROW_HEADERTEXT;
	}

	onPop = () => {}

	render = () => {
		let HeaderText = (this.props.HeaderText) ? this.props.HeaderText : this.HeaderText;
		this.layout = (
			<LinearLayout
				id={this.idSet.HeaderWithArrow}
				height={this.props.height ? this.props.height:"64"}
				width={this.props.width ? this.props.width:"360"}
				orientation="horizontal"
				gravity="center_vertical"
				padding="4,8,76,8"
				background="#ffffffff"
				cornerRadius="0"
				root={true}
				margin={this.props.margin ? this.props.margin:"0,0,0,0"}
				weight={this.props.weight ? this.props.weight:"0"}
				accessibilityHint={HINT.HEADERWITHARROW_HEADERWITHARROW}
				style={this.style_HeaderWithArrow}>
				<ImageView
					id={this.idSet.Icon}
					height="48"
					width="48"
					orientation="vertical"
					imageUrl="icon2"
					accessibilityHint={HINT.HEADERWITHARROW_ICON}
					style={this.style_Icon} />
				<TextView
					id={this.idSet.HeaderText}
					height="31"
					width="228"
					margin="4,0,0,0"
					weight="1"
					text={HeaderText}
					textSize="24"
					color="#ff484848"
					fontStyle="SourceSansPro-Bold"
					gravity="left"
					accessibilityHint={HINT.HEADERWITHARROW_HEADERTEXT}
					style={this.style_HeaderText} />
			</LinearLayout>
		);
		return this.layout.render();
	}

};

module.exports = HeaderWithArrow;
