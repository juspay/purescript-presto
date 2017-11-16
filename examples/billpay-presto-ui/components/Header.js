const View = require("presto-ui").baseView;
const dom = require("presto-ui").doms;

const LinearLayout = require("presto-ui").views.LinearLayout;
const TextView = require("presto-ui").views.TextView;

const Config = require('./../globalConfig');
const Controller = require('./../controller/components/Header');
const Strings = require('./../res/strings');
const Accessibility = require('./../res/accessibility');

let STR = {};
let HINT = {};

class Header extends Controller {

	constructor(props, children, state) {
		super(props, children, state);
		STR = Strings();
		HINT = Accessibility();
	}

	onPop = () => {}

	render = () => {
		this.layout = (
			<LinearLayout
				id={this.idSet.Header}
				height={this.props.height ? this.props.height:"64"}
				width={this.props.width ? this.props.width:"360"}
				orientation="vertical"
				padding="20,16,20,17"
				background="#ffffffff"
				cornerRadius="0"
				root={true}
				margin={this.props.margin ? this.props.margin:"0,0,0,0"}
				weight={this.props.weight ? this.props.weight:"0"}
				accessibilityHint={HINT.HEADER_HEADER}
				style={this.style_Header}>
				<TextView
					id={this.idSet.HeaderText}
					height="31"
					width="320"
					textSize="24"
					color="#ff0a338d"
					fontStyle="SourceSansPro-Semibold"
					gravity="left"
					accessibilityHint={HINT.HEADER_HEADERTEXT}
					text={STR.HEADER_HEADERTEXT}
					style={this.style_HeaderText} />
			</LinearLayout>
		);
		return this.layout.render();
	}

};

module.exports = Header;
