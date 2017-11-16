const View = require("presto-ui").baseView;
const dom = require("presto-ui").doms;

const LinearLayout = require("presto-ui").views.LinearLayout;
const ImageView = require("presto-ui").views.ImageView;
const TextView = require("presto-ui").views.TextView;

const Config = require('./../globalConfig');
const Controller = require('./../controller/components/Operator');
const Strings = require('./../res/strings');
const Accessibility = require('./../res/accessibility');

let STR = {};
let HINT = {};

class Operator extends Controller {

	constructor(props, children, state) {
		super(props, children, state);
		STR = Strings();
		HINT = Accessibility();
	}

	onPop = () => {}

	render = () => {
		this.layout = (
			<LinearLayout
				id={this.idSet.Operator}
				height={this.props.height ? this.props.height:"60"}
				width={this.props.width ? this.props.width:"360"}
				orientation="horizontal"
				gravity="center_vertical"
				padding="20,15,20,14"
				background="#ffffffff"
				cornerRadius="0"
				root={true}
				margin={this.props.margin ? this.props.margin:"0,0,0,0"}
				weight={this.props.weight ? this.props.weight:"0"}
				accessibilityHint={HINT.OPERATOR_OPERATOR}
				style={this.style_Operator}>
				<ImageView
					id={this.idSet.OperatorIcon}
					height="30"
					width="30"
					imageUrl={this.props.image}
					accessibilityHint={HINT.OPERATOR_OPERATORICON}
					style={this.style_OperatorIcon} />
				<TextView
					id={this.idSet.OperatorText}
					height="31"
					width="270"
					margin="20,0,0,0"
					weight="1"
					textSize="24"
					color="#ff000000"
					fontStyle="SourceSansPro-Regular"
					gravity="left"
					accessibilityHint={HINT.OPERATOR_OPERATORTEXT}
					text={this.props.operatorName}
					style={this.style_OperatorText} />
			</LinearLayout>
		);
		return this.layout.render();
	}

};

module.exports = Operator;
