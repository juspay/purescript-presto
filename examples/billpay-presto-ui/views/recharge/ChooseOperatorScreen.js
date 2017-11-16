const View = require("presto-ui").baseView;
const dom = require("presto-ui").doms;

const LinearLayout = require("presto-ui").views.LinearLayout;
const TextView = require("presto-ui").views.TextView;
const ScrollView = require("presto-ui").views.ScrollView;
const Operator = require('../../components/Operator');

const Config = require('./../../globalConfig');
const Controller = require('./../../controller/pages/Recharge/ChooseOperatorScreen');
const Strings = require('./../../res/strings');
const Accessibility = require('./../../res/accessibility');

let STR = {};
let HINT = {};

class ChooseOperatorScreen extends Controller {

	constructor(props, children, state) {
		super(props, children, state);
		STR = Strings();
		HINT = Accessibility();
		let ids = ["Operator_List_scroll"];
		this.setIds(ids);
		this.state = state
	}

	onPop = () => {}

	OperatorItem = (data) => {
		let layout = (
			<Operator
				parentProps={this.props_Operator}
				height="60"
				image = {data.toLowerCase()}
				operatorName={data}
				width="match_parent" />
		);
		return layout;
	}

	Operator_List_scroll = () => {
		let data = this.state.contents;
		data.forEach((d, i)=>{
			let view = this.OperatorItem(d).render();
			this.appendChild(this.idSet.Operator_List_scroll, view, i, null, false);
		});
	}

	render = () => {
		this.layout = (
			<LinearLayout
				height="match_parent"
				width="match_parent"
				orientation="vertical"
				gravity="center_horizontal"
				background="#ffffffff"
				cornerRadius="0"
				root={true}
				accessibilityHint={HINT.CHOOSEOPERATORSCREEN_CONTENT}
				style={this.style_Content}>
				<LinearLayout
					id={this.idSet.Header}
					height="64"
					width="match_parent"
					orientation="vertical"
					padding="20,16,20,17"
					background="#ffffffff"
					cornerRadius="0"
					accessibilityHint={HINT.CHOOSEOPERATORSCREEN_HEADER}
					style={this.style_Header}>
					<TextView
						id={this.idSet.HeaderText}
						height="31"
						width="320"
						textSize="24"
						color="#ff0a338d"
						fontStyle="Source Sans Pro-Semibold"
						gravity="left"
						accessibilityHint={HINT.CHOOSEOPERATORSCREEN_HEADERTEXT}
						text={STR.CHOOSEOPERATORSCREEN_HEADERTEXT}
						style={this.style_HeaderText} />
				</LinearLayout>
				<ScrollView
					id={this.idSet.Operator_List}
					height="0"
					width="match_parent"
					weight="1"
					background="#ffffffff"
					cornerRadius="0"
					accessibilityHint={HINT.CHOOSEOPERATORSCREEN_OPERATOR_LIST}
					style={this.style_Operator_List}>
					<LinearLayout
						id={this.idSet.Operator_List_scroll}
						height="match_parent"
						width="match_parent"
						orientation="vertical"
						gravity="center_horizontal"
						afterRender={this.Operator_List_scroll}
						accessibilityHint={HINT.CHOOSEOPERATORSCREEN_LIST_CONTENT}
						style={this.style_List_Content} />
				</ScrollView>
			</LinearLayout>
		);
		return this.layout.render();
	}

};

module.exports = ChooseOperatorScreen;
