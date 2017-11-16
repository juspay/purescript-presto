const View = require("presto-ui").baseView;
const dom = require("presto-ui").doms;

const LinearLayout = require("presto-ui").views.LinearLayout;
const ImageView = require("presto-ui").views.ImageView;
const TextView = require("presto-ui").views.TextView;
const EditText = require("presto-ui").views.EditText;

const Config = require('./../../globalConfig');
const Controller = require('./../../controller/pages/Recharge/AskAmountScreen');
const Strings = require('./../../res/strings');
const Accessibility = require('./../../res/accessibility');

let STR = {};
let HINT = {};

class AskAmountScreen extends Controller {

	constructor(props, children, state) {
		super(props, children, state);
		STR = Strings();
		HINT = Accessibility();
	}

	onPop = () => {}

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
				accessibilityHint={HINT.MOBILENUMBERSCREEN_SCREEN_2}
				style={this.style_Screen_2}>
				<LinearLayout
					id={this.idSet.HeaderWithArrow}
					height="64"
					width="match_parent"
					orientation="horizontal"
					gravity="center_vertical"
					padding="4,8,76,8"
					background="#ffffffff"
					cornerRadius="0"
					accessibilityHint={HINT.MOBILENUMBERSCREEN_HEADERWITHARROW}
					style={this.style_HeaderWithArrow}>
					<ImageView
						id={this.idSet.Icon}
						height="48"
						width="48"
						orientation="vertical"
						imageUrl="icon1"
						accessibilityHint={HINT.MOBILENUMBERSCREEN_ICON}
						style={this.style_Icon} />
					<TextView
						id={this.idSet.HeaderText}
						height="31"
						width="228"
						margin="4,0,0,0"
						weight="1"
						textSize="24"
						color="#ff0a338d"
						fontStyle="Source Sans Pro-Bold"
						gravity="left"
						accessibilityHint={HINT.AMOUNTSCREEN_HEADERTEXT}
						text={STR.AMOUNTSCREEN_HEADERTEXT}
						style={this.style_HeaderText} />
				</LinearLayout>
				<LinearLayout
					id={this.idSet.Screen_Content}
					height="576"
					width="match_parent"
					orientation="vertical"
					gravity="center_horizontal"
					padding="20,30,20,20"
					background="#ffffffff"
					cornerRadius="0"
					accessibilityHint={HINT.MOBILENUMBERSCREEN_SCREEN_CONTENT}
					style={this.style_Screen_Content}>
					<LinearLayout
						id={this.idSet.Form_Field_Copy}
						height="76"
						width="match_parent"
						orientation="vertical"
						gravity="center_horizontal"
						accessibilityHint={HINT.MOBILENUMBERSCREEN_FORM_FIELD_COPY}
						style={this.style_Form_Field_Copy}>
						<TextView
							id={this.idSet.Enter_Recharge_Amoun}
							height="20"
							width="match_parent"
							textSize="16"
							color="#89000000"
							fontStyle="Source Sans Pro-Regular"
							gravity="left"
							accessibilityHint={HINT.MOBILENUMBERSCREEN_ENTER_RECHARGE_AMOUN}
							text={STR.MOBILENUMBERSCREEN_ENTER_RECHARGE_AMOUN}
							style={this.style_Enter_Recharge_Amoun} />
						<LinearLayout
							id={this.idSet.Number_Field}
							height="36"
							width="match_parent"
							orientation="horizontal"
							gravity="center_vertical"
							padding="0,1,0,0"
							margin="0,20,1,0"
							background="#ffffffff"
							cornerRadius="0"
							accessibilityHint={HINT.MOBILENUMBERSCREEN_NUMBER_FIELD}
							style={this.style_Number_Field}>
							<LinearLayout
								id={this.idSet.Group}
								height="34"
								width="44"
								orientation="vertical"
								gravity="center"
								accessibilityHint={HINT.MOBILENUMBERSCREEN_GROUP}
								style={this.style_Group}>
								<ImageView
									id={this.idSet.Rupee_black}
									height="34"
									width="44"
									orientation="vertical"
									imageUrl="rupeeblack0"
									accessibilityHint={HINT.MOBILENUMBERSCREEN_RUPEE_BLACK}
									style={this.style_Rupee_black} />
							</LinearLayout>
							<LinearLayout
								id={this.idSet.Edit_Text}
								height="36"
								width="270"
								orientation="vertical"
								gravity="center_horizontal"
								margin="5,0,0,0"
								weight="1"
								accessibilityHint={HINT.MOBILENUMBERSCREEN_EDIT_TEXT}
								style={this.style_Edit_Text}>
								<EditText
									id={this.idSet.Placeholder2}
									height="34"
									width="match_parent"
									textSize="28"
									color="#ff4a494a"
									fontStyle="Source Sans Pro-Regular"
									padding = "0,0,0,0"
									background = "#ffffffff"
									lineHeight="34px"
									gravity="left"
									accessibilityHint={HINT.MOBILENUMBERSCREEN_PLACEHOLDER2}
									hint={STR.MOBILENUMBERSCREEN_PLACEHOLDER2}
									style={this.style_Placeholder2} />
								<LinearLayout
									id={this.idSet.Seperator}
									height="2"
									width="match_parent"
									background="#ff0a328d"
									cornerRadius="0"
									accessibilityHint={HINT.MOBILENUMBERSCREEN_SEPERATOR}
									style={this.style_Seperator} />
							</LinearLayout>
						</LinearLayout>
					</LinearLayout>
					<LinearLayout
						id={this.idSet.Space}
						weight="1"
						accessibilityHint={HINT.MOBILENUMBERSCREEN_SPACE}
						style={this.style_Space} />
					<LinearLayout
						id={this.idSet.Button}
						height="48"
						width="match_parent"
						orientation="vertical"
						padding="0,15,0,15"
						background="#ff202296"
						cornerRadius="4"
						accessibilityHint={HINT.MOBILENUMBERSCREEN_BUTTON}
						style={this.style_Button}>
						<TextView
							id={this.idSet.ButtonText}
							height="18"
							width="320"
							textSize="14"
							color="#ffffffff"
							fontStyle="Source Sans Pro-Semibold"
							gravity="center"
							accessibilityHint={HINT.MOBILENUMBERSCREEN_BUTTONTEXT}
							text={STR.MOBILENUMBERSCREEN_BUTTONTEXT}
							style={this.style_ButtonText} />
					</LinearLayout>
				</LinearLayout>
			</LinearLayout>
		);
		return this.layout.render();
	}

};

module.exports = AskAmountScreen;
