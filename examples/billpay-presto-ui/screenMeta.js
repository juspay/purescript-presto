const RootScreen = require("./views/RootScreen");
// Screens of page: Dashboard
const ChooseOperatorScreen = require("./views/recharge/ChooseOperatorScreen");
const AskAmountScreen = require("./views/recharge/AskAmountScreen");
const AskMobileNumberScreen = require("./views/recharge/AskMobileNumberScreen");
const SplashScreen = require("./views/recharge/SplashScreen");
const StatusScreen = require("./views/recharge/StatusScreen");

const screens = {
	ChooseOperatorScreen,
	AskAmountScreen,
	AskMobileNumberScreen,
	SplashScreen,
	StatusScreen,
	RootScreen
};

const INIT_UI = "SplashScreen";

module.exports = {
	screens,
	INIT_UI
};
