const purescriptObj = require('./output/Core/index.js');

import SplashScreen from './views/SplashScreen'
import ChooseOperator from './views/ChooseOperator';
import AskMobileNumber from './views/AskMobileNumber';
import AskAmount from './views/AskAmount';
import StatusScreen from './views/StatusScreen'

import React from 'react';
import ReactDOM from 'react-dom';
import './index.css';

window.__duiShowScreen = (callback,screenInfo) => {
	let screen = {  "SplashScreen":<SplashScreen/>,
					"ChooseOperatorScreen":<ChooseOperator operators={screenInfo.contents}/>,
					"AskMobileNumberScreen":<AskMobileNumber/>,
					"AskAmountScreen":<AskAmount/>,
					"StatusScreen":<StatusScreen state={screenInfo.contents} />}
	window.__setCallback(callback,screenInfo)
	try {
		ReactDOM.render( screen[screenInfo.screen], document.getElementById('root'));	
	} catch(e) {
		console.log(e)
	}
	
}


let setCallback = (callback, state) => {
  window.__duiCb = callback;
};

let runDuiCallback = (state) => {
  let callback = window.__duiCb;

  if (typeof callback == "function")
    callback(state)();
};

let purescriptInit = (purescriptObj_) => {
  let purescriptMain = purescriptObj.launchApp;
  let purescriptChangeFlow = purescriptObj.launchApp;
  window.__duiCb = null;
  window.__runDuiCallback = runDuiCallback;
  window.__setCallback = setCallback
  window.__changePureScriptFlow = purescriptChangeFlow;
  purescriptMain();
};

purescriptInit(purescriptObj);
