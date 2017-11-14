const DEFAULT = "en_US";

const strings = {
  "en_US": {
    "SPLASHSCREEN_MAIN_TITLE": "Bill Pay",
    "SPLASHSCREEN_SUB_TITLE": "make recharge simple",
    "CHOOSEOPERATORSCREEN_HEADERTEXT": "Choose Operator",
    "MOBILENUMBERSCREEN_HEADERTEXT": "Mobile Number",
    "AMOUNTSCREEN_HEADERTEXT": "Amount",
    "MOBILENUMBERSCREEN_ENTER_MOBILE_NUMBER": "Enter Mobile Number",
    "MOBILENUMBERSCREEN_PLACEHOLDER": "9740562690",
    "MOBILENUMBERSCREEN_ENTER_RECHARGE_AMOUN": "Enter Recharge Amount",
    "MOBILENUMBERSCREEN_PLACEHOLDER2": "349",
    "MOBILENUMBERSCREEN_BUTTONTEXT": "PROCEED",
    "STATUSSCREEN_SUCCESSLABEL": "Success",
    "STATUSSCREEN_SUCCESSTEXT": "You have successfully recharged ",
    "STATUSSCREEN_SUCCESSTEXT2": " with amount Rs ",
    "HEADER_HEADERTEXT": "Choose Operator",
    "HEADERWITHARROW_HEADERTEXT": "Choose Operator",
    "OPERATOR_OPERATORTEXT": "Operator",
    "BUTTON_BUTTONTEXT": "GO TO HOME"
  }
}

module.exports = () => Object.assign({}, strings[DEFAULT], strings[window.LANGUAGE]);
