module Product.BillPay where

import Prelude

import Engineering.Types.App (Flow)
import UI.Flow (splashScreen, chooseOperator, askMobileNumber, askAmount, billPayStatus) as UI
import Remote.Flow (fetchOperators, payBill) as Remote
import Product.Types (BillPayFailure)
import UI.Types (StatusScreenAction)

billPayFlow :: Flow BillPayFailure StatusScreenAction
billPayFlow = do
  _            <- UI.splashScreen
  operators    <- Remote.fetchOperators
  operator     <- UI.chooseOperator operators
  mobileNumber <- UI.askMobileNumber
  amount       <- UI.askAmount
  result       <- Remote.payBill mobileNumber amount operator
  UI.billPayStatus mobileNumber amount result



