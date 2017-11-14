module UI.Flow where

import Prelude (bind, pure)
import Engineering.Helpers.Commons (runUI')
import Engineering.Types.App (Flow,liftLeft)
import Product.Types (Operator, MobileNumber, Amount, BillPayFailure(..), BillPayStatus)
import UI.Types (AskAmountScreen(..), AskAmountScreenAction(..), AskMobileNumberScreen(..), AskMobileNumberScreenAction(..), ChooseOperatorScreen(..), ChooseOperatorScreenAction(..), SplashScreen(..), SplashScreenAction(..), StatusScreen(..), StatusScreenAction(..))


splashScreen :: Flow BillPayFailure SplashScreenAction
splashScreen = do
	action <- runUI' SplashScreen
	case action of
		SplashScreenRendered -> pure SplashScreenRendered

chooseOperator :: Array Operator -> Flow BillPayFailure Operator
chooseOperator operators = do
	action <- runUI' (ChooseOperatorScreen operators)
	case action of 
		OperatorSelected operator-> pure operator
		ChooseOperatorScreenAbort -> liftLeft UserAbort
	

askMobileNumber :: Flow BillPayFailure MobileNumber
askMobileNumber = do
	action <- runUI' AskMobileNumberScreen
	case action of
		SubmitMobileNumber mobileNumber -> pure mobileNumber
		AskMobileNumberScreenAbort -> liftLeft UserAbort

askAmount :: Flow BillPayFailure Amount
askAmount = do
	action <- runUI' AskAmountScreen
	case action of
		SubmitAmount amount -> pure amount
		AskAmountScreenAbort -> liftLeft UserAbort


billPayStatus :: MobileNumber -> Amount -> BillPayStatus -> Flow BillPayFailure StatusScreenAction
billPayStatus mobileNumber amount payStatus = do
	action <- runUI' (StatusScreen mobileNumber amount payStatus)
	case action of
		SuccessResult -> pure SuccessResult
		StatusScreenAbort -> liftLeft UserAbort

