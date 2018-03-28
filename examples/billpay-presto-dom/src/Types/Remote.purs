module Types.Remote where
  
import Types.Storage

data BillPayRequest  = BillPayRequest 
    {
      mobileNumber::String
     ,amount::Number
     ,operator::String
    }

data BillPayResponse = BillPayResponse Transaction