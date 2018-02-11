module Example.Reducer where

type Store =
  { userName :: String
  , userEmail :: String
  }

initialStore :: Store
initialStore =
  { userName: ""
  , userEmail: ""
  }

data Action
  = UpdateUserName String
  | UpdateUserEmail String

reduce :: Store -> Action -> Store
reduce store = case _ of
  UpdateUserName name -> store { userName = name }
  UpdateUserEmail email -> store { userEmail = email }
