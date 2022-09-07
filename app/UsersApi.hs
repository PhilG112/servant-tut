{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module UsersApi where
import Servant

import Models.User
import Data.Time

users1 :: [User]
users1 =
    [
        User "Isaac Newton" 66 "isaac@space.com" (fromGregorian 1999 1 1),
        User "Albert Einstein" 76 "Albert@space.com" (fromGregorian 1989 1 1)
    ]

-- Api1
type UserApi1 = "users"
    :> Get '[JSON] [User]

server1 :: Server UserApi1
server1 = return users1

userApi :: Proxy UserApi1
userApi = Proxy

-- Api2
type UserApi2 =
    "users"
        :> Get '[JSON] [User]
    :<|>
    "albert"
        :> Get '[JSON] User
    :<|>
    "isaac"
        :> Get '[JSON] User

isaac :: User
isaac = User "Isaac Newton" 372 "isaac@newton.co.uk" (fromGregorian 1683 3 1)

albert :: User
albert = User "Albert Einstein" 136 "ae@mc2.org" (fromGregorian 1905 12 1)

users2 :: [User]
users2 = [isaac, albert]

server2 :: Server UserApi2
server2 = return users2
     :<|> return albert
     :<|> return isaac

userApi2 :: Proxy UserApi2
userApi2 = Proxy
