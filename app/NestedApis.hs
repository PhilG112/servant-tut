{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module NestedApis where

import Servant
import Models.User
import Data.ByteString

type UserApi99 =
    Capture "userId" Int
        :> Get '[JSON] User
    :<|>
    Capture "userId" Int
        :> DeleteNoContent

-- Lets factor out the above userId which has been duplicatedd

type UserApiRefactor = Capture "userId" Int
    :>
    (
        Get '[JSON] User
        :<|>
        DeleteNoContent
    )

-- The above however does change the type of the server

--  each handler receives the userid argument
-- Server UserApi99 =
--     (Int -> Handler User)
--     :<|>
--     (Int -> Handler NoContent)

-- The whole Server takes the userid and has handlers that are
-- just computations in Handler, with no arguments
-- Server UserApiRefactor =
--     Int -> 
--         (
--             Handler User
--             :<|>
--             Handler NoContent
--         )

-- Before the refactor
server8 :: Server UserApi99
server8 = getUser :<|> deleteUser

  where getUser :: Int -> Handler User
        getUser _userid = error "..."

        deleteUser :: Int -> Handler NoContent
        deleteUser _userid = error "..."

-- notice how getUser and deleteUser
-- have a different type! no argument anymore,
-- the argument directly goes to the whole Server
server9 :: Server UserApiRefactor
server9 userid = getUser userid :<|> deleteUser userid

  where getUser :: Int -> Handler User
        getUser = error "..."

        deleteUser :: Int -> Handler NoContent
        deleteUser = error "..."

-- More refactors

-- we just factor out the "users" path fragment
type API1 = "users" :>
  (    Get '[JSON] [User] -- user listing
  :<|> Capture "userid" Int :> Get '[JSON] User -- view a particular user
  )

-- This is what the above is without refactoring out the users
-- path fragment
type Api2 =
    "users"
        :> Get '[JSON] [User]
    :<|>
    "users"
        :> Capture "userId" Int
        :> Get '[JSON] User

-- we factor out the Request Body
type API2 = ReqBody '[JSON] User :>
  (    Get '[JSON] User -- just display the same user back, don't register it
  :<|> PostNoContent -- register the user. empty response
  )

-- we factor out a Header
type API3 = Header "Authorization" Token :>
  (    Get '[JSON] SecretData -- get some secret data, if authorized
  :<|> ReqBody '[JSON] SecretData :> PostNoContent -- add some secret data, if authorized
  )

newtype Token = Token ByteString
newtype SecretData = SecretData ByteString

