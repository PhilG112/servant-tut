{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module MyApis where

import Data.Text
import Data.Time (UTCTime)
import Servant.API 

-- Example of root endpoint
-- type RootEndpoint =
--   Get '[JSON] User

type UserApi = "users"
    :> QueryParam "sortBy" SortBy
    :> Get '[JSON] [User]
-- equivalent to 'GET /users?sortby={age, name}'

data SortBy = Age | Name

data User = User {
  name :: String,
  age :: Int,
  email :: String,
  registrationDate :: UTCTime
}

type UserAPI3 =
  "users"
    :> "list-all"
    :> "now"
    :> Get '[JSON] [User]
    -- describes an endpoint reachable at:
    -- /users/list-all/now

type UserApiCapture =
  "user"
    :> Capture "userId" Integer
    :> Get '[JSON] User
  :<|> -- This combinator is used to define multiple endpoints in the one type
  "user"
    :> Capture "userId" Integer
    :> DeleteNoContent -- 204

type UserAPI7 =
  "users"
    :> ReqBody '[JSON] User
    :> Post '[JSON] User
  -- - equivalent to 'POST /users' with a JSON object
  --   describing a User in the request body
  -- - returns a User encoded in JSON
  :<|>
  "users"
    :> Capture "userid" Integer
    :> ReqBody '[JSON] User
    :> Put '[JSON] User
  -- - equivalent to 'PUT /users/:userid' with a JSON
  --   object describing a User in the request body
  -- - returns a User encoded in JSON

-- Headers
type UserAPI8 =
  "users"
    :> Header "User-Agent" Text
    :> Get '[JSON] [User]

-- Content types
type UserAPI9 =
  "users"
    :> Get '[JSON, PlainText, FormUrlEncoded, OctetStream] [User]

-- Response headers
type UserAPI10 =
  "users"
    :> Get '[JSON] (Headers '[Header "User-Count" Integer] [User])

-- Basic Auth
type ProtectedAPI11 =
  UserApi -- this is public
  :<|>
  BasicAuth "my-realm" User
    :> UserAPI8 -- this is protected by auth

-- Empty APIs
type UserAPI12 innerAPI = 
  UserAPI8            -- this is the fixed bit of the API
  :<|> 
  "inner"
    :> innerAPI  -- this lets us put various other APIs under /inner

-- INterop with wai and Raw
type UserAPI15 =
  "files"
    :> Raw
                 -- The raw endpoint is under the /files
                 -- static path, so it won't match /users.
  :<|>
  "users"
    :> Get '[JSON] [User]

type UserAPI16 = "users" :> Get '[JSON] [User]
            :<|> Raw
                 -- The Raw endpoint is matched last, so
                 -- it won't overlap another endpoint.