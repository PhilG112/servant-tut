-- https://docs.servant.dev/en/stable/tutorial/Server.html#from-combinators-to-handler-arguments
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module BiggerApi where
import Servant
import GHC.Generics
import Data.Aeson
import Data.List ( intercalate )

-- Setting up endpoints
type Api =
    "position"
        :> Capture "x" Int
        :> Capture "y" Int
        :> Get '[JSON] Position
    :<|>
    "hello"
        :> QueryParam "name" String
        :> Get '[JSON] HelloMessage
    :<|>
    "marketing"
        :> ReqBody '[JSON] ClientInfo
        :> Post '[JSON] Email

-- Setting up models
data Position = Position
    { xCoord :: Int 
    , yCoord :: Int
    } deriving Generic

instance ToJSON Position

newtype HelloMessage = HelloMessage { msg :: String }
    deriving Generic

instance ToJSON HelloMessage

data ClientInfo = ClientInfo
    { clientName :: String
    , clientEmail :: String
    , clientAge :: Int
    , clientInterests :: [String]
    } deriving Generic

instance ToJSON ClientInfo
instance FromJSON ClientInfo

data Email = Email
    { from :: String
    , to :: String
    , subject :: String
    , body :: String
    } deriving Generic

instance ToJSON Email

-- Setting up email response
emailForClient :: ClientInfo -> Email
emailForClient clientInfo = Email from' to' subject' body'
    
    where from' = "great@company.com"
          to' = clientEmail clientInfo
          subject' = "Hey " ++ clientName clientInfo ++ ", Hi"
          body' = "Hi " ++ clientName clientInfo ++ ",\n\n"
                ++ "Since you've recently turned " ++ show (clientAge clientInfo)
                ++ ", have you checked out our latest "
                ++ intercalate ", " (clientInterests clientInfo)
                ++ " products? Give us a visit!"

-- Setup handlers/server

biggerServer :: Server Api
biggerServer = position
    :<|> hello
    :<|> marketing

    where
        -- The params in these methods map to the params defined
        -- in the routes above
        position :: Int -> Int -> Handler Position
        position x y = return (Position x y)

        hello :: Maybe String -> Handler HelloMessage
        hello ms = return . HelloMessage $ case ms of
            Nothing -> "Coward"
            Just s -> "Hello, " ++ s

        marketing :: ClientInfo -> Handler Email
        marketing ci = return (emailForClient ci)

biggerApi :: Proxy Api
biggerApi = Proxy