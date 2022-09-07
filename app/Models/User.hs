{-# LANGUAGE DeriveGeneric #-}

module Models.User where

import Data.Time
import GHC.Generics
import Data.Aeson

data User = User {
    name :: String,
    age :: Int,
    email :: String,
    registrationDate :: Day
} deriving (Eq, Show, Generic)

instance ToJSON User