{-# LANGUAGE DataKinds #-}

-- Allow automatic deriving of instances for the Generic typeclass.
{-# LANGUAGE DeriveGeneric #-}

-- Allow a type parameter to occur twice within a type class instance.
{-# LANGUAGE FlexibleInstances #-}

-- https://downloads.haskell.org/~ghc/8.10.3/docs/html/users_guide/glasgow_exts.html#extension-GeneralizedNewtypeDeriving
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

-- https://wiki.haskell.org/Rank-N_types
{-# LANGUAGE RankNTypes #-}

-- https://wiki.haskell.org/Scoped_type_variables
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server (app1, app2, app3) where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import Servant.Types.SourceT (source)
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html

import UsersApi ( server1, userApi, server2, userApi2 )
import BiggerApi ( biggerApi, biggerServer )

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.

-- app1
app1 :: Application
app1 = serve userApi server1

-- app2
app2 :: Application
app2 = serve userApi2 server2

-- app3
app3 :: Application
app3 = serve biggerApi biggerServer