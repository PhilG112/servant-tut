{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ModularApi where

import Servant
import Models.User

-- https://docs.servant.dev/en/stable/tutorial/Server.html#nested-apis
-- Putting what we have learnt into practice from the above

type UsersApi =
    Get '[JSON] [User] -- list users
    :<|>
    ReqBody '[JSON] User
        :> PostCreated '[JSON] User -- Add a user
    :<|>
    Capture "userId" Int
        :>
            (
                Get '[JSON] User -- view a user
                :<|>
                ReqBody '[JSON] User
                    :> PutNoContent -- Update a user
                :<|>
                DeleteNoContent -- Delete a user
            )

usersServer :: Server UsersApi
usersServer = getUsers :<|> newUser :<|> userOperations

    where
        getUsers :: Handler [User]
        getUsers = error ""

        newUser :: User -> Handler User
        newUser = error ""

        userOperations userId =
            viewUser userId
            :<|> updateUser userId
            :<|> deleteUser userId

            where
                viewUser :: Int -> Handler User
                viewUser = error ""

                updateUser :: Int -> User -> Handler NoContent
                updateUser = error ""

                deleteUser :: Int -> Handler NoContent
                deleteUser = error ""

-- products api
type ProductsApi =
    Get '[JSON] [Product] -- list products
    :<|>
    ReqBody '[JSON] Product :> PostNoContent -- add a product
    :<|>
    Capture "productId" Int
        :>
         (
            Get '[JSON] Product -- view a product
            :<|>
            ReqBody '[JSON] Product :> PutNoContent -- update a product
            :<|>
            DeleteNoContent -- delete a product
         )

data Product = Product { productId :: Int }

productsServer :: Server ProductsApi
productsServer = getProducts :<|> newProduct :<|> productOperations

    where
        getProducts :: Handler [Product]
        getProducts = error "..."

        newProduct :: Product -> Handler NoContent
        newProduct = error "..."

        productOperations productId =
          viewProduct productId 
          :<|> updateProduct productId
          :<|> deleteProduct productId

            where
                viewProduct :: Int -> Handler Product
                viewProduct = error "..."

                updateProduct :: Int -> Product -> Handler NoContent
                updateProduct = error "..."

                deleteProduct :: Int -> Handler NoContent
                deleteProduct = error "..."

type CombinedApi =
    "users" :> UsersApi
    :<|>
    "products" :> ProductsApi
    
combinedServer :: Server CombinedApi
combinedServer = usersServer :<|> productsServer

-- user and product api are similar and can be abstracted...but why?
-- API for values of type 'a'
-- indexed by values of type 'i'
type APIFor a i =
    Get '[JSON] [a] -- list 'a's
    :<|>
    ReqBody '[JSON] a :> PostNoContent -- add an 'a'
    :<|>
    Capture "id" i
        :>
         (
            Get '[JSON] a -- view an 'a' given its "identifier" of type 'i'
            :<|>
            ReqBody '[JSON] a :> PutNoContent -- update an 'a'
            :<|>
            DeleteNoContent -- delete an 'a'
         )


-- Build the appropriate 'Server'
-- given the handlers of the right type.
serverFor :: Handler [a]
          -> (a -> Handler NoContent)
          -> (i -> Handler a)
          -> (i -> a -> Handler NoContent)
          -> (i -> Handler NoContent)
          -> Server (APIFor a i)
serverFor list create get update delete =
    list :<|> create :<|> (\i -> get i :<|> update i :<|> delete i)
    
server :: Server (APIFor a i)
server = serverFor listing creating getById updating deleting
    where
        listing :: Handler [a]
        listing = error ""

        creating :: a -> Handler NoContent
        creating a = error ""

        getById :: i -> Handler a
        getById id = error ""

        updating :: i -> a -> Handler NoContent
        updating i a = error ""

        deleting :: i -> Handler NoContent
        deleting i = error ""