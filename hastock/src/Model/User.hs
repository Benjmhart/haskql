{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DeriveGeneric              #-}

{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}

module Model.User where

import Import
import qualified Database.Persist.TH as PTH
import Database.Persist.Sql(toSqlKey)
import Database.Persist (Entity(..))
import Data.Aeson

data UnvalidatedUser = UnvalidatedUser { name :: Text
                                       , email :: Text
                                       , password :: Text } 
                                       deriving (Generic, Show)


instance ToJSON UnvalidatedUser where
  toJSON = genericToJSON defaultOptions

instance FromJSON UnvalidatedUser where
  parseJSON = genericParseJSON defaultOptions

data LoginInfo = LoginInfo { loginEmail    :: Text
                           , loginPassword :: Text 
                           }
                           deriving (Generic, Show)


instance ToJSON LoginInfo where
  toJSON = genericToJSON defaultOptions

instance FromJSON LoginInfo where
  parseJSON = genericParseJSON defaultOptions

PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
  User sql=users
    name Text
    email Text
    password Text
    UniqueEmail email
    deriving Show
|]

instance ToJSON User where
  toJSON (User userName userEmail _) = object ["username" .= userName, "email" .= userEmail]

getUserName :: User -> Text
getUserName (User n _ _) = n

data UserResponse = UserResponse { userResponseToken :: Text
                                 , userResponseName :: Text
                                 }
                                 deriving (Generic)

instance ToJSON UserResponse where
  toJSON = genericToJSON defaultOptions

instance FromJSON UserResponse where
  parseJSON = genericParseJSON defaultOptions

-- sampleUser :: Entity User
-- sampleUser = Entity (toSqlKey 1) $ User
--     { userName = "admin"
--     , userEmail = "admin@test.com"
--     , userPassword = "password1"
--     }
