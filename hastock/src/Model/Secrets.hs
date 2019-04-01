{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE ViewPatterns           #-}
{-# LANGUAGE DeriveGeneric          #-}

module Model.Secrets (Secrets(..), Error(..), apiKey) where

import Import

import qualified Data.Aeson             as A

data Secrets = Secrets { apiKey  :: Text
                       , secret1 :: Text
                       , secret2 :: Text
                       } deriving (Show, Generic)

instance FromJSON Secrets where
parseJSON = A.withObject "Secrets" $ \v -> Secrets
    <$> v .: "apiKey"
    <*> v .: "secret1"
    <*> v .: "secret2"

data Error = Error  { message :: Text
                    , statusCode :: Int
                    } deriving Generic

instance ToJSON Error where
toJSON (Error m sC) = object ["message" .= m, "statusCode" .= sC]
                    