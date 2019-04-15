{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

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


PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
  User sql=users
    name Text
    email Text
    password Text
    UniqueEmail email
    deriving Show Read
|]

sampleUser :: Entity User
sampleUser = Entity (toSqlKey 1) $ User
    { userName = "admin"
    , userEmail = "admin@test.com"
    , userPassword = "password1"
    }
