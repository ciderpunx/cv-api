{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module DbTypes where

import Data.Aeson
import Data.Text (Text)
import Data.Time
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite (runSqlite, runMigration)
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)
import GHC.Generics
import Control.Monad (mzero)

db :: Text
db = "cv.sqlite"

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
CV json
  basics      Basic
  work        [Work] Maybe
  volunteer   [Volunteer] Maybe
  education   [Education] Maybe
  award       [Award] Maybe
  publication [Publication] Maybe
  skills      [Skill]
  languages   [Language] Maybe
  interests   [Interest] Maybe
  -- references  [Reference] Maybe
  deriving Show
Basic json
  name     Text
  label    Text
  picture  Text
  email    Text
  phone    Text
  website  Text
  summary  Text
  profiles  [BasicProfile] Maybe
  location  BasicLocatioN
  deriving Show
BasicLocatioN json
  address     Text
  postalCode  Text
  city        Text
  countryCode Text
  region      Text
  deriving Show
BasicProfile json
  network  Text
  username Text
  url      Text
  deriving Show
Work json
  company   Text
  position  Text
  website   Text
  startDate Day
  endDate   Day Maybe
  summary   Text
  highlights [Text]
  deriving Show
Volunteer json
  organization Text
  position     Text
  website      Text
  startDate    Day
  endDate      Day Maybe
  summary      Text
  highlights   [Text]
  deriving Show
Education json
  institution Text
  area        Text
  studyType   Text
  startDate   Day
  endDate     Day Maybe
  gpa         Text
  courses     [Text]
  deriving Show
Award json
  title    Text
  date     UTCTime
  awarder  Text
  summary  Text
  deriving Show
Publication json
  name        Text
  publisher   Text
  releaseDate Day
  website     Text
  summary     Text
  deriving Show
Skill json
  name     Text
  level    Text
  keywords [Text] Maybe
  deriving Show
Language json
  name  Text
  level Text
  deriving Show
Interest json
  name Text
  keywords [Text]
  deriving Show
Reference
  cvId CVId Maybe
  name Text
  reference Text
  deriving Show
|]

type DbKey = BackendKey SqlBackend

instance FromJSON Reference where
    parseJSON (Object v) = Reference <$>
                            v .:? "cvid" <*>
                            v .: "name" <*>
                            v .: "reference"
    parseJSON _          = mzero

instance ToJSON Reference where
    toJSON (Reference _ name reference) = 
      object ["name" .= name, "reference" .= reference]

data JsonResume = JsonResume { cv :: CV
                             , references :: [Reference]
                             } deriving (Show, Generic)

instance FromJSON JsonResume
instance ToJSON JsonResume
