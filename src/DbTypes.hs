{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module DbTypes where

import Data.Aeson
import Data.Text (Text)
import Data.Time (Day, UTCTime)
import Database.Persist
import Database.Persist.Sql
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)
import GHC.Generics
import Control.Monad (mzero)

db :: Text
db = "cv.sqlite"

-- TODO support meta like this:
-- "{
--  "meta": {
--    "canonical": "https://raw.githubusercontent.com/jsonresume/resume-schema/master/resume.json",
--    "version": "v1.0.0",
--    "lastModified": "2017-12-24T15:53:00"
--  },

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
CV json
  deriving Show
Basic
  cvId     CVId Maybe
  name     Text
  label    Text
  picture  Text
  email    Text
  phone    Text
  website  Text
  summary  Text
  deriving Show
BasicLocation
  basicId     BasicId Maybe
  address     Text
  postalCode  Text
  city        Text
  countryCode Text
  region      Text
  deriving Show
BasicProfile
  basicId  BasicId Maybe
  network  Text
  username Text
  url      Text
  deriving Show
Work
  cvId      CVId Maybe
  company   Text
  position  Text
  website   Text
  startDate Day
  endDate   Day Maybe
  summary   Text
  highlights [Text]
  deriving Show
Volunteer
  cvId         CVId Maybe
  organization Text
  position     Text
  website      Text
  startDate    Day
  endDate      Day Maybe
  summary      Text
  highlights   [Text]
  deriving Show
Education
  cvId        CVId Maybe
  institution Text
  area        Text
  studyType   Text
  startDate   Day
  endDate     Day Maybe
  gpa         Text
  courses     [Text]
  deriving Show
Award
  cvId     CVId Maybe
  title    Text
  date     Day
  awarder  Text
  summary  Text
  deriving Show
Publication
  cvId        CVId Maybe
  name        Text
  publisher   Text
  releaseDate Day
  website     Text
  summary     Text
  deriving Show
Skill
  cvId     CVId Maybe
  name     Text
  level    Text
  keywords [Text] Maybe
  deriving Show
Language
  cvId CVId Maybe
  language  Text
  fluency Text
  deriving Show
Interest
  cvId CVId Maybe
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

instance FromJSON Basic where
    parseJSON (Object v) = Basic <$>
                            v .:? "cvid"   <*>
                            v .: "name"    <*>
                            v .: "label"   <*>
                            v .: "picture" <*>
                            v .: "email"   <*>
                            v .: "phone"   <*>
                            v .: "website" <*>
                            v .: "summary"
    parseJSON _          = mzero

instance ToJSON Basic where
    toJSON (Basic _ name label picture email phone website summary) =
      object [ "name"     .= name
             , "label"    .= label
             , "picture"  .= picture
             , "email"    .= email
             , "phone"    .= phone
             , "website"  .= website
             , "summary"  .= summary
             ]

instance FromJSON BasicLocation where
    parseJSON (Object v) = BasicLocation <$>
                            v .:? "basicid"    <*>
                            v .: "address"     <*>
                            v .: "postalCode"  <*>
                            v .: "city"        <*>
                            v .: "countryCode" <*>
                            v .: "region"
    parseJSON _          = mzero

instance ToJSON BasicLocation where
    toJSON (BasicLocation _ address postalCode city countryCode region) =
      object [ "address"      .= address
             , "postalCode"   .= postalCode
             , "city"         .= city
             , "countryCode"  .= countryCode
             , "region"       .= region
             ]

instance FromJSON BasicProfile where
    parseJSON (Object v) = BasicProfile <$>
                            v .:? "basicid" <*>
                            v .: "network"  <*>
                            v .: "username" <*>
                            v .: "url"
    parseJSON _          = mzero

instance ToJSON BasicProfile where
    toJSON (BasicProfile _ network username url) =
      object [ "network"  .= network
             , "username" .= username
             , "url"      .= url
             ]

instance FromJSON Work where
    parseJSON (Object v) = Work <$>
                            v .:? "cvid"        <*>
                            v .: "company" <*>
                            v .: "position"     <*>
                            v .: "website"      <*>
                            v .: "startDate"    <*>
                            v .:? "endDate"     <*>
                            v .: "summary"      <*>
                            v .: "highlights"
    parseJSON _          = mzero

instance ToJSON Work where
    toJSON (Work _ company position website startDate endDate summary highlights) =
      object [ "company" .= company
             , "position"     .= position
             , "website"      .= website
             , "startDate"    .= startDate
             , "endDate"      .= endDate
             , "summary"      .= summary
             , "highlights"   .= highlights
             ]

instance FromJSON Volunteer where
    parseJSON (Object v) = Volunteer <$>
                            v .:? "cvid"        <*>
                            v .: "organization" <*>
                            v .: "position"     <*>
                            v .: "website"      <*>
                            v .: "startDate"    <*>
                            v .:? "endDate"     <*>
                            v .: "summary"      <*>
                            v .: "highlights"
    parseJSON _          = mzero

instance ToJSON Volunteer where
    toJSON (Volunteer _ organization position website startDate endDate summary highlights) =
      object [ "organization" .= organization
             , "position"     .= position
             , "website"      .= website
             , "startDate"    .= startDate
             , "endDate"      .= endDate
             , "summary"      .= summary
             , "highlights"   .= highlights
             ]

instance FromJSON Education where
    parseJSON (Object v) = Education <$>
                            v .:? "cvid"       <*>
                            v .: "institution" <*>
                            v .: "area"        <*>
                            v .: "studyType"   <*>
                            v .: "startDate"   <*>
                            v .:? "endDate"    <*>
                            v .: "gpa"         <*>
                            v .: "courses"
    parseJSON _          = mzero

instance ToJSON Education where
    toJSON (Education _ institution area studyType startDate endDate gpa courses) =
      object [ "institution" .= institution
             , "area"        .= area
             , "studyType"   .= studyType
             , "startDate"   .= startDate
             , "endDate"     .= endDate
             , "gpa"         .= gpa
             , "courses"     .= courses
             ]

instance FromJSON Award where
    parseJSON (Object v) = Award <$>
                            v .:? "cvid"   <*>
                            v .: "title"   <*>
                            v .: "date"    <*>
                            v .: "awarder" <*>
                            v .: "summary"
    parseJSON _          = mzero

instance ToJSON Award where
    toJSON (Award _ title date awarder summary) =
      object [ "title"   .= title
             , "date"    .= date
             , "awarder" .= awarder
             , "summary" .= summary
             ]

instance FromJSON Publication where
    parseJSON (Object v) = Publication <$>
                            v .:? "cvid"       <*>
                            v .: "name"        <*>
                            v .: "publisher"   <*>
                            v .: "releaseDate" <*>
                            v .: "website"     <*>
                            v .: "summary"
    parseJSON _          = mzero

instance ToJSON Publication where
    toJSON (Publication _ name publisher releaseDate website summary) =
      object [ "name"        .= name
             , "publisher"   .= publisher
             , "releaseDate" .= releaseDate
             , "website"     .= website
             , "summary"     .= summary
             ]

instance FromJSON Skill where
    parseJSON (Object v) = Skill <$>
                            v .:? "cvid" <*>
                            v .: "name"  <*>
                            v .: "level" <*>
                            v .:? "keywords"
    parseJSON _          = mzero

instance ToJSON Skill where
    toJSON (Skill _ name level keywords) =
      object [ "name"     .= name
             , "level"    .= level
             , "keywords" .= keywords
             ]

instance FromJSON Language where
    parseJSON (Object v) = Language <$>
                            v .:? "cvid" <*>
                            v .: "language"  <*>
                            v .: "fluency"
    parseJSON _          = mzero

instance ToJSON Language where
    toJSON (Language _ language fluency) =
      object [ "language" .= language, "fluency" .= fluency ]

instance FromJSON Interest where
    parseJSON (Object v) = Interest <$>
                            v .:? "cvid" <*>
                            v .: "name"  <*>
                            v .: "keywords"
    parseJSON _          = mzero

instance ToJSON Interest where
    toJSON (Interest _ name keywords) =
      object [ "name" .= name, "keywords" .= keywords ]

instance FromJSON Reference where
    parseJSON (Object v) = Reference <$>
                            v .:? "cvid" <*>
                            v .: "name"  <*>
                            v .: "reference"
    parseJSON _          = mzero

instance ToJSON Reference where
    toJSON (Reference _ name reference) =
      object [ "name" .= name, "reference" .= reference ]

data Basics = Basics { basic :: Basic
                     , location :: BasicLocation
                     , profiles :: [BasicProfile]
                     } deriving (Show, Generic)

instance FromJSON Basics where
    parseJSON = withObject "basics" $ \o -> do
      cvid     <- o .:? "cvid"
      name     <- o .:  "name"
      label    <- o .:  "label"
      picture  <- o .:  "picture"
      email    <- o .:  "email"
      phone    <- o .:  "phone"
      website  <- o .:  "website"
      summary  <- o .:  "summary"
      location <- o .:  "location"
      profiles <- o .:  "profiles"
      return $ Basics (Basic cvid name label picture email phone website summary) location profiles

instance ToJSON Basics where
    toJSON (Basics (Basic _ name label picture email phone website summary) location profiles) =
      object [ "name"     .= name
             , "label"    .= label
             , "picture"  .= picture
             , "email"    .= email
             , "phone"    .= phone
             , "website"  .= website
             , "summary"  .= summary
             , "location" .= location
             , "profiles" .= profiles
             ]

data JsonResume = JsonResume { cv :: CV
                             , basics       :: Basics
                             , work         :: [Work]
                             , volunteer    :: [Volunteer]
                             , education    :: [Education]
                             , awards       :: [Award]
                             , publications :: [Publication]
                             , skills       :: [Skill]
                             , languages    :: [Language]
                             , interests    :: [Interest]
                             , references   :: [Reference]
                             } deriving (Show, Generic)

instance FromJSON JsonResume
instance ToJSON JsonResume
