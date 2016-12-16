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

module Db where

import Data.Aeson
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite (runSqlite, runMigration)
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)
import GHC.Generics

db = "cv.sqlite"

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
Resume json
  title Text
Basic json
  resumeId ResumeId
  name     Text
  label    Text
  picture  Text
  email    Text
  phone    Text
  website  Text
  summary  Text
  deriving Show
BasicLocation json
  basicId     BasicId
  address     Text
  postalCode  Text
  city        Text
  countryCode Text
  region      Text
  deriving Show
BasicProfile json
  basicId  BasicId
  network  Text
  profile  Text
  url      Text
  deriving Show
Work json
  resumeId  ResumeId
  company   Text
  position  Text
  website   Text
  startDate UTCTime
  endDate   UTCTime Maybe
  summary   Text
  deriving Show
WorkHighlight json
  workId WorkId
  text   Text
  deriving Show
Volunteer json
  resumeId     ResumeId
  organization Text
  position     Text
  website      Text
  startDate    UTCTime
  endDate      UTCTime Maybe
  summary      Text
  deriving Show
VolunteerHighlight json
  volunteerId VolunteerId
  text        Text
  deriving Show
Education json
  resumeId    ResumeId
  institution Text
  area        Text
  studyType   Text
  startDate   UTCTime
  endDate     UTCTime Maybe
  gpa         Text
  deriving Show
EducationCourse json
  educationId EducationId
  text        Text
  deriving Show
Award json
  resumeId ResumeId
  title    Text
  date     UTCTime
  awarder  Text
  summary  Text
  deriving Show
Publication json
  resumeId    ResumeId
  name        Text
  publisher   Text
  releaseDate UTCTime
  website     Text
  summary     Text
  deriving Show
Skill json
  resumeId ResumeId
  name     Text
  level    Text
  deriving Show
SkillKeyword json
  skillId SkillId
  keyword Text
  deriving Show
Language json
  name Text
  level Text
  deriving Show
Interest json
  resumeId ResumeId
  name Text
  deriving Show
InterestKeyword json
  interestId InterestId
  keyword Text
  deriving Show
Reference json
  resumeId ResumeId
  name Text
  reference Text
  deriving Show
|]

type DbKey = BackendKey SqlBackend

migrateResumeDb:: IO ()
migrateResumeDb =
    runSqlite db $ runMigration migrateTables
