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

import Data.Text (Text)
import Data.Time
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite (runSqlite, runMigration)
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)

db = "cv.sqlite"

-- Note that by letting persistent handle generating our types we end up with some unnecessary tables
-- But we don't have to fiddle round with writing our own instances so that basicName gets displayed 
-- as name in our output. It may be I should fix this at some point.
share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
Resume json
  basics      Basic
  work        [Work] Maybe
  volunteer   [Volunteer] Maybe
  education   [Education] Maybe
  award       [Award] Maybe
  publication [Publication] Maybe
  skills      [Skill]
  languages   [Language] Maybe
  interests   [Interest] Maybe
  references  [Reference] Maybe
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
Reference json
  name Text
  reference Text
  deriving Show
|]

type DbKey = BackendKey SqlBackend

createResume :: Resume -> IO (Key Resume)
createResume r =
    runSqlite db (insert r :: SqlPersistM (Key Resume))

retrieveResume :: DbKey -> IO (Maybe Resume)
retrieveResume k =
    runSqlite db (get (ResumeKey k) :: SqlPersistM (Maybe Resume))

updateResume :: (DbKey, Resume) -> IO ()
updateResume (k, r)  =
    runSqlite db (replace (ResumeKey k) r :: SqlPersistM ())

deleteResume :: DbKey -> IO ()
deleteResume k =
    runSqlite db (delete (ResumeKey k) :: SqlPersistM ())

listResumes :: IO [Resume]
listResumes = do
    es <- runSqlite db (selectList [] [] :: SqlPersistM [Entity Resume])
    return (map entityVal es)

migrateResumeDb:: IO ()
migrateResumeDb =
    runSqlite db $ runMigration migrateTables
