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

import Control.Monad.Trans
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Aeson
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite (runSqlite, runMigration)
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)
import GHC.Generics

-- http://www.yesodweb.com/book/persistent#persistent_custom_fields <- custom fields for URLs, emails and such

db = "cv.sqlite"

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
Resume json
  title Text
  basics Basic
Basic json
  name     Text
  label    Text
  picture  Text
  email    Text
  phone    Text
  website  Text
  summary  Text
  profiles [BasicProfile]
  location BasicLocatioN Maybe
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

listResumes = undefined -- and will remain so for now

createBasic = undefined
retrieveBasic = undefined
updateBasic = undefined
deleteBasic = undefined
listBasics = undefined

createBasicLocation = undefined
retrieveBasicLocation = undefined
updateBasicLocation = undefined
deleteBasicLocation = undefined
listBasicLocations = undefined

createBasicProfile = undefined
retrieveBasicProfile = undefined
updateBasicProfile = undefined
deleteBasicProfile = undefined
listBasicProfiles = undefined

createWork = undefined
retrieveWork = undefined
updateWork = undefined
deleteWork = undefined
listWorks = undefined

createWorkHighlight = undefined
retrieveWorkHighlight = undefined
updateWorkHighlight = undefined
deleteWorkHighlight = undefined
listWorkHighlights = undefined

createVolunteer = undefined
retrieveVolunteer = undefined
updateVolunteer = undefined
deleteVolunteer = undefined
listVolunteers = undefined

createVolunteerHighlight = undefined
retrieveVolunteerHighlight = undefined
updateVolunteerHighlight = undefined
deleteVolunteerHighlight = undefined
listVolunteerHighlights = undefined

createEducation = undefined
retrieveEducation = undefined
updateEducation = undefined
deleteEducation = undefined
listEducations = undefined

createEducationCourse = undefined
retrieveEducationCourse = undefined
updateEducationCourse = undefined
deleteEducationCourse = undefined
listEducationCourses = undefined

createAward = undefined
retrieveAward = undefined
updateAward = undefined
deleteAward = undefined
listAwards = undefined

createPublication = undefined
retrievePublication = undefined
updatePublication = undefined
deletePublication = undefined
listPublications = undefined

createSkill = undefined
retrieveSkill = undefined
updateSkill = undefined
deleteSkill = undefined
listSkills = undefined

createSkillKeyword = undefined
retrieveSkillKeyword = undefined
updateSkillKeyword = undefined
deleteSkillKeyword = undefined
listSkillKeywords = undefined

createLanguage = undefined
retrieveLanguage = undefined
updateLanguage = undefined
deleteLanguage = undefined
listLanguages = undefined

createInterest = undefined
retrieveInterest = undefined
updateInterest = undefined
deleteInterest = undefined
listInterests = undefined

createInterestKeyword = undefined
retrieveInterestKeyword = undefined
updateInterestKeyword = undefined
deleteInterestKeyword = undefined
listInterestKeywords = undefined

createReference = undefined
retrieveReference = undefined
updateReference = undefined
deleteReference = undefined
listReferences = undefined

migrateResumeDb:: IO ()
migrateResumeDb =
    runSqlite db $ runMigration migrateTables
