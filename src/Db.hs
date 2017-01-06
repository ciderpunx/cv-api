{-# LANGUAGE GADTs                      #-}

module Db where

import Data.Maybe (fromJust)
import Data.Text (Text)
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite (runSqlite, runMigration)

import DbTypes

createCV :: JsonResume -> IO (Key CV)
createCV j =
    runSqlite db $ do
      let b = basic (basics j)
          bps = profiles (basics j)
          bl  = location (basics j)
      cvKey <- insert (cv j) :: SqlPersistM (Key CV)
      basicKey <- createBasic cvKey b
      createBasicLocation basicKey bl
      mapM_ (createBasicProfile basicKey) bps
      mapM_ (createWork cvKey) (work j)
      mapM_ (createVolunteer cvKey) (volunteer j)
      mapM_ (createEducation cvKey) (education j)
      mapM_ (createAward cvKey) (awards j)
      mapM_ (createPublication cvKey) (publications j)
      mapM_ (createSkill cvKey) (skills j)
      mapM_ (createLanguage cvKey) (languages j)
      mapM_ (createInterest cvKey) (interests j)
      mapM_ (createReference cvKey) (references j)
      return cvKey

createBasic :: Key CV -> Basic -> SqlPersistM (Key Basic)
createBasic cvKey b =
      insert $ Basic
          (Just cvKey)
          (basicName b)
          (basicLabel b)
          (basicPicture b)
          (basicEmail b)
          (basicPhone b)
          (basicWebsite b)
          (basicSummary b)

createBasicProfile :: Key Basic -> BasicProfile -> SqlPersistM (Key BasicProfile)
createBasicProfile basicKey bp =
    insert $ BasicProfile
        (Just basicKey)
        (basicProfileNetwork bp)
        (basicProfileUsername bp)
        (basicProfileUrl bp)

createBasicLocation :: Key Basic -> BasicLocation -> SqlPersistM (Key BasicLocation)
createBasicLocation basicKey bl =
    insert $ BasicLocation
      (Just basicKey)
      (basicLocationAddress bl)
      (basicLocationPostalCode bl)
      (basicLocationCity bl)
      (basicLocationCountryCode bl)
      (basicLocationRegion bl)

createWork :: Key CV -> Work -> SqlPersistM (Key Work)
createWork cvKey w =
    insert $ Work
              (Just cvKey)
              (workCompany w)
              (workPosition w)
              (workWebsite w)
              (workStartDate w)
              (workEndDate w)
              (workSummary w)
              (workHighlights w)

createVolunteer :: Key CV -> Volunteer -> SqlPersistM (Key Volunteer)
createVolunteer cvKey v =
    insert $ Volunteer
              (Just cvKey)
              (volunteerOrganization v)
              (volunteerPosition v)
              (volunteerWebsite v)
              (volunteerStartDate v)
              (volunteerEndDate v)
              (volunteerSummary v)
              (volunteerHighlights v)

createEducation :: Key CV -> Education -> SqlPersistM (Key Education)
createEducation cvKey e =
    insert $ Education
              (Just cvKey)
              (educationInstitution e)
              (educationArea e)
              (educationStudyType e)
              (educationStartDate e)
              (educationEndDate e)
              (educationGpa e)
              (educationCourses e)

createAward :: Key CV -> Award -> SqlPersistM (Key Award)
createAward cvKey a =
    insert $ Award
              (Just cvKey)
              (awardTitle a)
              (awardDate a)
              (awardAwarder a)
              (awardSummary a)

createPublication :: Key CV -> Publication -> SqlPersistM (Key Publication)
createPublication cvKey p =
    insert $ Publication
              (Just cvKey)
              (publicationName p)
              (publicationPublisher p)
              (publicationReleaseDate p)
              (publicationWebsite p)
              (publicationSummary p)

createSkill :: Key CV -> Skill -> SqlPersistM (Key Skill)
createSkill cvKey l =
    insert $ Skill (Just cvKey) (skillName l) (skillLevel l) (skillKeywords l)

createLanguage :: Key CV -> Language -> SqlPersistM (Key Language)
createLanguage cvKey l =
    insert $ Language (Just cvKey) (languageLanguage l) (languageFluency l)

createInterest :: Key CV -> Interest -> SqlPersistM (Key Interest)
createInterest cvKey i =
    insert $ Interest (Just cvKey) (interestName i) (interestKeywords i)

createReference :: Key CV -> Reference -> SqlPersistM (Key Reference)
createReference cvKey r =
    insert $ Reference (Just cvKey) (referenceName r) (referenceReference r)

retrieveCV :: DbKey -> IO (Maybe JsonResume)
retrieveCV k =
    runSqlite db $ do
      let cvKey = CVKey k
      cv <- get cvKey :: SqlPersistM (Maybe CV)
      case cv of
        Nothing -> return Nothing
        Just cv -> do
          b <- selectFirst [BasicCvId ==. Just cvKey] []
          b' <- retrieveBasics b
          ws <- selectList [WorkCvId ==. Just cvKey] []
          vs <- selectList [VolunteerCvId ==. Just cvKey] []
          es <- selectList [EducationCvId ==. Just cvKey] []
          as <- selectList [AwardCvId ==. Just cvKey] []
          ps <- selectList [PublicationCvId ==. Just cvKey] []
          ss <- selectList [SkillCvId ==. Just cvKey] []
          ls <- selectList [LanguageCvId ==. Just cvKey] []
          is <- selectList [InterestCvId ==. Just cvKey] []
          rs <- selectList [ReferenceCvId ==. Just cvKey] []
          let basics       = fromJust b' -- TODO: elegant handling?
              work         = map entityVal ws
              volunteer    = map entityVal vs
              education    = map entityVal es
              awards       = map entityVal as
              publications = map entityVal ps
              skills       = map entityVal ss
              languages    = map entityVal ls
              references   = map entityVal rs
              interests    = map entityVal is
          return
            . Just
            $ JsonResume cv
                         basics
                         work
                         volunteer
                         education
                         awards
                         publications
                         skills
                         languages
                         interests
                         references

retrieveBasics :: Maybe (Entity Basic) -> SqlPersistM (Maybe Basics)
retrieveBasics b =
      case b of
        Just basic -> do
            let bid = entityKey basic
            bl <- selectFirst [BasicLocationBasicId ==. Just bid] []
            bps <- selectList [BasicProfileBasicId ==. Just bid] []
            return
              . Just
              $ Basics (entityVal basic)
                       (entityVal $ fromJust bl) -- TODO: elegant handling?
                       (map entityVal bps)
        Nothing -> return Nothing

-- TODO: Only updates CVs, not their fields
updateCV :: (DbKey, CV) -> IO ()
updateCV (k, r)  =
    runSqlite db (replace (CVKey k) r :: SqlPersistM ())

-- TODO: Only deletes CVS, not fields from them
deleteCV :: DbKey -> IO ()
deleteCV k =
    runSqlite db (delete (CVKey k) :: SqlPersistM ())

-- TODO: This should probably give us a list of the ids of all our CVs, or perhaps the
-- id and name?
listCVs :: IO [CV]
listCVs = do
    es <- runSqlite db (selectList [] [] :: SqlPersistM [Entity CV])
    return (map entityVal es)

migrateCVDb:: IO ()
migrateCVDb =
    runSqlite db $ runMigration migrateTables
