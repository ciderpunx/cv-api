{-# LANGUAGE GADTs                      #-}

module Db where

import Control.Monad.Trans (liftIO)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite (runSqlite, runMigration)

import DbTypes

createCV :: JsonResume -> IO (CvKey)
createCV j =
    runSqlite db $ do
      let b = basic (basics j)
          bps = profiles (basics j)
          bl  = location (basics j)
      cvKey <- insert (cv j) :: SqlPersistM (CvKey)
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

createBasic :: CvKey -> Basic -> SqlPersistM (Key Basic)
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

createWork :: CvKey -> Work -> SqlPersistM (Key Work)
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

createVolunteer :: CvKey -> Volunteer -> SqlPersistM (Key Volunteer)
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

createEducation :: CvKey -> Education -> SqlPersistM (Key Education)
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

createAward :: CvKey -> Award -> SqlPersistM (Key Award)
createAward cvKey a =
    insert $ Award
              (Just cvKey)
              (awardTitle a)
              (awardDate a)
              (awardAwarder a)
              (awardSummary a)

createPublication :: CvKey -> Publication -> SqlPersistM (Key Publication)
createPublication cvKey p =
    insert $ Publication
              (Just cvKey)
              (publicationName p)
              (publicationPublisher p)
              (publicationReleaseDate p)
              (publicationWebsite p)
              (publicationSummary p)

createSkill :: CvKey -> Skill -> SqlPersistM (Key Skill)
createSkill cvKey l =
    insert $ Skill (Just cvKey) (skillName l) (skillLevel l) (skillKeywords l)

createLanguage :: CvKey -> Language -> SqlPersistM (Key Language)
createLanguage cvKey l =
    insert $ Language (Just cvKey) (languageLanguage l) (languageFluency l)

createInterest :: CvKey -> Interest -> SqlPersistM (Key Interest)
createInterest cvKey i =
    insert $ Interest (Just cvKey) (interestName i) (interestKeywords i)

createReference :: CvKey -> Reference -> SqlPersistM (Key Reference)
createReference cvKey r =
    insert $ Reference (Just cvKey) (referenceName r) (referenceReference r)

retrieveCV :: CvKey -> IO (Maybe JsonResume)
retrieveCV k =
    runSqlite db $ do
      cv <- get k :: SqlPersistM (Maybe CV)
      case cv of
        Nothing -> return Nothing
        Just cv -> do
          ws <- selectList [WorkCvId ==. Just k] []
          vs <- selectList [VolunteerCvId ==. Just k] []
          es <- selectList [EducationCvId ==. Just k] []
          as <- selectList [AwardCvId ==. Just k] []
          ps <- selectList [PublicationCvId ==. Just k] []
          ss <- selectList [SkillCvId ==. Just k] []
          ls <- selectList [LanguageCvId ==. Just k] []
          is <- selectList [InterestCvId ==. Just k] []
          let work         = map entityVal ws
              volunteer    = map entityVal vs
              education    = map entityVal es
              awards       = map entityVal as
              publications = map entityVal ps
              skills       = map entityVal ss
              languages    = map entityVal ls
              interests    = map entityVal is
          basics <- liftIO $ retrieveBasics k
          references <- liftIO $ retrieveWork k
          references <- liftIO $ retrieveReferences k
          return
            . Just
            $ JsonResume cv
                         (fromJust basics) -- TODO: elegant handling?
                         work
                         volunteer
                         education
                         awards
                         publications
                         skills
                         languages
                         interests
                         references

retrieveBasics :: CvKey -> IO (Maybe Basics)
retrieveBasics k =
    runSqlite db $ do
      b <- selectFirst [BasicCvId ==. Just k] []
      case b of
        Just basic -> do
            let bid = entityKey basic
            bl <- selectFirst [BasicLocationBasicId ==. Just bid] []
            bps <- selectList [BasicProfileBasicId ==. Just bid] []
            return
              . Just
              $ Basics (entityVal basic)
                       (entityVal $ fromJust bl) -- TODO: elegant handling?
                       (map entityVal bps) :: SqlPersistM (Maybe Basics)
        Nothing -> return Nothing

retrieveWork :: CvKey -> IO [Work]
retrieveWork cvKey =
    runSqlite db $ do
      ws <- selectList [WorkCvId ==. Just cvKey] []
      return $ map entityVal ws :: SqlPersistM [Work]

retrieveVolunteer = undefined
retrieveEducation = undefined
retrieveAwards = undefined
retrievePublications = undefined
retrieveSkills = undefined
retrieveLanguages = undefined
retrieveInterests = undefined

retrieveReferences :: CvKey -> IO [Reference]
retrieveReferences cvKey =
    runSqlite db $ do
      rs <- selectList [ReferenceCvId ==. Just cvKey] []
      return $ map entityVal rs :: SqlPersistM [Reference]

-- TODO: Only updates CVs, not their fields
updateCV :: (DbKey, CV) -> IO ()
updateCV (k, r)  =
    runSqlite db (replace (CVKey k) r :: SqlPersistM ())

-- TODO: Only deletes CVS, not fields from them
deleteCV :: DbKey -> IO ()
deleteCV k =
    runSqlite db (delete (CVKey k) :: SqlPersistM ())

listCVs :: IO [CvKey]
listCVs = do
    es <- runSqlite db (selectList [] [] :: SqlPersistM [Entity CV])
    return (map entityKey es)

migrateCVDb:: IO ()
migrateCVDb =
    runSqlite db $ runMigration migrateTables
