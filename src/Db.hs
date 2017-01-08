{-# LANGUAGE GADTs            #-}
{-# LANGUAGE FlexibleContexts #-}

module Db where

import Control.Monad.Trans (liftIO)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite (runSqlite, runMigration)

import DbTypes

createCV :: JsonResume -> IO CvKey
createCV j =
    runSqlite db $ do
      cvKey <- insert (cv j) :: SqlPersistM CvKey
      createBasics cvKey              $ basics j
      mapM_ (createWork cvKey)        $ work j
      mapM_ (createVolunteer cvKey)   $ volunteer j
      mapM_ (createEducation cvKey)   $ education j
      mapM_ (createAward cvKey)       $ awards j
      mapM_ (createPublication cvKey) $ publications j
      mapM_ (createSkill cvKey)       $ skills j
      mapM_ (createLanguage cvKey)    $ languages j
      mapM_ (createInterest cvKey)    $ interests j
      mapM_ (createReference cvKey)   $ references j
      return cvKey


-- TODO: Decsion needed: If a basics object already exists do we:
-- fail?
-- silently replace existing object?
-- silently ignore request to create new object?
createBasics :: CvKey -> Basics -> SqlPersistM (Key Basic)
createBasics cvKey b = do
      basicKey <- createBasic cvKey $ basic b
      createBasicLocation basicKey $ location b
      mapM_ (createBasicProfile basicKey) $ profiles b
      return basicKey

-- Fucking annoyingly, we can't use a single, IO producing function
-- to create our records (in createCV) without hitting this error, due to thread contention I think:
-- SQLite3 returned ErrorBusy while attempting to perform step
-- So we write a dumb wrapper for each action thus
createBasicsIO :: CvKey -> Basics -> IO (Key Basic)
createBasicsIO cvKey b = runSqlite db $ createBasics cvKey b

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

createWorkIO :: CvKey -> Work -> IO (Key Work)
createWorkIO cvKey w = runSqlite db $ createWork cvKey w

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

createVolunteerIO :: CvKey -> Volunteer -> IO (Key Volunteer)
createVolunteerIO cvKey w = runSqlite db $ createVolunteer cvKey w

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

createEducationIO :: CvKey -> Education -> IO (Key Education)
createEducationIO cvKey w = runSqlite db $ createEducation cvKey w

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

createAwardIO :: CvKey -> Award -> IO (Key Award)
createAwardIO cvKey w = runSqlite db $ createAward cvKey w

createAward :: CvKey -> Award -> SqlPersistM (Key Award)
createAward cvKey a =
    insert $ Award
              (Just cvKey)
              (awardTitle a)
              (awardDate a)
              (awardAwarder a)
              (awardSummary a)

createPublicationIO :: CvKey -> Publication -> IO (Key Publication)
createPublicationIO cvKey w = runSqlite db $ createPublication cvKey w

createPublication :: CvKey -> Publication -> SqlPersistM (Key Publication)
createPublication cvKey p =
    insert $ Publication
              (Just cvKey)
              (publicationName p)
              (publicationPublisher p)
              (publicationReleaseDate p)
              (publicationWebsite p)
              (publicationSummary p)

createSkillIO :: CvKey -> Skill -> IO (Key Skill)
createSkillIO cvKey w = runSqlite db $ createSkill cvKey w

createSkill :: CvKey -> Skill -> SqlPersistM (Key Skill)
createSkill cvKey l =
    insert $ Skill (Just cvKey) (skillName l) (skillLevel l) (skillKeywords l)

createLanguageIO :: CvKey -> Language -> IO (Key Language)
createLanguageIO cvKey w = runSqlite db $ createLanguage cvKey w

createLanguage :: CvKey -> Language -> SqlPersistM (Key Language)
createLanguage cvKey l =
    insert $ Language (Just cvKey) (languageLanguage l) (languageFluency l)

createInterestIO :: CvKey -> Interest -> IO (Key Interest)
createInterestIO cvKey w = runSqlite db $ createInterest cvKey w

createInterest :: CvKey -> Interest -> SqlPersistM (Key Interest)
createInterest cvKey i =
    insert $ Interest (Just cvKey) (interestName i) (interestKeywords i)

createReferenceIO :: CvKey -> Reference -> IO (Key Reference)
createReferenceIO cvKey w = runSqlite db $ createReference cvKey w

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
          basics <- liftIO $ retrieveBasics k
          work <- liftIO $ retrieveWork k
          volunteer <- liftIO $ retrieveVolunteer k
          education <- liftIO $ retrieveEducation k
          awards <- liftIO $ retrieveAwards k
          publications <- liftIO $ retrievePublications k
          skills <- liftIO $ retrieveSkills k
          languages <- liftIO $ retrieveLanguages k
          interests <- liftIO $ retrieveInterests k
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

retrieveVolunteer :: CvKey -> IO [Volunteer]
retrieveVolunteer cvKey =
    runSqlite db $ do
      vs <- selectList [VolunteerCvId ==. Just cvKey] []
      return $ map entityVal vs :: SqlPersistM [Volunteer]

retrieveEducation :: CvKey -> IO [Education]
retrieveEducation cvKey =
    runSqlite db $ do
      es <- selectList [EducationCvId ==. Just cvKey] []
      return $ map entityVal es :: SqlPersistM [Education]

retrieveAwards :: CvKey -> IO [Award]
retrieveAwards cvKey =
    runSqlite db $ do
      as <- selectList [AwardCvId ==. Just cvKey] []
      return $ map entityVal as :: SqlPersistM [Award]

retrievePublications :: CvKey -> IO [Publication]
retrievePublications cvKey =
    runSqlite db $ do
      as <- selectList [PublicationCvId ==. Just cvKey] []
      return $ map entityVal as :: SqlPersistM [Publication]

retrieveSkills :: CvKey -> IO [Skill]
retrieveSkills cvKey =
    runSqlite db $ do
      as <- selectList [SkillCvId ==. Just cvKey] []
      return $ map entityVal as :: SqlPersistM [Skill]

retrieveLanguages :: CvKey -> IO [Language]
retrieveLanguages cvKey =
    runSqlite db $ do
      as <- selectList [LanguageCvId ==. Just cvKey] []
      return $ map entityVal as :: SqlPersistM [Language]

retrieveInterests :: CvKey -> IO [Interest]
retrieveInterests cvKey =
    runSqlite db $ do
      as <- selectList [InterestCvId ==. Just cvKey] []
      return $ map entityVal as :: SqlPersistM [Interest]

retrieveReferences :: CvKey -> IO [Reference]
retrieveReferences cvKey =
    runSqlite db $ do
      rs <- selectList [ReferenceCvId ==. Just cvKey] []
      return $ map entityVal rs :: SqlPersistM [Reference]

-- TODO: Only updates CVs, not their fields
updateCV :: (CvKey, CV) -> IO ()
updateCV (k, r)  =
    runSqlite db (replace k r :: SqlPersistM ())

-- TODO: Only deletes CVS, not fields from them
deleteCV :: CvKey -> IO ()
deleteCV k =
    runSqlite db (delete k :: SqlPersistM ())

listCVs :: IO [CvKey]
listCVs = do
    es <- runSqlite db (selectList [] [] :: SqlPersistM [Entity CV])
    return (map entityKey es)

migrateCVDb:: IO ()
migrateCVDb =
    runSqlite db $ runMigration migrateTables
