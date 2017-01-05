module Db where

import Data.Text (Text)
import Data.Time
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite (runSqlite, runMigration)

import DbTypes

createCV :: JsonResume -> IO (Key CV)
createCV j =
    runSqlite db $ do
      let
          is = interests j
          rs = references j
      cvKey <- insert (cv j) :: SqlPersistM (Key CV)
      mapM_ (createReference cvKey) rs
      mapM_ (createInterest cvKey) is
      return cvKey

createReference :: Key CV -> Reference -> SqlPersistM (Key Reference)
createReference cvKey r =
    insert $ Reference (Just cvKey) (referenceName r) (referenceReference r)

createInterest :: Key CV -> Interest -> SqlPersistM (Key Interest)
createInterest cvKey i =
    insert $ Interest (Just cvKey) (interestName i) (interestKeywords i)

retrieveCV :: DbKey -> IO (Maybe JsonResume)
retrieveCV k =
    runSqlite db $ do
      let cvKey = CVKey k
      cv <- get cvKey :: SqlPersistM (Maybe CV)
      case cv of
        Nothing -> return Nothing
        Just cv -> do
          is <- selectList [InterestCvId ==. Just cvKey] []
          rs <- selectList [ReferenceCvId ==. Just cvKey] []
          let references = map entityVal rs
              interests  = map entityVal is
          return . Just
            $ JsonResume cv interests references 

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
