{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}

module CVApi where

import Control.Monad.Trans
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite (runSqlite, runMigration)
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)
import Data.Text (Text)
import qualified Data.Text.Encoding as E
import GHC.Generics (Generic)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.API.BasicAuth (BasicAuthData (BasicAuthData))
import Servant.API.Experimental.Auth (AuthProtect)
import Servant.Client
import Servant.Docs
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)
import Servant.Server.Experimental.Auth()
import qualified Network.HTTP.Client as C

import Db
import DbTypes
import PrivateAuth

newtype User = User { userName :: Text }
  deriving (Eq, Show)

type PublicAPI =
         Get '[JSON] [CvKey]
    :<|> Capture "id" CvKey :> Get '[JSON] (Maybe JsonResume)
    :<|> Capture "id" CvKey :> "basics" :> Get '[JSON] (Maybe Basics)
    :<|> Capture "id" CvKey :> "work" :> Get '[JSON] [Work]
    :<|> Capture "id" CvKey :> "volunteer" :> Get '[JSON] [Volunteer]
    :<|> Capture "id" CvKey :> "education" :> Get '[JSON] [Education]
    :<|> Capture "id" CvKey :> "awards" :> Get '[JSON] [Award]
    :<|> Capture "id" CvKey :> "publications" :> Get '[JSON] [Publication]
    :<|> Capture "id" CvKey :> "skills" :> Get '[JSON] [Skill]
    :<|> Capture "id" CvKey :> "languages" :> Get '[JSON] [Language]
    :<|> Capture "id" CvKey :> "interests" :> Get '[JSON] [Interest]
    :<|> Capture "id" CvKey :> "references" :> Get '[JSON] [Reference]

type PrivateAPI =
          Capture "id" CvKey :> DeleteNoContent '[JSON] ()
    :<|>  ReqBody '[JSON] (CvKey, CV) :> PostNoContent '[JSON] ()
    :<|>  ReqBody '[JSON] JsonResume :> Put '[JSON] CvKey
    :<|>  Capture "id" CvKey :> "basics" :> ReqBody '[JSON] Basics :> Put '[JSON] (Key Basic)

type CVAPI =
          "cv" :> PublicAPI
    :<|>  "cv" :> BasicAuth "CV-API" User :> PrivateAPI

main :: IO ()
main = run 8081 ( serveWithContext cvAPI
                                   cvServerContext
                                   cvServer
                )

cvAPI :: Proxy CVAPI
cvAPI = Proxy

cvServerContext :: Context (BasicAuthCheck User : '[])
cvServerContext = authCheck :. EmptyContext

cvServer :: Server CVAPI
cvServer =
  let publicAPIHandler = (
             liftIO listCVs
        :<|> liftIO . retrieveCV
        :<|> liftIO . retrieveBasics
        :<|> liftIO . retrieveWork
        :<|> liftIO . retrieveVolunteer
        :<|> liftIO . retrieveEducation
        :<|> liftIO . retrieveAwards
        :<|> liftIO . retrievePublications
        :<|> liftIO . retrieveSkills
        :<|> liftIO . retrieveLanguages
        :<|> liftIO . retrieveInterests
        :<|> liftIO . retrieveReferences
           )
      privateAPIHandler (user :: User) =
             liftIO . deleteCV
        :<|> liftIO . updateCV
        :<|> liftIO . createCV
        :<|> liftIO2 createBasicsIO
  in publicAPIHandler
      :<|> privateAPIHandler

liftIO2 :: MonadIO m => (a -> b -> IO c) -> a -> b -> m c
liftIO2 f x y = liftIO $ f x y

authCheck :: BasicAuthCheck User
authCheck =
  let check (BasicAuthData username password) =
        if (username,password) `elem` validUsers
        then return (Authorized (User $ E.decodeUtf8 username))
        else return Unauthorized
  in BasicAuthCheck check
