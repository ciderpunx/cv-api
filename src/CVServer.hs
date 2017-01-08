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

module CVServer where

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
import CVApi

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
  let publicAPIHandler =
        (    liftIO listCVs
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
        :<|> liftIO2 createWorkIO
        :<|> liftIO2 createVolunteerIO
        :<|> liftIO2 createEducationIO
        :<|> liftIO2 createAwardIO
        :<|> liftIO2 createPublicationIO
        :<|> liftIO2 createSkillIO
        :<|> liftIO2 createLanguageIO
        :<|> liftIO2 createInterestIO
        :<|> liftIO2 createReferenceIO
  in publicAPIHandler :<|> privateAPIHandler

liftIO2 :: MonadIO m => (a -> b -> IO c) -> a -> b -> m c
liftIO2 f x y = liftIO $ f x y

authCheck :: BasicAuthCheck User
authCheck =
  let check (BasicAuthData username password) =
        if (username,password) `elem` validUsers
        then return (Authorized (User $ E.decodeUtf8 username))
        else return Unauthorized
  in BasicAuthCheck check

