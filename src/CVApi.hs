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
import PrivateAuth

newtype User = User { userName :: Text }
  deriving (Eq, Show)

type PublicAPI =
         Get '[JSON] [Resume]
    :<|> Capture "id" DbKey :> Get '[JSON] (Maybe Resume)

type PrivateAPI =
          Capture "id" DbKey :> DeleteNoContent '[JSON] ()
    :<|>  ReqBody '[JSON] (DbKey, Resume) :> PostNoContent '[JSON] ()
    :<|>  ReqBody '[JSON] Resume :> Put '[JSON] (Key Resume)

type ResumeAPI =
          "cv" :> PublicAPI
    :<|>  "cv" :> BasicAuth "CV-API" User :> PrivateAPI

main :: IO ()
main = run 8081 ( serveWithContext resumeAPI
                                   resumeServerContext
                                   resumeServer
                )

resumeAPI :: Proxy ResumeAPI
resumeAPI = Proxy

resumeServerContext :: Context (BasicAuthCheck User : '[])
resumeServerContext = authCheck :. EmptyContext

resumeServer :: Server ResumeAPI
resumeServer =
  let publicAPIHandler = (
             liftIO listResumes
        :<|> liftIO . retrieveResume
           )
      privateAPIHandler (user :: User) =
             liftIO . deleteResume
        :<|> liftIO . updateResume
        :<|> liftIO . createResume
  in publicAPIHandler
      :<|> privateAPIHandler

authCheck :: BasicAuthCheck User
authCheck =
  let check (BasicAuthData username password) =
        if (username,password) `elem` validUsers
        then return (Authorized (User $ E.decodeUtf8 username))
        else return Unauthorized
  in BasicAuthCheck check
