{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module CVApi where

import Control.Lens
import Control.Monad.Trans
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite (runSqlite, runMigration)
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Client
import Servant.Docs
import qualified Network.HTTP.Client as C

import Db

-- API type
type ResumeAPI =
         "resume" :> Get '[JSON] [Resume]
    :<|> "resume" :> ReqBody '[JSON] Resume :> Put '[JSON] (Key Resume)
    :<|> "resume" :> Capture "id" DbKey     :> Get '[JSON] (Maybe Resume)
    :<|> "resume" :> ReqBody '[JSON] (DbKey, Resume)
                                            :> PostNoContent '[JSON] ()
    :<|> "resume" :> Capture "id" DbKey     :> DeleteNoContent '[JSON] ()

-- main: run our server
main :: IO ()
main = run 8081 app

-- API server implementation
resumeServer :: Server ResumeAPI
resumeServer =
       liftIO listResumes
  :<|> liftIO . createResume
  :<|> liftIO . retrieveResume
  :<|> liftIO . updateResume
  :<|> liftIO . deleteResume

app :: Application
app = serve resumeAPI resumeServer

resumeAPI :: Proxy ResumeAPI
resumeAPI = Proxy

