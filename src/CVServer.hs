{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module CVServer where

import Control.Monad.Trans
import Data.Text (Text)
import qualified Data.Text.Encoding as E
import Network.Wai.Handler.Warp (run)
import Servant

import Db
import DbTypes
import PrivateAuth
import CVApi

main :: IO ()
main = run 8081 ( serveWithContext cvAPI
                                   cvServerContext
                                   cvServer
                )

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
  in
      publicAPIHandler :<|> privateAPIHandler

liftIO2 :: MonadIO m => (a -> b -> IO c) -> a -> b -> m c
liftIO2 f x y = liftIO $ f x y

authCheck :: BasicAuthCheck User
authCheck =
  let check (BasicAuthData username password) =
        if (username,password) `elem` validUsers
        then return (Authorized (User $ E.decodeUtf8 username))
        else return Unauthorized
  in BasicAuthCheck check
