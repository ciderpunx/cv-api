{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module CVApi where

import Data.Text (Text)
import Servant

import DbTypes

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
    :<|>  Capture "id" CvKey :> ReqBody '[JSON] JsonResume :> PostNoContent '[JSON] ()
    :<|>  Capture "id" CvKey :> "basics" :> ReqBody '[JSON] Basics :> PostNoContent '[JSON] ()
    :<|>  Capture "id" CvKey :> "work" :> ReqBody '[JSON] [Work] :> PostNoContent '[JSON] ()
    :<|>  Capture "id" CvKey :> "volunteer" :> ReqBody '[JSON] [Volunteer] :> PostNoContent '[JSON] ()
    :<|>  Capture "id" CvKey :> "education" :> ReqBody '[JSON] [Education] :> PostNoContent '[JSON] ()
    :<|>  Capture "id" CvKey :> "awards" :> ReqBody '[JSON] [Award] :> PostNoContent '[JSON] ()
    :<|>  Capture "id" CvKey :> "publications" :> ReqBody '[JSON] [Publication] :> PostNoContent '[JSON] ()
    :<|>  Capture "id" CvKey :> "skills" :> ReqBody '[JSON] [Skill] :> PostNoContent '[JSON] ()
    :<|>  Capture "id" CvKey :> "languages" :> ReqBody '[JSON] [Language] :> PostNoContent '[JSON] ()
    :<|>  Capture "id" CvKey :> "interests" :> ReqBody '[JSON] [Interest] :> PostNoContent '[JSON] ()
    :<|>  Capture "id" CvKey :> "references" :> ReqBody '[JSON] [Reference] :> PostNoContent '[JSON] ()
    :<|>  ReqBody '[JSON] JsonResume :> Put '[JSON] CvKey
    :<|>  Capture "id" CvKey :> "basics" :> ReqBody '[JSON] Basics :> Put '[JSON] (Key Basic)
    :<|>  Capture "id" CvKey :> "work" :> ReqBody '[JSON] Work :> Put '[JSON] (Key Work)
    :<|>  Capture "id" CvKey :> "volunteer" :> ReqBody '[JSON] Volunteer :> Put '[JSON] (Key Volunteer)
    :<|>  Capture "id" CvKey :> "education" :> ReqBody '[JSON] Education :> Put '[JSON] (Key Education)
    :<|>  Capture "id" CvKey :> "awards" :> ReqBody '[JSON] Award :> Put '[JSON] (Key Award)
    :<|>  Capture "id" CvKey :> "publications" :> ReqBody '[JSON] Publication :> Put '[JSON] (Key Publication)
    :<|>  Capture "id" CvKey :> "skills" :> ReqBody '[JSON] Skill :> Put '[JSON] (Key Skill)
    :<|>  Capture "id" CvKey :> "languages" :> ReqBody '[JSON] Language :> Put '[JSON] (Key Language)
    :<|>  Capture "id" CvKey :> "interests" :> ReqBody '[JSON] Interest :> Put '[JSON] (Key Interest)
    :<|>  Capture "id" CvKey :> "references" :> ReqBody '[JSON] Reference :> Put '[JSON] (Key Reference)

type CVAPI =
          "cv" :> PublicAPI
    :<|>  "cv" :> BasicAuth "CV-API" User :> PrivateAPI

cvAPI :: Proxy CVAPI
cvAPI = Proxy
