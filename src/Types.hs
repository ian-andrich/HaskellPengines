module Types where

import qualified    Data.HashMap                        as HM
import qualified    Data.Vector                         as V
import              Data.Aeson                          as Aes
import              Data.Aeson.Types
import              Data.Scientific                     (Scientific)
import              Data.Maybe                          (fromJust)

{- There are three parts to this file.
 - 1: HeaderBase data, and a headerDefaultBase to start with.
 - 2: Server Configuration data, and functions manipulating it.
 - 3: Pengine action type
 - 4: Helper functions for what we need for the requests.
 - -}

{- Begin Header Information -}
data HeaderBase = HeaderBase{ userAgent :: String
                            , contentType :: String
                            , accept :: String
                            , acceptLanguage :: String} deriving (Show)

headerDefault :: HeaderBase
headerDefault = HeaderBase{     userAgent      = "HaskellPengine"
                              , accept         = "application/json; charset=UTF-8"
                              , acceptLanguage = "en-us,en;q=0.5"
                              , contentType    = "application/x-prolog"}


{- Used in pengineCreate function -}
createHeader :: HeaderBase -> HeaderBase
createHeader h = h {accept = "application/json"}

{- End header information. -}

{- Begin server configuration information. -}
data ServerConfiguration = ServerConfiguration {
    serverBase :: String,
    pengineId :: Maybe String
    } deriving (Show)

setBasicServerPath :: String -> ServerConfiguration
setBasicServerPath url = ServerConfiguration{serverBase=url, pengineId=Nothing}

{- Helper function to create a path from an action and serverConfiguration. -}
makePath :: PengineAction -> ServerConfiguration -> String
makePath act server =
    case act of
        Create  -> "/pengine/create"
        _       -> "/pengine/send/?format=json&id=" ++ justpenid
    where justpenid = fromJust $ pengineId server

{- End server configuration -}

{- PengineActions -}
data PengineAction = Create | Query String | Destroy | Next | Stop deriving (Show)
{-End PengineActions.-}

{- TODO: Remove Prolog types to another file. -}
{- Tabularize this -- commands? -}
data PrologTerms = PAtom String | PNumber Scientific | PList [PrologTerms] | PTrue Bool | PFalse Bool | PNull | PNameValueList (HM.Map String PrologTerms) | PTerm (String, [PrologTerms])

{-encodeProlog :: Object -> HM.Map Text PrologTerms-}
