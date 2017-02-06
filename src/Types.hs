module Types where

import qualified    Data.HashMap                        as HM
import qualified    Data.Vector                         as V
import              Data.Aeson                          as Aes
import              Data.Aeson.Types
import              Data.Scientific                     (Scientific)
import              Data.Maybe                          (fromJust)

data HeaderBase = HeaderBase{ userAgent :: String
                            , contentType :: String
                            , accept :: String
                            , acceptLanguage :: String} deriving (Show)

headerDefault :: HeaderBase
headerDefault = HeaderBase{     userAgent      = "HaskellPengine"
                              , accept         = "application/json; charset=UTF-8"
                              , acceptLanguage = "en-us,en;q=0.5"
                              , contentType    = "application/x-prolog"}

data URI = URI {
    serverBase :: String,
    pengineId :: Maybe String,
    action :: PengineAction} deriving (Show)

{-makeURI :: URI -> String-}
{-makeURI URI = do-}
    {-base <- serverBase URI-}
    {-pid <- pengineID-}

makePath :: PengineAction -> URI -> String
makePath act uri =
    case act of
        Create  -> "/pengine/create"
        _       -> "/pengine/send/?format=json&id=" ++ justpenid
    where justpenid = fromJust $ pengineId uri

data PengineAction = Create | Query String | Destroy | Next | Stop deriving (Show)

{-Refactor this to headers file?-}
createHeader :: HeaderBase -> HeaderBase
createHeader h = h {accept = "application/json"}

{- Tabularize this -- commands? -}
data PrologTerms = PAtom String | PNumber Scientific | PList [PrologTerms] | PTrue Bool | PFalse Bool | PNull | PNameValueList (HM.Map String PrologTerms) | PTerm (String, [PrologTerms])

{-encodeProlog :: Object -> HM.Map Text PrologTerms-}
