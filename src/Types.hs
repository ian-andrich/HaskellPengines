module Types where

import qualified     Data.HashMap                       as HM

{-TODO: Finish this.-}
data HeaderBase = HeaderBase{userAgent :: String
                            , contentType :: String
                            , accept :: String
                            , acceptLanguage :: String} deriving (Show)

data Header      = HeaderBase{userAgent = "HaskellPengine"
, accept         = "application/x-prolog; charset=UTF-8"
, acceptLanguage = "en-us,en;q          = 0.5"
, contentType    = "application/json"}

{-Refactor this to headers file?-}
createHeader :: Header -> Header
createHeader h = h {accept = "application/json"}

{-Don't think this is necessary.-}
{-data PrologTypesBase = PrologTypes{list :: [a]} deriving (Show)-}

{- Tabularize this -- commands? -}
data PrologTerms = PAtom String | PNumber Num | PList [PrologTerms] | PTrue Bool | PFalse Bool | PNull | PNameValueList HM.Map | PTerm (String, [PrologTerms])
