{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Lib (lib, PengineConnection, CreateResponse) where
import              Data.Maybe (fromJust)
import              Data.Text
import              GHC.Generics
import              Data.Aeson                  as Aes
import              Data.Aeson.Types
import              Network.HTTP.Simple
import qualified    Data.ByteString.Char8       as C
import qualified    Data.ByteString.Lazy.Char8  as L
import qualified    Data.ByteString             as B
import              Control.Monad
import qualified    Data.Text.Lazy.IO           as T
import qualified    Data.Text.Lazy.Encoding     as T
import              Data.Text.Internal.Lazy     as LazyText
import              Data.ByteString.Lazy.Internal   as LazyByte

data Body = Body String String
instance ToJSON Body where
    toJSON (Body format json_type) = object
        ["format" .= format
        , "json" .= json_type]

data CreateResponse = CreateResponse {
    event :: String,
    pengine_id :: String,
    slave_limit :: Integer}
    deriving (Show)

instance FromJSON CreateResponse where
    parseJSON = withObject "createresponse" $ \o -> do
        event <- o .: "event"
        pengine_id <- o .: "id"
        slave_limit <- o .: "slave_limit"
        return CreateResponse{..}

lib :: IO String
lib = do
        request' <- parseRequest "POST http://localhost:4242"

        let request
                = setRequestPath "/pengine/create"
                $ setRequestQueryString []
                $ setRequestBodyJSON body
                $ setRequestSecure False
                $ setRequestHeader "Content-type" ["application/x-prolog; charset=UTF-8"]
                $ setRequestHeader "User-Agent" ["HaskellPengine"]
                {-$ setRequestHeader "Accept" ["application/json"]-}
                {-$ setRequestHeader "Accept-Language" ["en-us,en;q=0.5"]-}
                $ request'
        
        response <- httpLBS request
        Prelude.putStrLn $ "The status code was: " ++ show (getResponseStatusCode response)
        print $ getResponseHeader "Content-Type" response
        L.putStrLn $ getResponseBody response
        let responsebody = getResponseBody response
        return $ pengine_id $ fromJust $ decode responsebody :: IO String
    where   body = object ["format" .= String "json"]


pengineAsk :: String -> IO (Response ByteString)
pengineAsk pengine_id = do
    request' <- parseRequest "POST http://localhost:4242"
    {-_id <- pengine_id-}
    {-let requestPath = "/pengine/send?format=json&id=" ++ _id-}
    let requestPath = "/pengine/send?format=json&id=" ++ pengine_id
    let request 
            = setRequestPath (C.pack requestPath)
            {-$ setRequestQueryString []-}
            $ setRequestSecure False
            $ setRequestHeader "Content-type" ["application/x-prolog"]
            $ setRequestHeader "User-Agent" ["HaskellPengine"]
            $ setRequestHeader "Accept" ["application/json"]
            $ setRequestHeader "Accept-Language" ["en-us,en;q=0.5"]
            $ setRequestBodyLBS (L.pack ("ask(" ++ "member(X, [1,2,3])" ++ ",[])."))
            $ request'

    response <- httpLBS request
    Prelude.putStrLn $ "The Status code was: " ++ show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    L.putStrLn $ getResponseBody response
    return response

pengineNext :: String -> IO (Response ByteString)
pengineNext pengine_id = do
    request' <- parseRequest "POST http://localhost:4242"
    let requestPath = "/pengine/send?format=json&id=" ++ pengine_id
    let request 
            = setRequestPath (C.pack requestPath)
            {-$ setRequestQueryString []-}
            $ setRequestSecure False
            $ setRequestHeader "Content-type" ["application/x-prolog"]
            $ setRequestHeader "User-Agent" ["HaskellPengine"]
            $ setRequestHeader "Accept" ["application/json"]
            $ setRequestHeader "Accept-Language" ["en-us,en;q=0.5"]
            $ setRequestBodyLBS (L.pack ("next."))
            $ request'

    response <- httpLBS request
    Prelude.putStrLn $ "The Status code was: " ++ show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    L.putStrLn $ getResponseBody response
    return response

test = do
    pid <- lib
    pengineAsk pid 
    pengineNext pid
    pengineNext pid

data PengineConnection = PengineConnection {pengineresponse :: Response L.ByteString,
                                            request :: Request} deriving (Show)

data PengineAction = Create | Query String | Destroy

buildURL :: String -> PengineAction -> String
buildURL server Create                          = fixServer server ++ "create/"
buildURL server (Query _)                       = fixServer server ++ "send/"
buildURL server Destroy                         = fixServer server ++ "destroy/"

fixServer :: String -> String
fixServer server    | server == ""                      = ""
                    | Prelude.tail server == "/"        = server
                    | otherwise                 = server ++ "/"
