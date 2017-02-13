{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Lib (pengineConnect, PengineConnection, CreateResponse, test) where
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
import              Types

data Body = Body String String
instance ToJSON Body where
    toJSON (Body format json_type) = object
        ["format" .= format
        , "json" .= json_type]

data CreateResponse = CreateResponse {
    event :: String,
    _data :: Maybe String,
    pengine_id :: String,
    slave_limit :: Integer}
    deriving Show

instance FromJSON CreateResponse where
    parseJSON = withObject "createresponse" $ \o -> do
        event <- o .: "event"
        _data <- o .:? "data"
        pengine_id <- o .: "id"
        slave_limit <- o .: "slave_limit"
        return CreateResponse{..}

pengineConnect :: String -> IO ServerConfiguration
pengineConnect url = do
        request' <- parseRequest $ "POST " ++ url

        let request
                = setRequestPath "/pengine/create"
                $ setRequestQueryString []
                $ setRequestBodyJSON body
                $ setRequestSecure False
                $ setRequestHeader "Content-type" ["application/x-prolog; charset=UTF-8"]
                $ setRequestHeader "User-Agent" ["HaskellPengine"]
                $ request'

        response <- httpLBS request
        let responsebody = getResponseBody response
        let jsonBody = decode responsebody :: Maybe CreateResponse
        let pid = pengine_id $ fromJust $ jsonBody :: String
        return ServerConfiguration {serverBase = url, pengineId = Just pid}
    where   body = object ["format" .= String "json"]

pengineAsk serverConf query = do
    request' <- parseRequest $ "POST " ++ (serverBase serverConf)
    let requestPath = "/pengine/send?format=json&id=" ++ (fromJust $ pengineId serverConf)
    let request
            = setRequestPath (C.pack requestPath)
            $ setRequestSecure False
            $ setRequestHeader "Content-type" ["application/x-prolog"]
            $ setRequestHeader "User-Agent" ["HaskellPengine"]
            $ setRequestHeader "Accept" ["application/json"]
            $ setRequestHeader "Accept-Language" ["en-us,en;q=0.5"]
            $ setRequestBodyLBS (L.pack ("ask(" ++ query ++ ",[])."))
            $ request'

    response <- httpLBS request
    return response

pengineNext :: ServerConfiguration -> IO (Response ByteString)
pengineNext serverConf = do
    request' <- parseRequest $ "POST " ++ (serverBase serverConf)
    let requestPath = "/pengine/send?format=json&id=" ++ (fromJust $ pengineId serverConf)
    let request
            = setRequestPath (C.pack requestPath)
            {-$ setRequestQueryString []-}
            $ setRequestSecure False
            $ setRequestHeader "Content-type" ["application/x-prolog"]
            $ setRequestHeader "User-Agent" ["HaskellPengine"]
            $ setRequestHeader "Accept" ["application/json"]
            $ setRequestHeader "Accept-Language" ["en-us,en;q=0.5"]
            $ setRequestBodyLBS (L.pack "next.") request'

    response <- httpLBS request
    return response

pengineStop :: ServerConfiguration -> IO (Response ByteString)
pengineStop serverConf = do
    request' <- parseRequest $ "POST " ++ (serverBase serverConf)
    let requestPath = "/pengine/send?format=json&id=" ++ (fromJust $ pengineId serverConf)
    let request
            = setRequestPath (C.pack requestPath)
            {-$ setRequestQueryString []-}
            $ setRequestSecure False
            $ setRequestHeader "Content-type" ["application/x-prolog"]
            $ setRequestHeader "User-Agent" ["HaskellPengine"]
            $ setRequestHeader "Accept" ["application/json"]
            $ setRequestHeader "Accept-Language" ["en-us,en;q=0.5"]
            $ setRequestBodyLBS (L.pack "stop.") request'

    response <- httpLBS request
    return response

test = do
    serverConfig <- pengineConnect "http://localhost:4242/"
    let query = "member(X, [1,2,3])"
    bs <- pengineAsk serverConfig query
    pengineNext serverConfig
    pengineNext serverConfig
    pengineStop serverConfig

data PengineConnection = PengineConnection {pengineresponse :: Response L.ByteString, request :: Request} deriving (Show)


buildURL :: String -> PengineAction -> String
buildURL server Create                          = fixServer server ++ "create/"
buildURL server (Query _)                       = fixServer server ++ "send/"
buildURL server Destroy                         = fixServer server ++ "destroy/"

fixServer :: String -> String
fixServer server    | server == ""                      = ""
                    | Prelude.tail server == "/"        = server
                    | otherwise                         = server ++ "/"
