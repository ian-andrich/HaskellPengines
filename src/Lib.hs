{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module Lib (lib, PengineConnection, CreateResponse) where
import              Data.Maybe (fromJust)
import              Data.Text
import              GHC.Generics
import              Data.Aeson                  as Aes
import              Network.HTTP.Simple
import qualified    Data.ByteString.Char8       as C
import qualified    Data.ByteString.Lazy.Char8  as L
import qualified    Data.ByteString             as B
import              Control.Monad
import qualified    Data.Text.Lazy.IO           as T
import qualified    Data.Text.Lazy.Encoding     as T
import              Data.Text.Internal.Lazy     as LazyText

data Body = Body String String
instance ToJSON Body where
    toJSON (Body format json_type) = object
        ["format" .= format
        , "json" .= json_type]

data CreateResponse = CreateResponse {
    event :: String,
    id :: String,
    slave_limit :: String}
    deriving (Show, Generic)

instance FromJSON CreateResponse where
    parseJSON = withObject "createresponse" $ \o -> do
        event <- o .: "event"
        id <- o .: "age"
        slave_limit <- o .: "slave_limit"
        return CreateResponse{..}
instance ToJSON CreateResponse

lib = do
        request' <- parseRequest "POST http://localhost:4242"

        let request
                = setRequestPath "/pengine/create"
                $ setRequestQueryString []
                $ setRequestBodyJSON body
                $ setRequestSecure False
                $ setRequestHeader "Content-type" ["application/json"]
                $ setRequestHeader "User-Agent" ["HaskellPengine"]
                $ setRequestHeader "Accept" ["application/json"]
                $ setRequestHeader "Accept-Language" ["en-us,en;q=0.5"]
                $ request'
        response <- httpLBS request
        Prelude.putStrLn $ "The status code was: " ++ show (getResponseStatusCode response)
        print $ getResponseHeader "Content-Type" response
        L.putStrLn $ getResponseBody response

        {-return $ fmap Aes.decode $ T.encodeUtf8 $ getResponseBody response-}
        return $ getResponseBody response

    where body = object ["format" .= String "json"]

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
