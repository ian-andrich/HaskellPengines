{-# LANGUAGE OverloadedStrings #-}
module Lib (lib) where
import              Data.Aeson
import              Network.HTTP.Simple
import qualified    Data.ByteString.Char8       as S8
import qualified    Data.Yaml                   as Yaml
import qualified    Data.ByteString.Lazy.Char8  as L8
import qualified    Data.ByteString

data Body = Body String String
instance ToJSON Body where
    toJSON (Body format json_type) = object
        ["format" .= format
        , "json" .= json_type]


lib :: IO ()
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
    L8.putStrLn $ getResponseBody response
    where body = object ["format" .= String "json"]
