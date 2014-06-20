module Network.Oboe
  ( Oboe()
  , OboeOptions(..)
  , oboe
  , oboeGet
  ) where

  import Control.Monad.Eff

  import Data.Either

  import Network.HTTP

  foreign import data Oboe :: !

  foreign import oboe_
    "var oboe_;\
    \try {\
    \  oboe_ = require('oboe');\
    \} catch (e) {\
    \  oboe_ = window.oboe;\
    \}" :: Unit

  foreign import oboe
    "function oboe(obj) {\
    \  function() {\
    \    return oboe({\
    \      uri: obj.uri,\
    \      method: showVerb(obj.method),\
    \      headers: showHeaders(obj.headers),\
    \      body: showBody(obj.body),\
    \      cached: obj.cached,\
    \      withCredentials: obj.withCredentials\
    \    });\
    \  }\
    \}" :: forall eff response. OboeOptions -> Eff (oboe :: Oboe | eff) response

  type Body = forall r. Either String { | r}
  type OboeOptions =
    { url :: String
    , method :: Verb
    , headers :: [Header]
    , body :: Body
    , cached :: Boolean
    , withCredentials :: Boolean
    }

  oboeOptions :: OboeOptions
  oboeOptions =
    { url: ""
    , method: GET
    , headers: []
    , body: Left ""
    , cached: true
    , withCredentials: false
    }

  oboeGet :: forall eff response. String -> Eff (oboe :: Oboe | eff) response
  oboeGet url = oboe oboeOptions{url = url}

  -- FFI helpers
  showVerb :: Verb -> String
  showVerb = show
  showHeaders :: [Header] -> String
  showHeaders = show
  showBody :: Body -> String
  showBody = either id showObj

  foreign import showObj
    "var showObj = JSON.stringify" :: forall r. { | r} -> String
