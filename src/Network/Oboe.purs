module Network.Oboe
  ( OboeEff()
  , Oboe()
  , OboeOptions(..)
  , oboeOptions
  , JSON()
  , oboe
  , oboeGet
  , node
  , path
  , done
  , start
  , fail
  ) where

  import Control.Monad.Eff
  import Control.Monad.Eff.Exception

  import Data.Either
  import Data.Function

  import Network.HTTP

  foreign import data OboeEff :: !
  foreign import data Oboe :: *
  foreign import data JSON :: *

  instance showJSON :: Show JSON where
    show = showJSONImpl

  foreign import showJSONImpl
    "function showJSONImpl(json) {\
    \  if (toString.call(json).slice(8, -1) === 'String') {\
    \    return json;\
    \  } else {\
    \    return JSON.stringify(json);\
    \  }\
    \}" :: JSON -> String

  foreign import node
    "function node(o) {\
    \  return function(n) {\
    \    return function(f) {\
    \      return function() {\
    \        return o.node(n, function(x,y,z){return f(x)(y)(z)();});\
    \      }\
    \    }\
    \  }\
    \}" :: forall eff r result
        .  Oboe
        -> String
        -> (JSON -> [String] -> [{ | r}] -> Eff eff result)
        -> Eff (oboe :: OboeEff | eff) Oboe

  foreign import path
    "function path(o) {\
    \  return function(n) {\
    \    return function(f) {\
    \      return function() {\
    \        return o.path(n, function(x,y,z){return f(x)(y)(z)();});\
    \      }\
    \    }\
    \  }\
    \}" :: forall eff r result
        .  Oboe
        -> String
        -> (JSON -> [String] -> [{ | r}] -> Eff eff result)
        -> Eff (oboe :: OboeEff | eff) Oboe

  foreign import done
    "function done(o) {\
    \  return function(f) {\
    \    return function() {\
    \      return o.done(function(x){return f(x)();});\
    \    }\
    \  }\
    \}" :: forall eff result
        .  Oboe
        -> (JSON -> Eff eff result)
        -> Eff (oboe :: OboeEff | eff) Oboe

  foreign import start
    "function start(o) {\
    \  return function(f) {\
    \    return function() {\
    \      return o.start(function(x,y){return f(x)(y)();});\
    \    }\
    \  }\
    \}" :: forall eff r result
        .  Oboe
        -> (Number -> { | r} -> Eff eff result)
        -> Eff (oboe :: OboeEff | eff) Oboe

  foreign import fail
    "function fail(o) {\
    \  return function(f) {\
    \    return function() {\
    \      return o.fail(function(x){return f(x)();});\
    \    }\
    \  }\
    \}" :: forall eff e r result
        .  Oboe
        ->  ( { thrown :: Eff (err :: Exception e | eff) result
              , statusCode :: Number
              , body :: String
              , jsonBody :: JSON
              }
              -> Eff eff result
            )
        -> Eff (oboe :: OboeEff | eff) Oboe

  foreign import oboe_
    "var oboe_;\
    \try {\
    \  oboe_ = require('oboe');\
    \} catch (e) {\
    \  oboe_ = window.oboe;\
    \}" :: Oboe

  foreign import oboe
    "function oboe(obj) {\
    \  return function() {\
    \      var headers = {};\
    \      obj.headers.map(header2Obj).forEach(function(header) {\
    \        headers[header.head] = header.value;\
    \      });\
    \      return oboe_({\
    \        url: obj.url,\
    \        method: showVerb(obj.method),\
    \        headers: headers,\
    \        body: obj.body,\
    \        cached: obj.cached,\
    \        withCredentials: obj.withCredentials\
    \      });\
    \  }\
    \}" :: forall eff. OboeOptions -> Eff (oboe :: OboeEff | eff) Oboe

  type OboeOptions =
    { url :: String
    , method :: Verb
    , headers :: [Header]
    , body :: String
    , cached :: Boolean
    , withCredentials :: Boolean
    }

  oboeOptions :: OboeOptions
  oboeOptions =
    { url: ""
    , method: GET
    , headers: []
    , body: ""
    , cached: true
    , withCredentials: false
    }

  oboeGet :: forall eff. String -> Eff (oboe :: OboeEff | eff) Oboe
  oboeGet url = oboe oboeOptions{url = url}

  -- FFI helpers
  header2Obj :: Header -> {head :: String, value :: String}
  header2Obj (Header head value) = {head: show head, value: value}
  showVerb :: Verb -> String
  showVerb = show
  mkFn1_ = mkFn1
  mkFn2_ = mkFn2
  mkFn3_ = mkFn3
