# Module Documentation

## Module Network.Oboe

### Types

    data Oboe :: !

    type OboeOptions  = { withCredentials :: Boolean, cached :: Boolean, body :: Body, headers :: [Header], method :: Verb, url :: String }


### Values

    oboe :: forall eff response. OboeOptions -> Eff (oboe :: Oboe | eff) response

    oboeGet :: forall eff response. String -> Eff (oboe :: Oboe | eff) response



