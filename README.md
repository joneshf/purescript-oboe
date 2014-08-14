# Module Documentation

## Module Network.Oboe

### Types

    data JSON :: *

    data Oboe :: *

    data OboeEff :: !

    type OboeOptions  = { withCredentials :: Boolean, cached :: Boolean, body :: String, headers :: [Header], method :: Verb, url :: String }


### Type Class Instances

    instance showJSON :: Show JSON


### Values

    done :: forall eff result. Oboe -> (JSON -> Eff eff result) -> Eff (oboe :: OboeEff | eff) Oboe

    fail :: forall eff e r result. Oboe -> ({ jsonBody :: JSON, body :: String, statusCode :: Number, thrown :: Eff (err :: Exception | eff) result } -> Eff eff result) -> Eff (oboe :: OboeEff | eff) Oboe

    node :: forall eff r result. Oboe -> String -> (JSON -> [String] -> [{  | r }] -> Eff eff result) -> Eff (oboe :: OboeEff | eff) Oboe

    oboe :: forall eff. OboeOptions -> Eff (oboe :: OboeEff | eff) Oboe

    oboeGet :: forall eff. String -> Eff (oboe :: OboeEff | eff) Oboe

    oboeOptions :: OboeOptions

    path :: forall eff r result. Oboe -> String -> (JSON -> [String] -> [{  | r }] -> Eff eff result) -> Eff (oboe :: OboeEff | eff) Oboe

    start :: forall eff r result. Oboe -> (Number -> {  | r } -> Eff eff result) -> Eff (oboe :: OboeEff | eff) Oboe



