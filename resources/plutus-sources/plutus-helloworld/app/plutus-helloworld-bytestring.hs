import           Prelude
import           System.Environment

import           Cardano.Api
import           Cardano.Api.Shelley

import           Data.Aeson (encode)

import qualified Plutus.V1.Ledger.Api as Plutus

import           Cardano.PlutusExample.HelloWorldByteStringParametric (hello, helloWorldSerialised)

main :: IO ()
main = do
  args <- getArgs
  let nargs = length args
  let scriptname = if nargs > 0 then args!!0 else  "HelloWorldBS.plutus"
  print $ "Datum value: " <> encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ Plutus.toData hello)
  result <- writeFileTextEnvelope scriptname Nothing helloWorldSerialised
  case result of
    Left err -> print $ displayError err
    Right () -> print scriptname
