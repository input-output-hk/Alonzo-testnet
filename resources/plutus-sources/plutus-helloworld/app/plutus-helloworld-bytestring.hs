
import           Prelude
import           System.Environment

import           Cardano.Api
import           Cardano.Api.Shelley

import           Data.Aeson (encode)
import qualified Data.ByteString.Short as SBS

import qualified Plutus.V1.Ledger.Api as Plutus

import           PlutusTx.Prelude as P (ByteString)

import           Cardano.PlutusExample.HelloWorldByteStringParametric (hello, helloWorldSBS, helloWorldSerialised)

main :: IO ()
main = do
  args <- getArgs
  let nargs = length args
  let scriptname = if nargs > 1 then args!!1 else  "result.plutus"
  putStrLn $ "Writing output to: " ++ scriptname
  writePlutusScript hello scriptname helloWorldSerialised helloWorldSBS



writePlutusScript :: P.ByteString -> FilePath -> PlutusScript PlutusScriptV1 -> SBS.ShortByteString -> IO ()
writePlutusScript datum filename scriptSerial scriptSBS =
  do
  case Plutus.defaultCostModelParams of
        Just m ->
          let pData = Plutus.toData datum
              (logout, e) = Plutus.evaluateScriptCounting Plutus.Verbose m scriptSBS [pData]
          in do print ("Log output" :: String) >> print logout
                case e of
                  Left evalErr -> print ("Eval Error" :: String) >> print evalErr
                  Right exbudget -> print ("Ex Budget" :: String) >> print exbudget
                print $ "Datum value: " <> encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData pData)
        Nothing -> error "defaultCostModelParams failed"
  result <- writeFileTextEnvelope filename Nothing scriptSerial
  case result of
    Left err -> print $ displayError err
    Right () -> return ()
