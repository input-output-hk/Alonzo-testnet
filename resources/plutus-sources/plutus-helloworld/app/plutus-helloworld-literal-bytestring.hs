import           Prelude
import           Data.String
import           System.Environment

import           Cardano.Api

import qualified Plutus.V1.Ledger.Api as Plutus

import qualified Data.ByteString.Short as SBS

import           Cardano.PlutusExample.HelloWorldLiteralByteString (helloWorldSBS, helloWorldSerialised)

main :: IO ()
main = do
  args <- getArgs
  let nargs = length args
  let scriptData = if nargs > 0 then head args else "Hello World!"
  let scriptname = if nargs > 1 then args!!1 else  "result.plutus"
  putStrLn $ "Writing output to: " ++ scriptname ++ " with scriptData " ++ scriptData
  writePlutusScript (fromString scriptData) scriptname helloWorldSerialised helloWorldSBS

writePlutusScript :: Plutus.BuiltinByteString -> FilePath -> PlutusScript PlutusScriptV1 -> SBS.ShortByteString -> IO ()
writePlutusScript scriptData filename scriptSerial scriptSBS =
  do
  case Plutus.defaultCostModelParams of
        Just m ->
          let (logout, e) =
                Plutus.evaluateScriptCounting
                    Plutus.Verbose
                    m
                    scriptSBS
                    [Plutus.toData scriptData]
          in do print ("Log output" :: String) >> print logout
                case e of
                  Left evalErr -> print ("Eval Error" :: String) >> print evalErr
                  Right exbudget -> print ("Ex Budget" :: String) >> print exbudget
        Nothing -> error "defaultCostModelParams failed"
  result <- writeFileTextEnvelope filename Nothing scriptSerial
  case result of
    Left err -> print $ displayError err
    Right () -> return ()
