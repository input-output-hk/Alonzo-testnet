
import           Prelude

import           Cardano.Api
import           Cardano.Api.Shelley

import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Plutus.V1.Ledger.Api as Plutus

import           Cardano.PlutusExample.HelloWorld
                   (helloWorldSerialised,
                    helloWorldSBS)

main :: IO ()
main =
  args <- getArgs
  let scriptnum = 42
  let scriptname = "untyped-always-succeeds-txin.plutus"
  writePlutusScript scriptnum scriptname helloWorldSerialised helloWorldSBS
  

writePlutusScript scriptnum scriptname scripts scriptSBS
  do
  case Plutus.defaultCostModelParams of
        Just m ->
          let Alonzo.Data pData = toAlonzoData (ScriptDataNumber scriptnum)
              (logout, e) = Plutus.evaluateScriptCounting Plutus.Verbose m scriptSBS [pData]
          in do print ("Log output" :: String) >> print logout
                case e of
                  Left evalErr -> print ("Eval Error" :: String) >> print evalErr
                  Right exbudget -> print ("Ex Budget" :: String) >> print exbudget
        Nothing -> error "defaultCostModelParams failed"
  result <- writeFileTextEnvelope filename Nothing apiScript
  case result of
    Left err -> print $ displayError err
    Right () -> return ()
