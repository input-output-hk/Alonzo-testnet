import           Prelude
import           System.Environment
import           Cardano.Api
import           Cardano.PlutusExample.MintingScriptPurple (apiExamplePlutusMintingScriptPurple)

main :: IO ()
main = do
  args <- getArgs
  let nargs = length args
  let scriptname = if nargs > 0 then args!!0 else  "MintingPolicyPurple.plutus"
  result <- writeFileTextEnvelope scriptname Nothing apiExamplePlutusMintingScriptPurple
  case result of
    Left err -> print $ displayError err
    Right () -> print scriptname
