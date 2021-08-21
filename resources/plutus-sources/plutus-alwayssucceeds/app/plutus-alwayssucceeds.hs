import           Prelude
import           System.Environment
import           Cardano.Api
import           Cardano.PlutusExample.AlwaysSucceeds (alwaysSucceedsScript)

main :: IO ()
main = do
  args <- getArgs
  let nargs = length args
  let scriptname = if nargs > 0 then args!!0 else  "AlwaysSucceeds.plutus"
  result <- writeFileTextEnvelope scriptname Nothing alwaysSucceedsScript
  case result of
    Left err -> print $ displayError err
    Right () -> print scriptname
