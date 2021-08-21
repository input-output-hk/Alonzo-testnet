import           Prelude
import           System.Environment
import           Cardano.Api
import           Cardano.PlutusExample.AlwaysFails (alwaysFailsScript)

main :: IO ()
main = do
  args <- getArgs
  let nargs = length args
  let scriptname = if nargs > 0 then args!!0 else  "AlwaysFails.plutus"
  result <- writeFileTextEnvelope scriptname Nothing alwaysFailsScript
  case result of
    Left err -> print $ displayError err
    Right () -> print scriptname
