import           Prelude
import           System.Environment
import           Cardano.Api
import           Cardano.PlutusExample.HelloWorld (helloWorldSerialised)

main :: IO ()
main = do
  args <- getArgs
  let nargs = length args
  let scriptname = if nargs > 0 then args!!0 else "HelloWorldInt.plutus"
  result <- writeFileTextEnvelope scriptname Nothing helloWorldSerialised
  case result of
    Left err -> print $ displayError err
    Right () -> print scriptname
