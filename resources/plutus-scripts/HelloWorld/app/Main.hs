module Main where

import qualified Compile
import           HelloWorld
--import qualified HelloWorld2

main :: IO ()
--main = putStrLn "Hello, Haskell!"
--main = Compile.main
main = print $ Compile.convert $ helloworldCompiled

