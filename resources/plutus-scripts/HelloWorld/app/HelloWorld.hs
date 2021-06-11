{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}

module HelloWorld where

import qualified Plutus.V1.Ledger.Scripts as P
import qualified PlutusTx                 as P
import qualified PlutusTx.Prelude         as P


{-
  The "hello world" message as a data item
-}

hello :: P.Data
hello = P.I 0x48656c6c6f20576f726c64210a

helloworld :: P.Data -> P.Data -> P.Data -> ()
helloworld datum redeemer _ = if datum P.== hello then () else (P.error ())

helloworldCompiled = $$(P.compile [|| helloworld ||])
