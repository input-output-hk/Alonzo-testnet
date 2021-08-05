{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.PlutusExample.LockingScript
  ( apiExampleUntypedPlutusLockingScript
  , untypedLockingScriptAsShortBs
  ) where

import           Prelude hiding (($))

import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)

import           Codec.Serialise
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy  as LBS

import qualified Plutus.V1.Ledger.Scripts as Plutus
import qualified PlutusTx
import           PlutusTx.Prelude hiding (Semigroup (..), unless)

{-# INLINABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator _ _ _ = ()

validator :: Plutus.Validator
validator = Plutus.mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])

script :: Plutus.Script
script = Plutus.unValidatorScript validator

untypedLockingScriptAsShortBs :: SBS.ShortByteString
untypedLockingScriptAsShortBs =  SBS.toShort . LBS.toStrict $ serialise script

apiExampleUntypedPlutusLockingScript :: PlutusScript PlutusScriptV1
apiExampleUntypedPlutusLockingScript = PlutusScriptSerialised untypedLockingScriptAsShortBs

